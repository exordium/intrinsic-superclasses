module Quasi.Instances.Internal where
import Language.Haskell.TH
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Control.Monad.Writer
import Data.Foldable


-- | Implements the @instances@ quasiquoter ast transform
splitInstances :: Dec -> DecsQ
splitInstances = \case
  InstanceD Nothing ctx (AppT (ConT className) instancesFor) instanceMethods -> do
    instanceMethods' <- M.fromList <$> traverse globalizeDef instanceMethods
    superclasses <- getTransitiveSuperclassNames className

    superclassMethods <- fold <$> M.traverseWithKey (\k _ -> getClassMethods k) superclasses
    let badMethods = filter (\x -> not $ S.member x superclassMethods) $ M.keys instanceMethods'
    unless (null badMethods) $
      error $ "splitInstances: Trying to declare methods not in the superclass heirarchy\n"
           ++ unlines (map show badMethods)

    superclassHasInstance <- M.traverseWithKey (\k _ -> isInstance k [instancesFor]) superclasses
    let superclasses' = M.filterWithKey (\k _ -> not $ superclassHasInstance M.! k) superclasses
    classOps <- getClassOps instanceMethods superclasses'
    let classDefs = M.map (\names -> (instanceMethods' M.!) `S.map` names) classOps
    let instanceDecls = M.foldrWithKey (\c ms -> (declInstance ctx c instancesFor ms :)) [] classDefs
    pure instanceDecls
  d -> error $ "splitInstances: Not an instance declaration\n" ++ pprint d
  where
    declInstance ctx className targetType ms = InstanceD Nothing ctx (AppT (ConT className) targetType) (S.toList ms)
    -- Associate a definition with its toplevel qualified identifier
    globalizeDef d = (lookupValueName . nameBase . defName) d >>= \case
        Nothing -> error $ "globalizeDef: instance method " ++ show (nameBase (defName d)) ++ " not in scope"
        Just n -> pure (n,d)
    
-- | Create a Map of className to method declaration from a list of instance method definitions
getClassOps :: Traversable t => t Dec -> Map ParentName (Set Name) -> Q (Map ParentName (Set Name))
getClassOps decs superclasses = collectFromList S.insert superclasses <$> mapM (\d -> opClass <$> reify (defName d)) decs
  where
    opClass (ClassOpI n _t p) = (p,n)
    opClass x = error $ "opClass: not a class operation\n" ++ pprint x

-- | Get the name of a function or value declaration
defName :: Dec -> Name
defName x = case x of
  FunD n _ -> n
  ValD (VarP n) _ _ -> n
  d -> error $ "defName: Declaration is not a Function or Value definition\n" ++ pprint d
sigName :: Dec -> Name
sigName = \case
  SigD n _ -> n
  d -> error $ "sigName: Declaration is not a type signature\n" ++ pprint d


collectFromList :: (Ord k, Foldable t) => (a -> as -> as) -> Map k as -> t (k,a) -> Map k as
collectFromList f m0 x = foldr (\(k,a) -> M.adjust (f a) k) m0 x

-- | reify the names of the direct superclasses for a class name
getSuperclassNames :: Name -> Q [Name]
getSuperclassNames className = do
  ClassI (ClassD ctx _  (S.fromList . map _TyVarBndr_name -> classVars) _ _) _ <- reify className
  let
    -- if t represents a supeclass of n then `superclass t` is Just the superclass name, and Nothing otherwise
    superclass :: Type -> Maybe Name
    superclass = \case
      AppT t (VarT v) | S.member v classVars -> Just $ headAppT t
      AppT ConT{} _ -> Nothing
      AppT t _ -> superclass t
      x -> error $ show x
  pure $ mapMaybe superclass ctx
  where
    _TyVarBndr_name = \case {PlainTV n -> n; KindedTV n _ -> n}
    headAppT :: Type -> Name -- project the innermost @ConT@ in a chain of @AppT@
    headAppT = \case
      ConT n -> n
      AppT t _ -> headAppT t
      x -> error $ "headAppT: Malformed type\n" ++ show x

getClassMethods :: Name -> Q (Set Name)
getClassMethods className = reify className <&> (\(ClassI (ClassD _ _ _ _ (map sigName -> methods)) _) -> S.fromList methods)

-- | reify the names of all transitive superclasses for a class name, including itself
getTransitiveSuperclassNames :: Name -> Q (Map Name (Set a))
getTransitiveSuperclassNames = execWriterT . go where
  go n = do
    tell $ M.singleton n S.empty
    traverse_ go =<< lift (getSuperclassNames n)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
