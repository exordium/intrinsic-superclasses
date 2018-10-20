{-# language TemplateHaskell,QuasiQuotes #-}
{-# language UndecidableInstances #-}

-- | Live example source of how to use 'instances' quasiquoter. Try it by running:
--
-- >>> cabal new-build example --ghc-options="-ddump-splices"
module Example where
import Prelude hiding (Semigroup(..),Monoid(..))
import Quasi.Instances

class Semigroup a where mappend :: a -> a -> a
class Semigroup a => Commutative a
class Semigroup a => Monoid a where mempty :: a
class Monoid a => Group a where inverse :: a -> a
class (Commutative a, Group a) => CommutativeGroup a

return [] -- hack to bring above definitions into scope. Avoid by defining in a seperate module.


[instances| Num a => CommutativeGroup a where
   mempty = fromInteger 0
   mappend a b = a + b
   inverse = negate
   |]
--   ======>
{-
instance Num a => CommutativeGroup a
instance Num a => Commutative a
instance Num a => Group a where
  inverse = negate
instance Num a => Monoid a where
  mempty = fromInteger 0
instance Num a => Semigroup a where
  mappend a b = a + b
-}
