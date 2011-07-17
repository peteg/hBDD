-------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.BF
-- Copyright   :  (C) 2002-2005, 2009 University of New South Wales, (C) 2009-2011 Peter Gammie
-- License     :  LGPL (see COPYING.LIB for details)
-------------------------------------------------------------------
module Data.Boolean.BF
    (
    -- * A data-structure based instance of 'Boolean'.
    	BF(..)
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Data.Boolean

-------------------------------------------------------------------

-- | An abstract syntax tree-ish instance of the 'Boolean' interface,
-- sometimes useful for debugging.
--
-- Note the 'Eq' instance is /not/ semantic equality.
data BF = BFtrue
        | BFfalse
        | BFvar String
        | BF `BFand` BF
        | BF `BFor` BF
        | BF `BFxor` BF
--        | BFite BF BF BF
        | BF `BFimplies` BF
        | BF `BFiff` BF
        | BFneg BF
        | BFexists [BF] BF
        | BFforall [BF] BF
        | BFsubst [(BF, BF)] BF
          deriving (Eq, Show)

instance BooleanVariable BF where
    bvar = BFvar
    unbvar (BFvar v) = v
    unbvar _ = error $ "BF.unbvar: not a variable."

instance Boolean BF where
    false = BFfalse
    true = BFtrue
    (/\) = BFand
    (\/) = BFor
    xor = BFxor
--    ite = BFite
    (-->) = BFimplies
    (<->) = BFiff
    neg = BFneg

instance QBF BF where
    data Group BF = MkGroup { unMkGroup :: [BF] }
    mkGroup = MkGroup
    exists = BFexists . unMkGroup
    forall = BFforall . unMkGroup

instance Substitution BF where
    data Subst BF = MkBFpair [(BF, BF)]
    mkSubst = MkBFpair
    substitute (MkBFpair s) = BFsubst s
