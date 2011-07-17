-------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.Relation
-- Copyright   :  (C) 2002-2005, 2009 University of New South Wales, (C) 2009-2011 Peter Gammie
-- License     :  LGPL (see COPYING.LIB for details)
--
-- Represent "Boolean" relations as their characteristic functions
-- paired with the variables representing their domain and codomain.
-------------------------------------------------------------------
module Data.Boolean.Relation
    (
        BinaryRelation
    ,	mkBinaryRelation
    ,	charFn

    -- * Operations
    ,	transitiveClosure
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Data.Boolean

-------------------------------------------------------------------

-- | A 'BinaryRelation' is its 'Boolean' characteristic function
-- together with the variables representing the domain and range sets.
data Boolean b => BinaryRelation b = MkBR
    {
     charFn :: b, -- ^ Extract the characteristic function of a relation.
     domain :: [b],
     range :: [b]
    }

-- | Construct a binary relation.
mkBinaryRelation :: Boolean b =>
		    [b]		-- ^ Domain
		 -> [b]		-- ^ Range
		 -> b		-- ^ Characteristic function
		 -> BinaryRelation b
mkBinaryRelation d r cf
    | length d == length r = MkBR {charFn = cf, domain = d, range = r}
    | otherwise            = error $ "mkBinaryRelation: length xs != length ys"

-------------------------------------------------------------------
-- Operations on 'BinaryRelation's.
-------------------------------------------------------------------

-- | Computes the transitive closure of a relation @r@:
--
-- @transitiveClosure r = 'fix' false (\r' -> r' \\\/ (rel_product zs r[zs\/ran(r)] r[zs\/(dom(r)])@
--
-- assuming that @domain@ and @range@ have the same length and @zs@
-- are fresh.
transitiveClosure :: BDDOps b
                  => BinaryRelation b -> BinaryRelation b
transitiveClosure (MkBR {charFn = r, domain = xs, range = ys}) =
    MkBR {charFn = fix r f,
	  domain = xs,
	  range = ys}
    where f r' = r' \/ (rel_product gzs rzsys rzsxs)
	  rzsys = rename (mkSubst $ zip ys zs) r
	  rzsxs = rename (mkSubst $ zip xs zs) r
	  zs = [ bvar $ "transitiveClosureZ" ++ show i | i <- [1..length xs] ]
	  gzs = mkGroup zs
