-------------------------------------------------------------------
-- |
-- Module      :  Relation
-- Copyright   :  (C)opyright [2002..2005], 2009 UNSW
-- License     :  LGPL (see COPYING.LIB for details)
--
-- Author      :  Peter Gammie
-- Maintainer  :  peteg42 at gmail dot com
--
-- Extend a "Boolean" representation to support relations.
-------------------------------------------------------------------
module Data.Boolean.Relation
    (
        BinaryRelation
    ,	mkBinaryRelation
    ,	getCharFn

    -- * Operations.
    ,	transitiveClosure
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import Data.Boolean

-------------------------------------------------------------------
-- | A 'BinaryRelation' is its 'Boolean' characteristic function
-- together with the variables representing the domain and range sets.
-------------------------------------------------------------------
data Boolean b => BinaryRelation b = MkBR
    {
     charFn :: b,
     domain :: [b],
     range :: [b]
    }

mkBinaryRelation :: Boolean b =>
		    [b]		-- ^ Domain
		 -> [b]		-- ^ Range
		 -> b		-- ^ Characteristic function
		 -> BinaryRelation b
mkBinaryRelation d r cf
    | length d == length r = MkBR {charFn = cf, domain = d, range = r}
    | otherwise            = error $ "mkBinaryRelation: length xs != length ys"

getCharFn :: Boolean b => BinaryRelation b -> b
getCharFn = charFn

-------------------------------------------------------------------
-- Operations on 'BinaryRelation's.
-------------------------------------------------------------------

-- | Computes the transitive closure of a relation @r@:
--
-- @'fix' false (\r' -> r' \\\/ (rel_product zs r[zs\/ran(r)] r[zs\/(dom(r)])@
transitiveClosure :: BooleanOps b
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
