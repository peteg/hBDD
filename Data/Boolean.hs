-------------------------------------------------------------------
-- |
-- Module      :  Boolean
-- Copyright   :  (C)opyright [2002..2005], 2009 UNSW
-- License     :  LGPL (see COPYING.LIB for details)
--
-- Author      :  Peter Gammie
-- Maintainer  :  peteg42 at gmail dot com
--
-- An interface to libraries supporting efficient manipulation of Boolean
-- functions. It was originally based on:
--
--  \"Logical Abstractions in Haskell\" Nancy A. Day, John Launchbury
--   and Jeff Lewis, Haskell Workshop, Paris, October 1999.
--
-- but has evolved significantly.
--
-- Note the use of 'neg' rather than @not@ to avoid a clash with
-- @Prelude.not@.
--
-- Note that an instance of 'Boolean' must be an instance of 'Eq'. This
-- instance is expected to provide /semantic/ equality (as provided by a
-- BDD package).
--
-- FIXME improve docs.
-------------------------------------------------------------------
module Data.Boolean
    (
     -- * Abstract interface.
    	BooleanConstant(..)
    ,	BooleanVariable(..)
    ,	Boolean(..)
    ,	QBF(..)
    ,	Substitution(..)
    ,	BooleanOps(..)

    -- * Lazy boolean operators.
    ,	(/\), nand, (\/), nor, xor, xnor
    ,	(?), (==>), (<=>), neg

    -- * Higher-order (derived) boolean operators.
    ,	conjoin
    ,	disjoin
    ,	fix
    ,	fix2

    -- * Convert a boolean formula to a displayable form.
    ,	RenderBool(..)
    ,	sop

    -- * A data-structure based instance of 'Boolean'.
    ,	BF(..)

    -- * BDD-specific operations.
    ,	ReorderingMethod(..)
    ) where

-------------------------------------------------------------------
-- Type classes.
-------------------------------------------------------------------

-- | Boolean constants.
class (Eq b, Show b) => BooleanConstant b where
    false :: b
    true :: b

-- | Boolean variables.
class BooleanVariable b where
    -- | Injects objects of type @String@ into the 'Boolean' representation
    -- type @b@.
    bvar :: String -> b
    -- | Introduces a variable \'adjacent\' to an existing one. What this
    -- means is implementation-defined (it may reduce to 'bvar'), but the
    -- intention is to be able to do the classic (current, next)-state
    -- variable pairing optimisation.
    bvarAdj :: String -> b -> b
    bvarAdj label b  = b `seq` bvar label

    -- | Reverse mapping.
    unbvar :: b -> String

-- | The overloaded Boolean operations proper. Provides defaults for
-- operations with obvious expansions, such as 'xnor'. A minimal instance
-- should define 'bAND' and 'bNEG'.
--
-- These functions are expected to be /strict/; lazy variants
-- are provided by '/\', etc.
class BooleanConstant b => Boolean b where
    bAND :: b -> b -> b
    bNEG :: b -> b

    bNAND :: b -> b -> b
    x `bNAND` y = bNEG (x `bAND` y)
    bOR :: b -> b -> b
    x `bOR` y = bNEG $ (bNEG x) `bAND` (bNEG y)
    bNOR :: b -> b -> b
    x `bNOR` y = bNEG (x `bOR` y)
    bXOR :: b -> b -> b
    x `bXOR` y = (x `bOR` y) `bAND` (bNEG (x `bAND` y))
    bXNOR :: b -> b -> b
    x `bXNOR` y = bNEG (x `bXOR` y)
    -- | If-then-else.
    bITE :: b -> b -> b -> b
    bITE i t e = (i `bAND` t) `bOR` ((bNEG i) `bAND` e)
    -- | Implication.
    bIMP :: b -> b -> b
    x `bIMP` y = (bNEG x) `bOR` y
    -- | If-and-only-if.
    bIFF :: b -> b -> b
    x `bIFF` y = (x `bIMP` y) `bAND` (y `bIMP` x)

-- | Quantified Boolean Formulae (QBF) operations.
class (Boolean b, BooleanVariable b) => QBF b where
    data Group b :: *
    mkGroup :: [b] -> Group b
    exists :: Group b -> b -> b
    forall :: Group b -> b -> b
    -- | Computes the relational product of two 'Boolean' formulas:
    --
    -- @exists qvars (f \/\\ g)@
    rel_product :: Group b -> b -> b -> b
    rel_product qvars g h = exists qvars (g /\ h)

-- | Substitutions.
class (Boolean b, BooleanVariable b) => Substitution b where
    data Subst b :: *

    -- | Builds a new substitution. The arguments are
    -- @(Variable, Formula)@ pairs.
    mkSubst :: [(b, b)] -> Subst b

    -- | Substitutes variables for variables in a 'Boolean' formula.
    -- Note that it is the user's responsibility to ensure the
    -- @Formula@s in the substitution are in fact 'BDD' variables, and
    -- that the domain and range do not overlap.
    rename :: Subst b -> b -> b
    rename = substitute

    -- | Substitutes formulas for variables in a 'Boolean' formula.
    substitute :: Subst b -> b -> b

-- | Operations only provided by efficient (BDD) representations.
class (QBF b, Substitution b) => BooleanOps b where
    -- | Extract the variable labelling the topmost node in /f/.
    bif :: b	-- ^ /f/
	-> b
    -- | Extract the this-node-false-branch of a /f/.
    belse :: b	-- ^ /f/
	  -> b
    -- | Extract the this-node-true-branch of a /f/.
    bthen :: b	-- ^ /f/
	  -> b

    -- | Returns a BDD which agrees with /f/ for all valuations for which
    -- /g/ is true, and which is hopefully smaller than /f/.
    reduce :: b		-- ^ /f/
	   -> b		-- ^ /g/ (a /care set/)
	   -> b

    -- | Finds a satisfying variable assignment for /f/.
    satisfy :: b	-- ^ /f/
	    -> b

-------------------------------------------------------------------
-- Lazy versions of the type-class members.
-- The "optimisations" may or may not be such, but they make reading
-- a 'BF' structure a lot easier.
-------------------------------------------------------------------
infixl 7 /\
infixl 7 \/
infixl 7 `nand`
infixl 7 `nor`
infixl 4 <=>
infixr 4 ==>
infixl 4 `xor`

(/\) :: Boolean b => b -> b -> b
x /\ y
   | x == false = false
   | x == true  = y
   | y == false = false
   | y == true  = x
   | otherwise  = x `bAND` y
{-# INLINE (/\) #-}

nand :: Boolean b => b -> b -> b
x `nand` y
    | x == false = true
    | x == true  = neg y
    | y == false = true
    | y == true  = neg x
    | otherwise = x `bNAND` y
{-# INLINE nand #-}

(\/) :: Boolean b => b -> b -> b
x \/ y
    | x == false = y
    | x == true  = true
    | y == false = x
    | y == true  = true
    | otherwise  = x `bOR` y
{-# INLINE (\/) #-}

nor :: Boolean b => b -> b -> b
x `nor` y
    | x == false = neg y
    | x == true  = false
    | y == false = neg x
    | y == true  = false
    | otherwise  = x `bNOR` y
{-# INLINE nor #-}

xor :: Boolean b => b -> b -> b
x `xor` y
    | x == false = y
    | x == true  = neg y
    | y == false = x
    | y == true  = neg x
    | otherwise  = x `bXOR` y
{-# INLINE xor #-}

xnor :: Boolean b => b -> b -> b
xnor = bXNOR
{-# INLINE xnor #-}

(?) :: Boolean b => b -> (b, b) -> b
i ? (t, e)
    | i == false = e
    | i == true  = t
    | otherwise  = bITE i t e
{-# INLINE (?) #-}

(==>) :: Boolean b => b -> b -> b
x ==> y
    | x == false = true
    | otherwise  = x `bIMP` y
{-# INLINE (==>) #-}

(<=>) :: Boolean b => b -> b -> b
(<=>) = bIFF
{-# INLINE (<=>) #-}

neg :: Boolean b => b -> b
neg = bNEG
{-# INLINE neg #-}

-------------------------------------------------------------------
-- Higher-level combinators.
-------------------------------------------------------------------

-- | Forms the Big Conjunction of a list of 'Boolean' formulas.
conjoin :: Boolean b => [b] -> b
conjoin = foldr (/\) true
{-# INLINE conjoin #-}

-- | Forms the Big Disjunction of a list of 'Boolean' formulas.
disjoin :: Boolean b => [b] -> b
disjoin = foldr (\/) false
{-# INLINE disjoin #-}

-- | Compute the fixpoint of a "Boolean" function.
fix :: Eq b => b -> (b -> b) -> b
fix s0 f = loop s0
    where
      loop s =
        let s' = f s
         in if s == s'
	   then s
	   else loop s'

-- | "fix" with state.

fix2 :: Eq b => a -> b -> (a -> b -> (a, b)) -> (a, b)
fix2 a0 s0 f = loop (a0, s0)
    where
      loop as@(a, s) =
        let as'@(_a', s') = f a s
        in if s == s'
          then as
	  else loop as'

-------------------------------------------------------------------
-- | Render a 'Boolean' type as a sum-of-products. This was stolen
-- lock-stock from David Long's calculator example.
-------------------------------------------------------------------

sop :: (BooleanOps b, RenderBool a) => b -> (a -> a)
sop f0
    | f0 == true  = rbTrue
    | f0 == false = rbFalse
    | otherwise   = sop' f0
    where sop' f = let outside = neg f
		       cube = satisfy f
		       f' = f /\ (neg cube)
	            in (printCube $ reduce cube (outside \/ cube)) . 
		       (if f' == false
			  then id
			  else rbOr . (sop' f'))

	  printCube cube = let v        = bif cube
			       cubeThen = bthen cube
			       cubeElse = belse cube
			       cubeNext = if cubeThen == false
					    then cubeElse
					    else cubeThen
			    in (if v /\ cube == false
				  then rbNeg
				  else id)        .
			       (rbVar $ unbvar v) .
			       (if cubeNext == true
				  then id
				  else rbAnd . printCube cubeNext)
{-# SPECIALIZE sop :: BooleanOps b => b -> (String -> String) #-}

-- | A class for the text constants and operators used by 'sop'.
class RenderBool a where
    rbTrue  :: a -> a
    rbFalse :: a -> a
    rbVar   :: String -> a -> a
    rbAnd   :: a -> a
    rbOr    :: a -> a
    rbNeg   :: a -> a

instance RenderBool String where
    rbTrue  = showString "True"
    rbFalse = showString "False"
    rbVar   = showString
    rbAnd   = showString " & "
    rbOr    = showString " | "
    rbNeg   = showString "~"

-------------------------------------------------------------------
-- | An abstract syntax tree-ish instance of the 'Boolean' interface,
-- useful for debugging: shows the order in which operations are performed.
-- Note the 'Eq' instance is /not/ semantic equality.
-------------------------------------------------------------------
data BF = BFtrue
	| BFfalse
	| BFvar String
	| BF `BFand` BF
	| BF `BFor` BF
	| BF `BFxor` BF
	| BFite BF BF BF
	| BF `BFimplies` BF
	| BF `BFiff` BF
	| BFneg BF
	| BFexists [BF] BF
	| BFforall [BF] BF
	| BFsubst [(BF, BF)] BF
	  deriving (Eq, Show)

instance BooleanConstant BF where
    false = BFfalse
    true = BFtrue

instance BooleanVariable BF where
    bvar = BFvar
    unbvar (BFvar v) = v
    unbvar _ = error $ "BF.unbvar: not a variable."

instance Boolean BF where
    bAND = BFand
    bOR = BFor
    bXOR = BFxor
    bITE = BFite
    bIMP = BFimplies
    bIFF = BFiff
    bNEG = BFneg

instance QBF BF where
    data Group BF = MkGroup { unMkGroup :: [BF] }
    mkGroup = MkGroup
    exists = BFexists . unMkGroup
    forall = BFforall . unMkGroup

instance Substitution BF where
    data Subst BF = MkBFpair [(BF, BF)]
    mkSubst = MkBFpair
    substitute (MkBFpair s) = BFsubst s

-------------------------------------------------------------------
-- Overload the constants for Bool and String.
-------------------------------------------------------------------
instance BooleanConstant Bool where
    false = False
    true = True

instance Boolean Bool where
    bAND = (&&)
    bOR  = (||)

    True  `bXOR` True  = False
    False `bXOR` False = False
    _     `bXOR` _     = True

    bITE i t e = if i then t else e
    x `bIMP` y = (not x) || y
    x `bIFF` y = if x then y else neg y
    bNEG = not

instance BooleanConstant String where
    false = "False"
    true = "True"

-------------------------------------------------------------------
-- BDD-specific operations.
-------------------------------------------------------------------

-- | What kind of reordering should we use?
data ReorderingMethod = ReorderSift | ReorderStableWindow3
			deriving (Eq, Ord, Show)

