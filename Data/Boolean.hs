-------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean
-- Copyright   :  (C) 2002-2005, 2009 University of New South Wales, (C) 2009-2011 Peter Gammie
-- License     :  LGPL (see COPYING.LIB for details)
--
-- An interface to libraries supporting efficient manipulation of
-- Boolean functions, such as BDDs. It is an evolution of @Logical
-- Abstractions in Haskell@ by Nancy A. Day, John Launchbury and Jeff
-- Lewis, Haskell Workshop, Paris, October 1999.
--
-- The purity of this interface may make it difficult to predict when
-- BDDs actually get constructed.
--
-- Note the use of 'neg' rather than @not@ to avoid a clash with
-- @Prelude.not@.
-------------------------------------------------------------------
module Data.Boolean
    (
     -- * Abstract interface.
        BooleanVariable(..)
    ,	Boolean(..)
    ,	QBF(..)
    ,	Substitution(..)

    -- * Derived boolean operations.
    ,	(<--)
    ,	conjoin
    ,	disjoin
    ,	fix
    ,	fix2

    -- * BDD-specific operations.
    ,	BDDOps(..)
    ,	ReorderingMethod(..)
    ,	RenderBool(..)
    ,	sop
    ,	countPaths
    ) where

-------------------------------------------------------------------
-- Type classes.
-------------------------------------------------------------------

-- | The operators have similar fixities and associativies to the
-- standard boolean operators, but at higher precedence (they bind
-- more strongly).
infixr 8 `xor`, `nand`, `nor`
infixr 7 /\
infixr 6 \/
infixr 5 <->, -->
infixl 5 <--

-- | The overloaded Boolean operations proper. Provides defaults for
-- operations with obvious expansions, such as 'nand'. A minimal
-- instance should define '(/\)' and 'neg'.
class Boolean b where
  false :: b
  true :: b

  (/\) :: b -> b -> b
  neg :: b -> b

  nand :: b -> b -> b
  x `nand` y = neg (x /\ y)
  (\/) :: b -> b -> b
  x \/ y = neg $ (neg x) /\ (neg y)
  nor :: b -> b -> b
  x `nor` y = neg (x \/ y)
  xor :: b -> b -> b
  x `xor` y = (x \/ y) /\ (neg (x /\ y))
  -- | If-then-else FIXME
  -- (?) :: b -> b -> b -> b
  -- i ? (t, e) = (i /\ t) \/ (neg i /\ e)
  -- | Implication
  (-->) :: b -> b -> b
  x --> y = (neg x) \/ y
  -- | If-and-only-if is exclusive nor.
  (<->) :: b -> b -> b
  x <-> y = neg (x `xor` y)

-- | Reverse implication
(<--) :: Boolean b => b -> b -> b
x <-- y = y --> x

instance Boolean Bool where
  false = False
  true = True
  x /\ y = x && y
  neg = not

-- | Boolean variables.
class BooleanVariable b where
    -- | A single variable.
    bvar :: String -> b
    -- | A set of variables, notionally \'adjacent\'. What this means
    -- is implementation-defined, but the intention is to support
    -- (current, next)-state variable pairing.
    bvars :: [String] -> [b]
    bvars = map bvar

    -- | Reverse mapping.
    unbvar :: b -> String

-- | Quantified Boolean Formulae (QBF) operations.
class (Boolean b, BooleanVariable b) => QBF b where
    -- | The type of aggregations of 'BooleanVariable's.
    --
    -- Some BDD packages have efficient (reusable) variable aggregation structures.
    data Group b :: *

    -- | Construct aggregations.
    mkGroup :: [b] -> Group b
    -- | Existentially quantify out a given set of variables.
    exists :: Group b -> b -> b
    -- | Universally quantify out a given set of variables.
    forall :: Group b -> b -> b
    -- | Computes the relational product of two 'Boolean' formulas:
    --
    -- @rel_product qvars f g = exists qvars (f \/\\ g)@
    rel_product :: Group b -> b -> b -> b
    rel_product qvars f g = exists qvars (f /\ g)

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

-- BDD-specific operations.

-- | Operations provided by BDD representations.
--
-- Note that the 'Eq' instance is expected to provide /semantic/
-- equality on boolean functions, as is typical of BDD packages.
class (Eq b, QBF b, Substitution b) => BDDOps b where
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

    -- | Finds the set of variables that /f/ depends on.
    support :: b	-- ^ /f/
            -> [b]

-- | BDD libraries tend to include some kind of variable reordering
-- heuristics. These are some common ones.
data ReorderingMethod = ReorderSift | ReorderStableWindow3
                        deriving (Eq, Ord, Show)

-------------------------------------------------------------------

-- | A class for the text constants and operators used by 'sop'.
class RenderBool a where
    rbTrue   :: a
    rbFalse  :: a
    rbVar    :: String -> a
    rbAnd    :: a
    rbOr     :: a
    rbNeg    :: a

    rbEmpty  :: a
    rbConcat :: a -> a -> a

instance RenderBool ShowS where
    rbTrue   = showString "True"
    rbFalse  = showString "False"
    rbVar    = showString
    rbAnd    = showString " & "
    rbOr     = showString " | "
    rbNeg    = showString "~"

    rbEmpty  = id
    rbConcat = (.)

-- | Render a 'Boolean' type as a sum-of-products. This was stolen
-- lock-stock from David Long's calculator example.
sop :: (BDDOps b, RenderBool a) => b -> a
sop f0
    | f0 == true  = rbTrue
    | f0 == false = rbFalse
    | otherwise   = sop' f0
    where sop' f = let outside = neg f
                       cube = satisfy f
                       f' = f /\ neg cube
                    in (printCube $ reduce cube (outside \/ cube)) `rbConcat`
                       (if f' == false
                          then rbEmpty
                          else rbOr `rbConcat` (sop' f'))
          printCube cube = let v        = bif cube
                               cubeThen = bthen cube
                               cubeElse = belse cube
                               cubeNext = if cubeThen == false
                                            then cubeElse
                                            else cubeThen
                            in (if v /\ cube == false
                                  then rbNeg
                                  else rbEmpty)   `rbConcat`
                               (rbVar $ unbvar v) `rbConcat`
                               (if cubeNext == true
                                  then rbEmpty
                                  else rbAnd `rbConcat` printCube cubeNext)
{-# SPECIALIZE sop :: BDDOps b => b -> ShowS #-}

-------------------------------------------------------------------

-- | Count the number of paths in a BDD leading to 'true'.
countPaths :: (BDDOps b) => b -> Integer
countPaths f0
  | f0 == true  = 1
  | f0 == false = 0
  | otherwise   = countPaths (bthen f0) + countPaths (belse f0)
