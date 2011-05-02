-- -*- haskell -*-- -----------------------------------------------
-- |
-- Module      :  Data.Boolean.CUDD
-- Copyright   :  (C) 2002-2005, 2009 University of New South Wales, (C) 2009-2011 Peter Gammie
-- License     :  LGPL (see COPYING.LIB for details)
--
-- A binding for CUDD (Fabio Somenzi, University of Colorado).
--
-- Note this library is not thread-safe.
-------------------------------------------------------------------
module Data.Boolean.CUDD
    (
    	BDD
    ,	module Data.Boolean

        -- * CUDD-specific functions
-- 	    ,	gc
    ,	stats
    ,	dynamicReOrdering
    ,	varIndices
    ,	bddSize
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

#include "cudd_im.h"

import Control.Monad	( liftM, zipWithM_ )

import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
import Data.Map         ( Map )
import qualified Data.Map as Map

import Foreign
import Foreign.C

import GHC.ForeignPtr   ( newConcForeignPtr )

import System.IO	( Handle, hIsReadable, hIsWritable )
-- import System.Mem	( performGC )
import System.Posix.IO	( handleToFd )

import Data.Boolean

-------------------------------------------------------------------
-- Extra FFI functions.
-------------------------------------------------------------------

-- | A C file handle.
{#pointer *FILE -> CFile#}

-- | Convert a Haskell Handle into a C FILE *.
-- - FIXME: throw exception on error.
-- - suggested by Simon Marlow.
handleToCFile :: Handle -> IO FILE
handleToCFile h =
    do r <- hIsReadable h
       w <- hIsWritable h
       modestr <- newCString $ mode r w
       fd <- handleToFd h
       {#call unsafe fdopen#} (fromIntegral $ toInteger fd) modestr
    where mode :: Bool -> Bool -> String
	  mode False False = error "Handle not readable or writable!"
	  mode False True  = "w"
	  mode True  False = "r"
	  mode True  True  = "r+" -- FIXME

-- | The Haskell Handle is unusable after calling 'handleToCFile',
-- so provide a neutered "fprintf"-style thing here.
printCFile :: FILE -> String -> IO ()
printCFile file str =
    withCString str ({#call unsafe fprintf_neutered#} file)

--  Close a C FILE *.
-- - FIXME: throw exception on error.
-- closeCFile :: FILE -> IO ()
-- closeCFile cfile = do {#call unsafe fclose#} cfile
-- 		      return ()

cToNum :: (Num i, Integral e) => e -> i
cToNum  = fromIntegral . toInteger

cFromEnum :: (Integral i, Enum e) => e -> i
cFromEnum  = fromIntegral . fromEnum

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | The BDD manager, which we hide from clients.
{#pointer *DdManager as DDManager newtype#}

-- | The arguments to 'Cudd_Init'.
{#enum CuddSubtableSize {} #}
{#enum CuddCacheSize {} #}

-- | Variable reordering tree options.
{#enum CuddMTRParams {} #}

-- | The abstract type of BDDs.
{#pointer *DdNode as BDD foreign newtype#}

withBDD :: BDD -> (Ptr BDD -> IO a) -> IO a

-- Belt-and-suspenders equality checking and ordering.
instance Eq BDD where
    bdd0 == bdd1 = unsafePerformIO $
      withBDD bdd0 $ \bdd0p -> withBDD bdd1 $ \bdd1p ->
        return $ bdd0p == bdd1p

instance Ord BDD where
    bdd0 <= bdd1 = unsafePerformIO $
      withBDD bdd0 $ \bdd0p -> withBDD bdd1 $ \bdd1p ->
        return $ bdd0p == bdd1p || bdd0p < bdd1p

instance Show BDD where
-- Print out the BDD as a sum-of-products.
    showsPrec _ = sop

-------------------------------------------------------------------
-- Administrative functions.
-------------------------------------------------------------------

-- | The BDD manager. This needs to be invoked before any of the
-- following functions... which we arrange for automagically.
ddmanager :: DDManager
ddmanager = unsafePerformIO $
  {#call unsafe Cudd_Init as _cudd_Init #}
    0 0 (cFromEnum UNIQUE_SLOTS) (cFromEnum CACHE_SLOTS) 0
{-# NOINLINE ddmanager #-}

-- | A map to and from BDD variable numbers.
-- FIXME: get the right numeric type here.
type VarMap = (Map String CInt, Map CInt String)

-- | Tracks existing variables.
bdd_vars :: IORef VarMap
bdd_vars = unsafePerformIO $ newIORef (Map.empty, Map.empty)
{-# NOINLINE bdd_vars #-}

-- | Attaches a finalizer to a BDD object.
-- The call to "Cudd_Ref" happens in C to ensure atomicity.
-- Returning objects with initial refcount 0 is a bad design decision.
addBDDfinalizer :: Ptr BDD -> IO BDD
addBDDfinalizer bddp = liftM BDD $ newConcForeignPtr bddp bddf
    where bddf = {#call unsafe Cudd_RecursiveDeref as _cudd_RecursiveDeref#} ddmanager bddp
                   >> return ()
{-# INLINE addBDDfinalizer #-}

-- | Attaches a null finalizer to a BDD object.
-- Used for variables and constants.
-- The call to "Cudd_Ref" should be innocuous as the manual claims refcounts
-- on these objects are always > 0. FIXME verify.
addBDDnullFinalizer :: Ptr BDD -> IO BDD
addBDDnullFinalizer bddp =
    do {#call unsafe Cudd_Ref as _cudd_Ref#} bddp
       liftM BDD $ newForeignPtr_ bddp
{-# INLINE addBDDnullFinalizer #-}

-- Simulate an "apply" function to simplify atomic refcount incrementing.
{#enum CuddBinOp {} #}

-------------------------------------------------------------------
-- Logical operations.
-------------------------------------------------------------------

instance BooleanConstant BDD where
    true  = unsafePerformIO $
              {#call unsafe Cudd_ReadOne as _cudd_ReadOne#} ddmanager
                >>= addBDDnullFinalizer
    false = unsafePerformIO $
              {#call unsafe Cudd_ReadLogicZero as _cudd_ReadLogicZero#} ddmanager
		>>= addBDDnullFinalizer

instance BooleanVariable BDD where
    bvar label = unsafePerformIO $
      snd `liftM` newVar label ({#call unsafe Cudd_bddNewVar as _cudd_bddNewVar#} ddmanager)

    bvarPair (l, l') = unsafePerformIO $
     do (vid,  v)  <- newVar l  ({#call unsafe Cudd_bddNewVar as _cudd_bddNewVar#} ddmanager)
        (vid', v') <- newVar l' ({#call unsafe Cudd_bddNewVar as _cudd_bddNewVar#} ddmanager)
--        (vid', v') <- newVar l' (allocAfter vid)

        -- FIXME group vid vid'
        -- putStrLn $ "bvarPair: " ++ show (l, l') ++ " -> " ++ show (vid, vid')

--         when (vid' - vid == 1) $
--           do putStrLn ">> Grouping vars"
--              let uvid = (fromIntegral . toInteger) vid
--              makeTreeNode ddmanager uvid 2 (cFromEnum CUDD_MTR_DEFAULT)
--              return ()

        return (v, v')
     where
        allocAfter vid =
	  do level <- readPerm ddmanager (cToNum (vid :: Integer))
	     bddp' <- newVarAtLevel ddmanager (level + 1)
	     return bddp'

	readPerm = {#call unsafe Cudd_ReadPerm as _cudd_ReadPerm#}
        makeTreeNode = {#call unsafe Cudd_MakeTreeNode as _cudd_MakeTreeNode#}
	newVarAtLevel = {#call unsafe Cudd_bddNewVarAtLevel as _cudd_bddNewVarAtLevel#}


{-
    -- FIXME: get the variable grouping right.
    -- Are the variable groups adjusted when new variables are added using
    -- newVarAtLevel? - I can't find anything in the CUDD source code that
    -- indicates they are.
    -- This code does the "adjacent" thing, but doesn't group variables.
    bvarPair (l, l') = unsafePerformIO $ newVar l l' allocAfter
        where

--  		 makeTreeNode ddmanager vid 2 (cFromEnum CUDD_MTR_DEFAULT)
--  		 vid' <- withBDD bdd readIndex
-- 		 putStrLn $ "TreeNode: alloc 2 at vid = " ++ show vid ++ " vid' = " ++ show vid'
-- 		 vid <- withBDD bdd {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#}
-- 		 level <- {#call unsafe Cudd_ReadPerm as _cudd_ReadPerm#} ddmanager ((fromIntegral . toInteger) vid)
--                  vid' <- {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#} bddp
-- 		 level' <- {#call unsafe Cudd_ReadPerm as _cudd_ReadPerm#} ddmanager ((fromIntegral . toInteger) vid')
-- 		 putStrLn $ "bvarAdj.alloc: old: " ++ show bdd ++ "(" ++ show level ++ ") new: " ++ label  ++ "(" ++ show level' ++ ")"


	  readIndex = {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#}
-}

    unbvar bdd = unsafePerformIO $ bdd `seq`
		   withBDD bdd $ \bddp ->
		     do vid <- {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#} bddp
			(_, fromBDD) <- readIORef bdd_vars
			return $ case ((fromIntegral . toInteger) vid) `Map.lookup` fromBDD of
			           Nothing -> "(VID: " ++ show vid ++ ")"
				   Just v  -> v

newVar :: String -> IO (Ptr BDD) -> IO (CInt, BDD)
newVar label allocVar =
      do (toBDD, fromBDD) <- readIORef bdd_vars
         case label `Map.lookup` toBDD of
           Just vid ->
             {#call unsafe Cudd_bddIthVar as _cudd_bddIthVar#} ddmanager vid
               >>= addBDDnullFinalizer >>= \v -> return (vid, v)
           Nothing ->
             do putStrLn $ "Allocating for: " ++ label
                bddp <- allocVar
		vid <- {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#} bddp
                let svid = (fromIntegral . toInteger) vid
		--putStrLn $ label ++ " -> " ++ (show (vid, bdd))
		writeIORef bdd_vars ( Map.insert label svid  toBDD
                                    , Map.insert svid  label fromBDD )
		addBDDnullFinalizer bddp >>= \v -> return (svid, v)

instance Boolean BDD where
    bAND  = bddBinOp AND
    bNAND = bddBinOp NAND
    bOR   = bddBinOp OR
    bNOR  = bddBinOp NOR
    bXOR  = bddBinOp XOR
    bXNOR = bddBinOp XNOR

    bITE i t e = unsafePerformIO $
      withBDD i $ \ip -> withBDD t $ \tp -> withBDD e $ \ep ->
        {#call unsafe cudd_bddIte#} ddmanager ip tp ep
          >>= addBDDfinalizer

    x `bIFF` y = bITE x y (bNEG y)

    bNEG x = unsafePerformIO $
      withBDD x $ \xp ->
        {#call unsafe cudd_bddNot#} xp
          >>= addBDDfinalizer

bddBinOp :: CuddBinOp -> BDD -> BDD -> BDD
bddBinOp op x y = unsafePerformIO $
  withBDD x $ \xp -> withBDD y $ \yp ->
    {#call unsafe cudd_BinOp#} ddmanager (cFromEnum op) xp yp >>= addBDDfinalizer

instance QBF BDD where
    data Group BDD = MkGroup BDD

    mkGroup = MkGroup . conjoin
    exists = cudd_exists
    forall = cudd_forall
    rel_product = cudd_rel_product

instance Substitution BDD where
    data Subst BDD = MkSubst [(BDD, BDD)]

    mkSubst = MkSubst
    rename = cudd_rename
    substitute = error "CUDD substitute" -- cudd_substitute

instance BooleanOps BDD where
    bif bdd = unsafePerformIO $
      do vid <- withBDD bdd {#call unsafe Cudd_NodeReadIndex as _cudd_NodeReadIndex#}
         {#call unsafe Cudd_bddIthVar as _cudd_bddIthVar#} ddmanager ((fromIntegral . toInteger) vid)
           >>= addBDDnullFinalizer
    bthen bdd = unsafePerformIO $
      withBDD bdd $ \bddp ->
        {#call unsafe cudd_bddT#} bddp >>= addBDDfinalizer
    belse bdd = unsafePerformIO $
      withBDD bdd $ \bddp ->
        {#call unsafe cudd_bddE#} bddp >>= addBDDfinalizer

    reduce = cudd_reduce
    satisfy = cudd_satisfy

-------------------------------------------------------------------
-- Implementations.
-------------------------------------------------------------------

-- FIXME move
withGroup :: Group BDD -> (Ptr BDD -> IO a) -> IO a
withGroup (MkGroup g) = withBDD g

cudd_exists :: Group BDD -> BDD -> BDD
cudd_exists group bdd = unsafePerformIO $ bdd `seq`
  withGroup group $ \groupp -> withBDD bdd $ \bddp ->
    {#call unsafe cudd_bddExistAbstract#} ddmanager bddp groupp
      >>= addBDDfinalizer

cudd_forall :: Group BDD -> BDD -> BDD
cudd_forall group bdd = unsafePerformIO $ bdd `seq`
  withGroup group $ \groupp -> withBDD bdd $ \bddp ->
    {#call unsafe cudd_bddUnivAbstract#} ddmanager bddp groupp
      >>= addBDDfinalizer

cudd_rel_product :: Group BDD -> BDD -> BDD -> BDD
cudd_rel_product group f g = unsafePerformIO $
  withGroup group $ \groupp -> withBDD f $ \fp -> withBDD g $ \gp ->
    {#call unsafe cudd_bddAndAbstract#} ddmanager fp gp groupp
      >>= addBDDfinalizer

-- | FIXME: This function is a bit touchier than you'd hope.
-- The reaons is we use cudd_bddSwapVariables, which in fact swaps variables.
-- This is not exactly a "rename" behaviour.
cudd_rename :: Subst BDD -> BDD -> BDD
cudd_rename (MkSubst subst) f = unsafePerformIO $
  withBDD f $ \fp ->
    allocaArray len $ \arrayx -> allocaArray len $ \arrayx' ->
      do zipWithM_ (pokeSubst arrayx arrayx') subst [0..]
         bddp <- {#call unsafe cudd_bddSwapVariables#} ddmanager fp arrayx arrayx' (fromIntegral len)
         mapM_ (\ (BDD v, BDD v') -> do touchForeignPtr v
		                        touchForeignPtr v') subst
         addBDDfinalizer bddp
    where
      pokeSubst :: Ptr (Ptr BDD) -> Ptr (Ptr BDD) -> (BDD, BDD) -> Int -> IO ()
      pokeSubst arrayx arrayx' (v, v') i =
        do withBDD v  $ pokeElemOff arrayx  i
           withBDD v' $ pokeElemOff arrayx' i

      len = length subst

-- FIXME verify implementation.
cudd_reduce :: BDD -> BDD -> BDD
cudd_reduce f g = unsafePerformIO $
  withBDD f $ \fp -> withBDD g $ \gp ->
    {#call unsafe cudd_bddLICompaction#} ddmanager fp gp
      >>= addBDDfinalizer

-- FIXME verify implementation.
cudd_satisfy :: BDD -> BDD
cudd_satisfy bdd = unsafePerformIO $
  withBDD bdd ({#call unsafe cudd_satone#} ddmanager) >>= addBDDfinalizer

-------------------------------------------------------------------
-- Operations specific to this BDD binding.
-------------------------------------------------------------------

-- | Dump usage statistics to the given 'Handle'.
stats :: Handle -> IO ()
stats handle =
    do cfile <- handleToCFile handle
       printCFile cfile "CUDD stats\n"
       _ <- {#call unsafe Cudd_PrintInfo as _cudd_PrintInfo#} ddmanager cfile
       printCFile cfile "\nVariable groups\n"
       {#call unsafe cudd_printVarGroups#} ddmanager

----------------------------------------
-- Variable Reordering.
-- FIXME add an off switch
----------------------------------------

{#enum Cudd_ReorderingType as CUDDReorderingMethod {} deriving (Eq, Ord, Show)#}

roMap :: [(ReorderingMethod, CUDDReorderingMethod)]
roMap = [(ReorderSift, CUDD_REORDER_SIFT),
	 (ReorderStableWindow3, CUDD_REORDER_WINDOW3)]

-- | Set the dynamic variable ordering heuristic.
dynamicReOrdering :: ReorderingMethod -> IO ()
dynamicReOrdering rom =
    case lookup rom roMap of
      Nothing -> error $ "CUDD.dynamicReOrdering: method not supported: " ++ show rom
      Just brom ->
        {#call unsafe Cudd_AutodynEnable as _cudd_AutodynEnable#} ddmanager (cFromEnum brom) >> return ()

----------------------------------------
-- | Returns the relationship between BDD indices, BDD ids and
-- variable names. This may be useful for discovering what the
-- dynamic reordering is doing.
--
-- The intention of this function is to return
--   @[(position in variable order, immutable variable id, label)]@
-- so that the variable order can be saved.
----------------------------------------

varIndices :: IO [(CInt, CInt, String)]
varIndices = do (toBDD, _fromBDD) <- readIORef bdd_vars
		mapM procVar $ Map.toList toBDD
    where procVar :: (String, CInt) -> IO (CInt, CInt, String)
	  procVar (var, vid) =
	      do level <- {#call unsafe Cudd_ReadPerm as _cudd_ReadPerm#} ddmanager vid
	         return (level, vid, var)
{-# NOINLINE varIndices #-}

----------------------------------------
-- BDD Statistics.
----------------------------------------

-- | Determine the size of a BDD.
bddSize :: BDD -> Int
bddSize bdd = unsafePerformIO $
  do size <- withBDD bdd ({#call unsafe Cudd_DagSize as _cudd_DagSize#})
     return $ cToNum size
