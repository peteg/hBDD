-- -*- haskell -*-- -----------------------------------------------
-- |
-- Module      : Data.Boolean.CMUBDD
-- Copyright   : (C)opyright [2002..2005], 2009 UNSW
-- License     : LGPL (see COPYING.LIB for details)
--
-- A binding for CMU\/Long\'s BDD package.
--
-- Note the license on that library is /not/ free.
--
-- Note this library is not thread-safe.
-------------------------------------------------------------------
module Data.Boolean.CMUBDD
    (
        BDD
    ,	module Data.Boolean

        -- * Functions specific to this binding.
    ,	stats
    ,	gc

    ,	dynamicReOrdering
    ,	varIndices
    ,	bddSize
    ,   bdd_print
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

#include "bdduser.h"

import Control.DeepSeq  ( NFData )
import Control.Monad	( liftM, zipWithM_ )

import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
import Data.Map         ( Map )
import qualified Data.Map as Map

import Foreign
import Foreign.C

import GHC.ForeignPtr	( newConcForeignPtr )

import System.Mem	( performGC )
import System.IO	( Handle, hIsReadable, hIsWritable )
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

cToNum :: (Num i, Integral e) => e -> i
cToNum  = fromIntegral . toInteger

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | The BDD manager, which we hide from clients.
-- Note the absence of a finalizer.
{#pointer bdd_manager as BDDManager newtype#}

-- | The abstract type of BDDs.
{#pointer bdd as BDD foreign newtype#}

withBDD :: BDD -> (Ptr BDD -> IO a) -> IO a

-- BDDs are just pointers, so there's no work to do when we @deepseq@ them.
instance Control.DeepSeq.NFData BDD

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
bdd_manager :: BDDManager
bdd_manager = unsafePerformIO {#call unsafe bdd_init#}
{-# NOINLINE bdd_manager #-}

-- | A map to and from BDD variable numbers.
type VarMap = (Map String CLong, Map CLong String)

-- | Tracks existing variables.
-- FIXME: add garbage collection for variables. This might not be
-- possible/easy/useful.
bdd_vars :: IORef VarMap
bdd_vars = unsafePerformIO $ newIORef (Map.empty, Map.empty)
{-# NOINLINE bdd_vars #-}

-- | Attaches a finalizer to a BDD object.
addBDDfinalizer :: Ptr BDD -> IO BDD
addBDDfinalizer bddp = liftM BDD $ newConcForeignPtr bddp bddf
    where bddf = bdd_free bdd_manager bddp
{-# INLINE addBDDfinalizer #-}

-- c2hs doesn't seem to do the right thing here...
foreign import ccall unsafe "LongBDD.h bdd_free"
    bdd_free :: BDDManager -> Ptr BDD -> IO ()

-------------------------------------------------------------------
-- Groups and Substitutions.
-------------------------------------------------------------------

-- | The BDD library supports caching substitutions.
newtype Assoc = MkAssoc (ForeignPtr CInt)

withAssoc :: Assoc -> (Ptr CInt -> IO a) -> IO a
withAssoc (MkAssoc a) = withForeignPtr a

-- | Attaches a finalizer to an Association.
addAssocFinalizer :: Ptr CInt -> IO Assoc
addAssocFinalizer sint = liftM MkAssoc $ newConcForeignPtr sint subf
    where subf = bdd_free_assoc bdd_manager sint
--     where subf = {#call unsafe bdd_free_assoc#} bdd_manager sint
{-# INLINE addAssocFinalizer #-}

-- FIXME: nasty hack. Pretend assoc id's are Ptrs so we can use
-- newForeignPtr for GC.
foreign import ccall unsafe "LongBDD.h bdd_free_assoc"
  bdd_free_assoc :: BDDManager -> Ptr CInt -> IO ()

foreign import ccall unsafe "LongBDD.h bdd_new_assoc"
  bdd_new_assoc :: BDDManager -> Ptr BDD -> CInt -> IO (Ptr CInt)

foreign import ccall unsafe "LongBDD.h bdd_assoc"
  bdd_assoc :: BDDManager -> Ptr CInt -> IO (Ptr CInt)

-- Separate types for groups and substitutions, see the associated
-- types below.
withGroup :: Group BDD -> (Ptr CInt -> IO a) -> IO a
withGroup (MkGroup g) = withAssoc g

withSubst :: Subst BDD -> (Ptr CInt -> IO a) -> IO a
withSubst (MkSubst g) = withAssoc g

-------------------------------------------------------------------
-- Logical operations.
-------------------------------------------------------------------

newVar :: String -> IO (Ptr BDD) -> IO BDD
newVar label allocVar =
      do (toBDD, fromBDD) <- readIORef bdd_vars
         case label `Map.lookup` toBDD of
          Just vid ->
              {#call unsafe bdd_var_with_id#} bdd_manager vid >>= addBDDfinalizer
          Nothing  -> do bdd <- allocVar >>= addBDDfinalizer
                         vid <- withBDD bdd $ {#call unsafe bdd_if_id#} bdd_manager
                         writeIORef bdd_vars (Map.insert label vid toBDD,
                                              Map.insert vid label fromBDD)
--                       putStrLn $ label ++ " -> " ++ show (vid, bdd)
                         return bdd

instance BooleanVariable BDD where
    bvar label = unsafePerformIO $
      newVar label $ {#call unsafe bdd_new_var_last#} bdd_manager

    bvars labels =
      case labels of
        [] -> []
        l : ls -> unsafePerformIO $
          do bdd <- newVar l $ {#call unsafe bdd_new_var_last#} bdd_manager
             -- Add the other variables after the first one, and make
             -- them belong to the same variable block.
             -- FIXME unclear if the relative order of the variables matters.
             bdds <- mapM (\l' -> newVar l' (allocAdjVar bdd)) ls
             return (bdd : bdds)
      where allocAdjVar bdd =
              do bdd1 <- withBDD bdd $
                        {#call unsafe bdd_new_var_after#} bdd_manager
                 _ <- withBDD bdd $ \bddp ->
                        {#call unsafe bdd_new_var_block#} bdd_manager bddp 2
                 return bdd1

    unbvar bdd = unsafePerformIO $
                 do vid <- withBDD bdd $ {#call unsafe bdd_if_id#} bdd_manager
                    (_, fromBDD) <- readIORef bdd_vars
                    return $ case vid `Map.lookup` fromBDD of
                               Nothing -> "(VID: " ++ show vid ++ ")"
                               Just v  -> v

instance Boolean BDD where
  false = unsafePerformIO $
             {#call unsafe bdd_zero#} bdd_manager >>= addBDDfinalizer
  true  = unsafePerformIO $
             {#call unsafe bdd_one#} bdd_manager >>= addBDDfinalizer

  (/\)  = bddBinOp {#call unsafe bdd_and#}
  neg x = unsafePerformIO $
            withBDD x ({#call unsafe bdd_not#} bdd_manager) >>= addBDDfinalizer
  nand  = bddBinOp {#call unsafe bdd_nand#}
  (\/)  = bddBinOp {#call unsafe bdd_or#}
  nor   = bddBinOp {#call unsafe bdd_nor#}
  xor   = bddBinOp {#call unsafe bdd_xor#}
  (<->) = bddBinOp {#call unsafe bdd_xnor#}

    -- bITE i t e = unsafePerformIO $
    --   withBDD i $ \ip -> withBDD t $ \tp -> withBDD e $ \ep ->
    --     {#call unsafe bdd_ite#} bdd_manager ip tp ep >>= addBDDfinalizer

    -- It seems bdd_implies computes something like x /\ ~y == ~(x -> y) ??
    -- ...so we just use the default: x ==> y = (neg x) \/ y

bddBinOp :: (BDDManager -> (Ptr BDD) -> (Ptr BDD) -> IO (Ptr BDD)) -> BDD -> BDD -> BDD
bddBinOp op x y = unsafePerformIO $
  withBDD x $ \x' -> withBDD y $ \y' ->
    op bdd_manager x' y' >>= addBDDfinalizer
{-# INLINE bddBinOp #-}

instance QBF BDD where
    -- | The BDD library supports caching substitutions.
    data Group BDD = MkGroup Assoc

    mkGroup = longbdd_mkGroup
    exists = longbdd_exists
    forall = longbdd_forall
    rel_product = longbdd_rel_product

instance Substitution BDD where
    data Subst BDD = MkSubst Assoc

    mkSubst = longbdd_mkSubst
    rename = substitute -- FIXME implement
    substitute = longbdd_substitute

instance BDDOps BDD where
    bif   = bddUnaryOp {#call unsafe bdd_if#}
    bthen = bddUnaryOp {#call unsafe bdd_then#}
    belse = bddUnaryOp {#call unsafe bdd_else#}

--     bprint = longbdd_print
    reduce = longbdd_reduce
    satisfy = longbdd_satisfy
    support = longbdd_support

bddUnaryOp :: (BDDManager -> (Ptr BDD) -> IO (Ptr BDD)) -> BDD -> BDD
bddUnaryOp op x = unsafePerformIO $
  do bddp <- withBDD x $ \bdd' -> op bdd_manager bdd'
     addBDDfinalizer bddp
{-# INLINE bddUnaryOp #-}

-------------------------------------------------------------------
-- Implementations.
-------------------------------------------------------------------

longbdd_mkGroup :: [BDD] -> Group BDD
longbdd_mkGroup vars = unsafePerformIO $
  allocaArray0 len $ \group ->
    do zipWithM_ (pokeSubst group) vars [0..]
       pokeElemOff group len nullPtr
       gid <- bdd_new_assoc bdd_manager (castPtr group) 0
       mapM_ (\ (BDD v) -> touchForeignPtr v) vars
       liftM MkGroup $ addAssocFinalizer gid
  where pokeSubst :: Ptr (Ptr BDD) -> BDD -> Int -> IO ()
        pokeSubst ptr (BDD v) i =
          v `seq` pokeElemOff ptr i (unsafeForeignPtrToPtr v)

        len = length vars

longbdd_exists :: Group BDD -> BDD -> BDD
longbdd_exists group bdd = unsafePerformIO $ bdd `seq`
  withGroup group $ \ gid ->
    do _ <- bdd_assoc bdd_manager gid
       withBDD bdd ({#call unsafe bdd_exists#} bdd_manager) >>= addBDDfinalizer

longbdd_forall :: Group BDD -> BDD -> BDD
longbdd_forall group bdd = unsafePerformIO $ bdd `seq`
  withGroup group $ \gid ->
    do _ <- bdd_assoc bdd_manager gid
       withBDD bdd ({#call unsafe bdd_forall#} bdd_manager) >>= addBDDfinalizer

longbdd_rel_product :: Group BDD -> BDD -> BDD -> BDD
longbdd_rel_product group f g = unsafePerformIO $ f `seq` g `seq`
  withGroup group $ \gid ->
    do _ <- bdd_assoc bdd_manager gid
       withBDD f $ \f' -> withBDD g $ \g' ->
         {#call unsafe bdd_rel_prod#} bdd_manager f' g' >>= addBDDfinalizer

longbdd_mkSubst :: [(BDD, BDD)] -> Subst BDD
longbdd_mkSubst pairs = unsafePerformIO $
  allocaArray0 (2*len) $ \assoc ->
    do zipWithM_ (pokeSubst assoc) pairs [0..]
       pokeElemOff assoc (2*len) nullPtr
       aid <- bdd_new_assoc bdd_manager (castPtr assoc) 1
       mapM_ (\ (BDD v, BDD f) -> do touchForeignPtr v
                                     touchForeignPtr f) pairs
       liftM MkSubst $ addAssocFinalizer aid
    where pokeSubst :: Ptr (Ptr BDD) -> (BDD, BDD) -> Int -> IO ()
          pokeSubst ptr (v, f) i =
              do withBDD v $ pokeElemOff ptr (2*i)
                 withBDD f $ pokeElemOff ptr (2*i+1)

          len = length pairs

-- | Substitutes some BDDs for some variables.
longbdd_substitute :: Subst BDD -> BDD -> BDD
longbdd_substitute subst bdd = unsafePerformIO $ bdd `seq`
  withSubst subst $ \aid ->
    do _ <- bdd_assoc bdd_manager aid
       withBDD bdd ({#call unsafe bdd_substitute#} bdd_manager) >>= addBDDfinalizer

-- | Writes out the BDD using the default representation.
-- FIXME: we want to map var's -> strings here.
bdd_print :: Handle -> BDD -> IO ()
bdd_print handle bdd =
    do cfile <- handleToCFile handle
       withBDD bdd $ \bdd' -> {#call unsafe bdd_print_bdd#} bdd_manager bdd'
                                     namingFn nullFunPtr nullPtr
                                     cfile
    where namingFn = nullFunPtr
--     where namingFn = mkNamingFn (\_ f _ -> newCString $ unbvar f)

-- Converts Haskell "naming functions" to C ones. (see the BDD manpage).
-- foreign import ccall "wrapper"
--     mkNamingFn :: Ptr BDDManager -> BDD -> a -> IO CString
--             -> IO (FunPtr (Ptr BDDManager -> BDD -> a -> IO CString))

longbdd_reduce :: BDD -> BDD -> BDD
longbdd_reduce f g = unsafePerformIO $
  withBDD f $ \fp -> withBDD g $ \gp ->
    {#call unsafe bdd_reduce#} bdd_manager fp gp >>= addBDDfinalizer

longbdd_satisfy :: BDD -> BDD
longbdd_satisfy bdd = unsafePerformIO $ bdd `seq`
  withBDD bdd ({#call unsafe bdd_satisfy#} bdd_manager) >>= addBDDfinalizer

-------------------------------------------------------------------
-- Operations specific to this BDD binding.
-------------------------------------------------------------------

-- | Extracts the variables from a BDD.
longbdd_support :: BDD -> [BDD]
longbdd_support bdd = unsafePerformIO $
  do (vm, _) <- readIORef bdd_vars
     allocaArray0 (Map.size vm) $ \vars_array -> withBDD bdd $ \bdd' ->
       do {#call unsafe bdd_support as long_bdd_support#} bdd_manager bdd' vars_array
          support_vars <- peekArray0 nullPtr (castPtr vars_array)
          mapM addBDDfinalizer support_vars

-- | Performs a garbage collection (one in Haskell followed by one
-- in the BDD library).
gc :: IO ()
gc = do performGC
        {#call unsafe bdd_gc#} bdd_manager

-- | Writes out some statistics from the BDD library.
stats :: Handle -> IO ()
stats handle = do cfile <- handleToCFile handle
                  {#call unsafe bdd_stats#} bdd_manager cfile

----------------------------------------
-- Variable Reordering.
----------------------------------------

foreign import ccall "&" bdd_reorder_stable_window3 :: FunPtr (BDDManager -> IO ())
foreign import ccall "&" bdd_reorder_sift :: FunPtr (BDDManager -> IO ())

-- | Set the dynamic variable ordering heuristic.
dynamicReOrdering :: ReorderingMethod -> IO ()
dynamicReOrdering rom =
    {#call unsafe bdd_dynamic_reordering#} bdd_manager
      (case rom of
         ReorderSift          -> bdd_reorder_sift
         ReorderStableWindow3 -> bdd_reorder_stable_window3
      )

----------------------------------------

-- | Returns the relationship between BDD indices, BDD ids and variable
-- names. This may be useful for discovering what the dynamic reordering
-- is doing. (See the manpage for details about indices and ids.)
--
-- The intention of this function is to return
--   @[(position in variable order, immutable variable id, label)]@
-- so that the variable order can be saved.
varIndices :: IO [(CLong, CLong, String)]
varIndices = do (toBDD, _fromBDD) <- readIORef bdd_vars
                mapM procVar $ Map.toList toBDD
    where procVar :: (String, CLong) -> IO (CLong, CLong, String)
          procVar (var, vid) =
              do vindex <- withBDD (bvar var) $ {#call unsafe bdd_if_index#} bdd_manager
                 return (vindex, vid, var)
{-# NOINLINE varIndices #-}

----------------------------------------
-- BDD Statistics.
----------------------------------------

-- | Determine the size of a BDD.
bddSize :: BDD -> Int
bddSize bdd = unsafePerformIO $
  withBDD bdd $ \bddp ->
    do size <- ({#call unsafe bdd_size#}) bdd_manager bddp 1
       return $ cToNum size
