/*
 * Some extra functions for CUDD.
 *
 * (C)opyright 2005, 2009 University of New South Wales
 * (C)opyright 2009-2011 Peter Gammie, peteg42 at gmail dot com
 * Licence: LGPL
 *
 * "satone" is borrowed lock-stock from JavaBDD,
 *
 *     http://javabdd.sourceforge.net/
 *
 * which carries the following copyright:
 *
 *    Copyright (C) 2003  John Whaley (jwhaley at alum.mit.edu)
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifndef	_CUDD_IM_H_
#define	_CUDD_IM_H_

/* In your face Apple: c2hs chokes on (^) blocks non-standardness. */
#undef __BLOCKS__
#include <stdio.h>
#include "util.h"
#include "cudd.h"

/* Not CUDD-specific, but stuff it here for convenience. */
inline void
cFree(void *ptr)
{
  return free(ptr);
}

inline void
fprintf_neutered(FILE *file, const char *str)
{
    fprintf(file, "%s", str);
    fflush(file);
}

typedef enum {
    UNIQUE_SLOTS = CUDD_UNIQUE_SLOTS
} CuddSubtableSize;

typedef enum {
    CACHE_SLOTS = CUDD_CACHE_SLOTS
} CuddCacheSize;

typedef enum {
    CUDD_MTR_DEFAULT  = MTR_DEFAULT,
    CUDD_MTR_TERMINAL = MTR_TERMINAL,
    CUDD_MTR_SOFT     = MTR_SOFT,
    CUDD_MTR_FIXED    = MTR_FIXED,
    CUDD_MTR_NEWNODE  = MTR_NEWNODE
} CuddMTRParams;

/* The binary operations CUDD supports. */
typedef enum {
    AND,
    XOR,
    XNOR,
    OR,
    NAND,
    NOR
} CuddBinOp;

/* Defined in cudd_im.c */

DdNode *cudd_satone(DdManager *, DdNode *);
void cudd_printVarGroups(DdManager *manager);
unsigned int cudd_getRef(DdNode *dd);

/* Atomic refcount incrementers. See cudd_im.c */

DdNode *cudd_BinOp(DdManager *dd, CuddBinOp op, DdNode *f, DdNode *g);
DdNode *cudd_bddIte(DdManager *dd, DdNode *f, DdNode *g, DdNode *h);
DdNode *cudd_bddNot(DdNode *node);
DdNode *cudd_bddT(DdNode *node);
DdNode *cudd_bddE(DdNode *node);

DdNode *cudd_bddExistAbstract(DdManager *dd, DdNode *f, DdNode *cube);
DdNode *cudd_bddUnivAbstract(DdManager *dd, DdNode *f, DdNode *cube);
DdNode *cudd_bddAndAbstract(DdManager *dd, DdNode *f, DdNode *g, DdNode *cube);
DdNode *cudd_bddSwapVariables(DdManager *dd, DdNode *f, DdNode **x, DdNode **y, int n);
DdNode *cudd_bddLICompaction(DdManager *dd, DdNode *f, DdNode *g);

#endif /* _CUDD_IM_H_ */
