/*
 * Some extra functions for CUDD.
 *
 * Peter Gammie, peteg42 at gmail dot com
 * (C)opyright 2005, 2009 UNSW
 *
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

#include "cudd_im.h"
#include "cuddInt.h"

static DdNode *
satone_rec(DdManager *manager, DdNode* f)
{
    DdNode *zero = Cudd_ReadLogicZero(manager);
    DdNode *one  = Cudd_ReadOne(manager);
    DdNode *F = Cudd_Regular(f);
    DdNode *high;
    DdNode *low;
    DdNode *r;
    unsigned int index;

    if (F == zero ||
        F == one) {
        return f;
    }

    index = F->index;
    high = cuddT(F);
    low = cuddE(F);
    if (Cudd_IsComplement(f)) {
        high = Cudd_Not(high);
        low = Cudd_Not(low);
    }
    if(low == zero) {
        DdNode* res = satone_rec(manager, high);
        if (res == NULL) {
            return NULL;
        }
        cuddRef(res);
        if (Cudd_IsComplement(res)) {
            r = cuddUniqueInter(manager, (int)index, Cudd_Not(res), one);
            if (r == NULL) {
                Cudd_IterDerefBdd(manager, res);
                return NULL;
            }
            r = Cudd_Not(r);
        } else {
            r = cuddUniqueInter(manager, (int)index, res, zero);
            if (r == NULL) {
                Cudd_IterDerefBdd(manager, res);
                return NULL;
            }
        }
        cuddDeref(res);
    } else {
        DdNode* res = satone_rec(manager, low);
        if (res == NULL) return NULL;
        cuddRef(res);
        r = cuddUniqueInter(manager, (int)index, one, Cudd_Not(res));
        if (r == NULL) {
            Cudd_IterDerefBdd(manager, res);
            return NULL;
        }
        r = Cudd_Not(r);
        cuddDeref(res);
    }

    return r;
}

/* Return a satisfying variable assignment. */
DdNode *
cudd_satone(DdManager *manager, DdNode *bdd)
{
    DdNode *result;

    do {
        manager->reordered = 0;
        result = satone_rec(manager, bdd);
    } while (manager->reordered == 1);

    Cudd_Ref(result);

    return result;
}

/* Prints the variable groups. */
void
cudd_printVarGroups(DdManager *manager)
{
    if(manager->tree == NULL) {
        fprintf(stdout, ">> cudd_printVarGroups: no groups.\n");
    } else {
        Mtr_PrintGroups(manager->tree, 0);
    }
}

/* Prints the reference count on a DD node. */
unsigned int cudd_getRef(DdNode *dd)
{
    return Cudd_Regular(dd)->ref;
}

/*
 * CUDD returns BDDs with reference count 0, if they haven't already been
 * returned. GHC's garbage collector runs in a concurrent green thread, so
 * we get some atomicity problems, e.g.:
 *
 * 1. Create BDD, BDD.refcount++, add finalizer.
 * 2. BDD becomes unreferenced but not yet GC'd.
 * 3. Call BDD operation returning the same BDD as step 1. (call it BDD')
 * 4. GHC's GC kicks in and BDD.refcount--. (*)
 * 5. BDD'.refcount++, add finalizer.
 *
 * At (*), CUDD deallocates BDD (== BDD') and BDD' becomes a dangling
 * pointer.
 *
 * Solution (yuck): wrap all functions that return BDDs and do the ref count
 * increment in C, where it must be atomic and GHC's GC cannot be running.
 *
 * We're still screwed if there are OS threads doing things, though.
 */

DdNode *cudd_BinOp(DdManager *dd, CuddBinOp op, DdNode *f, DdNode *g)
{
    DdNode *res;

    switch(op) {
    case AND:   res = Cudd_bddAnd(dd, f, g); break;
    case XOR:   res = Cudd_bddXor(dd, f, g); break;
    case XNOR:  res = Cudd_bddXnor(dd, f, g); break;
    case OR:    res = Cudd_bddOr(dd, f, g); break;
    case NAND:  res = Cudd_bddNand(dd, f, g); break;
    case NOR:   res = Cudd_bddNor(dd, f, g); break;
    default: fprintf(stderr, "cudd_BinOp: unknown op: %d\n", op); exit(1);
    }

    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddIte(DdManager *dd, DdNode *f, DdNode *g, DdNode *h)
{
    DdNode *res = Cudd_bddIte(dd, f, g, h);
    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddNot(DdNode *node)
{
    DdNode *res = Cudd_Not(node);
    Cudd_Ref(res);
    return res;
}

/* cudd_bddT and cudd_bddE are complicated by CUDD's complemented pointers. */
DdNode *cudd_bddT(DdNode *node)
{
    DdNode *res = Cudd_T(node);

    if(Cudd_IsComplement(node)) {
        res = Cudd_Not(res);
    }

    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddE(DdNode *node)
{
    DdNode *res = Cudd_E(node);

    if(Cudd_IsComplement(node)) {
        res = Cudd_Not(res);
    }

    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddExistAbstract(DdManager *dd, DdNode *f, DdNode *cube)
{
    DdNode *res = Cudd_bddExistAbstract(dd, f, cube);
    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddUnivAbstract(DdManager *dd, DdNode *f, DdNode *cube)
{
    DdNode *res = Cudd_bddUnivAbstract(dd, f, cube);
    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddAndAbstract(DdManager *dd,
                            DdNode *f, DdNode *g, DdNode *cube)
{
    DdNode *res = Cudd_bddAndAbstract(dd, f, g, cube);
    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddSwapVariables(DdManager *dd, DdNode *f,
                              DdNode **x, DdNode **y, int n)
{
    DdNode *res = Cudd_bddSwapVariables(dd, f, x, y, n);
    Cudd_Ref(res);
    return res;
}

DdNode *cudd_bddLICompaction(DdManager *dd, DdNode *f, DdNode *g)
{
    DdNode *res = Cudd_bddLICompaction(dd, f, g);
    Cudd_Ref(res);
    return res;
}
