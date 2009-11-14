#ifndef __VATICAN_H__
#define __VATICAN_H__

#include <ios>
#include <string>

// A Bottom-up beta Substituor
// ---------------------------
//
// Based on:
// "Bottom-Up beta-Substitution: Uplinks and lambda-DAGs"
// Olin Shivers & Mitchell Wand, 2004

#ifdef __cplusplus
extern "C" {
#endif

typedef struct vnNode vnNode;
typedef struct vnHead vnHead;
typedef struct vnPrimNode vnPrimNode;

typedef vnPrimNode* vnApplyCallback(void* data, vnHead* arg);
typedef void vnCleanupCallback(void* data);

vnPrimNode* vnMakePrim(void* data, vnApplyCallback apply, vnCleanupCallback cleanup);

// Reduce a term to head normal form.  Can infinite loop.
void vnReduceHNF(vnHead*);

vnHead* vnMakeHead(vnNode* body);
vnHead* vnCopyHead(vnHead* other);
void vnFreeHead(vnHead*);

// Retrieve the Prim from within a head.  Returns NULL
// if the expression is not a normal form or is not a Prim.
vnPrimNode* vnGetHead(vnHead*);

vnNode* vnVar();
vnNode* vnFun(vnNode* var, vnNode* body);
vnNode* vnApp(vnNode* fun, vnNode* arg);

vnNode* vnPrim(vnPrimNode*);

#ifdef __cplusplus
}
#endif

#endif
