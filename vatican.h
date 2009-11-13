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

namespace vatican {

class Node;
class Head;

class PrimNode {
  public:
    virtual ~PrimNode() { }
    virtual PrimNode* action(PrimNode* other) = 0;
    virtual std::string repr() = 0;
};

// Find the first beta reduction in head normal order and
// reduce it.  Returns whether a reduction was performed.
bool hnf_reduce_1(Head*);

// Reduce a term to head normal form.  Can infinite loop.
void hnf_reduce(Head*);

// Write a dot graph of the expression.
void dotify(Head*, std::ostream&);

Head* MakeHead(Node* body);
Node* Var();
Node* Fun(Node* var, Node* body);
Node* App(Node* fun, Node* arg);

// Allocate a prim node.  The passed PrimNode must be allocated 
// with "new" (it will be "delete"d).
Node* Prim(PrimNode*);

// Retrieve the Prim from within a head.  Returns NULL
// if the expression is not a normal form or is not a Prim.
PrimNode* GetPrim(Head*);

}

#endif
