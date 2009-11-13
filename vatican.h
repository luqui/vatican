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

class PrimNode {
  public:
    virtual ~PrimNode() { }
    virtual PrimNode* action(PrimNode* other) = 0;
    virtual std::string repr() = 0;
};

// Find the first beta reduction in head normal order and
// reduce it.  Returns whether a reduction was performed.
bool hnf_reduce_1(Node*);

// Reduce a term to head normal form.  Can infinite loop.
void hnf_reduce(Node*);

// Write a dot graph of the expression.
void dotify(Node*, std::ostream&);

Node* Head(Node* body);
Node* Var();
Node* Fun(Node* var, Node* body);
Node* App(Node* fun, Node* arg);

// Allocate a prim node.  The passed PrimNode must be allocated 
// with "new" (it will be "delete"d).
Node* Prim(PrimNode*);

}

#endif
