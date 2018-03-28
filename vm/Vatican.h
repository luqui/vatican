#ifndef __VATICAN_H__
#define __VATICAN_H__

#include <cassert>
#include <stdexcept>

typedef int depth_t;

struct Node;
enum NodeType 
    { NODETYPE_LAMBDA
    , NODETYPE_APPLY
    , NODETYPE_SUBST
    , NODETYPE_VAR
    , NODETYPE_INDIR
    , NODETYPE_PRIM
    };

struct LambdaData {
    Node* body;
};

struct ApplyData {
    Node* f;
    Node* x;
};

struct SubstData {
    Node* body;
    depth_t var;
    Node* arg;
    depth_t shift;
};

struct VarData { };

struct IndrData {
    Node* target;
};

struct PrimData { };

struct Node {
    NodeType type;
    bool blocked;
    depth_t depth;

    union {
        LambdaData lambda;
        ApplyData apply;
        SubstData subst;
        VarData var;
        IndrData indir;
        PrimData prim;
    };
};


class Interp {
  public:
    Interp() : _fuel(0) { }

    Interp(int fuel) : _fuel(fuel) { }

    // Destructively reduce the node to whnf.  Returns the same node, 
    // possibily with indirections followed.
    Node* reduce_whnf(Node* node);

    Node* subst(Node* body, depth_t var, Node* arg, depth_t shift);

  private:
    int _fuel;
};

#endif
