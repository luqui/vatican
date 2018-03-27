#ifndef __VATICAN_H__
#define __VATICAN_H__

typedef int depth_t;

struct Node;

struct LambdaData {
    Node* body;
};

struct ApplyData {
    Node* f;
    Node* x;
};

struct SubstData {
    Node* body;
    Node* arg;
};

struct VarData { };

struct Node {
    enum NodeType 
        { NODETYPE_LAMBDA
        , NODETYPE_APPLY
        , NODETYPE_SUBST
        , NODETYPE_VAR
        };

    NodeType type;
    bool blocked;
    depth_t depth;

    union {
        LambdaData lambda;
        ApplyData apply;
        SubstData subst;
        VarData var;
    };
};

#endif
