#ifndef __VATICAN_H__
#define __VATICAN_H__


typedef int depth_t;
typedef unsigned char byte;

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


class Pool {
  public:
    Pool(size_t heapsize);
    virtual ~Pool();

    // Returns 0 if allocation was impossible
    void* allocate(size_t size);

    // Empties the pool for reuse.
    void clear();
  private:
    byte* _pool_start;
    byte* _cur;
    byte* _pool_end;
};


const size_t DEFAULT_HEAP_SIZE = 0x100000;  // 1MB

class Interp {
  public:
    Interp() { 
        init(DEFAULT_HEAP_SIZE, 0); 
    }

    Interp(size_t heap_size, int fuel) {
        init(heap_size, fuel);
    }

    virtual ~Interp() { }

    // Destructively reduce the node to whnf.  Returns the same node, 
    // possibily with indirections followed.
    Node* reduce_whnf(Node* node);

  private:
    Interp(const Interp&);  // No copying

    void init(size_t heap_size, int fuel);

    Node* subst(Node* body, depth_t var, Node* arg, depth_t shift);

    Node* allocate_node();

    int _fuel;
    Pool* _heap;
};

#endif
