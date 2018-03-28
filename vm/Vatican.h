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

struct Node {
    Node(NodeType type, bool blocked, depth_t depth) 
        : type(type), blocked(blocked), depth(depth)
    { }

    NodeType type;
    bool blocked;
    depth_t depth;
};

struct LambdaNode : Node {
    LambdaNode(depth_t depth, Node* body) 
        : Node(NODETYPE_LAMBDA, true, depth)
        , body(body)
    { }

    Node* body;
};

// This is separated out so that we can pad ApplyNode appropriately.
struct SubstData {
    Node* body;
    depth_t var;
    Node* arg;
    depth_t shift;
};

struct SubstNode : Node {
    SubstNode(depth_t depth, Node* body, depth_t var, Node* arg, depth_t shift)
        : Node(NODETYPE_SUBST, false, depth)
    {
        data.body = body;
        data.var = var;
        data.arg = arg;
        data.shift = shift;
    }
        
    SubstData data;
};

struct ApplyNode : Node {
    ApplyNode(depth_t depth, Node* f, Node* x)
        : Node(NODETYPE_APPLY, false, depth)
        , f(f)
        , x(x)
    { }

    union {
        struct {
            Node* f;
            Node* x;
        };

        // This is to make sure we have enough space for the transmogrification
        SubstData _padding;
    };
};

struct VarNode : Node {
    VarNode(depth_t depth)
        : Node(NODETYPE_VAR, true, depth)
    { }
};

struct IndirNode : Node {
    IndirNode(Node* target)
        : Node(NODETYPE_INDIR, false, target->depth)
        , target(target)
    { }

    Node* target;
};

struct PrimNode : Node 
{
    PrimNode()
        : Node(NODETYPE_PRIM, true, 0)
    { }
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

    Node* substitute(Node* body, depth_t var, Node* arg, depth_t shift);

    template<class T> 
    void* allocate_node() {
        void* mem = _heap->allocate(sizeof(T));
        if (mem == 0) {
            // Obv do GC now
            throw std::runtime_error("Out of memory in this heap");
        }
        else {
            return mem;
        }
    }

    int _fuel;
    Pool* _heap;
};

#endif
