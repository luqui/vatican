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
    NodeType type;
    bool blocked;
    depth_t depth;
};

struct LambdaNode : Node {
    Node* body;
};

struct SubstNode : Node {
    Node* body;
    depth_t var;
    Node* arg;
    depth_t shift;
};

struct ApplyNode : Node {
    union {
        struct {
            Node* f;
            Node* x;
        };

        // This is to make sure we have enough space for the transmogrification
        SubstNode _subst; 
    };
};

struct VarNode : Node 
{ };

struct IndirNode : Node {
    Node* target;
};

struct PrimNode : Node 
{ };


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
    T* allocate_node() {
        void* mem = _heap->allocate(sizeof(T));
        if (mem == 0) {
            // Obv do GC now
            throw std::runtime_error("Out of memory in this heap");
        }
        else {
            return new (mem) T;
        }
    }

    int _fuel;
    Pool* _heap;
};

#endif
