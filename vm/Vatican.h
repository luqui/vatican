#ifndef __VATICAN_H__
#define __VATICAN_H__

#include <vector>

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

class NodeVisitor {
public:
    virtual ~NodeVisitor() { }
    virtual void visit(Node*&) = 0;
};

struct Node {
    Node(NodeType type, bool blocked, depth_t depth) 
        : gc_next(0), depth(depth), blocked(blocked), type(type)
    { }

    Node* gc_next;
    depth_t depth;
    bool blocked;
    NodeType type;

    virtual void visit(NodeVisitor* visitor) = 0;
    virtual size_t size() = 0;
    virtual Node* copy(void* target) = 0;
};


struct LambdaNode : Node {
    LambdaNode(depth_t depth, Node* body) 
        : Node(NODETYPE_LAMBDA, true, depth)
        , body(body)
    { }

    Node* body;

    void visit(NodeVisitor* visitor) {
        visitor->visit(body);
    }
    size_t size() { return sizeof(LambdaNode); }
    Node* copy(void* target) {
        return new (target) LambdaNode(*this);
    } 
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

    void visit(NodeVisitor* visitor) {
        visitor->visit(data.body);
        visitor->visit(data.arg);
    }
    size_t size() { return sizeof(SubstNode); }
    Node* copy(void* target) {
        return new (target) SubstNode(*this);
    } 
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

    void visit(NodeVisitor* visitor) {
        visitor->visit(f);
        visitor->visit(x);
    }
    size_t size() { return sizeof(ApplyNode); }
    Node* copy(void* target) {
        return new (target) ApplyNode(*this);
    } 
};

struct VarNode : Node {
    VarNode(depth_t depth)
        : Node(NODETYPE_VAR, true, depth)
    { }

    void visit(NodeVisitor* visitor) { }
    size_t size() { return sizeof(VarNode); }
    Node* copy(void* target) {
        return new (target) VarNode(*this);
    } 
};

struct IndirNode : Node {
    IndirNode(Node* target)
        : Node(NODETYPE_INDIR, false, target->depth)
        , target(target)
    { }

    Node* target;

    void visit(NodeVisitor* visitor) {
        // XXX I think indir is a special case, so not sure what this should be...
        visitor->visit(target);
    }
    size_t size() { return sizeof(IndirNode); }
    Node* copy(void* target) {
        return new (target) IndirNode(*this);
    } 
};

struct PrimNode : Node 
{
    PrimNode()
        : Node(NODETYPE_PRIM, true, 0)
    { }

    void visit(NodeVisitor* visitor) { }
    size_t size() { return sizeof(PrimNode); }
    Node* copy(void* target) {
        return new (target) PrimNode(*this);
    } 
};


class Pool {
  public:
    Pool(size_t heapsize);
    virtual ~Pool();

    // Returns 0 if allocation was impossible
    void* allocate(size_t size);

    // Empties the pool for reuse.
    void clear();

    bool contains(void* ptr) {
        return _pool_start <= ptr && ptr < _pool_end;
    }

    size_t size() const {
        return _pool_end - _pool_start;
    }
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

    Node* add_root(Node* node) {
        _root_set.push_back(node);
        return node;
    }

  private:
    Interp(const Interp&);  // No copying

    void init(size_t heap_size, int fuel);

    Node* substitute(Node* body, depth_t var, Node* arg, depth_t shift);

    template<class T> 
    void* allocate_node() {
        return allocate_node(sizeof(T));
    }
    
    void* allocate_node(size_t size) {
        void* mem = _heap->allocate(size);
        if (mem == 0) {
            run_gc();
            void* mem2 = _heap->allocate(size);
            if (mem2 == 0) {
                throw std::runtime_error("Couldn't GC enough memory.");
            }
            else {  
                return mem2;
            }
        }
        else {
            return mem;
        }
    }

    void run_gc();

    int _fuel;
    Pool* _heap;
    Pool* _backup_heap;
    
    std::vector<Node*> _root_set;
};

#endif
