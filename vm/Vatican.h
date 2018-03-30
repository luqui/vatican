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
    , NODETYPE_MAX
    };

class NodeVisitor {
public:
    virtual ~NodeVisitor() { }
    virtual void visit(Node*&) = 0;
};

class GCRef {
public:
    virtual ~GCRef() { }
    virtual void visit(NodeVisitor* visitor) = 0;
};

struct Node : public GCRef {
    Node(NodeType type, bool blocked, depth_t depth) 
        : gc_next(0), depth(depth), blocked(blocked), type(type)
    { }

    Node* gc_next;
    depth_t depth;
    bool blocked;
    NodeType type;

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

private:
    // It's possible we can use gc_next to indirect to avoid this padding.
    Node* _indir_padding;
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

private:
    Node* _indir_padding;
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


class NodePtr : GCRef {
    friend class Interp;
    friend class NodeMaker;
  public:
    ~NodePtr() {
        // Remove this node from the rootset
        _prev->_next = _next;
        _next->_prev = _prev;
    }

    NodePtr(const NodePtr& p) {
        *this = p;
    }
    
    NodePtr& operator= (const NodePtr& const_p);

    Node* operator -> () {
        return _ptr;
    }
    const Node* operator -> () const {
        return _ptr;
    }

    void visit(NodeVisitor* visitor) {
        visitor->visit(_ptr);
    }

    bool operator == (const NodePtr& other) {
        return _ptr == other._ptr;
    }

    bool operator != (const NodePtr& other) {
        return !(*this == other);
    }

    Node* unsafe_get_ptr() const {
        return _ptr;
    }

  private:
    NodePtr()
        : _ptr(0)
        , _next(this)
        , _prev(this)
    {
        // Special constructor for terminal nodes
        // NOTE THE CYCLE!  We rely on comparison to the edges rather than
        // comparison to null pointer.  This is to remove conditionals from
        // the destructor.
    }
    
    NodePtr(class Interp* interp, Node* ptr);

    Node* _ptr;
    NodePtr* _next;
    NodePtr* _prev;
};


const size_t DEFAULT_HEAP_SIZE = 0x100000;  // 1MB

class Interp {
    friend class NodePtr;
    friend class NodeMaker;

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
    NodePtr reduce_whnf(const NodePtr& node);

  private:
    Interp(const Interp&);  // No copying

    void init(size_t heap_size, int fuel);
    
    Node* reduce_whnf_wrapper(Node* node);
    Node* reduce_whnf_rec(Node* node);

    Node* substitute(Node* body, depth_t var, Node* arg, depth_t shift);

    template<class T> 
    void* allocate_node() {
        return allocate_node(sizeof(T));
    }
    
    void* allocate_node(size_t size);

    void run_gc();

    int _fuel;
    Pool* _heap;
    Pool* _backup_heap;
    
    NodePtr _rootset_front;
    NodePtr _rootset_back;
};


class NodeMaker {
  public:
    NodeMaker(Interp* interp) : _interp(interp) { }

    // To build nodes for testing, we pun and use var depth as a de bruijn
    // index, then postprocess it into the correct depth form.
    NodePtr lambda(const NodePtr& body) {
        return NodePtr(_interp,
            new (_interp->allocate_node<LambdaNode>()) LambdaNode(-1, body._ptr));
    }
    NodePtr apply(const NodePtr& f, const NodePtr& x) {
        return NodePtr(_interp,
            new (_interp->allocate_node<ApplyNode>()) ApplyNode(-1, f._ptr, x._ptr));
    }
    NodePtr var(int debruijn) {
        // We use negative to indicate a debruijn index, because if the graph has
        // sharing it is possible to traverse nodes twice.  A variable's depth is
        // always strictly positive so there is no overlap at 0.
        return NodePtr(_interp,
            new (_interp->allocate_node<VarNode>()) VarNode(-debruijn));
    }

    NodePtr prim() {
        return NodePtr(_interp, new (_interp->allocate_node<PrimNode>()) PrimNode());
    }

    NodePtr fix() {
        Node* var = new (_interp->allocate_node<VarNode>()) VarNode(1);
        ApplyNode* body = new (_interp->allocate_node<ApplyNode>()) ApplyNode(1, var, 0);
        body->x = body;
        Node* lambda = new (_interp->allocate_node<LambdaNode>()) LambdaNode(0, body);
        return NodePtr(_interp, lambda);
    }

    // NOTE WELL -- DAGness in debruijn graphs can be tricky if you allow terms
    // to be open.  Don't do it.  E.g.
    //   t = var(0);
    //   x = apply(lambda(t), lambda(lambda(t)))
    // will fail to convert correctly because the two var(0)s end up being at
    // different depths.  Keep your shared terms closed, people.
    void fixup(const NodePtr& ptr);
    
  private:
    void fixup_rec(Node* node, depth_t depth);
    
    Interp* _interp;
};

#endif
