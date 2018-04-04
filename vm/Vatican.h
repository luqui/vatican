#ifndef __VATICAN_H__
#define __VATICAN_H__

#include <unordered_map>

#include "GC.h"

typedef int depth_t;

struct Node;

template<size_t s>
class padding {
private:
    byte _padding[s];
};


enum NodeType 
    { NODETYPE_LAMBDA
    , NODETYPE_APPLY
    , NODETYPE_SUBST
    , NODETYPE_VAR
    , NODETYPE_INDIR
    , NODETYPE_PRIM
    , NODETYPE_MAX
    };

struct Node : public GCRef {
    Node(NodeType type, bool blocked, depth_t depth) 
        : depth(depth), blocked(blocked), type(type)
    { }

    // This is a typesig change -- if you follow indirs on a node, you get
    // a node again.
    Node* follow_indir() { return this; }

    depth_t depth;
    bool blocked;
    NodeType type;
};

typedef Ptr<Node> NodePtr;

class RootPtr {
    friend class Interp;
    friend class NodeMaker;
  public:
    ~RootPtr() {
        // Remove this node from the rootset
        _prev->_next = _next;
        _next->_prev = _prev;
    }

    RootPtr(const RootPtr& p);
    
    RootPtr& operator= (const RootPtr& const_p);

    Node* operator -> () {
        return _ptr.operator->();
    }
    Node* operator -> () const {
        return _ptr.operator->();
    }

    void visit(GCVisitor* visitor) {
        visitor->visit(_ptr);
    }

    bool operator == (const RootPtr& other) const {
        return follow_indirs() == other.follow_indirs();
    }

    bool operator != (const RootPtr& other) const {
        return !(*this == other);
    }

    Node* unsafe_get_ptr() const {
        return _ptr.get_ptr();
    }

  private:
    RootPtr(class Interp* interp, const NodePtr& ptr);

    RootPtr()
        : _ptr(0)
        , _next(this)
        , _prev(this)
    {
        // Special constructor for terminal nodes
        // NOTE THE CYCLE!  We rely on comparison to the edges rather than
        // comparison to null pointer.  This is to remove conditionals from
        // the destructor.
    }

    Node* follow_indirs() const;

    NodePtr _ptr;
    RootPtr* _next;
    RootPtr* _prev;
};


const size_t DEFAULT_HEAP_SIZE = 0x100000;  // 1MB

template<class T> class HeapAllocator;

class time_to_gc_exception : public std::exception { };

class Interp {
    friend class RootPtr;
    friend class NodeMaker;
    template<class T> friend class HeapAllocator;

  public:
    Interp() {
        init(DEFAULT_HEAP_SIZE, 0); 
    }

    Interp(size_t heap_size, int fuel) {
        init(heap_size, fuel);
    }

    virtual ~Interp() { 
        delete _heap;
        delete _backup_heap;
    }

    // Destructively reduce the node to whnf.  Returns the same node, 
    // possibily with indirections followed.
    RootPtr reduce_whnf(const RootPtr& node);

    size_t heap_size() const {
        return _heap->size();
    }
    
    void run_gc();

  private:
    Interp(const Interp&);  // No copying

    void init(size_t heap_size, int fuel);
    
    void reduce_whnf_wrapper(NodePtr& node);
    void reduce_whnf_rec(NodePtr& node);

    NodePtr substitute_memo(struct SubstNode* subst);
    NodePtr substitute(struct SubstNode* subst);

    template<class T> 
    void* allocate_node() {
        return allocate_node(sizeof(T));
    }
    
    void* allocate_node(size_t size);
    
    template<class T> HeapAllocator<T> get_allocator();

    int _fuel;
    Heap* _heap;
    Heap* _backup_heap;
    
    RootPtr _rootset_front;
    RootPtr _rootset_back;
};


struct LambdaNode : Node {
    LambdaNode(depth_t depth, const NodePtr& body) 
        : Node(NODETYPE_LAMBDA, true, depth)
        , body(body)
    { }

    NodePtr body;

    void visit(GCVisitor* visitor) {
        visitor->visit(body);
    }
    size_t size() { return sizeof(LambdaNode); }
    Node* copy(void* target) {
        return new (target) LambdaNode(*this);
    } 
    void destroy() {
        body = 0;
        Node::destroy();
    }
};

template<class T>
class HeapAllocator {
    template<class U> friend class HeapAllocator;
  public:
    typedef T value_type;
    HeapAllocator(Interp* interp) noexcept : _interp(interp) { }
    template<class U> HeapAllocator(const HeapAllocator<U>& alloc) noexcept { 
        _interp = alloc._interp;
    }
    T* allocate(size_t n) {
        return (T*)_interp->allocate_node(sizeof(T)*n); 
    }
    void* allocate_bytes(size_t n) {
        return _interp->allocate_node(n);
    }
    void deallocate(T* p, size_t n) { }
  private:
    Interp* _interp;
};

template<class T>
HeapAllocator<T> Interp::get_allocator() {
    return HeapAllocator<T>(this);
};

typedef std::unordered_map<Node*, NodePtr, std::hash<Node*>, std::equal_to<Node*>, HeapAllocator<std::pair<Node* const, NodePtr>>> memo_table_t;


struct SubstNode : Node {
    SubstNode(depth_t depth, const NodePtr& body, depth_t var, const NodePtr& arg, depth_t shift, memo_table_t* memo)
        : Node(NODETYPE_SUBST, false, depth)
        , body(body)
        , arg(arg)
        , memo(memo)
        , var(var)
        , shift(shift)
    { }
        
    NodePtr body;
    NodePtr arg;
    memo_table_t* memo;
    depth_t var;
    depth_t shift;

    void visit(GCVisitor* visitor) {
        visitor->visit(body);
        visitor->visit(arg);
    }
    
    size_t size() { return sizeof(SubstNode); }
    Node* copy(void* target) {
        SubstNode* ret = new (target) SubstNode(*this);
        ret->memo = new (memo->get_allocator().allocate_bytes(sizeof(memo_table_t))) memo_table_t(*memo, memo->get_allocator());
        return ret;
    }
    void destroy() {
        body = 0;
        arg = 0;
        Node::destroy();
    }
};

struct ApplyNode : Node {
    ApplyNode(depth_t depth, const NodePtr& f, const NodePtr& x)
        : Node(NODETYPE_APPLY, false, depth)
        , f(f)
        , x(x)
    { }

    NodePtr f;
    NodePtr x;

    // This is to make sure we have enough space for the transmogrification
    padding<2*sizeof(depth_t) + sizeof(void*)> _padding;

    void visit(GCVisitor* visitor) {
        visitor->visit(f);
        visitor->visit(x);
    }
    size_t size() { return sizeof(ApplyNode); }
    Node* copy(void* target) {
        return new (target) ApplyNode(*this);
    } 
    void destroy() {
        f = 0;
        x = 0;
        Node::destroy();
    }
};

struct VarNode : Node {
    VarNode(depth_t depth)
        : Node(NODETYPE_VAR, true, depth)
    { }

    void visit(GCVisitor* visitor) { }
    size_t size() { return sizeof(VarNode); }
    Node* copy(void* target) {
        return new (target) VarNode(*this);
    } 
    void destroy() {
        Node::destroy();
    }

private:
    // It's possible we can use gc_next to indirect to avoid this padding.
    padding<sizeof(NodePtr)> _indir_padding;
};

struct IndirNode : Node {
    IndirNode(const NodePtr& target)
        : Node(NODETYPE_INDIR, false, target->depth)
        , target(target)
    { }

    NodePtr target;

    Node* follow_indir() {
        Node* r = target->follow_indir();
        target = r;
        return r;  // We "guarantee" that an indirnode points to a node
    }

    void visit(GCVisitor* visitor) {
        // XXX I think indir is a special case, so not sure what this should be...
        visitor->visit(target);
    }
    size_t size() { return sizeof(IndirNode); }
    Node* copy(void* target) {
        return new (target) IndirNode(*this);
    } 
    void destroy() {
        target = 0;
        Node::destroy();
    }
};

struct PrimNode : Node 
{
    PrimNode()
        : Node(NODETYPE_PRIM, true, 0)
    { }

    void visit(GCVisitor* visitor) { }
    size_t size() { return sizeof(PrimNode); }
    Node* copy(void* target) {
        return new (target) PrimNode(*this);
    } 
    void destroy() {
        Node::destroy();
    }

private:
    padding<sizeof(NodePtr)> _indir_padding;
};



class NodeMaker {
  public:
    NodeMaker(Interp* interp) : _interp(interp) { }

    // To build nodes for testing, we pun and use var depth as a de bruijn
    // index, then postprocess it into the correct depth form.
    RootPtr lambda(const RootPtr& body) {
        try {
            return RootPtr(_interp,
                new (_interp->allocate_node<LambdaNode>()) LambdaNode(-1, body._ptr));
        }
        catch (time_to_gc_exception& e) {
            _interp->run_gc();
            return lambda(body);
        }
    }
    RootPtr apply(const RootPtr& f, const RootPtr& x) {
        try {
            return RootPtr(_interp,
                new (_interp->allocate_node<ApplyNode>()) ApplyNode(-1, f._ptr, x._ptr));
        }
        catch (time_to_gc_exception& e) {
            _interp->run_gc();
            return apply(f, x);
        }
    }
    RootPtr var(int debruijn) {
        try {
            // We use negative to indicate a debruijn index, because if the graph has
            // sharing it is possible to traverse nodes twice.  A variable's depth is
            // always strictly positive so there is no overlap at 0.
            return RootPtr(_interp,
                new (_interp->allocate_node<VarNode>()) VarNode(-debruijn));
        }
        catch (time_to_gc_exception& e) {
            _interp->run_gc();
            return var(debruijn);
        }
    }

    RootPtr prim() {
        try {
            return RootPtr(_interp, new (_interp->allocate_node<PrimNode>()) PrimNode());
        }
        catch (time_to_gc_exception& e) {
            _interp->run_gc();
            return prim();
        }
    }

    RootPtr fix() {
        try {
            NodePtr var = new (_interp->allocate_node<VarNode>()) VarNode(1);
            NodePtr body = new (_interp->allocate_node<ApplyNode>()) ApplyNode(1, var, 0);

            body.get_subtype<ApplyNode>()->x = body;
            NodePtr lambda = new (_interp->allocate_node<LambdaNode>()) LambdaNode(0, body);
            return RootPtr(_interp, lambda);
        }
        catch (time_to_gc_exception& e) {
            _interp->run_gc();
            return prim();
        }
    }

    // NOTE WELL -- DAGness in debruijn graphs can be tricky if you allow terms
    // to be open.  Don't do it.  E.g.
    //   t = var(0);
    //   x = apply(lambda(t), lambda(lambda(t)))
    // will fail to convert correctly because the two var(0)s end up being at
    // different depths.  Keep your shared terms closed, people.
    void fixup(const RootPtr& ptr);
    
  private:
    void fixup_rec(const NodePtr& node, depth_t depth);
    
    Interp* _interp;
};

#endif
