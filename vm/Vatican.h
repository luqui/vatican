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
    , NODETYPE_UNEVAL
    , NODETYPE_VAR
    , NODETYPE_INDIR
    , NODETYPE_PRIM
    , NODETYPE_MAX
    };

struct Node : public GCRef {
    Node(NodeType type, bool blocked, depth_t depth) 
        : depth(depth), blocked(blocked), type(type)
    { }

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
        follow_indirs(const_cast<RootPtr*>(this)->_ptr);
        follow_indirs(const_cast<RootPtr&>(other)._ptr);
        return _ptr.get_ptr() == other._ptr.get_ptr();
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
    
    void calc_size_costs();

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


struct LambdaNode : public Node {
    LambdaNode(depth_t depth, const NodePtr& body) 
        : Node(NODETYPE_LAMBDA, true, depth)
        , body(body)
    { }

    NodePtr body;

    void visit(GCVisitor* visitor) {
        visitor->visit(body);
    }
    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
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


struct MemoTable : public GCRef {
    typedef std::pair<GCRef* const, NodePtr> value_t;
    typedef std::unordered_map<GCRef*, NodePtr, std::hash<GCRef*>, std::equal_to<GCRef*>, HeapAllocator<value_t>> memo_table_t;

    MemoTable(const memo_table_t& table) : table(table)
    { }

    memo_table_t table;

    void visit(GCVisitor* visitor) {
        for (memo_table_t::iterator i = table.begin(); i != table.end(); ++i) {
            if (visitor->alive(i->first)) {
                visitor->visit(i->second);
            }
        }
    }

    size_t size() {
        return sizeof(*this);
    }

    MemoTable* copy(void* target, GCVisitor* visitor) {
        MemoTable* copied = new (target) MemoTable(
            //memo_table_t(table.size()/2, 
            //             std::hash<GCRef*>(),
            //             std::equal_to<GCRef*>(),
            //             table.get_allocator()));
            memo_table_t(table.get_allocator()));
        for (memo_table_t::iterator i = table.begin(); i != table.end(); ++i) {
            // This will immediately add if the key is alive
            visitor->visit_memo_hook(copied, i->first, i->second);
        }
        return copied;
    }

    void destroy() {
        table.clear();
        GCRef::destroy();
    }

    NodePtr lookup(const NodePtr& key) {
        auto i = table.find(key.get_ptr());
        if (i != table.end()) {
            return i->second;
        }
        else {
            return 0;
        }
    }

    void insert(GCRef* key, const NodePtr& value) {
        table.insert(value_t(key, value));
    }
};



struct SubstNode : public Node {
    SubstNode(depth_t depth, const NodePtr& body, depth_t var, const NodePtr& arg, depth_t shift, const Ptr<MemoTable>& memo)
        : Node(NODETYPE_SUBST, false, depth)
        , body(body)
        , arg(arg)
        , memo(memo)
        , var(var)
        , shift(shift)
    { }
        
    NodePtr body;
    NodePtr arg;
    Ptr<MemoTable> memo;
    depth_t var;
    depth_t shift;

    void visit(GCVisitor* visitor) {
        visitor->visit(body);
        visitor->visit(arg);
        visitor->visit(memo);
    }
    
    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
        return new (target) SubstNode(*this);
    }
    void destroy() {
        body = 0;
        arg = 0;
        Node::destroy();
    }
};


struct UnevalNode : public Node {
    UnevalNode(depth_t depth, const NodePtr& alta, const NodePtr& altb)
        : Node(NODETYPE_UNEVAL, false, depth)
        , alta(alta)
        , altb(altb)
        , choice(CHOICE_UNDECIDED)
    { }

    enum Choice { CHOICE_UNDECIDED, CHOICE_A, CHOICE_B };

    NodePtr alta;
    NodePtr altb;
    Choice choice;

    void visit(GCVisitor* visitor) {
        switch (choice) {
            break; case CHOICE_UNDECIDED: {
                if (alta) visitor->visit(alta);
                if (altb) visitor->visit(altb);
            }
            break; case CHOICE_A: {
                visitor->visit(alta);
                altb = 0;
            }
            break; case CHOICE_B: {
                visitor->visit(altb);
                alta = 0;
            }
        }
    }

    void cost_hook() {
        if (alta && altb) {
            // If one of the costs is -1 that means we are pointing back to
            // one of our parents, so if this node exists so does that one
            // already.  So it's free.
            milibytes_t acost = alta->size_cost == -1 ? 0 : alta->size_cost;
            milibytes_t bcost = altb->size_cost == -1 ? 0 : altb->size_cost;

            if (acost < bcost) {
                choice = CHOICE_A;
                size_cost = acost;
            }
            else {
                choice = CHOICE_B;
                size_cost = bcost;
            }
        }
    }

    GCRef* follow_indir() {
        switch (choice) {
            break; case CHOICE_UNDECIDED: {
                return this;
            }
            break; case CHOICE_A: {
                return static_cast<Ptr<GCRef>&>(alta)->follow_indir();
            }
            break; case CHOICE_B: {
                return static_cast<Ptr<GCRef>&>(altb)->follow_indir();
            }
        }
        
    }

    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
        return new (target) UnevalNode(*this); // TODO or just return an indirection straight away?
                                               // or even squash the indirection straight away?
    }
    
    void destroy() {
        alta = 0;
        altb = 0;
        Node::destroy();
    }
};


struct ApplyNode : public Node {
    ApplyNode(depth_t depth, const NodePtr& f, const NodePtr& x)
        : Node(NODETYPE_APPLY, false, depth)
        , f(f)
        , x(x)
    { }

    NodePtr f;
    NodePtr x;

    void visit(GCVisitor* visitor) {
        visitor->visit(f);
        visitor->visit(x);
    }
    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
        return new (target) ApplyNode(*this);
    } 
    void destroy() {
        f = 0;
        x = 0;
        Node::destroy();
    }
};

struct VarNode : public Node {
    VarNode(depth_t depth)
        : Node(NODETYPE_VAR, true, depth)
    { }

    void visit(GCVisitor* visitor) { }
    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
        return new (target) VarNode(*this);
    } 

private:
    // It's possible we can use gc_next to indirect to avoid this padding.
    padding<sizeof(NodePtr)> _indir_padding;
};

struct PrimNode : public Node {
    PrimNode()
        : Node(NODETYPE_PRIM, true, 0)
    { }

    void visit(GCVisitor* visitor) { }
    size_t size() { return sizeof(*this); }
    Node* copy(void* target, GCVisitor*) {
        return new (target) PrimNode(*this);
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
    void fixup_rec(NodePtr& node, depth_t depth);
    
    Interp* _interp;
};

#endif
