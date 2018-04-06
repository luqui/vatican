#include <algorithm>
#include <cassert>
#include <cstring>
#include <new>
#include <stdexcept>
#include <iostream>

#include "Vatican.h"

int NODE_ID = 0;

Heap::Heap(size_t heapsize) {
    _start = _cur = new byte[heapsize];
    _end = _start + heapsize;
}

Heap::~Heap() {
    delete _start;
}

void* Heap::allocate(size_t size) {
    if (_cur + size < _end) {  // XXX should be <= right?
        void* ret = (void*)_cur;
        _cur += size;
        return ret;
    }
    else {
        return 0;
    }
}

void Heap::clear() {
    _cur = _start;

    // We clear it for now to catch any bugs faster.
    memset(_start, 0xbe, size());
}


RootPtr::RootPtr(const RootPtr& const_p) {
    RootPtr& p = const_cast<RootPtr&>(const_p);

    // Put this RootPtr just after p in the root set
    // [p] -> this -> [p.next]
    //     <-      <-  
    this->_next = p._next;
    this->_prev = &p;
    p._next->_prev = this;
    p._next = this;

    *this = const_p;
}


RootPtr& RootPtr::operator= (const RootPtr& p)
{
    _ptr = p._ptr;
    return *this;
}

RootPtr::RootPtr(Interp* interp, const NodePtr& ptr)
    : _ptr(ptr)
{
    // Put this RootPtr on the back of the rootset
    _next = &interp->_rootset_back;
    _prev = interp->_rootset_back._prev;
    interp->_rootset_back._prev->_next = this;
    interp->_rootset_back._prev = this;
}

void Interp::init(size_t heap_size, int fuel) {
    _heap = new Heap(heap_size);
    _backup_heap = 0;
    _heap->clear();
    _fuel = fuel;
    _rootset_front._next = &_rootset_back;
    _rootset_back._prev = &_rootset_front;
}

RootPtr Interp::reduce_whnf(const RootPtr& node) {
    // Negative depths are used for debruijn punning.  You forgot to fixup.
    assert(node->depth >= 0);

  REDO:
    // We only run the GC at the top level, because otherwise the GC might move
    // nodes that are on the C stack being reduced, and ain't nobody got time
    // for that
    try {
        reduce_whnf_rec(const_cast<NodePtr&>(node._ptr));
        return node;
    }
    catch (time_to_gc_exception& e) {
        try {
            run_gc();
        }
        catch (time_to_gc_exception& e) {
            throw std::runtime_error("GC during GC, wat?");
        }
        goto REDO;
    }
}

void Interp::reduce_whnf_rec(NodePtr& node) {
  REDO:
    if (_fuel > 0 && --_fuel == 0) {
        throw std::runtime_error("Out of fuel");
    }
    
    follow_indirs(node);

    if (node->blocked) {
        return;
    }

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            return;  // Already in whnf.
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = node.get_subtype<ApplyNode>();
            reduce_whnf_rec(apply->f);
            
            if (apply->f->type != NODETYPE_LAMBDA) {
                apply->blocked = true;
                return;
            }
            
            depth_t apply_depth = apply->depth;
            depth_t bind_depth = apply->f->depth + 1;
            depth_t shift = apply->depth - bind_depth;

            NodePtr subst_body = apply->f.get_subtype<LambdaNode>()->body;
            NodePtr subst_arg = apply->x;

            MemoTable* memo = new (allocate_node<MemoTable>()) MemoTable(MemoTable::memo_table_t(get_allocator<MemoTable::value_t>()));
            NodePtr subst = new (allocate_node<SubstNode>()) SubstNode(apply_depth, subst_body, bind_depth, subst_arg, shift, memo);

            if (node->refcount > 1) {
                if (apply->locked) {
                    node = subst;
                }
                else {
                    NodePtr apply_copy = new (allocate_node<ApplyNode>()) ApplyNode(apply_depth, apply->f, apply->x, true);
                    Ptr<GCRef> uneval = new (allocate_node<UnevalNode>()) UnevalNode(apply_depth, apply_copy, subst);
                    node->indirect(uneval);
                }
            }
            else {
                node->indirect(subst);
            }
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = node.get_subtype<SubstNode>();
            reduce_whnf_rec(subst->body);
            NodePtr substed = substitute_memo(subst);
    
            node->indirect(substed);
            goto REDO;
        }
        break; case NODETYPE_UNEVAL: {
            UnevalNode* uneval = node.get_subtype<UnevalNode>();
            // altb is the more evaluated one.
            node = uneval->altb;
            goto REDO;
        }
        break; case NODETYPE_INDIR: {
            throw std::runtime_error("Indir nodes should have been squashed already");
        }
        break; default: { 
            node->blocked = true;
            return;
        }
    }
}

NodePtr Interp::substitute_memo(SubstNode* subst) {
    // If the depth of the body is less than the depth of the variable we are
    // substituting, the variable cannot possibly occur in the body, so just
    // dissolve away.
    if (subst->body->depth < subst->var) {
        return subst->body;
    }
     
    if (NodePtr value = subst->memo->lookup(subst->body)) {
        return value;
    }

    int refcount = subst->body->refcount;  // substitute can return self
    NodePtr result = substitute(subst);
    if (refcount > 1) { // XXX removing this check seems to improve things time-wise
                        // See Thyer p. 64 for maybe why.
        subst->memo->insert(subst->body.get_ptr(), result);
    }

    return result;
}

NodePtr Interp::substitute(SubstNode* subst) {
    depth_t newdepth = subst->body->depth + subst->shift;
    switch (subst->body->type) {
        break; case NODETYPE_VAR: {
            VarNode* varnode = subst->body.get_subtype<VarNode>();
            if (varnode->depth == subst->var) {
                return subst->arg;
            }
            else {
                return new (allocate_node<VarNode>()) VarNode(newdepth);
            }
        }
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = subst->body.get_subtype<LambdaNode>();
            
            follow_indirs(lambda->body);
            // XXX We don't need this depth check, we know we will have to substitue,
            // otherwise the subst node would have evaporated earlier.
            NodePtr substbody = subst->var <= lambda->body->depth
                              ? new (allocate_node<SubstNode>()) SubstNode(
                                   newdepth+1, lambda->body, subst->var, subst->arg, subst->shift, subst->memo)
                              : lambda->body;

            return new (allocate_node<LambdaNode>()) LambdaNode(newdepth, substbody);
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = subst->body.get_subtype<ApplyNode>();
        
            follow_indirs(apply->f);
            NodePtr newf = subst->var <= apply->f->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->f, subst->var, subst->arg, subst->shift, subst->memo)
                         : apply->f;


            follow_indirs(apply->x);
            NodePtr newx = subst->var <= apply->x->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->x, subst->var, subst->arg, subst->shift, subst->memo)
                         : apply->x;

            return new (allocate_node<ApplyNode>()) ApplyNode(
                newdepth, newf, newx, false);  // unlock, because we have more info now
        }
        break; case NODETYPE_UNEVAL: {
            UnevalNode* uneval = subst->body.get_subtype<UnevalNode>();

            follow_indirs(uneval->alta);
            NodePtr newa = subst->var <= uneval->alta->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                                newdepth, uneval->alta, subst->var, subst->arg, subst->shift, subst->memo)
                         : uneval->alta;
            
            follow_indirs(uneval->altb);
            NodePtr newb = subst->var <= uneval->altb->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                                newdepth, uneval->altb, subst->var, subst->arg, subst->shift, subst->memo)
                         : uneval->altb;

            return new (allocate_node<UnevalNode>()) UnevalNode(newdepth, newa, newb);
        }
        break; case NODETYPE_INDIR: {
            throw std::runtime_error("Indir nodes should have been squashed already");
        }
        break; default: {
            return subst->body;
        }
    }
}

class SizeCostGCVisitor : public GCVisitor {
public:
    typedef std::unordered_map<GCRef*, int> penalty_map_t;

    SizeCostGCVisitor(penalty_map_t* circular_penalty) 
        : _cost(0), _circular_penalty(circular_penalty)
    { }

    static int get_effective_refcount(const Ptr<GCRef>& p, penalty_map_t* penalty) {
        auto i = penalty->find(p.get_ptr());
        if (i != penalty->end()) {
            return p->refcount - i->second;
        }
        else {
            return p->refcount;
        }
    }

    void visit(Ptr<GCRef>& ref) {
        _cost += calc_cost(ref, _circular_penalty);
    }

    static milibytes_t calc_cost(Ptr<GCRef>& ref, penalty_map_t* penalty) {
        follow_indirs(ref);
        if (ref->size_cost > 0) {
            return ref->size_cost / get_effective_refcount(ref, penalty);
        }
        // Pun negative cost as a seen marker
        else if (ref->size_cost < 0) {
            auto i = penalty->find(ref.get_ptr());
            if (i != penalty->end()) {
                penalty->insert(std::make_pair(ref.get_ptr(), i->second+1));
            }
            else {
                penalty->insert(std::make_pair(ref.get_ptr(), 1));
            }
            return 0;
        }

        ref->size_cost = -1;

        SizeCostGCVisitor visitor(penalty);
        ref->visit(&visitor);
        ref->size_cost = 1024*ref->size() + visitor._cost;

        // Gives unevaluation nodes a chance to make their choice and modify their
        // effective size.  
        ref->cost_hook();

        return ref->size_cost / get_effective_refcount(ref, penalty);
    }

    bool alive(GCRef* ref) {
        return ref->refcount > 0;  // XXX relies on being able to inspect dead nodes
                                   // (in memo keys), if refcount = 0 frees the memory
                                   // this would no longer work.  Then again neither
                                   // would memo tables as implemented...
    }

    virtual void visit_memo_hook(MemoTable*, GCRef*, const NodePtr&) {
        // Unused for size calculation
        assert(false);
    }
private:
    milibytes_t _cost;
    penalty_map_t* _circular_penalty;
};

void Interp::calc_size_costs() {
    SizeCostGCVisitor::penalty_map_t penalty;

    SizeCostGCVisitor root_visitor(&penalty);
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        i->visit(&root_visitor);
    }
}

typedef std::unordered_multimap<GCRef*, std::pair<MemoTable*, NodePtr>> memo_hooks_t;

class CopyingGCVisitor : public GCVisitor {
public:
    CopyingGCVisitor(Heap* old_heap, GCRef** gc_stack, memo_hooks_t* hooks)
        : work_left(false)
        , _gc_stack(gc_stack)
        , _old_heap(old_heap)
        , _hooks(hooks)
    { }

    void visit(Ptr<GCRef>& ref) {
        ref = ref->follow_indir();
        if (_old_heap->contains(ref.get_ptr())) {
            if (ref->gc_next == 0) {
                assert(ref->refcount > 0);
                ref->gc_next = *_gc_stack;
                *_gc_stack = ref.get_ptr();
            }
            work_left = true;
        }
    }

    bool alive(GCRef* p) {
        return !_old_heap->contains(p) || p->gc_next;
    }

    void visit_memo_hook(MemoTable* memo, GCRef* key, const NodePtr& value) {
        if (!_old_heap->contains(key)) {
            // Add immediately
            MemoTable::value_t pair(key, value);
            memo->table.insert(pair);
        }
        else {
            _hooks->insert(std::make_pair(key, std::make_pair(memo, value)));
        }
    }

    bool work_left;

private:
    GCRef** _gc_stack;
    Heap* _old_heap;
    memo_hooks_t* _hooks;
};

void Interp::run_gc() {
    calc_size_costs();

    if (_backup_heap == 0 || _backup_heap->size() < _heap->size()) {
        delete _backup_heap;
        _backup_heap = new Heap(_heap->size());
    }
        
    std::swap(_heap, _backup_heap);

    GCRef* const TERMINAL = (GCRef*)(-1);

    GCRef* top = TERMINAL;
    GCRef* cleanup = 0;
    memo_hooks_t hooks;

    // Visit root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        CopyingGCVisitor visitor(_backup_heap, &top, &hooks);
        i->visit(&visitor);
    }

    while (top != TERMINAL) {
        // Remove the node from the gc stack
        GCRef* node = top;
        top = node->gc_next;
        node->gc_next = TERMINAL;   // We won't traverse again, but we use this as
                                    // a "seen" marker.  Cleared when copied.
        
        assert(node->refcount > 0);
        
        // Add children to gc stack (and update indirections if already moved)
        CopyingGCVisitor visitor(_backup_heap, &top, &hooks);
        node->visit(&visitor);
        
        // Copy node to new heap (if it was in the old heap, so we don't copy 
        // externally allocated things).
        GCRef* copied;
        if (_backup_heap->contains(node)) {
            copied = node->copy(allocate_node(node->size()), &visitor);
            copied->gc_next = 0;
            copied->refcount = 0;
            copied->size_cost = 0;
        }
        else {
            // (Except there are no externally allocated things)
            assert(false);
        }

        // Make the old node an indirection to the new one (if it was copied)
        if (node != copied) {
            node->gc_indirect(copied);
        }
        
        // Handle any memo hooks
        auto range = hooks.equal_range(node);
        for (memo_hooks_t::iterator i = range.first; i != range.second; ++i) {
            follow_indirs(i->second.second);
            i->second.first->insert(copied, i->second.second);
            assert(static_cast<Ptr<GCRef>&>(i->second.second)->refcount >= 2); // Once in the memo table, once here
            // The value was discovered, so mark it and put it on the stack.
            visitor.visit(i->second.second);
            // And make sure the table is on cleanup stack.
            if (i->second.first->gc_next == 0) {
                i->second.first->gc_next = cleanup;
                cleanup = i->second.first;
            }
        }
        hooks.erase(range.first, range.second);

        // Add the node to the cleanup stack if it had uncopied children.
        if (visitor.work_left) {
            copied->gc_next = cleanup;
            cleanup = copied;
        }
    }
    
    while (cleanup) {
        // Remove the node from the cleanup stack
        GCRef* node = cleanup;
        cleanup = node->gc_next;
        node->gc_next = 0;

        // Update indirections.  Everything should be copied at this point.
        CopyingGCVisitor visitor(_backup_heap, &top, &hooks);
        node->visit(&visitor);
        assert(!visitor.work_left);
    }

    // Clean up root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        CopyingGCVisitor visitor(_backup_heap, &top, &hooks);
        i->visit(&visitor);
        assert(!visitor.work_left);
    }

    hooks.clear();

    // If we reclaimed less than half of the heap, grow the target heap for next time.
    size_t heapsize = _backup_heap->size();
    if (2 * _heap->allocated() > heapsize) {
        delete _backup_heap;
        _backup_heap = new Heap(2*heapsize);
    }
    _backup_heap->clear();
}

void* Interp::allocate_node(size_t size) {
    void* mem = _heap->allocate(size);
    if (mem == 0) {
        throw time_to_gc_exception();
    }
    else {
        return mem;
    }
}


void NodeMaker::fixup(const RootPtr& ptr) {
    fixup_rec(const_cast<NodePtr&>(ptr._ptr), 0);
}

void NodeMaker::fixup_rec(NodePtr& node, depth_t depth) {
    follow_indirs(node);

    if (node->type == NODETYPE_VAR ? node->depth > 0 : node->depth >= 0) {
        return;  // Already converted
    }

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = node.get_subtype<LambdaNode>();
            fixup_rec(lambda->body, depth+1);
            if (lambda->body->depth == depth+1) {
                lambda->depth = depth;
            }
            else {
                // Is this correct?  That a lambda that doesn't use its
                // argument has the depth of its body?
                lambda->depth = lambda->body->depth;
            }
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = node.get_subtype<ApplyNode>();
            fixup_rec(apply->f, depth);
            fixup_rec(apply->x, depth);
            apply->depth = std::max(apply->f->depth, apply->x->depth);
        }
        break; case NODETYPE_VAR: {
            if (node->depth <= 0) {
                node->depth = depth + node->depth;  // Convert from deBruijn;
            }
        }
        break; default: {
            node->depth = 0;
        }
    }
}
