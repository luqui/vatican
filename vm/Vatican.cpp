#include <algorithm>
#include <cassert>
#include <cstring>
#include <new>
#include <stdexcept>

#include "Vatican.h"

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
    memset(_start, 0xbf, size());
}


NodePtr squash_indirs(const NodePtr& node) {
    // TODO NodePtrs might be overkill for the traversal here
    NodePtr end = node;
    while (end->type == NODETYPE_INDIR) {
        end = end.get_subtype<IndirNode>()->target;
    }
    NodePtr iter = node;
    while (iter->type == NODETYPE_INDIR) {
        NodePtr next = iter.get_subtype<IndirNode>()->target;
        iter.get_subtype<IndirNode>()->target = end;
        iter = next;
    }
    return end;
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
    _ptr->inc();
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
    _ptr->inc();
}

class time_to_gc_exception : public std::exception { };

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
        return RootPtr(this, reduce_whnf_wrapper(node._ptr));
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

NodePtr Interp::reduce_whnf_wrapper(const NodePtr& node) {
    assert(!_backup_heap || !_backup_heap->contains(node.get_ptr()));
    assert(0 <= node->type && node->type < NODETYPE_MAX);
    NodePtr r = reduce_whnf_rec(node);
    assert(!_backup_heap || !_backup_heap->contains(r.get_ptr()));
    assert(0 <= node->type && node->type < NODETYPE_MAX);
    return r;
}

NodePtr Interp::reduce_whnf_rec(NodePtr node) {
  REDO:
    if (_fuel > 0 && --_fuel == 0) {
        throw std::runtime_error("Out of fuel");
    }

    node = squash_indirs(node);

    if (node->blocked) {
        return node;
    }

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            return node;  // Already in whnf.
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = node.get_subtype<ApplyNode>();
            apply->f = reduce_whnf_wrapper(apply->f);
            
            if (apply->f->type != NODETYPE_LAMBDA) {
                apply->blocked = true;
                return node;
            }
            
            depth_t bind_depth = apply->f->depth + 1;
            depth_t shift = apply->depth - bind_depth;

            NodePtr subst_body = apply->f.get_subtype<LambdaNode>()->body;
            NodePtr subst_arg = apply->x;

            int refcount = node->refcount;
            // NB this overwrites node!
            // Assert to make sure the transmogrification is safe.
            assert(sizeof(ApplyNode) >= sizeof(SubstNode));
            new (node.get_ptr()) SubstNode(
                apply->depth, subst_body, bind_depth, subst_arg, shift);
            node->refcount = refcount;
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = node.get_subtype<SubstNode>();
            subst->body = reduce_whnf_wrapper(subst->body);
            NodePtr substed = substitute(subst->body, subst->var, subst->arg, subst->shift);

            // Make sure the transmogrification is safe.
            int refcount = node->refcount;
            assert(sizeof(SubstNode) >= sizeof(IndirNode));
            new (node.get_ptr()) IndirNode(substed);
            node->refcount = refcount;
            goto REDO;
        }
        break; case NODETYPE_INDIR: {
            throw std::runtime_error("Indir nodes should have been squashed already");
        }
        break; default: { 
            node->blocked = true;
            return node;
        }
    }
}

NodePtr Interp::substitute(NodePtr body, depth_t var, const NodePtr& arg, depth_t shift) {
    body = squash_indirs(body);

    // If the depth of the body is less than the depth of the variable we are
    // substituting, the variable cannot possibly occur in the body, so just
    // dissolve away.
    if (body->depth < var) {
        return body;
    }

    depth_t newdepth = body->depth + shift;
    switch (body->type) {
        break; case NODETYPE_VAR: {
            VarNode* varnode = body.get_subtype<VarNode>();
            if (varnode->depth == var) {
                return arg;
            }
            else {
                return new (allocate_node<VarNode>()) VarNode(newdepth);
            }
        }
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = body.get_subtype<LambdaNode>();
            
            NodePtr substbody = var <= lambda->body->depth
                              ? new (allocate_node<SubstNode>()) SubstNode(
                                   newdepth+1, lambda->body, var, arg, shift)
                              : lambda->body;

            return new (allocate_node<LambdaNode>()) LambdaNode(newdepth, substbody);
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = body.get_subtype<ApplyNode>();
        
            NodePtr newf = var <= apply->f->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->f, var, arg, shift)
                         : apply->f;


            NodePtr newx = var <= apply->x->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->x, var, arg, shift)
                         : apply->x;

            return new (allocate_node<ApplyNode>()) ApplyNode(
                newdepth, newf, newx);
        }
        break; default: {
            return body;
        }
    }
}


class GCVisitor : public NodeVisitor {
public:
    GCVisitor(Heap* old_heap, Node** gc_stack)
        : work_left(false)
        , _gc_stack(gc_stack)
        , _old_heap(old_heap)
    { }

    void visit(NodePtr& node) {
        Node* raw_node = follow_indirs(node.get_ptr());
        if (_old_heap->contains(raw_node)) {
            if (raw_node->gc_next == 0) {
                raw_node->gc_next = *_gc_stack;
                *_gc_stack = raw_node;
            }
            work_left = true;
        }
        else {
            node = NodePtr(raw_node);
        }
    }

    bool work_left;

private:
    Node* follow_indirs(Node* node) {
        while (node->type == NODETYPE_INDIR) {
            node = ((IndirNode*)node)->target.get_ptr();
        }
        return node;
    }

    Node** _gc_stack;
    Heap* _old_heap;
};

void Interp::run_gc() {
    if (_backup_heap == 0) {
        _backup_heap = new Heap(_heap->size());    
    }
    std::swap(_heap, _backup_heap);

    Node* top = 0;
    Node* cleanup = 0;

    // Visit root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        GCVisitor visitor(_backup_heap, &top);
        i->visit(&visitor);
    }

    while (top) {
        // Remove the node from the gc stack
        Node* node = top;
        top = node->gc_next;
        node->gc_next = (Node*)(-1);  // We won't traverse again, but we use this as
                                      // a "seen" marker.  Cleared when copied.
        
        // Add children to gc stack (and update indirections if already moved)
        GCVisitor visitor(_backup_heap, &top);
        node->visit(&visitor);
        
        // Copy node to new heap (if it was in the old heap, so we don't copy 
        // externally allocated things).
        Node* copied;
        if (_backup_heap->contains(node)) {
            copied = node->copy(allocate_node(node->size()));
            copied->gc_next = 0;
        }
        else {
            // (Except there are no externally allocated things)
            assert(false);
        }
        copied->refcount = 0;
        
        // Make the old node an indirection to the new one (if it was copied)
        if (node != copied) {
            assert(node->size() >= sizeof(IndirNode));
            new (node) IndirNode(copied);
        }

        // Add the node to the cleanup stack if it had uncopied children.
        if (visitor.work_left) {
            copied->gc_next = cleanup;
            cleanup = copied;
        }
    }
    
    while (cleanup) {
        // Remove the node from the cleanup stack
        Node* node = cleanup;
        cleanup = node->gc_next;
        node->gc_next = 0;

        // Update indirections.  Everything should be copied at this point.
        GCVisitor visitor(_backup_heap, &top);
        node->visit(&visitor);
        assert(!visitor.work_left);
    }
    
    // Clean up root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        GCVisitor visitor(_backup_heap, &top);
        i->visit(&visitor);
        assert(!visitor.work_left);
    }

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
    fixup_rec(ptr._ptr, 0);
}

void NodeMaker::fixup_rec(const NodePtr& node, depth_t depth) {
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
