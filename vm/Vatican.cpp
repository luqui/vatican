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


NodePtr squash_indirs(const NodePtr& node) {
    // TODO NodePtrs might be overkill for the traversal here
    Node* p1 = node.get_ptr();
    Node* p2 = node.get_ptr();    

    while (p1->type == NODETYPE_INDIR) {
        p1 = ((IndirNode*)p1)->target.get_ptr();
        if (p1->type == NODETYPE_INDIR) {
            p1 = ((IndirNode*)p1)->target.get_ptr();
        }
        if (p1->type == NODETYPE_INDIR) {
            if (p2->type == NODETYPE_INDIR) {
                p2 = ((IndirNode*)p2)->target.get_ptr();
            }
            if (p1 == p2) {
                throw std::runtime_error("Indirection cycle detected");
            }
        }
        else {
            break;
        }
    }

    NodePtr iter = node;
    while (iter->type == NODETYPE_INDIR) {
        NodePtr next = iter.get_subtype<IndirNode>()->target;
        iter.get_subtype<IndirNode>()->target = p1;
        iter = next;
    }
    return iter;
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

Node* RootPtr::follow_indirs() const {
    Node* r = _ptr.get_ptr();
    while (r->type == NODETYPE_INDIR) {
        r = ((IndirNode*)r)->target.get_ptr();
    }
    return r;
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
        reduce_whnf_wrapper(const_cast<NodePtr&>(node._ptr));
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

void Interp::reduce_whnf_wrapper(NodePtr& node) {
    assert(!_backup_heap || !_backup_heap->contains(node.get_ptr()));
    assert(0 <= node->type && node->type < NODETYPE_MAX);
    reduce_whnf_rec(node);
    assert(!_backup_heap || !_backup_heap->contains(node.get_ptr()));
    assert(0 <= node->type && node->type < NODETYPE_MAX);
}

void Interp::reduce_whnf_rec(NodePtr& node) {
  REDO:
    if (_fuel > 0 && --_fuel == 0) {
        throw std::runtime_error("Out of fuel");
    }

    node = squash_indirs(node);

    if (node->blocked) {
        return;
    }

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            return;  // Already in whnf.
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = node.get_subtype<ApplyNode>();
            reduce_whnf_wrapper(apply->f);
            
            if (apply->f->type != NODETYPE_LAMBDA) {
                apply->blocked = true;
                return;
            }
            
            depth_t apply_depth = apply->depth;
            depth_t bind_depth = apply->f->depth + 1;
            depth_t shift = apply->depth - bind_depth;

            NodePtr subst_body = apply->f.get_subtype<LambdaNode>()->body;
            NodePtr subst_arg = apply->x;

            int refcount = node->refcount;
            memo_table_t* memo = new (allocate_node<memo_table_t>()) memo_table_t(get_allocator<memo_table_t::value_type>());
            node->destroy();
            // NB this overwrites node!
            // Assert to make sure the transmogrification is safe.
            assert(sizeof(ApplyNode) >= sizeof(SubstNode));
            new (node.get_ptr()) SubstNode(
                apply_depth, subst_body, bind_depth, subst_arg, shift, memo);
            node->refcount = refcount;
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = node.get_subtype<SubstNode>();
            reduce_whnf_wrapper(subst->body);
            NodePtr substed = substitute_memo(subst);
    

            // Make sure the transmogrification is safe.
            int refcount = node->refcount;
            assert(sizeof(SubstNode) >= sizeof(IndirNode));
            node->destroy();
            new (node.get_ptr()) IndirNode(substed);
            node->refcount = refcount;
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
     
    //memo_table_t::iterator i = subst->memo->find(subst->body.get_ptr());
    //if (i != subst->memo->end()) {
    //    return i->second;
    //}
    //int refcount = subst->body->refcount;  // substitute can return self
    NodePtr result = substitute(subst);
    //if (refcount > 1) {
    //    subst->memo->insert(std::pair<Node*, NodePtr>(subst->body.get_ptr(), result));
    //}

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
            
            NodePtr substbody = subst->var <= lambda->body->depth
                              ? new (allocate_node<SubstNode>()) SubstNode(
                                   newdepth+1, lambda->body, subst->var, subst->arg, subst->shift, subst->memo)
                              : lambda->body;

            return new (allocate_node<LambdaNode>()) LambdaNode(newdepth, substbody);
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = subst->body.get_subtype<ApplyNode>();
        
            NodePtr newf = subst->var <= apply->f->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->f, subst->var, subst->arg, subst->shift, subst->memo)
                         : apply->f;


            NodePtr newx = subst->var <= apply->x->depth
                         ? new (allocate_node<SubstNode>()) SubstNode(
                               newdepth, apply->x, subst->var, subst->arg, subst->shift, subst->memo)
                         : apply->x;

            return new (allocate_node<ApplyNode>()) ApplyNode(
                newdepth, newf, newx);
        }
        break; default: {
            return subst->body;
        }
    }
}


class CopyingGCVisitor : public GCVisitor {
public:
    CopyingGCVisitor(Heap* old_heap, GCRef** gc_stack)
        : work_left(false)
        , _gc_stack(gc_stack)
        , _old_heap(old_heap)
    { }

    void visit(Ptr<GCRef>& ref) {
        ref = ref->follow_indir();
        if (_old_heap->contains(ref.get_ptr())) {
            if (ref->gc_next == 0) {
                ref->gc_next = *_gc_stack;
                *_gc_stack = ref.get_ptr();
            }
            work_left = true;
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

    GCRef** _gc_stack;
    Heap* _old_heap;
};

void Interp::run_gc() {
    if (_backup_heap == 0) {
        _backup_heap = new Heap(_heap->size());    
    }
    std::swap(_heap, _backup_heap);

    GCRef* top = 0;
    GCRef* cleanup = 0;

    // Visit root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        CopyingGCVisitor visitor(_backup_heap, &top);
        i->visit(&visitor);
    }

    while (top) {
        // Remove the node from the gc stack
        GCRef* node = top;
        top = node->gc_next;
        node->gc_next = (GCRef*)(-1);  // We won't traverse again, but we use this as
                                       // a "seen" marker.  Cleared when copied.
        
        // Add children to gc stack (and update indirections if already moved)
        CopyingGCVisitor visitor(_backup_heap, &top);
        node->visit(&visitor);
        
        // Copy node to new heap (if it was in the old heap, so we don't copy 
        // externally allocated things).
        GCRef* copied;
        if (_backup_heap->contains(node)) {
            copied = node->copy(allocate_node(node->size()));
            copied->gc_next = 0;
        }
        else {
            // (Except there are no externally allocated things)
            assert(false);
        }
        
        // Make the old node an indirection to the new one (if it was copied)
        if (node != copied) {
            assert(node->size() >= sizeof(IndirNode));
            node->destroy();
            new (node) IndirNode((Node*)copied);  // XXX what if it wasn't a node?
        }
        copied->refcount = 0; // (But it doesn't really count as a reference)

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
        CopyingGCVisitor visitor(_backup_heap, &top);
        node->visit(&visitor);
        assert(!visitor.work_left);
    }
    
    // Clean up root set
    for (RootPtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        CopyingGCVisitor visitor(_backup_heap, &top);
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
