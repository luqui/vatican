#include <cassert>
#include <new>
#include <stdexcept>
#include <iostream>

#include "Vatican.h"

Pool::Pool(size_t heapsize) {
    _pool_start = _cur = new byte[heapsize];
    _pool_end = _pool_start + heapsize;
}

Pool::~Pool() {
    delete _pool_start;
}

void* Pool::allocate(size_t size) {
    if (_cur + size < _pool_end) {
        void* ret = (void*)_cur;
        _cur += size;
        return ret;
    }
    else {
        return 0;
    }
}

void Pool::clear() {
    _cur = _pool_start;

    // We clear it for now to catch any bugs faster.
    memset(_pool_start, 0xbf, size());
}


Node* squash_indirs(Node* node) {
    Node* end = node;
    while (end->type == NODETYPE_INDIR) {
        end = ((IndirNode*)end)->target;
    }
    Node* iter = node;
    while (iter->type == NODETYPE_INDIR) {
        Node* next = ((IndirNode*)iter)->target;
        ((IndirNode*)iter)->target = end;
        iter = next;
    }
    return end;
}

NodePtr::NodePtr(const NodePtr& const_p)
{
    NodePtr& p = const_cast<NodePtr&>(const_p);
    this->_ptr = p._ptr;
    
    // Put this NodePtr just after p in the root set
    this->_next = p._next;
    p._next = this;
    this->_prev = &p;
}

NodePtr::NodePtr(Interp* interp, Node* ptr)
    : _ptr(ptr)
{
    // Put this NodePtr on the back of the rootset
    _next = &interp->_rootset_back;
    _prev = interp->_rootset_back._prev;
    interp->_rootset_back._prev = this;
}

class time_to_gc_exception : public std::exception { };

void Interp::init(size_t heap_size, int fuel) {
    _heap = new Pool(heap_size);
    _backup_heap = 0;
    _fuel = fuel;
    _rootset_front._next = &_rootset_back;
    _rootset_back._prev = &_rootset_front;
}

NodePtr Interp::reduce_whnf(const NodePtr& node) {
    // Negative depths are used for debruijn punning.  You forgot to fixup.
    assert(node->depth >= 0);

  REDO:
    // We only run the GC at the top level, because otherwise the GC might move
    // nodes that are on the C stack being reduced, and ain't nobody got time
    // for that
    try {
        return NodePtr(this, reduce_whnf_wrapper(node._ptr));
    }
    catch (time_to_gc_exception& e) {
        std::cout << "Ok GC time\n";
        run_gc();
        goto REDO;
    }
}

Node* Interp::reduce_whnf_wrapper(Node* node) {
    assert(!_backup_heap || !_backup_heap->contains(node));
    Node* r = reduce_whnf_rec(node);
    assert(!_backup_heap || !_backup_heap->contains(r));
    return r;
}

Node* Interp::reduce_whnf_rec(Node* node) {
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
            ApplyNode* apply = (ApplyNode*)node;
            apply->f = reduce_whnf_wrapper(apply->f);
            
            if (apply->f->type != NODETYPE_LAMBDA) {
                apply->blocked = true;
                return node;
            }
            
            depth_t bind_depth = apply->f->depth + 1;
            depth_t shift = apply->depth - bind_depth;

            // Very sensitive!  Don't inline, we are mutating node's type!
            Node* subst_body = ((LambdaNode*)apply->f)->body;
            Node* subst_arg = apply->x;

            // NB this overwrites node!
            // Assert to make sure the transmogrification is safe.
            assert(sizeof(ApplyNode) >= sizeof(SubstNode));
            new (node) SubstNode(
                apply->depth, subst_body, bind_depth, subst_arg, shift);
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = (SubstNode*)node;
            subst->data.body = reduce_whnf_wrapper(subst->data.body);
            Node* substed = substitute(subst->data.body, subst->data.var, subst->data.arg, subst->data.shift);

            // Make sure the transmogrification is safe.
            assert(sizeof(SubstNode) >= sizeof(IndirNode));
            new (node) IndirNode(substed);
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


Node* Interp::substitute(Node* body, depth_t var, Node* arg, depth_t shift) {
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
            VarNode* varnode = (VarNode*)body;
            if (varnode->depth == var) {
                return arg;
            }
            else {
                return new (allocate_node<VarNode>()) VarNode(newdepth);
            }
        }
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = (LambdaNode*)body;
            
            SubstNode* substbody = new (allocate_node<SubstNode>()) SubstNode(
                newdepth+1, lambda->body, var, arg, shift);

            return new (allocate_node<LambdaNode>()) LambdaNode(newdepth, substbody);
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = (ApplyNode*)body;
        
            SubstNode* newf = new (allocate_node<SubstNode>()) SubstNode(
                newdepth, apply->f, var, arg, shift);

            SubstNode* newx = new (allocate_node<SubstNode>()) SubstNode(
                newdepth, apply->x, var, arg, shift);

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
    GCVisitor(Pool* old_pool, Node** gc_stack)
        : work_left(false)
        , _gc_stack(gc_stack)
        , _old_pool(old_pool)
    { }

    void visit(Node*& node) {
        node = squash_indirs(node);
        if (_old_pool->contains(node)) {
            if (node->gc_next == 0) {
                std::cout << "GC ----------- Adding " << node << "\n";
                node->gc_next = *_gc_stack;
                *_gc_stack = node;
            }
            work_left = true;
        }
    }

    bool work_left;

private:
    Node** _gc_stack;
    Pool* _old_pool;
};

void Interp::run_gc() {
    std::cout << "RUNNING GC\n";
    if (_backup_heap == 0) {
        _backup_heap = new Pool(_heap->size());    
    }
    std::swap(_heap, _backup_heap);
    _heap->clear();

    Node* top = 0;
    Node* cleanup = 0;

    // Visit root set
    for (NodePtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        GCVisitor visitor(_backup_heap, &top);
        i->visit(&visitor);
    }

    while (top) {
        // Remove the node from the gc stack
        Node* node = top;
        top = node->gc_next;
        node->gc_next = 0;

        std::cout << "GC ----------- Visiting " << node << "\n";

        // Copy node to new pool (if it was in the old pool, so we don't copy 
        // externally allocated things).
        Node* copied;
        if (_backup_heap->contains(node)) {
            copied = node->copy(allocate_node(node->size()));
        }
        else {
            copied = node;
        }

        // Add children to gc stack (and update indirections if already moved)
        GCVisitor visitor(_backup_heap, &top);
        node->visit(&visitor);
        
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
    for (NodePtr* i = _rootset_front._next; i != &_rootset_back; i = i->_next) {
        GCVisitor visitor(_backup_heap, &top);
        i->visit(&visitor);
        assert(!visitor.work_left);
    }

    // For debug.
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

void NodeMaker::fixup(const NodePtr& ptr) {
    fixup_rec(ptr._ptr, 0);
}

void NodeMaker::fixup_rec(Node* node, depth_t depth) {
    if (node->type == NODETYPE_VAR ? node->depth > 0 : node->depth >= 0) {
        return;  // Already converted
    }

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = (LambdaNode*)node;
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
            ApplyNode* apply = (ApplyNode*)node;
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
