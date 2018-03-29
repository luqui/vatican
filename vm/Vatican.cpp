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
    memset(_pool_start, 0, size());
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

void Interp::init(size_t heap_size, int fuel) {
    _heap = new Pool(heap_size);
    _backup_heap = 0;
    _fuel = fuel;
}

Node* Interp::reduce_whnf(Node* node) {
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
            apply->f = reduce_whnf(apply->f);
            
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
            subst->data.body = reduce_whnf(subst->data.body);
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
            std::cout << "GC ----------- Adding " << node << "\n";
            assert(node->gc_next == 0);
            node->gc_next = *_gc_stack;
            *_gc_stack = node;
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
    for (std::vector<Node*>::iterator i = _root_set.begin(); i != _root_set.end(); ++i) {
        std::cout << "GC ----------- Adding " << *i << " (root)\n";
        (*i)->gc_next = top;
        top = *i;
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
}


