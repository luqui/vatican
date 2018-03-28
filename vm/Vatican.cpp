#include <cassert>
#include <new>
#include <stdexcept>

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
}



Node* squash_indirs(Node* node) {
    Node* end = node;
    while (end->type == NODETYPE_INDIR) {
        end = end->indir.target;
    }
    Node* iter = node;
    while (iter->type == NODETYPE_INDIR) {
        Node* next = iter->indir.target;
        iter->indir.target = end;
        iter = next;
    }
    return end;
}

void Interp::init(size_t heap_size, int fuel) {
    _heap = new Pool(heap_size);
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
            Node* f = node->apply.f = reduce_whnf(node->apply.f);
            
            if (f->type != NODETYPE_LAMBDA) {
                node->blocked = true;
                return node;
            }
            
            depth_t bind_depth = f->depth + 1;
            depth_t shift = node->depth - bind_depth;

            // Very sensitive!  Don't inline, we are mutating node's type!
            Node* subst_body = f->lambda.body;
            Node* subst_arg = node->apply.x;

            node->blocked = false;
            node->type = NODETYPE_SUBST;
            node->subst.body   = subst_body;
            node->subst.var    = bind_depth;
            node->subst.arg    = subst_arg;
            node->subst.shift  = shift;
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            node->subst.body = reduce_whnf(node->subst.body);
            Node* substed = subst(node->subst.body, node->subst.var, node->subst.arg, node->subst.shift);
            node->type = NODETYPE_INDIR;
            node->depth = substed->depth;
            node->indir.target = substed;
            // XXX update depth and blocked?
            //   (Really I think indir needs to precede these properties)
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


Node* Interp::subst(Node* body, depth_t var, Node* arg, depth_t shift) {
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
            if (body->depth == var) {
                return arg;
            }
            else {
                Node* ret = allocate_node();
                ret->type = NODETYPE_VAR;
                ret->blocked = true;
                ret->depth = newdepth;
                return ret;
            }
        }
        break; case NODETYPE_LAMBDA: {
            Node* substbody = allocate_node();
            substbody->type = NODETYPE_SUBST;
            substbody->blocked = false;
            substbody->depth = newdepth+1;
            substbody->subst.body = body->lambda.body;
            substbody->subst.var = var;
            substbody->subst.arg = arg;
            substbody->subst.shift = shift;

            Node* ret = allocate_node();
            ret->type = NODETYPE_LAMBDA;
            ret->blocked = true;
            ret->depth = newdepth;
            ret->lambda.body = substbody;
            return ret;
        }
        break; case NODETYPE_APPLY: {
            Node* newf = allocate_node();
            newf->type = NODETYPE_SUBST;
            newf->blocked = false;
            newf->depth = newdepth;
            newf->subst.body = body->apply.f;
            newf->subst.var = var;
            newf->subst.arg = arg;
            newf->subst.shift = shift;

            Node* newx = allocate_node();
            newx->type = NODETYPE_SUBST;
            newx->blocked = false;
            newx->depth = newdepth;
            newx->subst.body = body->apply.x;
            newx->subst.var = var;
            newx->subst.arg = arg;
            newx->subst.shift = shift;

            Node* ret = allocate_node();
            ret->type = NODETYPE_APPLY;
            ret->blocked = false;
            ret->depth = newdepth;
            ret->apply.f = newf;
            ret->apply.x = newx;

            return ret;
        }
        break; default: {
            return body;
        }
    }
}

Node* Interp::allocate_node() {
    void* mem = _heap->allocate(sizeof(Node));
    if (mem == 0) {
        // Obv do GC now
        throw std::runtime_error("Out of memory in this heap");
    }
    else {
        return new (mem) Node;
    }
}
