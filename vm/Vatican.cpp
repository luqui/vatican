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

            // Note this overwrites node!
            SubstNode* subst = new (node) SubstNode;
            subst->blocked = false;
            subst->type    = NODETYPE_SUBST;
            subst->body    = subst_body;
            subst->var     = bind_depth;
            subst->arg     = subst_arg;
            subst->shift   = shift;
            goto REDO;
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = (SubstNode*)node;
            subst->body = reduce_whnf(subst->body);
            Node* substed = substitute(subst->body, subst->var, subst->arg, subst->shift);

            IndirNode* indir = new (node) IndirNode;
            indir->type = NODETYPE_INDIR;
            indir->depth = substed->depth;
            indir->target = substed;
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
                VarNode* ret = allocate_node<VarNode>();
                ret->type = NODETYPE_VAR;
                ret->blocked = true;
                ret->depth = newdepth;
                return ret;
            }
        }
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = (LambdaNode*)body;
            
            SubstNode* substbody = allocate_node<SubstNode>();
            substbody->type = NODETYPE_SUBST;
            substbody->blocked = false;
            substbody->depth = newdepth+1;
            substbody->body = lambda->body;
            substbody->var = var;
            substbody->arg = arg;
            substbody->shift = shift;

            LambdaNode* ret = allocate_node<LambdaNode>();
            ret->type = NODETYPE_LAMBDA;
            ret->blocked = true;
            ret->depth = newdepth;
            ret->body = substbody;
            return ret;
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = (ApplyNode*)body;
        
            SubstNode* newf = allocate_node<SubstNode>();
            newf->type = NODETYPE_SUBST;
            newf->blocked = false;
            newf->depth = newdepth;
            newf->body = apply->f;
            newf->var = var;
            newf->arg = arg;
            newf->shift = shift;

            SubstNode* newx = allocate_node<SubstNode>();
            newx->type = NODETYPE_SUBST;
            newx->blocked = false;
            newx->depth = newdepth;
            newx->body = apply->x;
            newx->var = var;
            newx->arg = arg;
            newx->shift = shift;

            ApplyNode* ret = allocate_node<ApplyNode>();
            ret->type = NODETYPE_APPLY;
            ret->blocked = false;
            ret->depth = newdepth;
            ret->f = newf;
            ret->x = newx;

            return ret;
        }
        break; default: {
            return body;
        }
    }
}
