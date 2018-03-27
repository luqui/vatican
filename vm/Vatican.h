#ifndef __VATICAN_H__
#define __VATICAN_H__

#include <cassert>
#include <stdexcept>

typedef int depth_t;

struct Node;
enum NodeType 
    { NODETYPE_LAMBDA
    , NODETYPE_APPLY
    , NODETYPE_SUBST
    , NODETYPE_VAR
    , NODETYPE_INDIR
    , NODETYPE_PRIM
    };

struct LambdaData {
    Node* body;
};

struct ApplyData {
    Node* f;
    Node* x;
};

struct SubstData {
    Node* body;
    depth_t var;
    Node* arg;
    depth_t shift;
};

struct VarData { };

struct IndrData {
    Node* target;
};

struct PrimData { };

struct Node {
    NodeType type;
    bool blocked;
    depth_t depth;

    union {
        LambdaData lambda;
        ApplyData apply;
        SubstData subst;
        VarData var;
        IndrData indir;
        PrimData prim;
    };
};


class Interp {
  public:
    // Destructively reduce the node to whnf.
    void reduce_whnf(Node* node) {
      REDO:
        if (node->blocked) {
            return;
        }

        switch (node->type) {
            break; case NODETYPE_LAMBDA: {
                return;  // Already in whnf.
            }
            break; case NODETYPE_APPLY: {
                Node* f = node->apply.f;
                reduce_whnf(f);
                assert(f->type == NODETYPE_LAMBDA);
                
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
                reduce_whnf(node->subst.body);
                Node* substed = subst(node->subst.body, node->subst.var, node->subst.arg, node->subst.shift);
                node->type = NODETYPE_INDIR;
                node->indir.target = substed;
                // XXX update depth and blocked?
                //   (Really I think indir needs to precede these properties)
                goto REDO;
            }
            break; case NODETYPE_INDIR: {
                reduce_whnf(node->indir.target);
                // Squash nested indirections
                while (node->indir.target->type == NODETYPE_INDIR) {
                    node->indir = node->indir.target->indir;
                    // XXX update depth and such?
                }
            }
            break; default: { 
                node->blocked = true;
            }
        }
    };

    Node* subst(Node* body, depth_t var, Node* arg, depth_t shift) {
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
                    Node* ret = new Node;   // TODO gc
                    ret->type = NODETYPE_VAR;
                    ret->blocked = true;
                    ret->depth = newdepth;
                    return ret;
                }
            }
            break; case NODETYPE_LAMBDA: {
                Node* ret = new Node;  // TODO gc
                ret->type = NODETYPE_SUBST;
                ret->blocked = false;
                ret->depth = newdepth+1;
                ret->subst.body = body->lambda.body;
                ret->subst.var = var;
                ret->subst.arg = arg;
                ret->subst.shift = shift;
                return ret;
            }
            break; case NODETYPE_APPLY: {
                Node* newf = new Node;   // TODO gc
                newf->type = NODETYPE_SUBST;
                newf->blocked = false;
                newf->depth = newdepth;
                newf->subst.body = body->apply.f;
                newf->subst.var = var;
                newf->subst.arg = arg;
                newf->subst.shift = shift;

                Node* newx = new Node;   // TODO gc
                newx->type = NODETYPE_SUBST;
                newx->blocked = false;
                newx->depth = newdepth;
                newx->subst.body = body->apply.x;
                newx->subst.var = var;
                newx->subst.arg = arg;
                newx->subst.shift = shift;

                Node* ret = new Node;   // TODO gc
                ret->type = NODETYPE_APPLY;
                ret->blocked = false;
                ret->depth = newdepth;
                ret->apply.f = newf;
                ret->apply.x = newx;

                return ret;
            }
            break; case NODETYPE_INDIR: {
                return subst(body->indir.target, var, arg, shift);  // TODO tail call
            }
            break; default: {
                return body;
            }
        }
    };
};

#endif
