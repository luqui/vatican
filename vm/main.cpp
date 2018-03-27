#include <iostream>
#include "Vatican.h"

// To build nodes for testing, we pun and use var depth as a de bruijn
// index, then postprocess it into the correct depth form.
Node* lambda(Node* body) {
    Node* ret = new Node;
    ret->type = NODETYPE_LAMBDA;
    ret->blocked = true;
    ret->depth = 0;
    ret->lambda.body = body;
    return ret;
}

Node* apply(Node* f, Node* x) {
    Node* ret = new Node;
    ret->type = NODETYPE_APPLY;
    ret->blocked = false;
    ret->depth = 0;
    ret->apply.f = f;
    ret->apply.x = x;
    return ret;
};

Node* var(int dbi) {
    Node* ret = new Node;
    ret->type = NODETYPE_VAR;
    ret->blocked = true;
    ret->depth = dbi;
    return ret;
};

Node* prim() {
    Node* ret = new Node;
    ret->type = NODETYPE_PRIM;
    ret->blocked = true;
    ret->depth = 0;
    return ret;
};


void fixup_debruijn(Node* node, int depth = 0) {
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            fixup_debruijn(node->lambda.body, depth+1);
            if (node->lambda.body->depth == depth+1) {
                node->depth = depth;
            }
            else {
                // Is this correct?  That a lambda that doesn't use its
                // argument has the depth of its body?
                node->depth = node->lambda.body->depth;
            }
        }
        break; case NODETYPE_APPLY: {
            fixup_debruijn(node->apply.f, depth);
            fixup_debruijn(node->apply.x, depth);
            node->depth = std::max(node->apply.f->depth, node->apply.x->depth);
        }
        break; case NODETYPE_VAR: {
            node->depth = depth - node->depth;  // Convert from deBruijn;
        }
        break; default: {
        }
    }
};


void show_node(Node* node, bool lambda_parens = false, bool apply_parens = false) {
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            if (lambda_parens) { std::cout << "("; }
            std::cout << "\\[" << node->depth << "]. ";
            show_node(node->lambda.body, false, false);
            if (lambda_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_APPLY: {
            if (apply_parens) { std::cout << "("; }
            show_node(node->apply.f, true, false);
            std::cout << " ";
            show_node(node->apply.x, true, true);
            if (apply_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_VAR: {
            std::cout << "x[" << node->depth << "]";
        }
        break; case NODETYPE_PRIM: {
            std::cout << "PRIM";
        }
        break; default: {
            std::cout << "UNSUPPORTED";
        }
    }
}


int main() {
    Node* ignoremiddle = lambda(lambda(lambda(apply(var(2), var(0)))));
    fixup_debruijn(ignoremiddle);
    show_node(ignoremiddle);
    std::cout << "\n";
}
