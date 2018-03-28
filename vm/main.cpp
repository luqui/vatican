#include <iostream>
#include <string>
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

    // We use negative to indicate a debruijn index, because if the graph has
    // sharing it is possible to traverse nodes twice.  A variable's depth is
    // always positive so there is no overlap at 0.
    ret->depth = -dbi;
    return ret;
};

Node* prim() {
    Node* ret = new Node;
    ret->type = NODETYPE_PRIM;
    ret->blocked = true;
    ret->depth = 0;
    return ret;
};

void fixup_debruijn_rec(Node* node, int depth) {
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            fixup_debruijn_rec(node->lambda.body, depth+1);
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
            fixup_debruijn_rec(node->apply.f, depth);
            fixup_debruijn_rec(node->apply.x, depth);
            node->depth = std::max(node->apply.f->depth, node->apply.x->depth);
        }
        break; case NODETYPE_VAR: {
            if (node->depth <= 0) {
                node->depth = depth + node->depth;  // Convert from deBruijn;
            }
        }
        break; default: {
        }
    }
};


Node* fixup_debruijn(Node* node) {
    fixup_debruijn_rec(node, 0);
    return node;
};

void show_node_rec(Node* node, bool lambda_parens, bool apply_parens) {
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            if (lambda_parens) { std::cout << "("; }
            std::cout << "\\[" << node->depth << "]. ";
            show_node_rec(node->lambda.body, false, false);
            if (lambda_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_APPLY: {
            if (apply_parens) { std::cout << "("; }
            show_node_rec(node->apply.f, true, false);
            std::cout << " ";
            show_node_rec(node->apply.x, true, true);
            if (apply_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_VAR: {
            std::cout << "x[" << node->depth << "]";
        }
        break; case NODETYPE_PRIM: {
            std::cout << "PRIM";
        }
        break; case NODETYPE_SUBST: {
            std::cout << "(";
            show_node_rec(node->subst.body, true, true);
            std::cout << " @[ " << node->subst.var << " | " << node->subst.shift << " ] ";
            show_node_rec(node->subst.arg, true, true);
            std::cout << ")";
        }
        break; case NODETYPE_INDIR: {
            std::cout << "!";
            show_node_rec(node->indir.target, false, true);
        }
        break; default: {
            std::cout << "UNSUPPORTED";
        }
    }
}

void show_node(Node* node) {
    show_node_rec(node, false, false);
    std::cout << "\n";
}

void test_idf() {
    Node* idf = lambda(var(0));
    Node* arg = prim();
    Node* test = fixup_debruijn(apply(idf, arg));

    show_node(test);    
    (new Interp())->reduce_whnf_nolimit(test);
    if (test == arg) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(test);
    }
}

void test_loop() {
    Node* w = lambda(apply(var(0), var(0)));
    Node* loop = apply(w,w);

    Node* test = fixup_debruijn(loop);
    show_node(test);
    try {
        (new Interp())->reduce_whnf(test, 1000);
        // Shouldn't ever get here
        std::cout << "FAIL\n";
        show_node(test);
    } 
    catch (std::runtime_error& e) {
        if (std::string(e.what()) == "Reduction stack size limit reached") {
            std::cout << "PASS (" << e.what() << ")\n";
        }
    }
}

int main() {
    test_idf();
    test_loop();
}
