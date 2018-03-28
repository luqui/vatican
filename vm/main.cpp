#include <iostream>
#include <string>
#include <set>
#include "Vatican.h"

// To build nodes for testing, we pun and use var depth as a de bruijn
// index, then postprocess it into the correct depth form.
Node* lambda(Node* body) {
    return new LambdaNode(0, body);
}

Node* apply(Node* f, Node* x) {
    return new ApplyNode(0, f, x);
}

Node* var(int dbi) {
    // We use negative to indicate a debruijn index, because if the graph has
    // sharing it is possible to traverse nodes twice.  A variable's depth is
    // always positive so there is no overlap at 0.
    return new VarNode(-dbi);
}

Node* prim() {
    return new PrimNode();
}

void fixup_debruijn_rec(Node* node, int depth) {
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = (LambdaNode*)node;
            fixup_debruijn_rec(lambda->body, depth+1);
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
            fixup_debruijn_rec(apply->f, depth);
            fixup_debruijn_rec(apply->x, depth);
            apply->depth = std::max(apply->f->depth, apply->x->depth);
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

void show_node_rec(Node* node, bool lambda_parens, bool apply_parens, std::set<Node*> seen) {
    if (seen.find(node) != seen.end()) {
        std::cout << "LOOP";
        return;
    }
    seen.insert(node);

    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = (LambdaNode*)node;
            if (lambda_parens) { std::cout << "("; }
            std::cout << "\\[" << lambda->depth+1 << "]. ";
            show_node_rec(lambda->body, false, false, seen);
            if (lambda_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = (ApplyNode*)node;
            if (apply_parens) { std::cout << "("; }
            show_node_rec(apply->f, true, false, seen);
            std::cout << " ";
            show_node_rec(apply->x, true, true, seen);
            if (apply_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_VAR: {
            std::cout << "x[" << node->depth << "]";
        }
        break; case NODETYPE_PRIM: {
            std::cout << "PRIM";
        }
        break; case NODETYPE_SUBST: {
            SubstNode* subst = (SubstNode*)node;
            std::cout << "(";
            show_node_rec(subst->data.body, true, true, seen);
            std::cout << " @[ " << subst->data.var << " | " << subst->data.shift << " ] ";
            show_node_rec(subst->data.arg, true, true, seen);
            std::cout << ")";
        }
        break; case NODETYPE_INDIR: {
            IndirNode* indir = (IndirNode*)node;
            std::cout << "!";
            show_node_rec(indir->target, true, true, seen);
        }
        break; default: {
            std::cout << "UNSUPPORTED";
        }
    }
}

void show_node(Node* node) {
    std::set<Node*> seen;
    show_node_rec(node, false, false, seen);
    std::cout << "\n";
}

void test_idf() {
    std::cout << "test_idf\n";
    Node* idf = lambda(var(0));
    Node* arg = prim();
    Node* test = fixup_debruijn(apply(idf, arg));

    show_node(test);    
    test = (new Interp())->reduce_whnf(test);
    if (test == arg) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(test);
    }
}

void test_loop() {
    std::cout << "test_loop\n";
    Node* w = lambda(apply(var(0), var(0)));
    Node* loop = apply(w,w);

    Node* test = fixup_debruijn(loop);
    show_node(test);
    try {
        test = (new Interp(DEFAULT_HEAP_SIZE, 1000))->reduce_whnf(test);
        // Shouldn't ever get here
        std::cout << "FAIL\n";
        show_node(test);
    } 
    catch (std::runtime_error& e) {
        if (std::string(e.what()) == "Out of fuel") {
            std::cout << "PASS\n";
        }
        else {
            throw;
        }
    }
}

void test_fix_idf() {
    std::cout << "test_fix_idf\n";
    Node* fix = fixup_debruijn(lambda(apply(var(0), var(0))));
    ((ApplyNode*)((LambdaNode*)fix)->body)->x = ((LambdaNode*)fix)->body;
    Node* idf = fixup_debruijn(lambda(var(0)));
    Node* test = apply(fix, idf);
    show_node(test);
    try {
        test = (new Interp(DEFAULT_HEAP_SIZE, 1000))->reduce_whnf(test);
        std::cout << "FAIL\n";
        show_node(test);
    }
    catch (std::runtime_error& e) {
        if (std::string(e.what()) == "Out of fuel") {
            std::cout << "PASS\n";
        }
        else {
            throw;
        }
    }
}

void test_fix_const() {
    std::cout << "test_fix_const\n";
    Node* fix = fixup_debruijn(lambda(apply(var(0), var(0))));
    ((ApplyNode*)((LambdaNode*)fix)->body)->x = ((LambdaNode*)fix)->body;

    Node* arg = prim();
    Node* test = apply(fix, fixup_debruijn(lambda(arg)));
    show_node(test);
    test = (new Interp())->reduce_whnf(test);
    if (test == arg) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(test);
    }
}

void test_scott_tuple() {
    std::cout << "test_scott_tuple\n";
    // λx y c. c x y
    Node* tuple = fixup_debruijn(lambda(lambda(lambda(apply(apply(var(0), var(2)), var(1))))));
    // λt. t (λx y. x)
    Node* fst = fixup_debruijn(lambda(apply(var(0), lambda(lambda(var(1))))));
    // λt. t (λx y. y)
    Node* snd = fixup_debruijn(lambda(apply(var(0), lambda(lambda(var(0))))));

    Node* primx = prim();
    Node* primy = prim();

    Interp* interp = new Interp;

    Node* tup = apply(apply(tuple, primx), primy);

    Node* resultx = interp->reduce_whnf(apply(fst, tup));
    if (resultx == primx) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(resultx);
    }

    Node* resulty = interp->reduce_whnf(apply(snd, tup));
    if (resulty == primy) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(resulty);
    }
}

void test_scott_stream(size_t heap_size) {
    std::cout << "test_scott_stream(" << heap_size << ")\n";
    
    Node* fix = fixup_debruijn(lambda(apply(var(0), var(0))));
    ((ApplyNode*)((LambdaNode*)fix)->body)->x = ((LambdaNode*)fix)->body;

    // λx y c. c x y
    Node* tuple = fixup_debruijn(lambda(lambda(lambda(apply(apply(var(0), var(2)), var(1))))));
    // λt. t (λx y. x)
    Node* fst = fixup_debruijn(lambda(apply(var(0), lambda(lambda(var(1))))));
    // λt. t (λx y. y)
    Node* snd = fixup_debruijn(lambda(apply(var(0), lambda(lambda(var(0))))));

    Node* arg = prim();
    Node* stream = apply(fix, apply(tuple, arg));

    Interp* interp = new Interp(heap_size, 0);

    try {
        for (int i = 0; i < 100; i++) {
            Node* item = interp->reduce_whnf(apply(fst, stream));
            if (item != arg) {
                std::cout << "FAIL\n";
                std::cout << "i = " << i << "\n";
                show_node(item);
                return;
            }
            stream = apply(snd, stream);
        }
    }
    catch (std::runtime_error& e) {
        std::cout << "FAIL\n";
        std::cout << e.what() << "\n";
        return;
    }

    std::cout << "PASS\n";
}

int main() {
    test_idf();
    test_loop();
    test_fix_idf();
    test_fix_const();
    test_scott_tuple();
    test_scott_stream(DEFAULT_HEAP_SIZE);
    test_scott_stream(1024);
}
