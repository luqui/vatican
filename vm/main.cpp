#include <cassert>
#include <iostream>
#include <string>
#include <set>
#include "Vatican.h"

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
            assert(false);
        }
    }
}

void inspect(Node* node) {
    std::set<Node*> seen;
    show_node_rec(node, false, false, seen);
    std::cout << "\n";
}

void show_node(const RootPtr& node) {
    std::set<Node*> seen;
    show_node_rec(node.unsafe_get_ptr(), false, false, seen);
    std::cout << "\n";
}

void test_idf() {
    std::cout << "test_idf\n";

    Interp interp;
    NodeMaker lib(&interp);

    RootPtr arg = lib.prim();
    RootPtr test = lib.apply(lib.lambda(lib.var(0)), arg);
    lib.fixup(test);

    test = interp.reduce_whnf(test);
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

    Interp interp(DEFAULT_HEAP_SIZE, 1000);
    NodeMaker lib(&interp);

    RootPtr w = lib.lambda(lib.apply(lib.var(0), lib.var(0)));
    RootPtr loop = lib.apply(w,w);
    lib.fixup(loop);

    try {
        loop = interp.reduce_whnf(loop);
        // Shouldn't ever get here
        std::cout << "FAIL\n";
        show_node(loop);
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

    Interp interp(DEFAULT_HEAP_SIZE, 1000);
    NodeMaker lib(&interp);

    RootPtr test = lib.apply(lib.fix(), lib.lambda(lib.var(0)));
    lib.fixup(test);
    try {
        test = interp.reduce_whnf(test);
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

    Interp interp;
    NodeMaker lib(&interp);

    RootPtr arg = lib.prim();
    RootPtr test = lib.apply(lib.fix(), lib.lambda(arg));
    lib.fixup(test);
    test = interp.reduce_whnf(test);
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

    Interp interp;
    NodeMaker lib(&interp);

    // λx y c. c x y
    RootPtr tuple = lib.lambda(lib.lambda(lib.lambda(lib.apply(lib.apply(lib.var(0), lib.var(2)), lib.var(1)))));
    lib.fixup(tuple);
    // λt. t (λx y. x)
    RootPtr fst = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(1)))));
    lib.fixup(fst);
    // λt. t (λx y. y)
    RootPtr snd = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(0)))));
    lib.fixup(snd);

    RootPtr primx = lib.prim();
    lib.fixup(primx);
    RootPtr primy = lib.prim();
    lib.fixup(primy);

    RootPtr tup = lib.apply(lib.apply(tuple, primx), primy);
    lib.fixup(tup);

    RootPtr testx = lib.apply(fst, tup);
    lib.fixup(testx);

    RootPtr resultx = interp.reduce_whnf(testx);
    if (resultx == primx) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(resultx);
    }

    RootPtr testy = lib.apply(snd, tup);
    lib.fixup(testy);
    RootPtr resulty = interp.reduce_whnf(testy);
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

    Interp interp(heap_size, 0);
    NodeMaker lib(&interp);
    
    // λx y c. c x y
    RootPtr tuple = lib.lambda(lib.lambda(lib.lambda(lib.apply(lib.apply(lib.var(0), lib.var(2)), lib.var(1)))));
    lib.fixup(tuple);
    // λt. t (λx y. x)
    RootPtr fst = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(1)))));
    lib.fixup(fst);
    // λt. t (λx y. y)
    RootPtr snd = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(0)))));
    lib.fixup(snd);

    RootPtr arg = lib.prim();
    lib.fixup(arg);
    RootPtr stream = lib.apply(lib.fix(), lib.apply(tuple, arg));
    lib.fixup(stream);

    try {
        for (int i = 0; i < 100; i++) {{
            RootPtr item = lib.apply(fst, stream);
            lib.fixup(item);
            item = interp.reduce_whnf(item);
            if (item != arg) {
                std::cout << "FAIL\n";
                std::cout << "i = " << i << "\n";
                show_node(item);
                return;
            }
            stream = lib.apply(snd, stream);
            lib.fixup(stream);
        }}
    }
    catch (std::runtime_error& e) {
        std::cout << "FAIL\n";
        std::cout << e.what() << "\n";
        return;
    }

    std::cout << "PASS\n";
}

void test_heap_resize() {
    std::cout << "test_heap_resize\n";

    const size_t heap_size = 4096;

    Interp interp(heap_size, 0);
    NodeMaker lib(&interp);

    RootPtr idf = lib.lambda(lib.var(0));
    lib.fixup(idf);

    // λf x. x
    RootPtr zero = lib.lambda(lib.lambda(lib.var(0)));
    lib.fixup(zero);
    
    // λn f x. f (n f x)
    RootPtr succ = lib.lambda(lib.lambda(lib.lambda(lib.apply(lib.var(1), lib.apply(lib.apply(lib.var(2), lib.var(1)), lib.var(0))))));
    lib.fixup(succ);

    // λx y. x (y succ) zero
    RootPtr times = lib.lambda(lib.lambda(lib.apply(lib.apply(lib.var(1), lib.apply(lib.var(0), succ)), zero)));
    lib.fixup(times);

    // λf x. f (f (f (f (f (f (f (f (f (f x)))))))))
    RootPtr ten = lib.lambda(lib.lambda(
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1),
        lib.apply(lib.var(1), lib.var(0)))))))))))));
    lib.fixup(ten);
    
    RootPtr thousand = lib.apply(lib.apply(times, lib.apply(lib.apply(times, ten), ten)), ten);
    lib.fixup(thousand);

    RootPtr arg = lib.prim();
    lib.fixup(arg);

    // thousand idf (thousand idf prim)
    RootPtr test = lib.apply(lib.apply(thousand, idf), lib.apply(lib.apply(thousand, idf), arg));
    lib.fixup(test);

    test = interp.reduce_whnf(test);
    if (test == arg && interp.heap_size() > heap_size) {  // if the heap didn't grow, we need to alter the test
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(test);
    }
}

void test_cycle_preservation() {
    std::cout << "test_cycle_preservation\n";
    // This is a test that we need the memoization infrastructure for

    Interp interp;
    NodeMaker lib(&interp);
    
    // λx y c. c x y
    RootPtr tuple = lib.lambda(lib.lambda(lib.lambda(lib.apply(lib.apply(lib.var(0), lib.var(2)), lib.var(1)))));
    lib.fixup(tuple);
    // λt. t (λx y. x)
    RootPtr fst = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(1)))));
    lib.fixup(fst);
    // λt. t (λx y. y)
    RootPtr snd = lib.lambda(lib.apply(lib.var(0), lib.lambda(lib.lambda(lib.var(0)))));
    lib.fixup(snd);

    RootPtr arg = lib.prim();
    lib.fixup(arg);
    
    RootPtr stream = lib.apply(lib.fix(), lib.apply(tuple, arg));
    lib.fixup(stream);

    RootPtr test = lib.apply(snd, stream);
    lib.fixup(test);
    test = interp.reduce_whnf(test);
    if (test == stream) {
        std::cout << "PASS\n";
    }
    else {
        std::cout << "FAIL\n";
        show_node(stream);
        std::cout << "/=\n";
        show_node(test);
    }
}


int main() {
    test_idf();
    test_loop();
    test_fix_idf();
    test_fix_const();
    test_scott_tuple();
    test_scott_stream(DEFAULT_HEAP_SIZE);
    test_scott_stream(4096);  // Should be enough heap to carry out the calculation with GCs but no resizing
    test_heap_resize();
    test_cycle_preservation();
}
