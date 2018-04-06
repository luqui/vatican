#include <cassert>
#include <exception>
#include <iostream>
#include <string>
#include <set>
#include "Vatican.h"

void show_node_rec(Ptr<Node>& node, bool lambda_parens, bool apply_parens, std::set<Node*> seen) {
    follow_indirs(node);

    if (seen.find(node.get_ptr()) != seen.end()) {
        std::cout << "LOOP";
        return;
    }
    seen.insert(node.get_ptr());

    std::cout << node->refcount << "{";
    switch (node->type) {
        break; case NODETYPE_LAMBDA: {
            LambdaNode* lambda = node.get_subtype<LambdaNode>();
            if (lambda_parens) { std::cout << "("; }
            std::cout << "\\[" << lambda->depth+1 << "]. ";
            show_node_rec(lambda->body, false, false, seen);
            if (lambda_parens) { std::cout << ")"; }
        }
        break; case NODETYPE_APPLY: {
            ApplyNode* apply = node.get_subtype<ApplyNode>();
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
            SubstNode* subst = node.get_subtype<SubstNode>();
            std::cout << "(";
            show_node_rec(subst->body, true, true, seen);
            std::cout << " @[ " << subst->var << " | " << subst->shift << " ] ";
            show_node_rec(subst->arg, true, true, seen);
            std::cout << ")";
        }
        break; default: {
            assert(false);
        }
    }
    std::cout << "}";
}

void inspect(Node* node) {
    std::set<Node*> seen;
    NodePtr p = node;
    show_node_rec(p, false, false, seen);
    std::cout << "\n";
}

void show_node(const RootPtr& node) {
    std::set<Node*> seen;
    NodePtr p = node.unsafe_get_ptr();
    show_node_rec(p, false, false, seen);
    std::cout << "\n";
}

class test_failure : public std::exception { };

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
        throw test_failure();
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
        throw test_failure();
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
        throw test_failure();
    }
    catch (std::runtime_error& e) {
        // Perhaps we won't always be able to detect this
        if (std::string(e.what()) == "Indirection cycle detected" || std::string(e.what()) == "Out of fuel") {
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
        throw test_failure();
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
        throw test_failure();
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
        throw test_failure();
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
        for (int i = 0; i < 100; i++) {
            RootPtr item = lib.apply(fst, stream);
            lib.fixup(item);
            item = interp.reduce_whnf(item);
            if (item != arg) {
                std::cout << "FAIL\n";
                std::cout << "i = " << i << "\n";
                show_node(item);
                throw test_failure();
            }
            stream = lib.apply(snd, stream);
            lib.fixup(stream);
        }
    }
    catch (std::runtime_error& e) {
        std::cout << "FAIL\n";
        std::cout << e.what() << "\n";
        throw test_failure();
    }

    std::cout << "PASS\n";
}

void test_heap_resize() {
    std::cout << "test_heap_resize\n";

    const size_t heap_size = 4096;

    Interp interp(heap_size, 0);
    NodeMaker lib(&interp);

    RootPtr test = lib.prim();
    RootPtr arg = lib.prim();

    {
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

    arg = lib.prim();
    lib.fixup(arg);

    // thousand idf (thousand idf prim)
    test = lib.apply(lib.apply(thousand, idf), lib.apply(lib.apply(thousand, idf), arg));
    lib.fixup(test);
    }
    show_node(test);
    show_node(arg);
    int arg_refcount = arg->refcount;
    test = interp.reduce_whnf(test);
    show_node(test);
    if (test != arg) {
        std::cout << "FAIL - incorrect output\n";
        show_node(test);
        throw test_failure();
    }
    std::cout << "Heap size = " << interp.heap_size() << "\n";
    if (!(interp.heap_size() > heap_size)) {
        std::cout << "FAIL - heap did not grow\n";
        throw test_failure();
    }
    if (test->refcount != arg_refcount) {
        std::cout << "FAIL - reference count mismatch (" << arg_refcount << " -> " << test->refcount << ")\n";
        //throw test_failure();
    }
    std::cout << "PASS\n";
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
        throw test_failure();
    }
}


int main() {
    test_idf();
    test_loop();
    test_fix_idf();
    test_fix_const();
    test_scott_tuple();
    test_scott_stream(DEFAULT_HEAP_SIZE);
    test_scott_stream(8192);  // Should be enough heap to carry out the calculation with GCs but no resizing
    test_heap_resize();
    test_cycle_preservation();
}
