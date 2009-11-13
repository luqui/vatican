#include "vatican.h"
#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace vatican;

Node* Identity() {
    Node* x = Var();
    return Fun(x,x);
}

Node* SelfApply() {
    Node* x = Var();
    return Fun(x, App(x,x));
}

Node* TripleApply() {
    Node* x = Var();
    Node* y = Var();
    return Fun(y,Fun(x, App(App(y,x), x)));
}

Node* Fix() {
    Node* f = Var();
    Node* x = Var();
    Node* inner = Fun(x, App(f, App(x,x)));
    Node* outer = Fun(f, App(SelfApply(), inner));
    return outer;
}

Node* Zero() {
    // \f. \x. x
    Node* f = Var();
    Node* x = Var();
    return Fun(f, Fun(x, x));
}

Node* Const() {
    // \x. \y. x
    Node* x = Var();
    Node* y = Var();
    return Fun(x, Fun(y, x));
}

Node* Succ() {
    // \n. \f. \x. f (n f x)
    Node* n = Var();
    Node* f = Var();
    Node* x = Var();
    return Fun(n, Fun(f, Fun(x, App(f, App(App(n, f), x)))));
}

std::string int_to_string(int i) {
    std::stringstream ss;
    ss << i;
    return ss.str();
}

class IntPrim : public PrimNode {
  public:
    int value;
    IntPrim(int v) : value(v) { }
    PrimNode* action(PrimNode* other) { abort(); }
    std::string repr() { return int_to_string(value); }
};

class PlusIntPrim : public PrimNode {
  public:
    int value;
    PlusIntPrim(int v) : value(v) { }
    PrimNode* action(PrimNode* other) { return new IntPrim(((IntPrim*)other)->value + value); }
    std::string repr() { return "(" + int_to_string(value) + "+)"; }
};

class PlusPrim : public PrimNode {
  public:
    PrimNode* action(PrimNode* other) { return new PlusIntPrim(((IntPrim*)other)->value); }
    std::string repr() { return "(+)"; }
};


int main() {
    Node* expr = Head(App(App(Prim(new PlusPrim()), Prim(new IntPrim(1))), Prim(new IntPrim(2))));
    //Node* expr = Dummy(App(Fix(), Identity()));  // broken!
    //Node* self = SelfApply();
    //Node* expr = Dummy(App(self, self));

    // Node* expr = Dummy(App(App(Succ(), App(Succ(), Zero())), Identity()));

    while (true) {
        dotify(expr, std::cout);
        std::cout << std::flush;
        if (!hnf_reduce_1(expr)) break;
    }
}
