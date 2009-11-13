#include "vatican.h"
#include <deque>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sstream>

using namespace vatican;

enum Token {
    TOK_LAMBDA = 0,
    TOK_APPLY = 1,
    TOK_VAR = 2,
    TOK_INT32 = 3,
    TOK_PLUS = 4
};

void error(std::string msg) {
    std::cerr << msg << "\n";
    std::exit(1);
}

std::string int_to_string(int i) {
    std::stringstream ss;
    ss << i;
    return ss.str();
}

class PrimInt32 : public PrimNode {
  public:
    int value;
    PrimInt32(int v) : value(v) { }
    PrimNode* apply(Head* other) const { abort(); }
    std::string repr() const { return int_to_string(value); }
};

class PrimPlusPartial : public PrimNode {
  public:
    int value;
    PrimPlusPartial(int v) : value(v) { }
    PrimNode* apply(Head* other) const { 
        hnf_reduce(other);
        return new PrimInt32(((PrimInt32*)get_prim(other))->value + value); 
    }
    std::string repr() const { return "(" + int_to_string(value) + "+)"; }
};

class PrimPlus : public PrimNode {
  public:
    PrimNode* apply(Head* other) const { 
        hnf_reduce(other);
        return new PrimPlusPartial(((PrimInt32*)get_prim(other))->value); 
    }
    std::string repr() const { return "(+)"; }
};

Node* build_node(std::streambuf& stream, std::deque<Node*>& varstack) {
    int code = stream.sbumpc();
    switch (code) {
        case TOK_LAMBDA: { // lambda
            Node* var = Var();
            varstack.push_front(var);
            Node* body = build_node(stream, varstack);
            varstack.pop_front();
            return Fun(var, body);
        }
        case TOK_APPLY: {
            Node* fun = build_node(stream, varstack);
            Node* arg = build_node(stream, varstack);
            return App(fun, arg);
        }
        case TOK_VAR: {
            int ix = stream.sbumpc();
            return varstack[ix];
        }
        case TOK_INT32: {
            int data1 = stream.sbumpc();
            int data2 = stream.sbumpc();
            int data3 = stream.sbumpc();
            int data4 = stream.sbumpc();
            int data = (data1 << 24) | (data2 << 16) | (data3 << 8) | data4;
            return Prim(new PrimInt32(data));
        }
        case TOK_PLUS: {
            return Prim(new PrimPlus());
        }
        default: {
            error("Unknown token");
            return 0;
        }
    }
}

void interpret(Node* node) {
    Head* head = make_head(node);
    hnf_reduce(head);
    std::cout << ((PrimInt32*)get_prim(head))->value << "\n";
    free_head(head);
}

int main(int argc, char** argv) {
    std::deque<Node*> stack;
    std::streambuf* pbuf;
    if (argc == 1) { pbuf = std::cin.rdbuf(); }
    else {
        std::fstream* fin = new std::fstream(argv[1]);
        pbuf = fin->rdbuf();
    }
    interpret(build_node(*pbuf, stack));
}
