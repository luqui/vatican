#include "vatican.h"
#include <deque>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sstream>
#include <map>

using namespace vatican;

enum Token {
    TOK_LAMBDA = 0,
    TOK_APPLY = 1,
    TOK_VAR = 2,
    TOK_LET = 3,
    TOK_LETREF = 4,
    TOK_INT32 = 5,
    TOK_PLUS = 6
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
    bool breakage;
    int value;
    PrimPlusPartial(int v) : value(v) { }
    PrimNode* apply(Head* other) const { 
        hnf_reduce(other);
        PrimNode* node = get_prim(other);
        if (node == 0) return 0;
        else return new PrimInt32(((PrimInt32*)node)->value + value); 
    }
    std::string repr() const { return "(" + int_to_string(value) + "+)"; }
};

class PrimPlus : public PrimNode {
  public:
    PrimNode* apply(Head* other) const { 
        hnf_reduce(other);
        PrimNode* node = get_prim(other);
        if (node == 0) return 0;
        else return new PrimPlusPartial(((PrimInt32*)node)->value); 
    }
    std::string repr() const { return "(+)"; }
};

int read_int32(std::streambuf& stream) {
    int data1 = stream.sbumpc();
    int data2 = stream.sbumpc();
    int data3 = stream.sbumpc();
    int data4 = stream.sbumpc();
    return (data1 << 24) | (data2 << 16) | (data3 << 8) | data4;
}

Node* build_node(std::streambuf& stream, std::deque<Node*>& varstack, std::map<int, Node*>& letPad) {
    int code = stream.sbumpc();
    switch (code) {
        case TOK_LAMBDA: { // lambda
            Node* var = Var();
            varstack.push_front(var);
            Node* body = build_node(stream, varstack, letPad);
            varstack.pop_front();
            return Fun(var, body);
        }
        case TOK_APPLY: {
            Node* fun = build_node(stream, varstack, letPad);
            Node* arg = build_node(stream, varstack, letPad);
            return App(fun, arg);
        }
        case TOK_VAR: {
            int ix = stream.sbumpc();
            return varstack[ix];
        }
        case TOK_LET: {
            int id = read_int32(stream);
            Node* val = build_node(stream, varstack, letPad);
            letPad[id] = val;
            Node* in = build_node(stream, varstack, letPad);
            letPad.erase(id);
            return in;
        }
        case TOK_LETREF: {
            int id = read_int32(stream);
            return letPad[id];
        }
        case TOK_INT32: {
            int data = read_int32(stream);
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

void interpret(Head* head) {
    hnf_reduce(head);
    std::cout << ((PrimInt32*)get_prim(head))->value << "\n";
}

Head* HEAD;
void redhook() {
    dotify(HEAD, std::cout);
    std::cout << std::flush;
}

int main(int argc, char** argv) {
    std::deque<Node*> stack;
    std::map<int, Node*> letPad;
    std::streambuf* pbuf;
    if (argc == 1) { pbuf = std::cin.rdbuf(); }
    else {
        std::fstream* fin = new std::fstream(argv[1]);
        pbuf = fin->rdbuf();
    }

    Node* node = build_node(*pbuf, stack, letPad);
    HEAD = make_head(node);
    if (argc == 3 && std::string(argv[2]) == "-t") {
        post_red_hook = &redhook;
    }
    interpret(HEAD);
    free_head(HEAD);
}
