// A Bottom-up beta Substituor
// ---------------------------
//
// Based on:
// "Bottom-Up beta-Substitution: Uplinks and lambda-DAGs"
// Olin Shivers & Mitchell Wand, 2004

#include <list>
#include <cstdlib>
#include <iostream>
#include <set>

struct Node;

enum UplinkType { UPLINK_APPL, UPLINK_APPR, UPLINK_NA };

struct Uplink {
    Node* link;
    UplinkType type;
    Uplink* prev;
    Uplink* next;
};

struct UplinkSet {
    UplinkSet() {
        head = 0;
        tail = 0;
    }

    Uplink* head;
    Uplink* tail;

    Uplink* add(Node* link, UplinkType type) {
        Uplink* up = new Uplink;
        up->link = link;
        up->type = type;
        up->prev = tail;
        up->next = 0;
        if (tail) { tail->next = up; }
        tail = up;
        if (head == 0) head = up;
        return up;
    }

    void remove(Uplink* link) {
        if (link->prev) { 
            link->prev->next = link->next; 
        }
        else {
            head = link->next;
        }
        
        if (link->next) { 
            link->next->prev = link->prev;
        }
        else {
            tail = link->prev;
        }

        delete link;
    }

    void unlink(Node* parent, UplinkType type) {
        Uplink* cur = head;
        while (cur) {
            if (cur->link == parent && cur->type == type) {
                remove(cur);
                return;
            }
            cur = cur->next;
        }
        std::abort();
    }
};


struct AppNode {
    Node* left;
    Node* right;
};

struct LambdaNode {
    Node* body;
    Node* var;
};

struct VarNode { };

enum NodeType { NODE_APP, NODE_LAMBDA, NODE_VAR };

struct Node {
    Node* cache;
    UplinkSet uplinks;
    NodeType type;
    union {
        AppNode app;
        LambdaNode lambda;
        VarNode var;
    };
};


void upcopy(Node* newchild, Node* into, UplinkType type) {
    Node* newNode;

    switch (into->type) {
        case NODE_APP: {
            if (into->cache == 0) {
                newNode = new Node;
                newNode->type = NODE_APP;
                newNode->cache = 0;
                // don't install uplinks when creating app nodes,
                // they will be created on the clear pass
                if (type == UPLINK_APPL) {
                    newNode->app.left = newchild;
                    newNode->app.right = into->app.right;
                }
                else {
                    newNode->app.left = into->app.left;
                    newNode->app.right = newchild;
                }
            }
            else {
                newNode = into->cache;
                if (type == UPLINK_APPL) {
                    newNode->app.left = newchild;
                }
                else {
                    newNode->app.right = newchild;
                }
                return;  // don't traverse!
            }

            into->cache = newNode;
            break;
        }
        case NODE_LAMBDA: {
            if (into->cache == (Node*)~0) { // special terminal condition
                return; 
            } 

            // allocate a fresh variable node for the new lambda 
            Node* newVar = new Node;
            newVar->type = NODE_VAR;
            newVar->var = VarNode();
            newVar->cache = 0;
            
            // prepare the new lambda node
            newNode = new Node;
            newNode->type = NODE_LAMBDA;
            newNode->lambda.body = newchild;
            newNode->lambda.var = newVar;
            newNode->cache = 0;

            into->cache = newNode;

            // replace occurrences of the old variable with the new one
            // (tricky business, see paper)
            upcopy(newVar, into->lambda.var, UPLINK_NA);
            break;
        }
        case NODE_VAR: {
            newNode = newchild;
            into->cache = newNode;
            break;
        }
        default: std::abort();
    }
    
    Uplink* cur = into->uplinks.head;
    while (cur) {
        upcopy(newNode, cur->link, cur->type);
        cur = cur->next;
    }
}

void clear(Node* node) {
    Uplink* cur = node->uplinks.head;
    while (cur) {
        if (cur->link->cache == 0) {
            cur = cur->next;
            continue;
        }

        // install uplinks, since we omitted them above
        switch (cur->link->type) {
            case NODE_APP: {
                cur->link->cache->app.left->uplinks.add(cur->link->cache, UPLINK_APPL);
                cur->link->cache->app.right->uplinks.add(cur->link->cache, UPLINK_APPR);
                cur->link->cache = 0;
                break;
            }
            case NODE_LAMBDA: {
                cur->link->cache->lambda.body->uplinks.add(cur->link->cache, UPLINK_NA);
                cur->link->cache = 0;
                clear(cur->link->lambda.var);
                break;
            }
            default: std::abort();
        }

        clear(cur->link);
        cur = cur->next;
    }
}

void cleanup(Node* node) {
    if (node->uplinks.head != 0) return;

    switch (node->type) {
        case NODE_LAMBDA: {
            node->lambda.body->uplinks.unlink(node, UPLINK_NA);
            cleanup(node->lambda.body);
            delete node;
            break;
        }
        case NODE_APP: {
            node->app.left->uplinks.unlink(node, UPLINK_APPL);
            cleanup(node->app.left);
            node->app.right->uplinks.unlink(node, UPLINK_APPR);
            cleanup(node->app.right);
            delete node;
            break;
        }
        case NODE_VAR: {
            delete node;
            break;
        }
        default: std::abort();
    }
}

void upreplace(Node* newchild, Node* into, UplinkType type) {
    switch (into->type) {
        case NODE_APP: {
            if (type == UPLINK_APPL) { 
                Node* old = into->app.left;
                old->uplinks.unlink(into, UPLINK_APPL);
                into->app.left = newchild;
                newchild->uplinks.add(into, UPLINK_APPL);
                cleanup(old);
            }
            else {
                Node* old = into->app.right;
                old->uplinks.unlink(into, UPLINK_APPR);
                into->app.right = newchild;
                newchild->uplinks.add(into, UPLINK_APPR);
                cleanup(old);
            }
            break;
        }
        case NODE_LAMBDA: {
            Node* old = into->lambda.body;
            old->uplinks.unlink(into, UPLINK_NA);
            into->lambda.body = newchild;
            newchild->uplinks.add(into, UPLINK_NA);
            cleanup(old);
            break;
        }
        default: std::abort();
    }
}

void beta_reduce(Node* app) {
    Node* fun = app->app.left;
    Node* arg = app->app.right;
    
    Node* result;
    if (fun->lambda.var->uplinks.head == 0) {
        result = fun->lambda.body;
    }
    else {
        fun->cache = (Node*)~0x0;  // special code for where to stop
        
        upcopy(arg, fun->lambda.var, UPLINK_NA);

        result = fun->lambda.body->cache;
        fun->cache = 0;
        clear(fun->lambda.var);
    }

    Uplink* cur = app->uplinks.head;
    while (cur) {
        Uplink* nexty = cur->next;
        upreplace(result, cur->link, cur->type);
        cur = nexty;
    }
}


bool hnf_reduce_1(Node* ptr) {
    switch (ptr->type) {
        case NODE_LAMBDA: {
            return hnf_reduce_1(ptr->lambda.body);
        }
        case NODE_APP: {
            if (hnf_reduce_1(ptr->app.left)) { return true; }

            if (ptr->app.left->type == NODE_LAMBDA) {
                beta_reduce(ptr);
                return true;
            }
            else {
                return false;
            }
        }
        case NODE_VAR: {
            return false;
        }
        default: std::abort();
    }
}

void hnf_reduce(Node* top) {
    while (hnf_reduce_1(top)) { }
}

void dotify_rec(Node* top, std::ostream& stream, std::set<Node*>* seen) {
    if (seen->find(top) != seen->end()) return;
    seen->insert(top);

    switch (top->type) {
        case NODE_LAMBDA: {
            stream << "p" << top << " [label=\"\\\\\"];\n";
            stream << "p" << top << " -> p" << top->lambda.body << ";\n";
            stream << "p" << top << " -> p" << top->lambda.var << " [color=blue];\n";
            dotify_rec(top->lambda.body, stream, seen);
            break;
        }
        case NODE_APP: {
            stream << "p" << top << " [label=\"*\"];\n";
            stream << "p" << top << " -> p" << top->app.left << " [label=\"fv\"];\n";
            stream << "p" << top << " -> p" << top->app.right << " [label=\"av\"];\n";
            dotify_rec(top->app.left, stream, seen);
            dotify_rec(top->app.right, stream, seen);
            break;
        }
        case NODE_VAR: {
            stream << "p" << top << " [label=\"x\"];\n";
            break;
        }
        default: std::abort();
    }

    Uplink* cur = top->uplinks.head;
    while (cur) {
        stream << "p" << top << " -> p" << cur->link << " [color=red];\n";
        cur = cur->next;
    }
}

void dotify(Node* top, std::ostream& stream) {
    stream << "digraph Lambda {\n";
    std::set<Node*> set;
    dotify_rec(top, stream, &set);
    stream << "}\n";
}



Node* Var() {
    Node* ret = new Node;
    ret->type = NODE_VAR;
    ret->cache = 0;
    return ret;
}

Node* Fun(Node* var, Node* body) {
    Node* ret = new Node;
    ret->type = NODE_LAMBDA;
    ret->lambda.body = body;
    ret->lambda.var = var;
    ret->cache = 0;
    body->uplinks.add(ret, UPLINK_NA);
    return ret;
}

Node* App(Node* left, Node* right) {
    Node* ret = new Node;
    ret->type = NODE_APP;
    ret->app.left = left;
    ret->app.right = right;
    ret->cache = 0;
    left->uplinks.add(ret, UPLINK_APPL);
    right->uplinks.add(ret, UPLINK_APPR);
    return ret;
}

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

Node* Dummy(Node* body) {
    return Fun(Var(), body);
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

int main() {
    Node* expr = Dummy(App(Fix(), Identity()));  // broken!
    //Node* self = SelfApply();
    //Node* expr = Dummy(App(self, self));

    // Node* expr = Dummy(App(App(Succ(), App(Succ(), Zero())), Identity()));

    while (true) {
        dotify(expr, std::cout);
        std::cout << std::flush;
        if (!hnf_reduce_1(expr)) break;
    }
}
