#ifndef __GC_H__
#define __GC_H__

#include <cstring>
#include <new>
#include <iostream>

typedef unsigned char byte;

class Heap {
  public:
    Heap(size_t heapsize);
    virtual ~Heap();

    // Returns 0 if allocation was impossible
    void* allocate(size_t size);

    // Empties the pool for reuse.
    void clear();

    bool contains(void* ptr) {
        return _start <= ptr && ptr < _end;
    }

    size_t size() const {
        return _end - _start;
    }

    size_t allocated() const {
        return _cur - _start;
    }
  private:
    byte* _start;
    byte* _cur;
    byte* _end;
};


class GCVisitor;

extern int NODE_ID;

template<class T> class Ptr;

class GCRef {
public:
    GCRef() : gc_next(0), size_cost(0), refcount(0) {
        node_id = NODE_ID++;
    }
    virtual ~GCRef() { }
    virtual void visit(GCVisitor* visitor) = 0;
    // TODO separate GCVisitor into GCContext that makes sense with the copy.
    virtual GCRef* copy(void* target, class GCVisitor* visitor) = 0;
    virtual size_t size() = 0;
    virtual void destroy() {
        // Unnecessary, but clear the memory for debugging to make sure we
        // aren't over-freeing.
        int id = node_id;
        memset((void*)this, 0xbf, size());
        node_id = id;
    }
    virtual GCRef* follow_indir() {
        return this;
    }
    virtual void cost_hook() { }

    // Note that we do not take a const Ptr<GCRef>& target, because of a very
    // subtle bug if you try to indirect a node to one of its members.  The
    // destructor will end up destroying the pointer before we have a chance to
    // indirect to it.
    void indirect(Ptr<GCRef> target);

    void gc_indirect(GCRef* target);

    void inc() {
        assert(refcount >= 0);
        refcount++;
    }
    void dec() {
        assert(refcount >= 0);
        refcount--;
        if (refcount == 0) {
            destroy();
        }
    }

    GCRef* gc_next;
    float size_cost;
    int refcount;
    // For debugging
    int node_id;
};


template<class T>
class Ptr {
  public:
    Ptr() : _ptr(0) { }
    
    Ptr(T* ptr) : _ptr(ptr) {
        if (_ptr) {
            _ptr->inc();
        }
    }
    ~Ptr() {
        if (_ptr) {
            _ptr->dec();
        }
    }

    Ptr(const Ptr<T>& p) : _ptr(p._ptr) {
        if (_ptr) {
            _ptr->inc();
        }
    }

    Ptr<T>& operator= (const Ptr<T>& p) {
        if (p._ptr == _ptr) return *this;
        T* old_ptr = _ptr;
        _ptr = p._ptr;
        if (_ptr) {
            _ptr->inc();
        }
        if (old_ptr) {
            old_ptr->dec();
        }
        return *this;
    }

    Ptr<T>& operator= (T* p) {
        if (_ptr == p) return *this;
        T* old_ptr = _ptr;
        _ptr = p;
        if (_ptr) {
            _ptr->inc();
        }
        if (old_ptr) {
            old_ptr->dec();
        }
        return *this;
    }

    template<class U>
    operator Ptr<U>& () {
        static_assert(std::is_base_of<U, T>::value, "Non-upcast of Ptr");
        return *reinterpret_cast<Ptr<U>*>(this);
    }

    operator bool() {
        return _ptr != 0;
    }

    T* operator-> () const {
        assert(!_ptr || dynamic_cast<T*>(static_cast<GCRef*>(_ptr)));
        return _ptr;
    }
    T* get_ptr() const {
        assert(!_ptr || dynamic_cast<T*>(static_cast<GCRef*>(_ptr)));
        return _ptr;
    }
    template<class U>
    U* get_subtype() const {
        U* r = dynamic_cast<U*>(static_cast<GCRef*>(_ptr));
        assert(!_ptr || r);
        return r;
    }

  private:
    T* _ptr;
};

template<class T>
void follow_indirs(Ptr<T>& p) {
    T* r = dynamic_cast<T*>(((Ptr<GCRef>&)p)->follow_indir());
    assert(r);
    p = r;
}

class GCVisitor {
public:
    virtual ~GCVisitor() { }
    virtual void visit(Ptr<GCRef>&) = 0;
    virtual bool alive(GCRef*) = 0;
    // Hook a conditional insert -- if GCRef* is found to exist, then
    // put its moved copy in the memo table pointing to the given NodePtr
    // value.
    virtual void visit_memo_hook(struct MemoTable*, GCRef* key, const Ptr<struct Node>& value) = 0;
};


class Indirection : public GCRef {
public:
    Indirection(const Ptr<GCRef>& target) : _target(target)
    { }

    void visit(GCVisitor* visitor) {
        visitor->visit(_target);
        // XXX or maybe _target->visit(visitor)  ?
    }

    GCRef* copy(void* mem, GCVisitor*) {
        assert(false);  // Indirections should be followed, not copied.
    }

    size_t size() {
        return sizeof(*this);
    }

    void destroy() {
        _target = 0;
    }
    
    GCRef* follow_indir() {
        // XXX do this without the C stack
        if (!_target) {
            throw std::runtime_error("Indirection cycle detected");
        }
        
        Ptr<GCRef> temp = _target;
        _target = 0;
        GCRef* r = temp->follow_indir();
        _target = r;
        return r;
    }

private:
    // TODO maybe we can reuse gc_next for this to avoid having to pad.
    Ptr<GCRef> _target;
};


// A GCIndirection is like an indirection except that it is not refcounted.
class GCIndirection : public GCRef {
public:
    GCIndirection(GCRef* target) : _target(target)
    { }

    void visit(GCVisitor* visitor) {
        // We've already visited this, that's why it's an indirection.
        assert(false);
    }

    GCRef* copy(void* mem, GCVisitor*) {
        // We shouldn't ever be copying one of these.
        assert(false);
    }

    size_t size() {
        return sizeof(*this);
    }
    
    GCRef* follow_indir() {
        return _target->follow_indir();
    }

private:
    // TODO maybe we can reuse gc_next for this to avoid having to pad.
    GCRef* _target;
};


#endif
