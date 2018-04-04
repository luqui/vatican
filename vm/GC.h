#ifndef __GC_H__
#define __GC_H__

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

class GCRef {
public:
    GCRef() : gc_next(0), refcount(0) {
        node_id = NODE_ID++;
    }
    virtual ~GCRef() { }
    virtual void visit(GCVisitor* visitor) = 0;
    virtual GCRef* copy(void* target) = 0;
    virtual size_t size() = 0;
    virtual void destroy() {
        // Unnecessary, but clear the memory for debugging to make sure we
        // aren't over-freeing.
        memset((void*)this, 0xbf, sizeof(*this));
    }
    virtual GCRef* follow_indir() {
        return this;
    }

    void inc() {
        refcount++;
    }
    void dec() {
        refcount--;
        if (refcount == 0) {
            destroy();
        }
    }

    GCRef* gc_next;
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
    Ptr<U>& cast() {
        return *(Ptr<U>*)this;
    }

    T* operator-> () const {
        return _ptr;
    }
    T* get_ptr() const {
        return _ptr;
    }
    template<class U>
    U* get_subtype() const {
        return (U*)_ptr;
    }

  private:
    T* _ptr;
};


class GCVisitor {
public:
    virtual ~GCVisitor() { }
    virtual void visit(Ptr<GCRef>&) = 0;

    template<class T>
    void visit(Ptr<T>& ptr) {
        visit(ptr.template cast<GCRef>());
    }
};


#endif
