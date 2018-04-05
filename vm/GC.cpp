#include <cassert>

#include "GC.h"

void GCRef::indirect(const Ptr<GCRef>& target) {
    assert(sizeof(Indirection) <= size());

    int rc = refcount;
    destroy();
    new (this) Indirection(target);
    refcount = rc;
}

void GCRef::gc_indirect(GCRef* target) {
    assert(sizeof(GCIndirection) <= size());

    int rc = refcount;
    destroy();
    new (this) GCIndirection(target);
    refcount = rc;
}
