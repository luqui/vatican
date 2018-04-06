#include <cassert>

#include "GC.h"
#include "Vatican.h"

void GCRef::indirect(Ptr<GCRef> target) {
    assert(sizeof(Indirection) <= size());

    Ptr<GCRef> target_copy = target;
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
