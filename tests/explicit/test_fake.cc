#include "cilk_explicit.hh"
THREAD(fib);
THREAD(sn_1);
THREAD(sn_2);

CLOSURE_DEF(fib,
    long n;
);
CLOSURE_DEF(sn_1,
    long n;
    long x;
    long y;
);
CLOSURE_DEF(sn_2,
    long x;
    long y;
    long D;
    long z;
    long a;
);
#include <cilk/cilk.h>

THREAD(fib) {
    long w;
    long * p;
    long z;
    int n1;
    fib_closure *largs = (fib_closure*)(args.get());
    if (largs->n < 2) {
        w = l;
        *p = (long)&w + w;
        p[3] = w;
        p = &w;
        z = *p;
        SEND_ARGUMENT(largs->k, w);
    } else {
        sn_1_closure SN_sn_1c(largs->k);
        spawn_next<sn_1_closure> SN_sn_1(SN_sn_1c);
        for (n1 = 0; n1 < 8; n1++) {
            cont sp0k;
            SN_BIND(SN_sn_1, &sp0k, x);
            fib_closure sp0c(sp0k);
            sp0c.n = n1-5;
            spawn<fib_closure> sp0(sp0c);

            cont sp1k;
            SN_BIND(SN_sn_1, &sp1k, y);
            fib_closure sp1c(sp1k);
            sp1c.n = n1-2;
            spawn<fib_closure> sp1(sp1c);

        }
        ((sn_1_closure*)SN_sn_1.cls.get())->n = largs->n;
        // Original sync was here
    }
    return;
}
THREAD(sn_1) {
    long D;
    sn_1_closure *largs = (sn_1_closure*)(args.get());
    sn_2_closure SN_sn_2c(largs->k);
    spawn_next<sn_2_closure> SN_sn_2(SN_sn_2c);
    cont sp0k;
    SN_BIND(SN_sn_2, &sp0k, a);
    fib_closure sp0c(sp0k);
    sp0c.n = largs->x-1;
    spawn<fib_closure> sp0(sp0c);

    cont sp1k;
    SN_BIND(SN_sn_2, &sp1k, z);
    fib_closure sp1c(sp1k);
    sp1c.n = largs->n-3;
    spawn<fib_closure> sp1(sp1c);

    ((sn_2_closure*)SN_sn_2.cls.get())->x = largs->x;
    ((sn_2_closure*)SN_sn_2.cls.get())->y = largs->y;
    ((sn_2_closure*)SN_sn_2.cls.get())->D = D;
    // Original sync was here
    return;
}
THREAD(sn_2) {
    long w;
    sn_2_closure *largs = (sn_2_closure*)(args.get());
    w = (largs->D + largs->x+largs->y + largs->a*largs->z);
    SEND_ARGUMENT(largs->k, w);
    return;
}
