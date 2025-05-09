#include "cilk_explicit.hh"
THREAD(fib);
int main();
THREAD(sn_2);
THREAD(sn_3);

CLOSURE_DEF(fib,
    int n;
);
CLOSURE_DEF(sn_2,
    int f1;
    int f2;
);
CLOSURE_DEF(sn_3,
    int n;
);
#include <cilk/cilk.h>
#include <stdio.h>



THREAD(fib) {
    fib_closure *largs = (fib_closure*)(args.get());
    if (largs->n < 2) {
        SEND_ARGUMENT(largs->k, (largs->n));
    } else {
        sn_2_closure SN_sn_2c(largs->k);
        spawn_next<sn_2_closure> SN_sn_2(SN_sn_2c);
        cont sp0k;
        SN_BIND(SN_sn_2, &sp0k, f1);
        fib_closure sp0c(sp0k);
        sp0c.n = largs->n-1;
        spawn<fib_closure> sp0(sp0c);

        cont sp1k;
        SN_BIND(SN_sn_2, &sp1k, f2);
        fib_closure sp1c(sp1k);
        sp1c.n = largs->n-2;
        spawn<fib_closure> sp1(sp1c);

        // Original sync was here
    }
    return;
}
int main() {
    sn_3_closure SN_sn_3c(CONT_DUMMY);
    spawn_next<sn_3_closure> SN_sn_3(SN_sn_3c);
    cont sp0k;
    SN_BIND(SN_sn_3, &sp0k, n);
    fib_closure sp0c(sp0k);
    sp0c.n = 20;
    spawn<fib_closure> sp0(sp0c);

    // Original sync was here
    return 0;
}
THREAD(sn_2) {
    sn_2_closure *largs = (sn_2_closure*)(args.get());
    SEND_ARGUMENT(largs->k, (largs->f1 + largs->f2));
    return;
}
THREAD(sn_3) {
    sn_3_closure *largs = (sn_3_closure*)(args.get());
    printf("fib = %d\n", largs->n);
    return;
}
