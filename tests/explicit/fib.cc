#include "cilk_explicit.hh"

THREAD(fib);
THREAD(sum);
THREAD(main_cont);

CLOSURE_DEF(fib, 
    int n;
);

CLOSURE_DEF(sum, 
    int arg1;
    int arg2;
);

CLOSURE_DEF(main_cont,
    int fibResult;
);

THREAD(sum) {
    sum_closure *sum_args = (sum_closure*)(args.get());
    SEND_ARGUMENT(sum_args->k, sum_args->arg1 + sum_args->arg2);
}

THREAD(fib) {
    fib_closure *fib_args = (fib_closure*)(args.get());
    if (fib_args->n < 2) {
        SEND_ARGUMENT(fib_args->k, fib_args->n);
    } else {
        cont x, y;
        sum_closure s1c(fib_args->k);
        spawn_next<sum_closure> s1 (s1c);
        SN_BIND(s1, &x, arg1);
        SN_BIND(s1, &y, arg2);
        fib_closure f1c(x);
        f1c.n = fib_args->n - 1;
        spawn<fib_closure> f1 ( f1c);
        fib_closure f2c(y);
        f2c.n = fib_args->n - 2;
        spawn<fib_closure> f2 ( f2c);
    }
}

THREAD(main_cont) {
    main_cont_closure *main_cont_args = (main_cont_closure*)(args.get());
    printf("Fib result %d\n", main_cont_args->fibResult);
    return;
}

int main() {
    cont k;
    main_cont_closure mcc(CONT_DUMMY);
    spawn_next<main_cont_closure> s1 (mcc);
    ((main_cont_closure*)s1.cls)->fibResult = 0;
        // this doesn't make any sense but im too lazy ^^^^
        // to put a null continuation. 
        // doesnt matter tho since it doesnt call k.
    SN_BIND(s1, &k, fibResult);
    fib_closure fc(k);
    fc.n = 30;
    spawn<fib_closure> f ( fc );
    return 0;
}