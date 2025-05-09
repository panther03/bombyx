#include <cilk/cilk.h>
#include <stdio.h>

int fib(int n) {
    if (n < 2) {
        return (n);
    } else {
        int x = 0;
        int f1 = cilk_spawn fib(n-1);
        int f2 = cilk_spawn fib(n-2);
        cilk_sync;
        f2 = x * 3;
        x = x + x;
        return (f1 + f2);
    }
}

int main() {
    int n = cilk_spawn fib(6);
    cilk_sync;
    printf("fib = %d\n", n);
}