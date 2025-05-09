#include <cilk/cilk.h>

long fib(long n) {
  long w;
  long *p;
  if (n < 2) {
    w = 2 + n;
    //*p = (long)&w + w;
    p[3] = 3 + w;
    p = &w;
    long z = *p;
  } else {
    long x, y, z, a;
    x += 1;
    for (int n = 0; n < 8; n++) {
      x = cilk_spawn fib(n-5);
      y = cilk_spawn fib(n-2);
    }
    cilk_sync;
    long D;
    x = 3 + x;
    a = cilk_spawn fib(x-1);
    z = cilk_spawn fib(n-3);
    cilk_sync;
    
    w = (D + x+y + a*z);
  }
  return w;
}