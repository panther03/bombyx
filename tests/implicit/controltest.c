#include <stdio.h>
#include <stdlib.h>

#include <cilk/cilk.h>


long fib(long n) {
  if (n < 2)
    return n;
    
  long x, y, z, w, a; 

  if (n < 3){
    x += 1;
    return n+2;
  }
  if (n < 4){
    x += 2;
    return n+2;
  }

  if (n < 5) { 
    x+=2;
  } else {
    x += 5;
  }


  a = a + 1;
  {
  long x = w;
  y = x + 2;
  }

  while (n > 0) {
    x = cilk_spawn fib(n-1);
    do {
      y = cilk_spawn fib(n-2);
      cilk_sync;
    } while ( x > 1);
    a = cilk_spawn fib(x-1);
    for (int i = 0; i < 9; i++) {
      if (a) {
        z = cilk_spawn fib(n-3);
      }
    }
    cilk_sync;
    return 1;
  }
  
  
  w = x+y + a*z;
  return w;
}

/*

void fib(cont int k, long n) {
  if (n < 2) {
    send_argument(k, n);
  } else {
    cont int x,y;
    spawn_next(sum(k, ?x, ?y));
    spawn(fib(x, n-2));
    spawn(fib(y, n-1));
  }
}

void sum(cont int k, long x, long y) {
  send_argument(k, x+y);
}
*/