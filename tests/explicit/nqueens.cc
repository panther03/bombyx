#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdint.h>

#include "cilk_explicit.hh"

#if CILKSAN
#include "cilksan.h"
#endif

#ifdef SERIAL
#include <cilk/cilk_stub.h>
#endif

unsigned long long todval (struct timeval *tp) {
    return tp->tv_sec * 1000 * 1000 + tp->tv_usec;
}

THREAD(nqueens);
THREAD(nqueens_cont);
THREAD(main_cont);

struct nqueens_closure : closure { 
  int n;
  int j;
  char *a;

  task_fn_t getTask() override {
      return &nqueens;
  }

  nqueens_closure (cont k1, int n, int j, char *a): n(n), j(j), a(a) {k = k1;}
};

struct nqueens_cont_closure : closure {
  int n;
  int *count;

  task_fn_t getTask() override {
    return &nqueens_cont;
  }

  nqueens_cont_closure(cont k1,int n, int *count): n(n), count(count) {k = k1;}
};

struct main_cont_closure : closure {
  int res;
  struct timeval t1;

  task_fn_t getTask() override {
    return &main_cont;
  }

  main_cont_closure(cont k1, int res, struct timeval t1): res(res), t1(t1) {k = k1;}
};

// int * count;

/* 
 * nqueen  4 = 2 
 * nqueen  5 = 10 
 * nqueen  6 = 4 
 * nqueen  7 = 40 
 * nqueen  8 = 92 
 * nqueen  9 = 352 
 * nqueen 10 = 724
 * nqueen 11 = 2680 
 * nqueen 12 = 14200 
 * nqueen 13 = 73712 
 * nqueen 14 = 365596 
 * nqueen 15 = 2279184 
 */

/*
 * <a> contains array of <n> queen positions.  Returns 1
 * if none of the queens conflict, and returns 0 otherwise.
 */
int ok (int n, char *a) {

  int i, j;
  char p, q;

  for (i = 0; i < n; i++) {
    p = a[i];
    for (j = i + 1; j < n; j++) {
      q = a[j];
      if (q == p || q == p - (j - i) || q == p + (j - i))
        return 0;
    }
  }

  return 1;
}

THREAD(nqueens_cont) {
  nqueens_cont_closure *nargs = (nqueens_cont_closure*)(args.get());
  int solNum = 0;
  // #pragma clang loop vectorize(disable)
  for(int i = 0; i < nargs->n; i++) {
    solNum += nargs->count[i];
  }
  SEND_ARGUMENT(nargs->k, solNum);
}

THREAD(nqueens) {
  nqueens_closure *nargs = (nqueens_closure*)(args.get());
  char *b;
  int i;
  int *count;

  if (nargs->n == nargs->j) {
    SEND_ARGUMENT(nargs->k, 1);
  }

  count = (int *) alloca(nargs->n * sizeof(int));
  (void) memset(count, 0, nargs->n * sizeof (int));

  cont k2;
  spawn_next<nqueens_cont_closure> s1 (nqueens_cont_closure(nargs->k, nargs->n, count));
  k2.init(&s1, s1.cls.get());
  for (i = 0; i < (nargs->n); i++) {

    /***
     * ANGE: strictly speaking, this (alloca after spawn) is frowned 
     * up on, but in this case, this is ok, because b returned by 
     * alloca is only used in this iteration; later spawns don't 
     * need to be able to access copies of b from previous iterations 
     ***/
    /* b = (char *) alloca((j + 1) * sizeof (char)); */
    char *b_alloc = (char *) alloca((nargs->j + 1) * sizeof (char) + 31);
    b = (char*)(((uintptr_t)b_alloc + 31) & ~31);
    memcpy(b, nargs->a, nargs->j * sizeof (char));
    b[nargs->j] = i;

    if(ok (nargs->j + 1, b)) {
      k2.ret = &count[i];
      spawn<nqueens_closure> s2(nqueens_closure(k2, nargs->n, nargs->j + 1, b));
    }
  }
}


THREAD(main_cont) {
  main_cont_closure* main_cont_args = (main_cont_closure*)(args.get());
  struct timeval &t1 = main_cont_args->t1;
  int &res = main_cont_args->res;
  struct timeval t2;
  gettimeofday(&t2,0);
  unsigned long long runtime_ms = (todval(&t2)-todval(&t1))/1000;
  printf("%f\n", runtime_ms/1000.0);

  if (res == 0) {
    fprintf (stderr, "No solution found.\n");
  } else {
    fprintf (stderr, "Total number of solutions : %d\n", res);
  }

  return;
}

int main(int argc, char *argv[]) { 

  int n = 13;
  char *a;
  int res;

  if (argc < 2) {
    fprintf (stderr, "Usage: %s [<cilk-options>] <n>\n", argv[0]);
    fprintf (stderr, "Use default board size, n = 13.\n");

  } else {
    n = atoi (argv[1]);
    fprintf (stderr, "Running %s with n = %d.\n", argv[0], n);
  }

  a = (char *) alloca (n * sizeof (char));
  res = 0;

  struct timeval t1;
  gettimeofday(&t1,0);

  cont k;
  spawn_next<main_cont_closure> s1 (main_cont_closure(k, 0, t1));
  SN_BIND(s1, &k, res);

  spawn<nqueens_closure> nq (nqueens_closure(k, n, 0, a));

  return 0;
}