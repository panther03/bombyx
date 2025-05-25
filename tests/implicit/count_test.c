#include <cilk/cilk.h>
#include <stdio.h>
#include <stdlib.h>

int nonsense(int n) {
    int *y = (int*)malloc(sizeof(int) * n);
    for (int i = 0; i < n; i += 1) {
        y[i] = cilk_spawn nonsense(i);
    }
    cilk_sync;
    int sum = 0;
    for (int j = 0; j < n; j += 1) {
        sum += y[j];
    }
    free(y);
    return sum;
}

int main() {
    int n = cilk_spawn nonsense(5);
    cilk_sync;
    printf("big number = %d\n", n);
}