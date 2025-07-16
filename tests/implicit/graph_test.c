//#include <stdlib.h>
#include <cilk/cilk.h>
#include <stdlib.h>

typedef struct node {
    size_t n_len;
    struct node* neighbors;
} node_t;

void visit(int *n) {
    int nd;
    nd = *n;
    
    for (int i = 0; i < nd; i += 1) {
        //nd.neighbors[i]
        cilk_spawn visit(n);
    }
    //return 0;
}

int main() {
    int *n = malloc(sizeof(int) * 10);
    visit(n);
    return 0;
}