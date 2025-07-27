#include <stdlib.h>
#include <stdio.h>
#include <cilk/cilk.h>


// hardcilk backend doesn't like typedefs
#define u64 unsigned long long
#define bool unsigned char
#define true 1
#define false 0

struct node_t {
    bool visited;
    u64 len;
    int* neighbors;
};

struct graph_t {
    u64 n_len;
    struct node_t *nodes;
}; 

void visit(struct graph_t *g, struct node_t *n) {
    #pragma BOMBYX DAE
    struct node_t nd = *n;
    
    if (nd.visited) {
        return;
    }
    
    n->visited = true;

    for (int i = 0; i < nd.len; i += 1) {
        cilk_spawn visit(g, &g->nodes[nd.neighbors[i]]);
    }
}

#define NUM_NODES 10

#pragma BOMBYX IGNORE main
int main() {
    struct node_t *nodes = (struct node_t*) malloc(sizeof(struct node_t) * NUM_NODES);

    for (int i = 0; i < NUM_NODES; i += 1) {
        // fully connected graph
        int *neighbors = (int*) malloc(sizeof(int) * (NUM_NODES - 1));
        int k = 0;
        for (int j = 0; j < NUM_NODES; j += 1) {
            if (i != j) {
                neighbors[k] = j;
                k += 1;
            }
        }
        nodes[i] = (struct node_t){
            .visited = false,
            .len = NUM_NODES - 1,
            .neighbors = neighbors
        };
    }

    struct graph_t *g = (struct graph_t*) malloc(sizeof(struct graph_t));
    g->n_len = NUM_NODES;
    g->nodes = nodes;
    visit(g, &nodes[0]);
    return 0;
}