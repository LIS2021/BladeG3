#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

void printmu(int* mu, int dim) {
  printf("|");
  for(int i = 0; i < dim; i++) {
    printf(" %d |", *(mu + i));
  }
  printf("\n\n");
}

void printvar(int* name, int named, int val) {
  for(int i = 0; i < named; i++) 
    printf("%c", *(name + i));
  printf(" = %d\n", val);
}

void fail() {
  printf("Something bad happened\n");
  exit(-1);
}
