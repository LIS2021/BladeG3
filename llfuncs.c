#include <stdlib.h>
#include <stdio.h>
//#include <time.h>

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
/*
long starttime() {
  return clock();
}

void endtime(long start) {
  clock_t end = clock();

  printf("time: %f\n\n", (double)(end - start) / CLOCKS_PER_SEC);

}
*/
void fail() {
  printf("Something bad happened\n");
  exit(-1);
}

