#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

long long gstart = 0L;

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
void starttime() {
  struct timeval start;
  gettimeofday(&start, NULL);

  gstart = (start.tv_sec * 1000000) + start.tv_usec;
}

void endtime() {
  struct timeval end;
  gettimeofday(&end, NULL);

  printf("%lld\n", (end.tv_sec * 1000000) + end.tv_usec - gstart);

}
*/

void fail() {
  printf("Something bad happened\n");
  exit(-1);
}

