#include <stdio.h>
#include "ChrisMatrix.h"

int main(int argc, char** argv)
{
  double** A;
  int m = 3;
  int n = 3;
  int i, j; 

  A = malloc(sizeof(double*) * m);
  for(i = 0; i < m; i++)
    A[i] = malloc(sizeof(double) * n);
  
  int k = 0;
  for(i = 0; i < m; i++)
  {
    for(j = 0; j < n; j++)
    {
      A[i][j] = k;
      k++;
    }//end j loop
  }//end i loop
  
  printMatrix(m, n, A);
  transpose(m,n,A);
  printMatrix(m,n,A);
  printf("Hello World\n");

  return 0;
}
