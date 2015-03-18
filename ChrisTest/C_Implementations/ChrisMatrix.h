#include <stdlib.h>

/*********************
 *  Matrix Transpose
 *  
 *  No real concept of vector transpose
 *  in this kraken implementation
 * *******************/
void transpose(int m, int n, double** A)
{
  int i, j;
  
  /******************************
   * Allocate memory for tmp array
   * ***************************/

  double** A_trans;
  A_trans = malloc(sizeof(double*) * m);
  
  for(i = 0; i < m; i++)
  {
    A_trans[i] = malloc(sizeof(double) * n);
  }
  
  /*********************
   * Copy A into tmp
   ********************/
  for(i = 0; i < m; i++)
  {
    for(j = 0; j < n; j++)
    {
      A_trans[i][j] = A[i][j];
    }//end j loop
  }//end i loop
  
  /***************
   * Transpose operation
   ***************/
  for(i = 0; i < m; i++)
  {
    for(j = 0; j < n; j++)
    {
      A[i][j] = A_trans[j][i];
    }//end j loop
  }//end i loop
  
  /**********
   * Free Memory
   * *******/
  for(i = 0; i < m; i++)
    free(A_trans[i]);
  free(A_trans);

}//end transpose function

void multiply(int m, int n, int q, double** A, double** B, double**C)
{
  //Kraken: check if C is empty, then allocate memory



  return;
}

void printMatrix(int m, int n, double** A)
{
  int i, j;

  for(i = 0; i < m; i++)
  {
    for(j = 0; j < n; j++)
    {
      printf("%f ",A[i][j]);
    }//end j loop
    printf("\n");
  }//end i loop
  printf("\n");
}//end print function






