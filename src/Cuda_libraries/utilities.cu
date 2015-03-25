#include "legendre_poly.h"

//Helper functions are defined.
/*
template<class T>
void cudaErrorCheck(T error) {
  if ( strcmp(_cudaGetErrorEnum(error), "cudaSuccess") != 0 ) {
    printf ("%s\n", _cudaGetErrorEnum(error));
  }
  return;
}
*/
void cudaErrorCheck(cudaError_t error) {
  if ( strcmp(_cudaGetErrorEnum(error), "cudaSuccess") != 0 ) {
    printf ("%s\n", _cudaGetErrorEnum(error));
  }
  return;
}

void cudaErrorCheck(cufftResult error) {
  if ( strcmp(_cudaGetErrorEnum(error), "cudaSuccess") != 0 ) {
    printf ("%s\n", _cudaGetErrorEnum(error));
  }
  return;
}
/*void cublasStatusCheck(cublasStatus_t stat) {
  if ( strcmp(_cublasGetErrorEnum(stat),"cublasSuccess") != 0 ) {
    printf("%s\n", _cublasGetErrorEnum(stat));
  }
  return;
}*/

int findMin(int *vector, int size) {
  int min_index=0;
  int current_min=INT_MAX, i;

  for(i = 0; i < size; i++) {
    if( vector[i] < current_min ) {
      current_min = vector[i];
      min_index = i;
    }
  }

  return min_index;
}

int findMax(int *vector, int size) {
  int max_index=0;
  int current_max=INT_MIN, i;

  for(i = 0; i < size; i++) {
    if( vector[i] < current_max ) {
      current_max = vector[i];
      max_index = i;
    }
  }

  return max_index;
}

