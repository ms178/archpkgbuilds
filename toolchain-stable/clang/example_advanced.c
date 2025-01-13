#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void matrix_multiply(float* A, float* B, float* C, int N) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      float sum = 0.0f;
      for (int k = 0; k < N; k++) {
        sum += A[i * N + k] * B[k * N + j];
      }
      C[i * N + j] = sum;
    }
  }
}

int main() {
  const int N = 500;
  float *A = malloc(N * N * sizeof(float));
  float *B = malloc(N * N * sizeof(float));
  float *C = malloc(N * N * sizeof(float));

  // Initialize matrices
  srand(time(NULL));
  for (int i = 0; i < N * N; i++) {
    A[i] = (float)(((double)rand() / RAND_MAX));
    B[i] = (float)(((double)rand() / RAND_MAX));
  }

  // Perform multiplication multiple times
  for (int i = 0; i < 3; i++) {
    matrix_multiply(A, B, C, N);
  }

  // Print checksum
  float sum = 0.0f;
  for (int i = 0; i < N * N; i++) {
    sum += C[i];
  }
  printf("Checksum: %f\n", sum);

  free(A);
  free(B);
  free(C);
  return 0;
}
