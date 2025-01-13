#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>

template<typename T>
void quicksort(std::vector<T>& arr, int left, int right) {
  if (left >= right) return;

  T pivot = arr[(left + right) / 2];
  int i = left, j = right;

  while (i <= j) {
    while (arr[i] < pivot) i++;
    while (arr[j] > pivot) j--;
    if (i <= j) {
      std::swap(arr[i], arr[j]);
      i++;
      j--;
    }
  }

  quicksort(arr, left, j);
  quicksort(arr, i, right);
}

int main() {
  const int size = 1000000;
  std::vector<int> data(size);

  // Initialize with random values
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(1, size);

  for (int i = 0; i < size; ++i) {
    data[i] = dis(gen);
  }

  // Sort multiple times to generate more PGO data
  for (int i = 0; i < 5; ++i) {
    std::vector<int> temp = data;
    quicksort(temp, 0, temp.size() - 1);
  }

  return 0;
}
