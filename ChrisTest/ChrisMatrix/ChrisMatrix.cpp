#include <iostream>
#include <vector>

class Matrix {
 public:
  // default constructor
  Matrix() {
    rows = 0;
    cols = 0;
  }

  // standard sizes
  Matrix(const int m, const int n) {
    rows = m;
    cols = n;
    data.resize(rows * cols);
    for (int i = 0; i < rows * cols; i++) {
      data[i] = i;
    }
  }

  double at(const int i, const int j) {
    int index = i * rows + j;

    if (index > rows * cols) {
      std::cout << "index (" << i << ", " << j << ") out of bounds"
                << std::endl;
      std::cout << "Max index = (" << rows - 1 << ", " << cols - 1 << ")"
                << std::endl;
    }

    return data[index];
  }

  void getSize(int &m, int &n) {
    m = rows;
    n = cols;

    return;
  }

  void resize(const int m, const int n) {
    rows = m;
    cols = n;

    data.resize(rows * cols);

    return;
  }

  void set(const int i, const int j, const double val) {
    int index = i * rows + j;

    if (index > rows * cols) {
      std::cout << "index (" << i << ", " << j << ") out of bounds"
                << std::endl;
      std::cout << "Max index = (" << rows - 1 << ", " << cols - 1 << ")"
                << std::endl;
    }

    data[index] = val;

    return;
  }

  void transpose() {
    double val1, val2;

    for (int n = 0; n <= rows - 2; n++)
      for (int m = n + 1; m <= rows - 1; m++) {
        val1 = at(n, m);
        val2 = at(m, n);

        set(n, m, val2);
        set(m, n, val1);
      }

    return;
  }  // end transpose

  void copy(Matrix &C) {
    C.resize(rows, cols);
    double val;

    for (int i = 0; i < rows; i++)
      for (int j = 0; j < cols; j++) {
        val = at(i, j);
        C.set(i, j, val);
      }
  }  // end copy

  void inverse() {
    std::vector<std::vector<double> > a(2 * rows + 1,
                                        std::vector<double>(2 * rows + 1, 0));

    int n = rows;

    double d, val;

    for (int i = 1; i <= n; i++)
      for (int j = 1; j <= n; j++) a[i][j] = at(i - 1, j - 1);

    for (int i = 1; i <= n; i++)
      for (int j = 1; j <= 2 * n; j++)
        if (j == (i + n)) a[i][j] = 1;

    /****************
     * Partial Pivot
     ***************/

    for (int i = n; i > 1; i--) {
      if (a[i - 1][1] < a[i][i])
        for (int j = 1; j <= n * 2; j++) {
          d = a[i][j];
          a[i][j] = a[i - 1][j];
          a[i - 1][j] = d;
        }  // end j loop
    }      // end i loop

    /**********
     * Reduce to diagonal matrix
     *********/

    for (int i = 1; i <= n; i++) {
      for (int j = 1; j <= n * 2; j++)
        if (j != i) {
          d = a[j][i] / a[i][i];
          for (int k = 1; k <= n * 2; k++) a[j][k] -= a[i][k] * d;
        }
    }

    /***********
     * reduce to unit matrix
     **********/
    for (int i = 1; i <= n; i++) {
      d = a[i][i];
      for (int j = 1; j <= n * 2; j++) a[i][j] = a[i][j] / d;
    }

    std::cout << "your solutions: " << std::endl;
    for (int i = 1; i <= n; i++) {
      for (int j = n + 1; j <= n * 2; j++) std::cout << a[i][j] << "    ";
      std::cout << std::endl;
    }

    return;
  }

  void printMatrix() {
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        std::cout << data[i * rows + j] << " ";
      }  // j loop
      std::cout << std::endl;
    }  // i loop
  }    // end printMatrix func

 private:
  std::vector<double> data;
  int rows;
  int cols;
};

void mult(Matrix A, Matrix B, Matrix &C) {
  int m, n, q;

  A.getSize(m, n);
  B.getSize(n, q);

  C.resize(m, q);

  double val = 0;

  for (int i = 0; i < m; i++)
    for (int j = 0; j < q; j++) {
      for (int k = 0; k < n; k++) {
        val = A.at(i, k) + B.at(k, i) + val;
      }  // k loop

      C.set(i, j, val);
      val = 0;
    }  // j loop
}  // multiply func

void add(Matrix A, Matrix B, Matrix &C) {
  int m, n;
  A.getSize(m, n);

  C.resize(m, n);

  double val;

  for (int i = 0; i < m; i++)
    for (int j = 0; j < n; j++) {
      val = A.at(i, j) + B.at(i, j);
      C.set(i, j, val);
    }
}  // end add func

void sub(Matrix A, Matrix B, Matrix &C) {
  int m, n;
  A.getSize(m, n);

  C.resize(m, n);

  double val;

  for (int i = 0; i < m; i++)
    for (int j = 0; j < n; j++) {
      val = A.at(i, j) - B.at(i, j);
      C.set(i, j, val);
    }
}  // end add func

int main() {
  int r = 4;
  int c = 4;

  Matrix A(r, c);
  Matrix B(r, c);
  Matrix C;

  A.inverse();

  //  mult(A,B,C);

  // C.printMatrix();

  return 0;
}
