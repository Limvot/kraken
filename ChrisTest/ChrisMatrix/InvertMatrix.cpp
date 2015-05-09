#include<iostream>

    using namespace std;

int main() {
  int i, j, k, n;
  float a[10][10] = {0}, d;
  cout << "No of equations ? ";
  cin >> n;
  cout << "Read all coefficients of matrix with b matrix too " << endl;
  for (i = 1; i <= n; i++)
    for (j = 1; j <= n; j++) cin >> a[i][j];

  for (i = 1; i <= n; i++)
    for (j = 1; j <= 2 * n; j++)
      if (j == (i + n)) a[i][j] = 1;

  /************** partial pivoting **************/
  for (i = n; i > 1; i--) {
    if (a[i - 1][1] < a[i][1])
      for (j = 1; j <= n * 2; j++) {
        d = a[i][j];
        a[i][j] = a[i - 1][j];
        a[i - 1][j] = d;
      }
  }
  cout << "pivoted output: " << endl;
  for (i = 1; i <= n; i++) {
    for (j = 1; j <= n * 2; j++) cout << a[i][j] << "    ";
    cout << endl;
  }
  /********** reducing to diagonal  matrix ***********/

  for (i = 1; i <= n; i++) {
    for (j = 1; j <= n * 2; j++)
      if (j != i) {
        d = a[j][i] / a[i][i];
        for (k = 1; k <= n * 2; k++) a[j][k] -= a[i][k] * d;
      }
  }
  /************** reducing to unit matrix *************/
  for (i = 1; i <= n; i++) {
    d = a[i][i];
    for (j = 1; j <= n * 2; j++) a[i][j] = a[i][j] / d;
  }

  cout << "your solutions: " << endl;
  for (i = 1; i <= n; i++) {
    for (j = n + 1; j <= n * 2; j++) cout << a[i][j] << "    ";
    cout << endl;
  }

  return 0;
}
