
// Column and row sums for matrices
row_vector colsum(matrix obj) {
  int ncols = cols(obj);
  row_vector[ncols] res;
  for (i in 1:ncols) {
    res[i] = sum(obj[, i]);
  }
  return res;
}

vector rowsum(matrix obj) {
  int nrows = rows(obj);
  vector[nrows] res;
  for (i in 1:nrows) {
    res[i] = sum(obj[i, ]);
  }
  return res;
}
