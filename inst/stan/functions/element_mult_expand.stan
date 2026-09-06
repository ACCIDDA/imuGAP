
// create a matrix, each column multiplied by corresponding row entry
matrix element_mult_expand(vector colv, row_vector rowv) {
  int nrows = size(colv), ncols = size(rowv);
  matrix[nrows, ncols] result;
  for (i in 1:nrows) {
    result[i, ] = rowv * colv[i];
  }
  return result;
}
