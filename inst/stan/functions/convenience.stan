
  // a function to convert lower bounds l_1, 1_2, ... 1_n
  // to (lower, upper) pairs (l_1, l_2-1), (l_2, l_3-1), ...
  array[,] int bounds_to_range(array[] int lowers, int ub) {
    int size_bounds = size(lowers);
    if (lowers[size_bounds] > ub) {
        print("Upper bound, ", ub, " is less than last lower bound, ", lowers[size_bounds]);
    }
    array[size_bounds] int uppers;
    for (i in 1:(size_bounds - 1)) {
        uppers[i] = lowers[i+1] - 1;
    }
    uppers[size_bounds] = ub;
    return { lowers, uppers };
  }

  // create a matrix, each column multiplied by corresponding row entry
  matrix element_mult_expand(vector colv, row_vector rowv) {
    int nrows = size(colv), ncols = size(rowv);
    matrix[nrows, ncols] result;
    for (i in 1:nrows) {
        result[i,] = rowv * colv[i];
    }
    return result;
  }

  // Sequential diff
  vector diff(vector obj) {
    int sz = size(obj);
    return obj[2:] - obj[:(sz-1)];
  }

  row_vector diff(row_vector obj) {
    int sz = size(obj);
    return obj[2:] - obj[:(sz-1)];
  }

  row_vector colsum(matrix obj) {
    int ncols = cols(obj);
    row_vector[ncols] res;
    for (i in 1:ncols) {
        res[i] = sum(obj[, i]);
    }
    return res;
  }

  vector rowsum(matrix obj) {
    int nrows = cols(obj);
    vector[nrows] res;
    for (i in 1:nrows) {
        res[i] = sum(obj[i, ]);
    }
    return res;
  }
