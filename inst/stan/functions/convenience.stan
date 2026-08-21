
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
    return rep_row_vector(1.0, rows(obj)) * obj;
  }

  vector rowsum(matrix obj) {
    return obj * rep_vector(1.0, cols(obj));
  }

  /**
   * Precomputes the K x (K - 1) orthonormal Helmert basis projection matrix Q
   * for an unweighted zero-sum constraint of size K.
   */
  matrix build_zero_sum_matrix(int K) {
    int N = K - 1;
    matrix[K, N] Q = rep_matrix(0.0, K, N);

    if (N == 0) {
      return Q;
    }

    for (k in 1:N) {
      real k_real = k;
      real denom = sqrt(k_real * (k_real + 1.0));
      Q[1:k, k] = 1.0 / denom;
      Q[k + 1, k] = -k_real / denom;
    }

    return Q;
  }

  /**
   * Precomputes the K x (K - 1) generalized weighted orthonormal basis matrix Q
   * for a weight vector v of length K.
   */
  matrix build_weighted_zero_sum_matrix(vector v) {
    int K = num_elements(v);
    int N = K - 1;
    matrix[K, N] Q = rep_matrix(0.0, K, N);

    if (N == 0) {
      return Q;
    }

    vector[K] S = cumulative_sum(square(v));

    for (k in 1:N) {
      real denom = sqrt(S[k] * S[k + 1]);
      Q[1:k, k] = (v[k + 1] * v[1:k]) / denom;
      Q[k + 1, k] = -S[k] / denom;
    }

    return Q;
  }

  /**
   * Worker function executed in parallel for each parent location group via map_rect.
   */
  vector apply_zero_sum_transform(
      vector phi,
      vector theta,
      array[] real real_data,
      array[] int int_data
  ) {
    int num_children = int_data[1];
    int num_free = num_children - 1;

    if (num_free == 0) {
      return rep_vector(0.0, 1);
    }

    matrix[num_children, num_free] Q_p = to_matrix(
      to_vector(real_data[1:(num_children * num_free)]),
      num_children,
      num_free
    );
    return Q_p * theta;
  }

