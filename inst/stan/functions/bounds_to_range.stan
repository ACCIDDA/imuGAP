
/**
 * @file bounds_to_range.stan
 * @brief Index utility to convert segment starting bounds to [start, end] ranges.
 */

/**
 * Convert strictly increasing segment start indices to contiguous [start, end] index pairs.
 *
 * Given lower bounds [l_1, l_2, ..., l_m] and upper bound ub, constructs a 2 x m array
 * where column i contains [l_i, l_{i+1} - 1] for i < m and [l_m, ub] for the last segment.
 *
 * @param lowers Array of strictly increasing 1-based start indices (length m).
 * @param ub Final upper bound index (must satisfy ub >= lowers[m]).
 * @return 2 x m integer array with row 1 containing start indices and row 2 containing end indices.
 */
array[,] int bounds_to_range(array[] int lowers, int ub) {
  int size_bounds = size(lowers);
  if (size_bounds == 0) {
    array[2, 0] int empty_res;
    return empty_res;
  }
  if (lowers[1] < 1) {
    reject("First lower bound must be >= 1, but found lowers[1] = ", lowers[1]);
  }
  for (i in 1:(size_bounds - 1)) {
    if (lowers[i] >= lowers[i + 1]) {
      reject("Lower bounds must be strictly increasing, but found lowers[", i, "] = ", lowers[i], " >= lowers[", i + 1, "] = ", lowers[i + 1]);
    }
  }
  if (lowers[size_bounds] > ub) {
    reject("Upper bound ", ub, " is less than last lower bound ", lowers[size_bounds]);
  }
  array[size_bounds] int uppers;
  for (i in 1:(size_bounds - 1)) {
    uppers[i] = lowers[i + 1] - 1;
  }
  uppers[size_bounds] = ub;
  return { lowers, uppers };
}
