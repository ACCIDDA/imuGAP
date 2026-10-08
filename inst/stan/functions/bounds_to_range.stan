/**
 * @file bounds_to_range.stan
 * @brief Utility for converting 1-based start indices to inclusive (lower, upper) segment ranges.
 */

/**
 * Convert strictly increasing lower bounds to 2D range matrix [2, n_bounds].
 *
 * Transforms bounds [l_1, l_2, ..., l_n] and upper bound `ub` into paired segments:
 * [[l_1, l_2, ...], [l_2 - 1, l_3 - 1, ..., ub]].
 *
 * @param lowers 1D array of strictly increasing 1-based starting indices.
 * @param ub Maximum upper bound boundary for the final segment.
 * @return 2D integer array of dimension [2, size(lowers)] where row 1 = lowers, row 2 = uppers.
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
