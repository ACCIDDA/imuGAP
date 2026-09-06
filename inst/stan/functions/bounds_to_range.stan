
// a function to convert lower bounds l_1, 1_2, ... 1_n
// to (lower, upper) pairs (l_1, l_2-1), (l_2, l_3-1), ...
array[,] int bounds_to_range(array[] int lowers, int ub) {
  int size_bounds = size(lowers);
  if (lowers[size_bounds] > ub) {
    print("Upper bound, ", ub, " is less than last lower bound, ", lowers[size_bounds]);
  }
  array[size_bounds] int uppers;
  for (i in 1:(size_bounds - 1)) {
    uppers[i] = lowers[i + 1] - 1;
  }
  uppers[size_bounds] = ub;
  return { lowers, uppers };
}
