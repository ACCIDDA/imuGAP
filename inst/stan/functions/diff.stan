
// Sequential diff for vector and row_vector
vector diff(vector obj) {
  int sz = size(obj);
  return obj[2:] - obj[:(sz - 1)];
}

row_vector diff(row_vector obj) {
  int sz = size(obj);
  return obj[2:] - obj[:(sz - 1)];
}
