int n_cnty_free = n_cnty - 1;
int n_sch_free = n_sch - n_cnty;

matrix[n_cnty, n_cnty_free] Q_cnty;
if (n_cnty > 1) {
  matrix[n_cnty, n_cnty] Q_full_c = qr_Q(rep_matrix(1.0, n_cnty, 1));
  Q_cnty = Q_full_c[, 2:n_cnty];
} else {
  Q_cnty = rep_matrix(0.0, 1, 0);
}

matrix[n_sch, n_sch_free] Q_sch = rep_matrix(0.0, n_sch, n_sch_free);
{
  int raw_col_start = 1;
  for (c in 1:n_cnty) {
    int start_s = cnty_map[1, c];
    int end_s = cnty_map[2, c];
    int k_s = end_s - start_s + 1;
    if (k_s > 1) {
      matrix[k_s, k_s] Q_full_s = qr_Q(rep_matrix(1.0, k_s, 1));
      int raw_col_end = raw_col_start + k_s - 2;
      Q_sch[start_s:end_s, raw_col_start:raw_col_end] = Q_full_s[, 2:k_s];
      raw_col_start = raw_col_end + 1;
    }
  }
}
