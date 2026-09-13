vector[n_cohort] logit_phi_st = bs * beta_bs;
vector[n_locs - 1] off_layer = compute_layer_offsets(qr_basis, z_layer, loc_pop_scale, sigma_layer, loc_layer_idx);
vector[n_locs] logit_phi_loc = accumulate_layer_offsets(n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer);
vector[n_cohort * n_locs] phi = compute_hierarchical_phi(logit_phi_st, logit_phi_loc, n_cohort, n_locs);

#include model/common_phi.stan

