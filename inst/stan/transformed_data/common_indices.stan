#include transformed_data/uncensored/indices.stan
#include transformed_data/right/indices.stan
#include transformed_data/left/indices.stan

int n_obs_total = n_obs_unmixed_uncensored + n_obs_mixed_uncensored
                + n_obs_unmixed_right + n_obs_mixed_right
                + n_obs_unmixed_left + n_obs_mixed_left;
