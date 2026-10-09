{
  int p_offset = 0;
  if (n_obs_unmixed_uncensored > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_unmixed_uncensored)] = p_obs_unmixed_uncensored;
    p_offset += n_obs_unmixed_uncensored;
  }
  if (n_obs_mixed_uncensored > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_mixed_uncensored)] = p_obs_mixed_uncensored;
    p_offset += n_obs_mixed_uncensored;
  }
  if (n_obs_unmixed_right > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_unmixed_right)] = p_obs_unmixed_right;
    p_offset += n_obs_unmixed_right;
  }
  if (n_obs_mixed_right > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_mixed_right)] = p_obs_mixed_right;
    p_offset += n_obs_mixed_right;
  }
  if (n_obs_unmixed_left > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_unmixed_left)] = p_obs_unmixed_left;
    p_offset += n_obs_unmixed_left;
  }
  if (n_obs_mixed_left > 0) {
    p_obs[(p_offset + 1):(p_offset + n_obs_mixed_left)] = p_obs_mixed_left;
    p_offset += n_obs_mixed_left;
  }
}
