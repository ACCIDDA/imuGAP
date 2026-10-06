// Probability sanitization / clamping to prevent boundary overflow/underflow
// in link and inverse-link transformations.

real clamp_probability(real p) {
  return fmax(1e-12, fmin(1.0 - 1e-12, p));
}

vector clamp_probability(vector p) {
  return fmax(1e-12, fmin(1.0 - 1e-12, p));
}
