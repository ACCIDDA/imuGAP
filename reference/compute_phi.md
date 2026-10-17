# Compute hierarchical saturation probabilities and link parameters

Computes posterior draws of the link-scale parameters or coverage
probabilities (\\\phi\\) across cohorts and locations from an
`[imugap_fit]` object.

## Usage

``` r
compute_phi(object, link_scale = FALSE, posterior_size = NULL, ...)
```

## Arguments

- object:

  an object of class `imugap_fit` returned by `[sampling()]`.

- link_scale:

  logical scalar; return link-scale parameters (\\\eta\\) instead of
  probabilities (\\\phi\\)? (default: `FALSE`).

- posterior_size:

  integer scalar; optional number of posterior draws to retain from the
  tail of each chain (default: `NULL`, keeping all draws).

- ...:

  additional arguments (currently ignored).

## Value

a 3D numeric array with dimensions `c(draw, cohort, location)`
containing the reconstructed posterior draws.

## Details

In multi-layer models,
[`sampling()`](https://accidda.github.io/imuGAP/reference/sampling.md)
retains orthonormalized spatial offsets (`off_layer`) and parent
aggregation shifts (`mu_offset`) while omitting the full unrolled
parameter matrix (`raw_phi_mat`) by default to conserve memory.
`compute_phi()` reconstructs the complete link-scale and probability
profiles across the location hierarchy: \$\$\eta\_{l, t} =
\eta\_{\text{parent}(l), t} + \mu\_{\text{parent}(l), t} + \delta_l\$\$
\$\$\phi\_{l, t} = g^{-1}(\eta\_{l, t})\$\$ where \\g^{-1}\\ is the
configured inverse-link function (e.g. logit or probit).

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
data("fit_sim", package = "imuGAP")
phi_draws <- compute_phi(fit_sim, posterior_size = 10)
dim(phi_draws)
# }
}
```
