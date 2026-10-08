# imuGAP Inference & Offset Solver Benchmarking Plan

This document outlines the formal benchmarking framework for evaluating Stan link functions, analytical initial guessers, and root-finding solvers in `imuGAP`.

---

## 0. Technical Notes

We use a make-based approach to manage intermediate results and artifacts (e.g. compiled models), to avoid unnecessarily recomputing long running tasks.

The whole benchmarking pipeline is designed to take input configuration files, so that we can initially stress-test all steps.

Benchmarking work should run in the `data-raw/benchmarks/` folder as the working directory.

---

## 1. Executive Summary & Objective

The `imuGAP` package models hierarchical vaccination coverage by decomposing latent propensities into a top-level cohort baseline $p_{0, c}$ and location-specific random-walk deviations $\delta_k$. Because observation likelihoods operate on the probability scale while random effects are additive on the link scale ($g$), calculating parent-level aggregate coverage requires solving the nonlinear aggregation identity for the shift parameter $\mu_c$:

$$\sum_{k=1}^K w_k \, g^{-1}\big(g(p_{0, c}) + \mu_c + \delta_k\big) = p_{0, c}$$

where $w_k$ denotes normalized subpopulation population weights ($\sum w_k = 1$), $g$ is the link function (logit or probit), and $\delta_k$ are zero-mean balanced spatial offsets ($\sum w_k \delta_k = 0$).

The goal of this benchmark is to quantify the trade-offs between computational throughput (Effective Sample Size per second, HMC gradient evaluation time) and inferential fidelity (recovery of true $p_0$ trends, spatial offsets $\delta_k$, credible interval calibration, and divergence freedom) across varying subpopulation densities ($K$) and offset dispersion levels ($\sigma_\delta$).

---

## 2. Experimental Design & Factorial Grid

The benchmark simulates synthetic 2-layer hierarchies (1 root parent entity with $K$ offspring sub-locations) across a factorial grid of population structure and variance parameters.

The grid is supplied to `benchmark_synthetic_populations.R` as a path argument, which points to a yaml configuration file for grid points and sample size. That script in turn writes a population result file of corresponding name:

```
$ Rscript benchmark_synthetic_populations.R some_config_A.yml
# creates some_config_A.rds
```

### 2.1 Stress-Test Parameter Dimensions

- **Subpopulation Count ($K$)**:
  - $K \in \{3, 10\}$
  - Captures small-county aggregations ($K=3$) up to dense school-district partitions ($K=100$).
- **Offset Dispersion ($\sigma_\delta$)**:
  - $\sigma_\delta \in \{0.2, 1.4\}$
  - Ranges from mild to extreme heterogeneity ($\sigma=0.2$ to $\sigma=1.4$).
- **Top-Level Baseline Trend ($p_0$)**:
  - $p_0 \in \{0.05, 0.50, 0.95\}$
  - Evaluates solver stability at the boundary limits ($p_0 = 0.05, 0.95$) and center ($p_0 = 0.50$).
- **Replications ($N$)**:
  - $N = 10$ synthetic datasets per grid point with distinct pseudo-random seeds.

### 2.2 Strenuous Focused Parameter Dimensions (`config_strenuous.yml`)

- **Subpopulation Count ($K$)**:
  - $K \in \{3, 10, 100\}$
  - Tests finite-sample discrete noise ($K=3$), standard reference ($K=10$), and heavy-sum computational burden ($K=100$).
- **Offset Dispersion ($\sigma_\delta$)**:
  - $\sigma_\delta \in \{0.6, 1.4\}$
  - Tests moderate baseline control ($\sigma=0.6$) and extreme Taylor breakdown threshold ($\sigma=1.4$), pruning uninformative $\sigma \le 0.2$.
- **Top-Level Baseline Trend ($p_0$)**:
  - Targeted 7-cohort trend focusing on high-curvature, steep gradients, and tail saturation:
    $$p_0 \in \{0.02, 0.05, 0.15, 0.50, 0.85, 0.95, 0.98\}$$
- **Replications ($N$)**:
  - $N = 100$ synthetic datasets per grid point (1,200 datasets total across logit & probit).

### 2.3 Full Parameter Dimensions (`config_full.yml`)

- **Subpopulation Count ($K$)**:
  - $K \in \{3, 10, 30, 100\}$
  - Captures small-county aggregations ($K=3$) up to dense school-district partitions ($K=100$).
- **Offset Dispersion ($\sigma_\delta$)**:
  - $\sigma_\delta \in \{0.2, 0.6, 1.0, 1.4\}$
  - Ranges from mild to extreme heterogeneity ($\sigma=0.2$ to $\sigma=1.4$).
- **Top-Level Baseline Trend ($p_0$)**:
  - A deterministic linear trend across $C = 19$ cohorts:
    $$p_{0, c} = 0.05 + 0.05 \cdot (c - 1), \quad c \in \{1, \dots, 19\}$$
  - Evaluates solver stability across near-boundary tails ($p_0 = 0.05, 0.95$) and central linear regimes ($p_0 = 0.50$).
- **Replications ($N$)**:
  - $N = 300$ synthetic datasets per grid point with distinct pseudo-random seeds.

### 2.4 Data Generation & Likelihood Setup

1. **True Offsets & Shifts**:
   - For each sample, raw shifts $\delta_k^{\text{raw}} \sim \mathcal{N}(0, \sigma_\delta^2)$ are drawn and centered: $\delta_k = \delta_k^{\text{raw}} - \sum_{k} w_k \delta_k^{\text{raw}}$. N.b. that balancing this way (rather than via orthonormalized approach) is fine for test purposes
   - Reference ground-truth shifts $\mu_c$ are solved to machine precision via 1D root-finding (`uniroot`).
2. **Noiseless Observations**:
   - Expected subpopulation coverage is computed:
     $$p_{c, k} = g^{-1}\big(g(p_{0, c}) + \mu_c + \delta_k\big)$$
   - Most-likely integer observation counts are generated under a single-dose vaccine model with high hazard $\lambda$:
     $$y_{c, k} = \text{round}(N_{\text{samp}} \cdot p_{c, k}), \quad N_{\text{samp}} = 1000$$
   - Single-dose observations are assigned to eligible age $a = 2$ (`age_min = 2L`) satisfying dose schedule changepoint invariants (`dose_schedule = c(1L)`).

---

## 3. Evaluated Model Configurations

Models are assembled and compiled from `inst/stan/templates/bspline_static_offsets.stan.template` by substituting `@LINK@`, `@GUESS@`, and `@SOLVE@` placeholders. Processed model templates are created and stored as an intermediate product in the `benchmodels` directory. Cached compiled models are stored in `benchobjs` folder. These are managed by the `benchmark_compilation.R` script.

The models for a benchmark are determined by a configuration file. The full set of available combinations are listed below. The stress test configuration uses all links, the `zero` and `taylor2` guessers, and the `direct` and `builtin` solvers. The full test uses all relevant combinations.

### 3.1 Link Functions (`@LINK@`)
- `logit`: Logistic link $g(p) = \log(p / (1 - p))$.
- `probit`: Probit link $g(p) = \Phi^{-1}(p)$.

### 3.2 Initial Guessers (`@GUESS@`)
- `zero`: Naive baseline ($\hat{\mu} = 0$).
- `taylor2`: 2nd-order Taylor expansion around $\delta = 0$.
- `taylor4`: 4th-order Taylor expansion accounting for variance and kurtosis of $\delta$.
- `pade`: [1/1] Padé rational polynomial approximant (`probit` only)
- `asymptotic`: Extreme-tail asymptotic expansion.
- `conditioned`: Region-partitioned hybrid dispatcher selecting optimal analytical approximations based on $|g(p_0)|$ and $\sigma_\delta$.

### 3.3 Solvers (`@SOLVE@`)
- `direct`: Direct analytical evaluation using the initial guess without iterative root-finding.
- `halley2`: 2-iteration Halley's third-order rational solver.
- `newton2`: 2-iteration Newton-Raphson second-order solver.
- `builtin`: Stan built-in algebraic Newton solver (`solve_newton_tol`).
- `halley10`: 10-iteration high-precision Halley solver.

---

## 4. Evaluation Metrics & Diagnostics

For every fit, the following metrics are recorded and aggregated across Monte Carlo replications:

### 4.1 Inferential Accuracy & Coverage
- **Root Trend Recovery ($p_0$)**:
  - Link-Scale Mean Absolute Error ($\eta$-scale MAE): $\frac{1}{C} \sum_{c=1}^C |g(\hat{p}_{0, c}) - g(p_{0, c}^{\text{true}})|$.
  - Jensen-Shannon Distance ($\sqrt{\text{JSD}}$): $\frac{1}{C} \sum_{c=1}^C \sqrt{\frac{1}{2} D_{\text{KL}}(p_{0, c} \parallel m_c) + \frac{1}{2} D_{\text{KL}}(\hat{p}_{0, c} \parallel m_c)}$, where $m_c = \frac{p_{0, c} + \hat{p}_{0, c}}{2}$.
  - 95% Credible Interval Coverage: Proportion of true cohort points falling within posterior 2.5% and 97.5% quantiles.
- **Subpopulation Offset Recovery ($\delta_k$)**:
  - Evaluated directly from the Stan model's `off_layer` transformed parameter draws (with unconstrained `z_layer` dropped by default).
  - Link-Scale MAE: $\frac{1}{K} \sum_{k=1}^K |\hat{\delta}_k - \delta_k^{\text{true}}|$.
  - 95% Credible Interval Coverage across all $K$ child offsets.

### 4.2 MCMC Efficiency & Numerical Health
- **Effective Sample Size (ESS)**:
  - Minimum ESS (`min_ess`) and Mean ESS (`mean_ess`) across all model parameters.
  - Computational Efficiency: Effective samples per second of wall-clock time ($\text{ESS} / \text{sec}$).
- **Convergence & Stability**:
  - Maximum potential scale reduction factor ($\max \hat{R}$).
  - Divergent Transitions count post-warmup.
  - Total elapsed sampling duration (`elapsed_sec`).

---

## 5. Execution Pipeline & Phasing

```
[Phase 1: Bounding Grid Pilot]
 4 Bounding Points: K in {3, 100} x sigma in {0.2, 1.4}
 8 Key Configurations x 10 Replications = 320 MCMC Fits
            │
            ▼ Verify pipeline, ESS metrics, and parameter recovery
[Phase 2: Full Factorial Grid]
 16 Grid Points: K in {3, 10, 30, 100} x sigma in {0.2, 0.6, 1.0, 1.4}
 Full Model Matrix x 10 Replications
            │
            ▼ Export tidy results to benchmark_inference_grid_results.rds
[Phase 3: Pareto Optimization & Documentation]
 Identify optimal default (direct vs warm Halley2 vs Taylor4)
 Update package defaults and write summary vignette
```

### 5.1 Artifacts & Reproducibility
- Unified compilation & model assembly: `data-raw/benchmarks/benchmark_compilation.R`
- Synthetic population generation: `data-raw/benchmarks/benchmark_synthetic_populations.R`
- Core MCMC inference & metric runner: `data-raw/benchmarks/benchmark_inference_runner.R`
- Orchestration Makefile: `data-raw/benchmarks/Makefile`
- Primary configurations:
  - Stress Test: `config_stress_test.yml` -> `results_stress_test.rds`
  - Strenuous Grid: `config_strenuous.yml` -> `results_strenuous.rds`
  - Full Grid: `config_full.yml` -> `results_full_grid.rds`
