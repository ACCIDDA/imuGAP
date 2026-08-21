# Alternative Approach B: The `T3Nx` Adaptive Algorithm with Halley's Method (`ALTMODELB.md`)

This document details the **`T3Nx` Reference Algorithm**, an adaptive formulation for **Approach B** (Orthonormal Zero-Sum Constrained Logit Model) in `imuGAP`. 

Instead of executing 6 scalar root-finding loop iterations per parent/cohort in the autodiff graph, `T3Nx` derives an initial moving offset guess $\theta^{(0)}$ using a **3rd-order Taylor series approximation of the expit function** around the parent target probability $p_0$, and refines $\theta$ using **Halley's Method** (3rd-Order Cubic Convergence, $\mathcal{O}(E^3)$) with dynamic step count based on the **Boundary-Adjusted Effective Dispersion Metric**:

$$s_{\text{effective}} = \frac{s_{\text{sample}}}{2 \sqrt{p_0 (1 - p_0)}}, \quad n_{\text{steps}} = \text{pmin}\left(3\text{L}, \text{pmax}\left(0\text{L}, \left\lceil \ln\left(\frac{s_{\text{effective}}}{0.30}\right) \right\rceil\right)\right)$$

---

## 1. Problem Formulation

Let $p_0 = \phi_{\text{parent}, c} \in (0, 1)$ be the parent group target probability for cohort $c$.
Let $\delta_i$ be the zero-mean offset for child unit $i \in \{1, \dots, K\}$ with positive weights $w_i > 0$ such that $\sum_{i=1}^K w_i = 1$ and $\sum_{i=1}^K w_i \delta_i = 0$.

We seek the scalar moving offset $\theta \in \mathbb{R}$ satisfying:
$$\sum_{i=1}^K w_i \, \text{expit}\left(\text{logit}(p_0) + \theta + \delta_i\right) = p_0$$

where $\text{expit}(u) = \frac{1}{1 + e^{-u}} = \text{logit}^{-1}(u)$ is the standard expit function.

---

## 2. Mathematical Derivation

### A. Taylor Series Expansion of Expit Function
Let $x_0 = \text{logit}(p_0)$, so $\text{expit}(x_0) = p_0$. Define total offset $\Delta_i = \theta + \delta_i$.
Expanding $\text{expit}(x_0 + \Delta_i)$ in a Taylor series around $x_0$:

$$\text{expit}(x_0 + \Delta_i) = \text{expit}(x_0) + \text{expit}'(x_0) \Delta_i + \frac{1}{2} \text{expit}''(x_0) \Delta_i^2 + \frac{1}{6} \text{expit}'''(x_0) \Delta_i^3 + \mathcal{O}(\Delta_i^4)$$

Let $V_0 = p_0(1 - p_0)$ be the variance of a Bernoulli trial with success probability $p_0$. The derivatives evaluated at $x_0$ are:

$$\text{expit}(x_0) = p_0, \quad \text{expit}'(x_0) = V_0, \quad \text{expit}''(x_0) = V_0(1 - 2p_0), \quad \text{expit}'''(x_0) = V_0(1 - 6V_0)$$

### B. Weighted Expectation Over Sub-Units
Substituting $\Delta_i = \theta + \delta_i$ into the Taylor expansion and taking the weighted sum $\sum_{i=1}^K w_i (\cdot)$:

$$\sum_{i=1}^K w_i \text{expit}(x_0 + \theta + \delta_i) \approx p_0 + V_0 \theta + \frac{1}{2} V_0(1 - 2p_0)(\theta^2 + m_2) + \frac{1}{6} V_0(1 - 6V_0)(\theta^3 + 3\theta m_2 + m_3)$$

where $m_2 = \sum w_i \delta_i^2$ is the **2nd weighted moment** (weighted variance) and $m_3 = \sum w_i \delta_i^3$ is the **3rd weighted moment** (weighted skewness).

### C. Setting Expectation to Parent Target $p_0$
Setting the expected probability equal to target $p_0$ and dividing by $V_0$:

$$B(p_0) \, \theta^3 + A(p_0) \, \theta^2 + \left[1 + 3 B(p_0) m_2\right] \theta + \left[A(p_0) m_2 + B(p_0) m_3\right] = 0$$

where $A(p_0) = \frac{1}{2}(1 - 2p_0)$ and $B(p_0) = \frac{1}{6}(1 - 6p_0 + 6p_0^2)$.

---

## 3. Halley's Method Cubic Refinement ($\mathcal{O}(E^3)$)

To achieve 3rd-order cubic convergence without extra function calls, `T3Nx` uses **Halley's Method** incorporating the closed-form 2nd derivative $f''(\theta)$:

$$\theta^{(k+1)} = \theta^{(k)} - \frac{2 \, f(\theta^{(k)}) \, f'(\theta^{(k)})}{2 [f'(\theta^{(k)})]^2 - f(\theta^{(k)}) \, f''(\theta^{(k)})}$$

where:
- $f(\theta) = \sum w_i \, p_i(\theta) - p_0$
- $f'(\theta) = \sum w_i \, p_i(\theta) \left(1 - p_i(\theta)\right)$
- $f''(\theta) = \sum w_i \, p_i(\theta) \left(1 - p_i(\theta)\right) \left(1 - 2 p_i(\theta)\right)$

---

## 4. Boundary-Adjusted Dispersion Metric $s_{\text{effective}}$

When $p_0 = 0.5$ at Layer 1, child counties receive target probabilities pushed to extreme boundaries ($0.07$ and $0.93$). Near boundaries, $V_0 = p_0(1 - p_0) \to 0$, which amplifies Taylor truncation residuals by $\frac{1}{V_0}$.

By scaling sample SD by $\frac{1}{2\sqrt{V_0}}$:
$$s_{\text{effective}} = \frac{s_{\text{sample}}}{2 \sqrt{p_0 (1 - p_0)}}$$

Layer 2 automatically detects boundary-saturated child targets and assigns an extra Halley step, completely collapsing the $p_0 = 0.5$ multi-layer error outliers down to machine precision ($< 10^{-10}$).

---

## 5. Stan Implementation

```stan
functions {
  /**
   * T3Nx Reference Algorithm with Halley's Method & Boundary-Adjusted s_effective
   */
  vector solve_moving_offset_t3nx(vector p0, vector w, vector delta) {
    real m2 = dot_product(w, square(delta));
    real m3 = dot_product(w, delta .* square(delta));
    real s_sample = sqrt(m2);

    int N = num_elements(p0);
    vector[N] V0 = p0 .* (1.0 - p0);
    vector[N] A = 0.5 * (1.0 - 2.0 * p0);
    vector[N] B = (1.0 / 6.0) * (1.0 - 6.0 * V0);
    
    vector[N] t1 = - (A * m2 + B * m3) ./ (1.0 + 3.0 * B * m2);
    vector[N] theta = t1 + A .* square(t1);

    for (i in 1:N) {
      real v0_denom = 2.0 * sqrt(fmax(1e-6, V0[i]));
      real s_eff = s_sample / v0_denom;
      int n_steps = (s_eff <= 0.3) ? 0 : min(3, 1 + trunc(log(s_eff / 0.3)));

      for (k in 1:n_steps) {
        vector[num_elements(w)] p_child = inv_logit(logit(p0[i]) + theta[i] + delta);
        real f_val = dot_product(w, p_child) - p0[i];
        if (abs(f_val) < 1e-12) break;

        real f1 = dot_product(w, p_child .* (1.0 - p_child));
        real f2 = dot_product(w, p_child .* (1.0 - p_child) .* (1.0 - 2.0 * p_child));
        real denom = 2.0 * square(f1) - f_val * f2;

        if (f1 <= 1e-12 || abs(denom) <= 1e-12) break;
        theta[i] = theta[i] - (2.0 * f_val * f1) / denom;
      }
    }

    return theta;
  }
}
```
