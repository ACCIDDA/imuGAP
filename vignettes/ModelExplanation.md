# Model

We imagine that the underlying propensity (odds) to vaccinate is expressed as:

$$
O_i\left(t\right) =  o_0\left(t\right)\prod_{j\in\mathbb{k}} o_j
$$

where:
 - $i$ corresponds to the lowest level entity,
 - $\mathbb{k}$ corresponds to the set of the entity and its enclosing entities, except the highest level.
 - $o_0\left(t\right)$ is the largest enclosing entity, which varies with time.

Notably, $O_j$ where $j$ *not* a lowest level entity is not directly interpretable as we will show shortly.

The probability of observing an outcome for an $i$ is therefore:

$$
P_i\left(t\right) = \frac{O_i\left(t\right)}{1+O_i\left(t\right)}
$$

If we have an entity $j$ which encloses a size $N$ set of equally contributing $i$ s, then

$$
P_j\left(t\right) = \frac{1}{N}\sum_{i} P_i\left(t\right)
$$

To the interpretability problem for the corresponding $O_j$. We desire that some kind of average odds should correspond to this average probability.

The geometric average would be

$$
O_j^\dagger\left(t\right) = \sqrt[N]{\prod_i \frac{P_i\left(t\right)}{1 - P_i\left(t\right)}} = o_j o_0\left(t\right)\sqrt[N]{\prod_i o_i}
$$

So under the condition that $\prod_i o_i = 1$, $O_j^\dagger = o_j$.

Actual $O_j$ is

$$
O_j = \frac{\frac{1}{N}\sum_i P_i}{1 - \frac{1}{N}\sum_i P_i}
$$

Let's assume we have enforced the condition that $\prod_i o_i = 1$. Does $O_j = O_j^\dagger$?

Consider the case of $o_i = \left\{ \frac{1}{2}, 2 \right\}$, $o_0\left(t\right) = 1$, and $o_j = 1$. This corresponds to $P_i = \left\{ \frac{1}{3}, \frac{2}{3} \right\}$, the average would $P_j = \frac{1}{2}$, which corresponds to $o_j = 1$. So: it is *possible* for $O_j = O_j^\dagger$.

However, what if $o_j = 2$? Then $P_i = \left\{ \frac{1}{2}, \frac{4}{5} \right\}$, so $P_j = \frac{13}{20}$ which clearly does not match $O_j^\dagger = 2$.

By inspection, this problem does not disappear if the probabilities are for different sized sub populations and therefore combine as a weighted
average instead of all being equally weighted, nor does it vanish for $o_0\left(t\right)$ having actual function form instead of being a constant 1.


Ignoring the time component for a moment, the arithmetic average odds would be

$$
O_j^* = \frac{1}{N}\sum_i \frac{P_i}{1 - P_i}
$$

from identies for the geometric and arithmetic averages, these can only be equal
when all individual components are equal.


imagine for a moment
that $\prod_{i} o_i = 1$ and that $o_0\left(t\right) = 1$.

Logic here: average odds does not equal average probability
