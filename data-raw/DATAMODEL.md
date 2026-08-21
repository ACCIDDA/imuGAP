# Current Model

We have locations defined by their overall position in the hierarcy. A location's probability equals the sum of its parent offsets, up to a root probability at the base of the hierarchy.

Imagining a three tier structure for example, we would define probabilities:

$$
p_0 = \textrm{logit}^{-1}\left(\textrm{logit}\left(p_0\right)\right) \\
p_i = \textrm{logit}^{-1}\left(\textrm{logit}\left(p_0\right) + \delta_i\right) \\
p_{ij} = \textrm{logit}^{-1}\left(\textrm{logit}\left(p_0\right) + \delta_i + \delta_{ij}\right)
$$

We draw the $\delta_{\{i\}}$ from a zero-mean normal, with distinct $\sigma$ for each tier. These $\delta$ do not vary in time, but $p_0$ does. When we fit these elements, we compare observations at a tier with the associated probability (vice explicitly aggregating up smaller units). We do not constrain $\delta_i$ to attain any particular balance.

# Problem

By fitting the parameters that specify the aggregate population to the corresponding observations, and likewise using them to impute aggregate features, we are assuming that the contained sub-populations features actually aggregate to the whole population. That remains true whether we have observations for all the sub-populations are not.

However, the design of the model also has these features influence 

$$
\log(L) = Y\log\left(p_0\right) + \left(N-Y\right)\log\left(1-p_0\right) \sum_i y_i\log\left(p_i\right) + \left(n_i-y_i\right)\log\left(1-p_i\right)
$$

Keeping in mind that $p_i = \textrm{logit}^{-1}\left(\textrm{logit}\left(p_0\right) + \delta_i\right)$.