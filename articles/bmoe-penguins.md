# Worked Example: Palmer Penguins

``` r

library(palmerpenguins)
library(rsample)
```

## Palmer Penguins

To demonstrate the typical use-case of the Bayesian Mixture of Experts
(`bmoe`) we make use of the [Palmer
Penguins](https://allisonhorst.github.io/palmerpenguins/) dataset. For
model validation purposes, we split our inputs into training and testing
datasets using random sampling, stratified by species.

``` r

data_split <- initial_split(penguins, prop = 0.9, strata = "species")
```

Suppose we are interested in fitting a model to explain `bill_length_mm`
using `flipper_length_mm` and assume a linear relationship. We could
improve this model by assuming the relationship will vary by penguin
species.

This can be done in a single linear mixed effects model with `lme4` or
`brms`. Alternatively, we could fit three separate models as shown
below.

![](https://allisonhorst.github.io/palmerpenguins/reference/figures/README-flipper-bill-1.png)

For the Bayesian Mixture of Experts model, we assume that the underlying
component allocations (species) is not observed. While assigning meaning
to each component helps with interpretation, it is not necessary for the
model to be fit.

Therefore, the `bmoe` engine will simultaneously classify each
observation into one of $`K`$ components, where $`K`$ is assumed known,
and fit a mixture of regressions.

``` r

model_fit <-
  bmoe::bmoe(
    bill_length_mm ~ flipper_length_mm,
    prior = bmoe::bmoe_prior(k = 3),
    data = rsample::training(data_split)
  )
```

    Compiling model graph
       Declaring variables
       Resolving undeclared variables
       Allocating nodes
    Graph information:
       Observed stochastic nodes: 306
       Unobserved stochastic nodes: 319
       Total graph size: 3972

    Initializing model

## TODO

Both fits seem similar, need to improve classification accuracy

``` r

lm_fit <-
  stats::lm(
    bill_length_mm ~ flipper_length_mm,
    data = rsample::training(data_split)
  )
```

``` r

validation_data <- 
  dplyr::mutate(
    rsample::testing(data_split),
    # TODO: Need to reduce the amount of work to get point estimates
    .pred_response = 
      stats::predict(
        model_fit,
        new_data = rsample::testing(data_split),
        type = "response"
      ) |> 
      purrr::pluck(".pred_bill_length_mm") |> 
      purrr::map_dbl(\(.x) mean(.x$mean)),
    
    .pred_lm = 
      stats::predict(
        lm_fit,
        newdata = rsample::testing(data_split)
      ),
    
    .after = "bill_length_mm"
  )
```
