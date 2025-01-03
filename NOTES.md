# Development Notes (`bmoe`)

## Task View (Vignettes)

* Worked example with no censoring (`palmerpenguins`)

  * Model fitting

  * Prediction of out-of-sample data

  * Recovery of true allocations (species) against fitted allocations

  * Allocation visualisation

  * Log-likelihood and PSIS diagnostic

* Simulation study (left-censoring)

  * Simulating from described model with simulated censoring

  * Choosing $K$?

  * Recovery of truth

* Label switching

  * Example

  * Diagnosing with visualisations

  * Solution (via `boys_henderson_2002`)

## Function List

### Allocation Visualisations

* `visualise_allocations_with_response_data`
* `visualise_allocation_data`
* `visualise_response_data`
* `component_allocation_visualisation`

### Parameter Visualisations

* `autoplot.bmoe_fit`

### Argument Helpers

* `bmoe_prior`
* `bmoe_jags_n`
* `bmoe_inits`

### MCMC Wrangling

* `bmoe_array`
* `print.bmoe_array`
* `tidy.bmoe_array`
* `pmap_bmoe_array`

### Attribute Getters

* `get_dims_from_bmoe_fit`
* `get_names_from_bmoe_fit`

### Model Fit Functions

* `bmoe`
* `subset_bmoe`
* `predict.bmoe_fit`

### Model Fit Extractors

* `extract_*`

### Relabelling

* `apply_relabelling`
* `table_permutations`
* `boys_henderson_2002`

### Report Generation

* `render_bmoe_fit`
* `.set_bmoe_render_options`?
* `printer_tabset`

### Examples and Simulation

* `example_bmoe_fit`
* `example_simulate_bmoe`
* `example_label_switching_bmoe`
* `simulate_bmoe`
* `simulate_multilogit`
* `artificial_Surv`

### Utilities

* `sweep_ref_vals`
* `softmax`