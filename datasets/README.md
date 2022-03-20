# Example Datasets

This folder includes
- 5 example datasets generated with our favourite random seed 42 (with one for each scenario; see description below) as `.csv` files for the convenience of users.
- `generate_datasets.R` has the code used to generate the datasets in `complexity_1` to `complexity_5`.

# Data Generation

[`SPLICE`](https://CRAN.R-project.org/package=SPLICE) v1.1.0+ provides a `generate_data()` function which produces datasets of varying levels of complexity, where 1 represents the simplest, and 5 represents the most complex:

- **Scenario 1**: simple, homogeneous claims experience, with zero inflation.
- **Scenario 2**: slightly more complex than 1, with dependence of notification delay and settlement delay on claim size, and 2% p.a. base inflation.
- **Scenario 3**: steady increase in claim processing speed over occurrence periods (i.e. steady decline in settlement delays).
- **Scenario 4**: inflation shock at time 30 (from 0% to 10% p.a.).
- **Scenario 5**: default distributional models, with complex dependence structures (e.g. dependence of settlement delay on claim occurrence period).

We refer to the paper and package documentation for more details.
