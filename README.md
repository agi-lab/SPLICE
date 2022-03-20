# SPLICE [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SPLICE)](https://CRAN.R-project.org/package=SPLICE)

## Introduction
We introduce `SPLICE` (**S**ynthetic **P**aid **L**oss and **I**ncurred **C**ost **E**xperience), an extension to the individual claim simulator called [`SynthETIC`](https://CRAN.R-project.org/package=SynthETIC). `SPLICE` serves to simulate the evolution of case estimates of incurred losses through the lifetime of a claim, under a flexible modelling framework which resembles that of [`SynthETIC`](https://CRAN.R-project.org/package=SynthETIC).

An initial set of test parameters, designed to mirror the experience of a real insurance portfolio, were set up and applied by default to generate a realistic test data set of incurred histories (see vignette). However, the distributional assumptions used to generate this data set can be easily modified by users to match their experiences.

## Reference
For a full description of `SPLICE`'s structure and test parameters, readers should refer to:

Avanzi, B., Taylor, G., Wang, M., 2021. `SPLICE`: A Synthetic Paid Loss and Incurred Cost Experience Simulator. [arXiv:2109.04058](https://arxiv.org/abs/2109.04058).

## Install Package
To install the [CRAN version of the package](https://CRAN.R-project.org/package=SPLICE), do

`install.packages("SPLICE")`

To install the development version of the package from this GitHub repository, do

```
if (!require(remotes)) install.packages("remotes")
remotes::install_github("agi-lab/SPLICE/SPLICE", build_vignettes = TRUE)
```

After the installation, run

`library(SPLICE)`

as you would normally do will load the package. View a full demonstration of the package (which is used to generate the built-in test datasets discussed in the paper) by running

`vignette("SPLICE-demo", package = "SPLICE")`

## Additional Resources
* [Chain ladder analysis for the test data set](https://github.com/agi-lab/SPLICE/blob/main/CL_Incurred_Analysis.xlsx)
* [Example datasets](https://github.com/agi-lab/SPLICE/tree/main/datasets)

