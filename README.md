## About
This repository contains analyses conducted on a dataset obtained from [Wright et al (2017)](http://science.sciencemag.org/content/357/6354/917).


We present the results of generalized linear mixed effects models (GLMMs), run in R (using the ['lme4'](https://cran.r-project.org/web/packages/lme4/index.html) package),  examining the response of leaf size to latitude and global climates. 
We demonstrate that taxonomic information captures a significant proportion of global variation in leaf size; even more than  do climatic variables.


## Analyses - [here](Main-Analysis.md)
We generate GLMMs with Leaf Size (LS) as the response variable with different combinations of latitude and climatic variables as fixed effect (population-level effects), and site, species, and family as random effects (group-level effects).

We evaluate the model fit using [AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion), a now widely used method of model selection.

We also calculate GLMM-based coefficients of determination (R<sup>2</sup>), using the R package ['MuMIn'](https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf), which uses the method of [Nakagawa and Schielzeth (2013)](http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210x.2012.00261.x/full), and its extension by [Johnson (2014)](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12225/full). 
