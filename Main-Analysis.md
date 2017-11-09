# Analysis Template
  





### Overview

In a break from Wright et al., we used the more recent approach to calulating R^2^ for mixed model from [Nakagawa & Schielzeth 2013](http://dx.doi.org/10.1111/j.2041-210x.2012.00261.x) as implemented in the [MuMIn](https://cran.r-project.org/web/packages/MuMIn/index.html) package.

The Nakagawa and Shielzeth method calculates a marginal R^2^ that shows the proportion of variation attributable to fixed effects only (analagous to those reported by Wright et al.), and also a conditional R^2^ that shows the total variation explained by the model (both fixed and random effects)







```r

# Clean up the dataset
global <- data %>% 
    
    # Select columns: family, species, site, size, latitude, & climate variables
    select(Family, Species = `Genus species`, Site = `Site name`, 
        Leaf.size = `Leaf size (cm2)`, Latitude, MAT, MAP, MIann, 
        Tgs, RADann) %>%
    
    # First filter out all rows without a leaf size
    filter(!is.na(Leaf.size))

# Show dataset structure
#str(global)

```




```r

##### Random Effect Models ##### 

# Model with Site + Species random effects
sitesp <- lmer(log10(Leaf.size) ~ (1|Site) + (1|Species), data = global)

# Model with Family random effect
family <- lmer(log10(Leaf.size) ~ (1|Family), data = global)

# Model with Site + Family + Species random effects
sitefamilysp <- lmer(log10(Leaf.size) ~ (1|Site) + (1|Family) + (1|Species), 
    data = global)

```

## Latitude

The best model for explaining latitudinal gradients in leaf size includes Site, Species, and Family as random effects.  Models with family alone outperform models that include Latitude without random effects. In the model used by Wright et al., with species and site as random effects, most of the variance was explained by these random effects, rather than latitude.  


```r

##### Latitude Models ##### 

# Full Wright et al. model: Latitude + Site + Species
lat.m1 <- lmer(log10(Leaf.size) ~ poly(Latitude, 2, raw = TRUE) + 
    (1|Site) + (1|Species), data = global)

# Full Wright et al model with Family: Latitude + Site + Family + Species
lat.m2 <- lmer(log10(Leaf.size) ~ poly(Latitude, 2, raw  = TRUE) + 
    (1|Site) + (1|Family) + (1|Species), data = global)

# Model with only Latitude: no random effects
lat.m3 <- lm(log10(Leaf.size) ~ poly(Latitude, 2, raw  = TRUE), data = global)

# Make a table with AIC and Marginal and Conditional R2
lat.tbl <- AIC.R2.table(lat.m1, lat.m2, lat.m3, sitesp, family, sitefamilysp, 
    mnames = c("Latitude + Species + Site (Wright)", 
        "Latitude + Site + Family + Species", "Latitude", "Site + Species", 
       "Family", "Site + Family + Species"))

kable(lat.tbl, format = "markdown")
```



|Model                              |      AIC|     dAIC| R2.marg| R2.cond|
|:----------------------------------|--------:|--------:|-------:|-------:|
|Latitude + Site + Family + Species | 14986.72|     0.00|    0.17|    0.95|
|Site + Family + Species            | 15386.82|   400.10|    0.00|    0.95|
|Latitude + Species + Site (Wright) | 16538.27|  1551.54|    0.23|    0.95|
|Site + Species                     | 16948.59|  1961.87|    0.00|    0.95|
|Family                             | 29081.79| 14095.06|    0.00|    0.48|
|Latitude                           | 30835.92| 15849.20|    0.28|    0.28|

## MAP


```r


##### MAP Models ##### 

# Full Wright et al. model: MAP + Site + Species
map.m1 <- lmer(log10(Leaf.size) ~ MAP + 
    (1|Site) + (1|Species), data = global)

# Full Wright et al model with Family: MAP + Site + Family + Species
map.m2 <- lmer(log10(Leaf.size) ~ MAP + 
    (1|Site) + (1|Family) + (1|Species), data = global)

# Model with only MAP: no random effects
map.m3 <- lm(log10(Leaf.size) ~ MAP, data = global)

MAT.tbl <- AIC.R2.table(map.m1, map.m2, map.m3, sitesp, family, sitefamilysp, 
    mnames = c("MAP + Species + Site (Wright)", 
        "MAP + Site + Family + Species", "MAP", "Site + Species", 
       "Family", "Site + Family + Species"))

kable(MAT.tbl, format = "markdown")
```



|Model                         |      AIC|     dAIC| R2.marg| R2.cond|
|:-----------------------------|--------:|--------:|-------:|-------:|
|MAP + Site + Family + Species | 15305.31|     0.00|    0.05|    0.95|
|Site + Family + Species       | 15386.82|    81.51|    0.00|    0.95|
|MAP + Species + Site (Wright) | 16859.78|  1554.46|    0.08|    0.95|
|Site + Species                | 16948.59|  1643.28|    0.00|    0.95|
|Family                        | 29081.79| 13776.47|    0.00|    0.48|
|MAP                           | 33228.50| 17923.18|    0.14|    0.14|




```r

# Put each graph in a separate code block with a different name

```


### Session Information


```
R version 3.4.1 (2017-06-30)
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS/LAPACK: /usr/lib64/R/lib/libRblas.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] bindrcpp_0.2    knitr_1.17      MuMIn_1.40.0    lme4_1.1-14    
 [5] Matrix_1.2-11   readxl_1.0.0    cowplot_0.8.0   dplyr_0.7.4    
 [9] purrr_0.2.4     readr_1.1.1     tidyr_0.7.2     tibble_1.3.4   
[13] ggplot2_2.2.1   tidyverse_1.1.1

loaded via a namespace (and not attached):
 [1] reshape2_1.4.2   splines_3.4.1    haven_1.1.0      lattice_0.20-35 
 [5] colorspace_1.3-2 stats4_3.4.1     htmltools_0.3.6  yaml_2.1.14     
 [9] rlang_0.1.4      nloptr_1.0.4     foreign_0.8-69   glue_1.2.0      
[13] modelr_0.1.1     bindr_0.1        plyr_1.8.4       stringr_1.2.0   
[17] munsell_0.4.3    gtable_0.2.0     cellranger_1.1.0 rvest_0.3.2     
[21] psych_1.7.8      evaluate_0.10.1  forcats_0.2.0    parallel_3.4.1  
[25] highr_0.6        broom_0.4.2      Rcpp_0.12.13     scales_0.5.0    
[29] backports_1.1.1  jsonlite_1.5     mnormt_1.5-5     hms_0.3         
[33] digest_0.6.12    stringi_1.1.5    grid_3.4.1       rprojroot_1.2   
[37] tools_3.4.1      magrittr_1.5     lazyeval_0.2.1   pkgconfig_2.0.1 
[41] MASS_7.3-47      xml2_1.1.1       lubridate_1.7.1  assertthat_0.2.0
[45] minqa_1.2.4      rmarkdown_1.6    httr_1.3.1       R6_2.2.2        
[49] nlme_3.1-131     compiler_3.4.1  
```


