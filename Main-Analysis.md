# Analysis Template
  





### Overview

In a break from Wright et al., we used the more recent approach to calulating R^2^ for mixed model from [Nakagawa & Schielzeth 2013](http://dx.doi.org/10.1111/j.2041-210x.2012.00261.x) as implemented in the [MuMIn](https://cran.r-project.org/web/packages/MuMIn/index.html) package.

The Nakagawa and Shielzeth method calculates a marginal R^2^ that shows the proportion of variation attributable to fixed effects only (analagous to those reported by Wright et al.), and also a conditional R^2^ that shows the total variation explained by the model (both fixed and random effects)



```r

### Load Packages
require(ggplot2) # Needed for plotting
require(cowplot) # Needed for publication-quality ggplots
require(dplyr) # Needed for data wrangling
require(readxl) # Needed to read in the Excel file
require(lme4) # Needed for mixed modelling
require(MuMIn) # Needed for R^2 calculation
require(knitr) # Needed to print out tables


### Import a function to make a nice table with AIC and R^2
source("AIC.R2.table.R")

### Import the Wright et al. dataset

# Specify the Science URL for the Wright et al. dataset
# NOTE: if this doesn't work, use read_excel() to import the dataset directly.
dataURL <- "http://science.sciencemag.org/highwire/filestream/698792/field_highwire_adjunct_files/1/aal4760-Wright-SM_Data_Set_S1.xlsx"

# Download the xlsx dataset and read into R
tmpfile <- tempfile(fileext = ".xlsx")
download.file(dataURL, tmpfile, mode = "wb")
data <- read_excel(path = tmpfile, sheet = "Global leaf size dataset")

```



```r

# Clean up the dataset
global <- data %>% 
    
    # Select columns: family, species, site, size, latitude, & climate variables
    select(Family, Species = `Genus species`, Site = `Site name`, 
        Leaf.size = `Leaf size (cm2)`, Latitude, MAT, MAP, MIann, 
        Tgs, RADann) %>%
    
    # Filter out all rows without a leaf size
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

# Make a table with AIC and Marginal and Conditional R^2
lat.tbl <- AIC.R2.table(lat.m1, lat.m2, lat.m3, sitesp, family, sitefamilysp, 
    mnames = c("Latitude + Species + Site (Wright)", 
        "Latitude + Site + Family + Species", "Latitude", "Site + Species", 
       "Family", "Site + Family + Species"))

# Print out a markdown table
kable(lat.tbl, format = "markdown")
```



|Model                              |      AIC|     dAIC| df| R2.marg| R2.cond|
|:----------------------------------|--------:|--------:|--:|-------:|-------:|
|Latitude + Site + Family + Species | 14986.72|     0.00|  7|   0.171|   0.948|
|Site + Family + Species            | 15386.82|   400.10|  5|   0.000|   0.947|
|Latitude + Species + Site (Wright) | 16538.27|  1551.54|  6|   0.231|   0.945|
|Site + Species                     | 16948.59|  1961.87|  4|   0.000|   0.945|
|Family                             | 29081.79| 14095.06|  3|   0.000|   0.481|
|Latitude                           | 30835.92| 15849.20|  4|   0.282|   0.282|

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

# Make a table with AIC and Marginal and Conditional R^2
MAT.tbl <- AIC.R2.table(map.m1, map.m2, map.m3, sitesp, family, sitefamilysp, 
    mnames = c("MAP + Species + Site (Wright)", 
        "MAP + Site + Family + Species", "MAP", "Site + Species", 
       "Family", "Site + Family + Species"))

# Print out a markdown table
kable(MAT.tbl, format = "markdown")
```



|Model                         |      AIC|     dAIC| df| R2.marg| R2.cond|
|:-----------------------------|--------:|--------:|--:|-------:|-------:|
|MAP + Site + Family + Species | 15305.31|     0.00|  6|   0.051|   0.947|
|Site + Family + Species       | 15386.82|    81.51|  5|   0.000|   0.947|
|MAP + Species + Site (Wright) | 16859.78|  1554.46|  5|   0.076|   0.945|
|Site + Species                | 16948.59|  1643.28|  4|   0.000|   0.945|
|Family                        | 29081.79| 13776.47|  3|   0.000|   0.481|
|MAP                           | 33228.50| 17923.18|  3|   0.142|   0.142|




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
[1] bindrcpp_0.2  knitr_1.17    MuMIn_1.40.0  lme4_1.1-14   Matrix_1.2-11
[6] readxl_1.0.0  dplyr_0.7.4   cowplot_0.8.0 ggplot2_2.2.1

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.13     highr_0.6        compiler_3.4.1   cellranger_1.1.0
 [5] nloptr_1.0.4     plyr_1.8.4       bindr_0.1        tools_3.4.1     
 [9] digest_0.6.12    evaluate_0.10.1  tibble_1.3.4     gtable_0.2.0    
[13] nlme_3.1-131     lattice_0.20-35  pkgconfig_2.0.1  rlang_0.1.4     
[17] yaml_2.1.14      stringr_1.2.0    stats4_3.4.1     rprojroot_1.2   
[21] grid_3.4.1       glue_1.2.0       R6_2.2.2         rmarkdown_1.6   
[25] minqa_1.2.4      magrittr_1.5     codetools_0.2-15 backports_1.1.1 
[29] scales_0.5.0     htmltools_0.3.6  splines_3.4.1    MASS_7.3-47     
[33] assertthat_0.2.0 colorspace_1.3-2 stringi_1.1.5    lazyeval_0.2.1  
[37] munsell_0.4.3   
```


