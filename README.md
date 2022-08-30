# CausalPoliceStops

This repository contains the data and code for the article:

> Oliveira, Thiago R. (2022). Aggressive policing and undermined legitimacy: assessing the impact of police stops at gunpoint on perceptions of police in Sao Paulo, Brazil. *Journal of Experimental Criminology*. [https://doi.org/10.1007/s11292-022-09527-9](https://doi.org/10.1007/s11292-022-09527-9)

Please cite this repository as:

> Oliveira, Thiago. (2022). *Repository of R code and data for 'Aggressive policing and undermined legitimacy*. Accessed on XXXX.

### Structure

-   `data`
    -   contains the relevant data sets necessary to replicate all analyses
    -   `export` is an empty directory that is populated when `measurement.R` runs.
-   `plots` is an empty directory that is populated when `panelmatch.R` runs.
-   `measurement.R` replicates the measurement models used in the paper and generates the data set `data/export/df_new.RData` used in all subsequent analyses.
-   `panelmatch.R` replicates the main analyses of the paper
-   `dropouts.R` replicates the analysis assessing attrition bias between the three waves.