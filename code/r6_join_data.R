

library(tidyverse)

## Load exposure
pm_data_2010 <- read.csv(file.path(pr_results, "aggregated_pm_data_2010.csv"))

## Load gridmet
gridmet_2010 <- read.csv(file.path(pr_results, "aggregated_gridmet_2010.csv"))

## Load census
census_2010 <- read.csv(file.path(pr_results, "census_2010.csv"))

## Load CMS 
cms_2010 <- read.csv(file.path(pr_results, "CMS_2010.csv"))

## Load BRFSS
brfss_2010 <- read.csv(file.path(pr_results, "aggregated_brfss_data_2010.csv"))


## Merge data
multi_merge <- function(x, y){
  df <- full_join(x, y, by= "FIPS")
  return(df)
}

df_joined <- Reduce(multi_merge, list(pm_data_2010,
                                      gridmet_2010,
                                      census_2010,
                                      cms_2010,
                                      brfss_2010))

write.csv(df_joined,
          file.path(pr_results, "Study_dataset.csv"),
          row.names = FALSE)

