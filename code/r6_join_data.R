

library(tidyverse)

## Load exposure
pm_data_2010 <- read.csv(file.path(pr_results, "pm_data_2010.csv"), stringsAsFactors = FALSE)

## Load gridmet
gridmet_2010 <- read.csv(file.path(pr_results, "gridmet_data_2010.csv"))

## Load census
census_2010 <- read.csv(file.path(pr_results, "census_data_2010.csv"))

## Load CMS 
cms_2010 <- read.csv(file.path(pr_results, "CMS_data_2010.csv"))

## Load BRFSS
brfss_2010 <- read.csv(file.path(pr_results, "brfss_data_2010.csv"))


## Merge data
multi_merge <- function(x, y){
  x$FIPS <- sprintf("%05d", pm_data_2010$FIPS)
  y$FIPS <- sprintf("%05d", pm_data_2010$FIPS)
  df <- full_join(x, y, by= "FIPS")
  return(df)
}

df_joined <- Reduce(multi_merge, list(pm_data_2010,
                                      census_2010,
                                      brfss_2010,
                                      gridmet_2010,
                                      cms_2010
                                      ))


## Add regions
df_joined$STATE <- substr(df_joined$FIPS, 1, 2)

# Load state to fips crosswalk
state_fips <- read.csv(file.path(get_options("input_dir"),
                                 "public/Geospatial", "FIPS_state_crosswalk.csv"))
state_fips$STATE <- sprintf("%02d", state_fips$FIPS)

# drop non-contiguous states
non_c_states <- c("02","15","60","66","69","72","78")
state_fips <- state_fips[!state_fips$STATE %in% non_c_states,]

state_fips <- state_fips[, !colnames(state_fips) %in% c("FIPS","NAME")]
state_fips <- rename(state_fips, STATE_CODE = POSTAL.CODE)
setDT(state_fips)

# define regions
NORTHEAST=c("NY","MA","PA","RI","NH","ME","VT","CT","NJ")  
SOUTH=c("DC","VA","NC","WV","KY","SC","GA","FL","AL","TN","MS","AR","MD","DE",
        "OK","TX","LA")
MIDWEST=c("OH","IN","MI","IA","MO","WI","MN","SD","ND","IL","KS","NE")
WEST=c("MT","CO","WY","ID","UT","NV","CA","OR","WA","AZ","NM")

state_fips[STATE_CODE %in% NORTHEAST, region := "NORTHEAST"]
state_fips[STATE_CODE %in% SOUTH, region := "SOUTH"]
state_fips[STATE_CODE %in% MIDWEST, region := "MIDWEST"]
state_fips[STATE_CODE %in% WEST, region := "WEST"]

df_joined <- full_join(df_joined, state_fips, by="STATE")

write.csv(df_joined,
          file.path(pr_results, "Study_dataset_2010.csv"),
          row.names = FALSE)

