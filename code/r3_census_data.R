##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Downloading Census Data 



# load libraries
library(tidycensus)
library(tidyverse)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

v10 <- load_variables(2010, "acs5", cache = TRUE)
v09 <- load_variables(2009, "acs1", cache = TRUE)
v08 <- load_variables(2008, "acs1", cache = TRUE)

census_vars = c("B01001_001", "B17001_002", "B17001_001", "B03001_001",
                "B03001_003", "B01001_001", "B25077_001", "B02001_003",
                "B02001_001", "B02001_002", "B02001_004", "B02001_005",
                "B19013_001", "B25003_002", "B25003_001", "B01002_001")

census_2008 <- get_acs(geography = "county", 
                       variables = census_vars,
                       year = 2008,
                       survey = "acs1",
                       output = "wide")

census_2009 <- get_acs(geography = "county", 
                       variables = census_vars,
                       year = 2009,
                       survey = "acs1",
                       output = "wide")

census_2010 <- get_acs(geography = "county", 
                       variables = census_vars,
                       year = 2010,
                       survey = "acs5",
                       output = "wide")

# Drop Margin of error column. 
census_2010_e <- census_2010 %>% select(-ends_with("M"))

# see number of missing data for each column
sapply(census_2010_e, function(x) sum(is.na(x)))

# add new variables
census_2010_e_m <- census_2010_e %>%
                   add_column(poverty = .$B17001_002E/.$B17001_001E,
                              pct_hispanic = .$B03001_003E/.$B03001_001E,
                              pct_black = .$B02001_003E/.$B02001_001E,
                              pct_white = .$B02001_002E/.$B02001_001E,
                              pct_native = .$B02001_004E/.$B02001_001E,
                              pct_asian = .$B02001_005E/.$B02001_001E,
                              owner_occ = .$B25003_002E/.$B25003_001E,
                              median_house_value = .$B25077_001E,
                              median_household_income = .$B19013_001E,
                              median_age = .$B01002_001E)


# Drop initial columns
census_2010_processed <- census_2010_e_m %>%
                             select(!matches("B[0-9]+_[0-9]+E"))

colnames(census_2010_processed)[which(names(census_2010_processed) == "GEOID")] <- "FIPS"


write.csv(census_2010_processed,
          file.path(pr_results, "census_2010.csv"),
          row.names = FALSE)


merged_obj <- merge(cs_inland, census_2010_processed, by=c("FIPS"))
spplot(merged_obj, zcol = "median_house_value",
       col.regions=terrain.colors(51, rev = FALSE),
       xlab="Longitude", ylab="Latitude",
       main="Median House Value in the Contiguous United States (2010)")


