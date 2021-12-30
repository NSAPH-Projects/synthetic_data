##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Downloading Census Data 



# load libraries
library(tidycensus)
library(tidyverse)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

v10 <- load_variables(2010, "acs5", cache = TRUE)
# v09 <- load_variables(2009, "acs1", cache = TRUE)
# v08 <- load_variables(2008, "acs1", cache = TRUE)

census_vars = c("B01001_020", "B01001_021", "B01001_022", "B01001_023",
                "B01001_024", "B01001_025",
                "B01001_044", "B01001_045", "B01001_046", "B01001_047",
                "B01001_048", "B01001_049",
                "B17001_015", "B17001_016",
                "B17001_029", "B17001_030",
                "B01001I_014", "B01001I_015", "B01001I_016",
                "B01001I_029", "B01001I_030", "B01001I_031",
                "B01001B_014", "B01001B_015", "B01001B_016",
                "B01001B_029", "B01001B_030", "B01001B_031",
                "B01001H_014", "B01001H_015", "B01001H_016",
                "B01001H_029", "B01001H_030", "B01001H_031",
                "B01001C_014", "B01001C_015", "B01001C_016",
                "B01001C_029", "B01001C_030", "B01001C_031",
                "B01001D_014", "B01001D_015", "B01001D_016",
                "B01001D_029", "B01001D_030", "B01001D_031",
                "B15001_035", "B15001_036", "B15001_037",
                "B15001_076", "B15001_077", "B15001_078",
                "B19049_005",
                "B25077_001")

census_2010 <- get_acs(geography = "county", 
                       variables = census_vars,
                       year = 2010,
                       survey = "acs5",
                       output = "wide")

# Drop Margin of error column. 
census_2010_e <- census_2010 %>% select(-ends_with("M"))
census_2010_e$STATE <- substr(census_2010_e$GEOID, 1, 2)
census_2010_e$COUNTY <- substr(census_2010_e$GEOID, 3, 5)

# Drop non contiguous us data
inland_states <- county_per_state$STATE
census_2010_e_inland <- census_2010_e[census_2010_e$STATE %in% inland_states, ]


# see number of missing data for each column
sapply(census_2010_e_inland, function(x) sum(is.na(x)))

# add new variables
census_2010_e_m <- census_2010_e_inland %>%
  add_column(tmp_total_pop = (.$B01001_020E + .$B01001_021E + .$B01001_022E +
                              .$B01001_023E + .$B01001_024E + .$B01001_025E +
                              .$B01001_044E + .$B01001_045E + .$B01001_046E +
                              .$B01001_047E + .$B01001_048E + .$B01001_049E))

census_2010_e_m <- census_2010_e_m %>%
                   add_column(cs_poverty = (.$B17001_015E + .$B17001_016E +
                                            .$B17001_029E + .$B17001_030E)/
                                            (.$tmp_total_pop),
                              cs_hispanic = (.$B01001I_014E + .$B01001I_015E +
                                             .$B01001I_016E + .$B01001I_029E +
                                             .$B01001I_030E + .$B01001I_031E)/
                                            (.$tmp_total_pop),
                              cs_black = (.$B01001B_014E + .$B01001B_015E + 
                                          .$B01001B_016E + .$B01001B_029E + 
                                          .$B01001B_030E + .$B01001B_031E)/
                                         (.$tmp_total_pop),
                              cs_white = (.$B01001H_014E + .$B01001H_015E + 
                                          .$B01001H_016E + .$B01001H_029E + 
                                          .$B01001H_030E + .$B01001H_031E)/
                                         (.$tmp_total_pop),
                              cs_native = (.$B01001C_014E + .$B01001C_015E +
                                           .$B01001C_016E + .$B01001C_029E + 
                                           .$B01001C_030E + .$B01001C_031E)/
                                         (.$tmp_total_pop),
                              cs_asian = (.$B01001D_014E + .$B01001D_015E + 
                                          .$B01001D_016E + .$B01001D_029E + 
                                          .$B01001D_030E + .$B01001D_031E)/
                                         (.$tmp_total_pop),
                              cs_ed_below_highschool = (
                                .$B15001_036E + .$B15001_037E +
                                .$B15001_077E + .$B15001_078E)/ 
                                (.$B15001_035E + .$B15001_076E),
                              cs_household_income = .$B19049_005E,
                              cs_median_house_value = .$B25077_001E
)

# One county is missing data (FIPS: 48301). 
census_2010_e_m <- census_2010_e_m %>%
 mutate_all(~replace(., is.na(.), 0))

tmp <- rowSums(census_2010_e_m[, c("cs_hispanic", "cs_black", "cs_white",
                                  "cs_native", "cs_asian")])

census_2010_e_m$cs_other <- ifelse((1-tmp)<0, 0, 1-tmp)

# Drop initial columns
census_2010_processed <- census_2010_e_m %>%
                             select(!matches("B[0-9]+[A-Z]*_[0-9]+E"))

census_2010_processed$tmp_total_pop <- NULL

colnames(census_2010_processed)[which(names(census_2010_processed) == "GEOID")] <- "FIPS"

census_2010_processed$STATE <- NULL
census_2010_processed$COUNTY <- NULL

# Assigning average of four surrounding counties:
compute_cols <- c("cs_poverty", "cs_hispanic", "cs_black",
                  "cs_white",
                  "cs_native", "cs_asian",
                  "cs_household_income",
                  "cs_other", "cs_ed_below_highschool")
tmp_four <- census_2010_processed[census_2010_processed$FIPS %in% c("48495",
                                                                    "48475",
                                                                    "48389",
                                                                    "35025"),
                                  compute_cols]

tmp_mean <- colMeans(tmp_four)
tmp_mean[["cs_household_income"]] <- floor(tmp_mean[["cs_household_income"]])

for (item in compute_cols){
  census_2010_processed[census_2010_processed$FIPS=="48301", item] <- getElement(tmp_mean, item)
}

write.csv(census_2010_processed,
          file.path(pr_results, "census_data_2010.csv"),
          row.names = FALSE)

merged_obj <- merge(cs_inland, census_2010_processed, by=c("FIPS"))
spplot(merged_obj, zcol = "cs_median_house_value",
       col.regions=terrain.colors(51, rev = FALSE),
       xlab="Longitude", ylab="Latitude",
       main="Median House Value in the Contiguous United States (2010)")


