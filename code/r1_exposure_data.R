##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Aggregating exposure data to the county level in the United States


# load required source codes and libraries

library(rgdal)
library(sf)
library(dplyr)
library(memoise)


# Set up memoization
cd <- cachem::cache_disk(pr_cache)

m_match_exposure_to_sitecode <- memoise(match_exposure_to_sitecode, cache = cd)
m_map_point_shape <- memoise(map_point_shape, cache = cd)



# Download data
# These data are located in input/public_data/Di_2019.
# The information about downloading data is located in the same folder inside 
# data_source.md file.

# Create file path for each rds file and read data.

fpath_2008 <- file.path(get_options("input_dir"),"public/Di_2019","2008.rds")
fpath_2009 <- file.path(get_options("input_dir"),"public/Di_2019","2009.rds")
fpath_2010 <- file.path(get_options("input_dir"),"public/Di_2019","2010.rds")
usgpath <- file.path(get_options("input_dir"),"public/Di_2019","USGridSite.rds")

di_pm25_2008 <- readRDS(fpath_2008)
di_pm25_2009 <- readRDS(fpath_2009)
di_pm25_2010 <- readRDS(fpath_2010)
us_grid <- readRDS(usgpath)

# Match exposure to the site code

pm25_2008 <- m_match_exposure_to_sitecode(site_code = us_grid,
                                          exp_data = di_pm25_2008,
                                          exp_name = "pm25")

pm25_2009 <- m_match_exposure_to_sitecode(site_code = us_grid,
                                          exp_data = di_pm25_2009,
                                          exp_name = "pm25")

pm25_2010 <- m_match_exposure_to_sitecode(site_code = us_grid,
                                          exp_data = di_pm25_2010,
                                          exp_name = "pm25")

# Read County shape file

fpath <- file.path(get_options("input_dir"),
                   "public/Geospatial", "gz_2010_us_050_00_500k/")

county_shape_file <-  rgdal::readOGR(fpath)
county_shape_file <- spTransform(county_shape_file, CRS("+proj=longlat +datum=WGS84"))

# Compute only contiguous united states

# We are interested in contiguous states.
# Here is the list of states and their codes:
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
# non-contiguous states:
# 02: Alaska, 15: Hawaii, 60: American Samoa, 66: Guam,
# 69: Northern Mariana Islands, 72: Puerto Rico, 78: Virgin Islands

non_c_states <- c("02","15","60","66","69","72","78")
cs_inland <- county_shape_file[!county_shape_file$STATE %in% non_c_states, ]

# Generate FIPS code
cs_inland$FIPS <- paste(cs_inland$STATE,cs_inland$COUNTY,sep = "")

# Convert pm2.5 data frame to SpatialPoints data frame

coordinates(pm25_2008) <- ~Lon+Lat
coordinates(pm25_2009) <- ~Lon+Lat
coordinates(pm25_2010) <- ~Lon+Lat

# Join points with polygons
cs_inland_pm_2008 <- m_map_point_shape(shape_object = cs_inland, 
                                       point_object = pm25_2008, 
                                       value_name = "pm25",
                                       extra_fields_name = c("STATE","COUNTY",
                                                              "NAME","FIPS"), 
                                       group_field_name = "FIPS",
                                       field_na_drop = "STATE")

cs_inland_pm_2009 <- m_map_point_shape(shape_object = cs_inland, 
                                       point_object = pm25_2009, 
                                       value_name = "pm25",
                                       extra_fields_name = c("STATE","COUNTY",
                                                             "NAME","FIPS"), 
                                       group_field_name = "FIPS",
                                       field_na_drop = "STATE")

cs_inland_pm_2010 <- m_map_point_shape(shape_object = cs_inland, 
                                       point_object = pm25_2010, 
                                       value_name = "pm25",
                                       extra_fields_name = c("STATE","COUNTY",
                                                             "NAME","FIPS"), 
                                       group_field_name = "FIPS",
                                       field_na_drop = "STATE")

# Aggregate data for FIPS code level

aggregated_pm_data_2008 <- cs_inland_pm_2008 %>%
  group_by(FIPS) %>%
  summarise(mean_pm25 = mean(pm25))

aggregated_pm_data_2009 <- cs_inland_pm_2009 %>%
  group_by(FIPS) %>%
  summarise(mean_pm25 = mean(pm25))

aggregated_pm_data_2010 <- cs_inland_pm_2010 %>%
  group_by(FIPS) %>%
  summarise(mean_pm25 = mean(pm25))

# Write the results into file
write.csv(aggregated_pm_data_2008,
          file.path(pr_results, "aggregated_pm_data_2008.csv"),
          row.names = FALSE)

write.csv(aggregated_pm_data_2009,
          file.path(pr_results, "aggregated_pm_data_2009.csv"),
          row.names = FALSE)

write.csv(aggregated_pm_data_2010,
          file.path(pr_results, "aggregated_pm_data_2010.csv"),
          row.names = FALSE)

# Step 10: Take a look at data
# merge the new values with the shape file.
merged_obj <- merge(cs_inland, aggregated_pm_data_2010, by=c("FIPS"))


spplot(merged_obj, zcol = "mean_pm25",
       col.regions=heat.colors(51, rev = TRUE),
       xlab="Longitude", ylab="Latitude",
       main="Mean PM2.5 in the Contiguous United States (2010)")
