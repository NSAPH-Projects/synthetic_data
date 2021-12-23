##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Aggregating gridmet data to county level 


# load libraries
library(ncdf4)
library(PCICt)


# Add functions to memoization
cd <- cachem::cache_disk(pr_cache)
m_aggregate_netcdf_data <- memoise(aggregate_netcdf_data, cache = cd)
m_map_point_shape <- memoise(map_point_shape, cache = cd)


loop_over_year <- function(year){

# Temrature 
tmmn_path <- file.path(get_options("input_dir"),
                            "public/gridmet_data",
                            paste0("tmmn_",year,".nc"))
tmmx_path <- file.path(get_options("input_dir"),
                            "public/gridmet_data",
                            paste0("tmmx_",year,".nc"))

# Humidity 
rmin_path <- file.path(get_options("input_dir"),
                            "public/gridmet_data",
                            paste0("rmin_",year,".nc"))
rmax_path <- file.path(get_options("input_dir"),
                            "public/gridmet_data",
                            paste0("rmax_",year,".nc"))

sph_path <- file.path(get_options("input_dir"),
                            "public/gridmet_data",
                           paste0("sph_",year,".nc"))

tmmn <- compile_gridmet_data(nc_path = tmmn_path, 
                                  param_name = "air_temperature",
                                  start_date = paste0(year,"-01-01"),
                                  end_date = paste0(year,"-12-30"),
                                  agg_fun = mean,
                                  shape_obj = cs_inland,
                                  extra_fields_name = c("STATE","COUNTY",
                                                        "NAME","FIPS"),
                                  group_field_name = "FIPS",
                                  field_na_drop = "STATE",
                                  agg_field_name = "mean_tmmn")

tmmx <- compile_gridmet_data(nc_path = tmmx_path, 
                                  param_name = "air_temperature",
                                  start_date = paste0(year,"-01-01"),
                                  end_date = paste0(year,"-12-30"),
                                  agg_fun = mean,
                                  shape_obj = cs_inland,
                                  extra_fields_name = c("STATE","COUNTY",
                                                        "NAME","FIPS"),
                                  group_field_name = "FIPS",
                                  field_na_drop = "STATE",
                                  agg_field_name = "mean_tmmx")

rmn <- compile_gridmet_data(nc_path = rmin_path, 
                                  param_name = "relative_humidity",
                                  start_date = paste0(year,"-01-01"),
                                  end_date = paste0(year,"-12-30"),
                                  agg_fun = mean,
                                  shape_obj = cs_inland,
                                  extra_fields_name = c("STATE","COUNTY",
                                                        "NAME","FIPS"),
                                  group_field_name = "FIPS",
                                  field_na_drop = "STATE",
                                  agg_field_name = "mean_rmn")

rmx <- compile_gridmet_data(nc_path = rmax_path, 
                                  param_name = "relative_humidity",
                                  start_date = paste0(year,"-01-01"),
                                  end_date = paste0(year,"-12-30"),
                                  agg_fun = mean,
                                  shape_obj = cs_inland,
                                  extra_fields_name = c("STATE","COUNTY",
                                                        "NAME","FIPS"),
                                  group_field_name = "FIPS",
                                  field_na_drop = "STATE",
                                  agg_field_name = "mean_rmx")

sph <- compile_gridmet_data(nc_path = sph_path, 
                                 param_name = "specific_humidity",
                                 start_date = paste0(year,"-01-01"),
                                 end_date = paste0(year,"-12-30"),
                                 agg_fun = mean,
                                 shape_obj = cs_inland,
                                 extra_fields_name = c("STATE","COUNTY",
                                                       "NAME","FIPS"),
                                 group_field_name = "FIPS",
                                 field_na_drop = "STATE",
                                 agg_field_name = "mean_sph")

## Merge data

multi_merge <- function(x, y){
  df <- full_join(x, y, by= "FIPS")
  return(df)
}

df_gridmet <- Reduce(multi_merge, list(tmmn,
                                       tmmx,
                                       rmn,
                                       rmx,
                                       sph))


assign_data <- function(data, from_fips, to_fips){
  tmp <- data[data$FIPS == from_fips,]
  tmp$FIPS <- to_fips
  return(tmp)
}

# These are independent cities in the Common Wealth Virginia. Since there is no 
# gridMET data are located in these cities they are missing. In this report
# We consider assign the surrounding counties values to these cities. 

df_gridmet<- rbind(df_gridmet,
                   assign_data(df_gridmet, 51005, 51580),
                   assign_data(df_gridmet, 51163, 51678),
                   assign_data(df_gridmet, 51059, 51610),
                   assign_data(df_gridmet, 51019, 51515))

return(df_gridmet)
}


data_2010 <- loop_over_year(2010)

data_2010_cp <- data_2010


## Write data into file.
write.csv(data_2010,
          file.path(pr_results, "gridmet_data_2010.csv"),
          row.names = FALSE)

# The rest is just for representation purposes
# merge the new values with the shape file.
merged_obj <- merge(cs_inland, data_2010, by=c("FIPS"))

spplot(merged_obj, zcol = "mean_tmmx",
       col.regions=topo.colors(51, rev = FALSE),
       xlab="Longitude", ylab="Latitude",
       main="Mean daily maximum temprature in the Contiguous United States (2010)")

