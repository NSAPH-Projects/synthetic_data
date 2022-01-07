## Geospatial functions --------------------------------------------------------

#' @title
#' Map value object to the shape file
#'
#' @param shape_object SpatialPolygonsDataFrame object (see sp package)
#' @param point_object SpatialPointDataFrame object (see sp package)
#' @param value_name The attribute name in the point_object input parameter that needs to be aggregated.
#' @param extra_fields_name List of extra fields to include in the merged data.
#' @param group_field_name The field name that shows the group field name.
#' @param field_na_drop
#'
#' @return
#' A data.frame of FIPS code and average value of the point
#' @export
#'
map_point_shape <- function(shape_object, point_object, value_name,
                            extra_fields_name, group_field_name,
                            field_na_drop){

  # Make sure that both objects have the same projection

  if (is.na(shape_object@proj4string@projargs)){
    error("Shape object projection is not defined!")
  }


  if (is.na(point_object@proj4string@projargs)){
    message("Point object projection is not defined. Shape object projection is assigned.")
    point_object@proj4string <- shape_object@proj4string
  } else if (shape_object@proj4string@projargs != point_object@proj4string@projargs){
    message("Point and shape object projections are not the same. Shape object projection is assigned.")
    point_object@proj4string <- shape_object@proj4string
  }

  # Match the points from shape_object to point_object
  points_in_shape <- over(point_object, shape_object)

  # Put data together
  merged_data <- cbind(point_object, points_in_shape[,extra_fields_name])

  # Remove those data that could not match (these are data that are not located in-land)
  merged_data_drop_na <- merged_data[!is.na(getElement(as.data.frame(merged_data),
                                                       field_na_drop)),]

  # Convert Spatial data into ordinary data frame.
  merged_data_df <- as.data.frame(merged_data_drop_na)

  # Generate FIPS code
  #pm_data_with_county$fips <- paste(pm_data_with_county$STATE, pm_data_with_county$COUNTY, sep = "")

  # assign(value_name, value_name)
  # assign(group_field_name, group_field_name)

  # aggregated_data <- merged_data_df %>%
  #   group_by(FIPS) %>%
  #   summarise(mean(pm25))
  #
  # # check to see if there is missing value
  # if (sum(is.na(aggregated_data)) > 0){
  #   message("There is at least one missing value. In the aggregated data.")
  # }

  return(merged_data_df)
}

## Gridmet functions -----------------------------------------------------------

#' Title
#'
#' @param nc_obj netcdf4 object
#' @param param_name Parameter name that should be extracted from the nc_obj
#' @param start_date Start date for querying data, e.g., "2008-01-23"
#' @param end_date End date for querying data, similar format to the start date (end date is inclusive)
#' @param agg_fun Function to aggregate the data for different the dates (e.g., sum, mean, min, max)
#'
#' @return
#' @export
#'
aggregate_netcdf_data <- function(nc_obj, param_name, start_date,
                                  end_date, agg_fun){


  # Extract lon and lat values
  lon <- ncdf4::ncvar_get(nc_obj, "lon")
  lat <- ncdf4::ncvar_get(nc_obj, "lat")

  # available dates
  available_days <- ncdf4::ncvar_get(nc_obj, "day")

  # Compute grid points
  grid_points <- expand.grid(lon,lat)

  # Origin date in gridmet data
  origin_date <- "1900-01-01"

  # compute date parameters
  org_d <- as.Date(origin_date)
  str_d <- as.Date(start_date)
  end_d <- as.Date(end_date)

  org_d_i <- 1
  str_d_i <- (str_d - org_d)[[1]]
  end_d_i <- (end_d - org_d)[[1]]

  count_i <- (end_d_i - str_d_i)+1

  # check if dates are valid
  if (!(str_d_i %in% available_days)){
    stop("Start date is not in the available days.")
  }

  if (!(end_d_i %in% available_days)){
    stop("End date is not in the available days.")
  }

  # Compute first date and start date on the file

  first_date <- available_days[1]
  start_date_on_file <- (str_d_i - first_date)+1

  # Get the data
  day_val <- ncdf4::ncvar_get(nc_obj, param_name,
                              start = c(1,1,start_date_on_file),
                              count=c(-1,-1,count_i))

  # Aggregate the data
  agg_day_val <- apply(day_val, MARGIN = c(1,2), agg_fun, na.rm = TRUE)

  # Expand aggregated data and combine it with grid points
  expanded_day_val <- array(agg_day_val, dim=c(length(agg_day_val),1))
  data <- cbind(grid_points, val=expanded_day_val)

  # drop missing points with missing values
  data <- data[!is.na(data$val), ]

  names(data) <- c("lon", "lat", param_name)

  return(data)
}




#' Title
#'
#' @param nc_path
#' @param param_name
#' @param start_date
#' @param end_date
#' @param agg_fun
#' @param shape_obj
#' @param extra_fields_name
#' @param group_field_name
#' @param field_na_drop
#'
#' @return
#' @export
#'
#' @examples
compile_gridmet_data <- function(nc_path, param_name, start_date,
                                 end_date, agg_fun, shape_obj, extra_fields_name,
                                 group_field_name, field_na_drop, agg_field_name){


  # Open netcdf data
  nc_data <- nc_open(nc_path, readunlim = FALSE)

  # Aggregate daily data into date range.
  agg_data <- m_aggregate_netcdf_data(nc_data,
                                      param_name = param_name,
                                      start_date = start_date,
                                      end_date = end_date,
                                      agg_fun = agg_fun)

  # close connection with netcdf file.
  nc_close(nc_data)

  # Convert data.frame into spatial data.fram
  coordinates(agg_data) <- ~lon+lat

  # Map points into shapefile
  cs_agg_data <- m_map_point_shape(shape_object = shape_obj,
                                   point_object = agg_data,
                                   value_name = param_name,
                                   extra_fields_name = extra_fields_name,
                                   group_field_name = group_field_name,
                                   field_na_drop = field_na_drop)


  # Aggregate data

  aggregated_data <- cs_agg_data %>%
    group_by(.data[[group_field_name]]) %>%
    summarise(agg_val = agg_fun(.data[[param_name]]))

  names(aggregated_data) <- c(group_field_name, agg_field_name)

  return(aggregated_data)
}



compute_winter <- function(path_cy, path_ly, year_cy,
                           param_name,
                           field_name){

  # An alternative function could be merging two netcdf files.

  if ((year_cy %% 4)==0){
    winter_end_date <- "-02-29"
    winter_n_day <- (31 + 29)
  } else {
    winter_end_date <- "-02-28"
    winter_n_day <- (31 + 28)
  }

  fname <- paste0("mean_",field_name)
  winter_cy <- compile_gridmet_data(nc_path = path_cy,
                                    param_name = param_name,
                                    start_date = paste0(year_cy,"-01-01"),
                                    end_date = paste0(year_cy,winter_end_date),
                                    agg_fun = mean,
                                    shape_obj = cs_inland,
                                    extra_fields_name = c("STATE","COUNTY",
                                                          "NAME","FIPS"),
                                    group_field_name = "FIPS",
                                    field_na_drop = "STATE",
                                    agg_field_name = fname)

  winter_ly <- compile_gridmet_data(nc_path = path_ly,
                                    param_name = param_name,
                                    start_date = paste0(year_cy-1,"-12-01"),
                                    end_date = paste0(year_cy-1, "-12-31"),
                                    agg_fun = mean,
                                    shape_obj = cs_inland,
                                    extra_fields_name = c("STATE","COUNTY",
                                                          "NAME","FIPS"),
                                    group_field_name = "FIPS",
                                    field_na_drop = "STATE",
                                    agg_field_name = fname)

  fname <- paste0("mean_",field_name)
  winter_val  <- (winter_cy[[fname]]*winter_n_day +
                    winter_ly[[fname]]*31)/(winter_n_day + 31)


  winter <- dplyr::tibble(FIPS = winter_cy$FIPS)
  winter[[fname]] <- winter_val

  return(winter)
}

## CMS functions ---------------------------------------------------------------

#' Title
#'
#' @param file_path
#' @param year_
#' @param ssa_to_fips_cross
#'
#' @return
#' @export
#'
extract_CMS_data <- function(file_path, year_, ssa_to_fips_cross){

  file_head <- paste0("DE1_0_",year_,"_Beneficiary_Summary_File_Sample")
  x <- rbindlist(lapply(list.files(file_path,
                                   pattern = file_head,
                                   full.names = T),
                        fread))
  x <- x[, .(DESYNPUF_ID,
             BENE_BIRTH_DT,
             BENE_DEATH_DT,
             BENE_SEX_IDENT_CD,
             BENE_RACE_CD,
             SP_STATE_CODE,
             BENE_COUNTY_CD)]
  x[, year := year_]
  x[, BENE_DEATH_DT := ymd(BENE_DEATH_DT)]
  x[, BENE_BIRTH_DT := ymd(BENE_BIRTH_DT)]
  x[, dead := year(BENE_DEATH_DT) == year]
  x[is.na(dead), dead := F]
  x[, age := year - year(BENE_BIRTH_DT)]
  x[, SSA :=  paste(sprintf("%02d",SP_STATE_CODE),
                    sprintf("%03d", BENE_COUNTY_CD),
                    sep = "")]

  return(x)
}


#' Title
#'
#' @param cms_data_fips
#'
#' @return
#' @export
#'
aggregate_cms_data <- function(cms_data_fips){

  # This function is tailored for CMS synthetic data.
  cms_data_fips$dead <- as.numeric(cms_data_fips$dead)

  # https://www.cms.gov/files/document/de-10-codebook.pdf-0
  # Create dummy variables for race
  cms_data_fips$race_white <- ifelse(cms_data_fips$BENE_RACE_CD == 1, 1, 0)
  cms_data_fips$race_black <- ifelse(cms_data_fips$BENE_RACE_CD == 2, 1, 0)
  cms_data_fips$race_others <- ifelse(cms_data_fips$BENE_RACE_CD == 3, 1, 0)
  cms_data_fips$race_hispanic <- ifelse(cms_data_fips$BENE_RACE_CD == 5, 1, 0)

  # Create dummy variable for sex
  cms_data_fips$sex_male <- ifelse(cms_data_fips$BENE_SEX_IDENT_CD == 1, 1, 0)
  cms_data_fips$sex_female <- ifelse(cms_data_fips$BENE_SEX_IDENT_CD == 2, 1, 0)

  cms_data_fips <- cms_data_fips[, c("year","FIPS","age","dead","race_white",
                                     "race_black","race_others","race_hispanic",
                                     "sex_male","sex_female")]

  agg_data <- cms_data_fips %>%
    group_by(year, FIPS) %>%
    summarise(cms_mortality_pct = sum(dead)/length(dead),
              cms_white_pct = sum(race_white)/length(race_white),
              cms_black_pct = sum(race_black)/length(race_black),
              cms_others_pct = sum(race_others)/length(race_others),
              cms_hispanic_pct = sum(race_hispanic)/length(race_hispanic),
              cms_female_pct = sum(sex_female)/length(sex_female))

  return(agg_data)
}

## CDC Functions

extract_brfss_vars <- function(brfss_file, varlist){
  return(brfss_file[, varlist])
}

polish_bfrss_vars <- function(brfss_data){

  # Drop data without county value
  brfss_subset_drop_na <- brfss_data[!is.na(getElement(brfss_data, "county")),]

  # Create FIPS code
  brfss_subset_drop_na$FIPS <-  paste(sprintf("%02d",
                                              getElement(brfss_subset_drop_na,
                                                         "state")),
                                      sprintf("%03d",
                                              getElement(brfss_subset_drop_na,
                                                         "county")),
                                      sep = "")

  # Create dummy variables for smoker
  data_dummy <- brfss_subset_drop_na %>%
    add_column(current_smoker = if_else(getElement(.,"smoker") == 1, 1, 0),
               somedays_smoker = if_else(getElement(.,"smoker") == 2, 1, 0),
               former_smoker = if_else(getElement(.,"smoker") == 3, 1, 0),
               never_smoker = if_else(getElement(.,"smoker") == 4, 1, 0),
               notknown_smoker = if_else(getElement(.,"smoker") == 9, 1, 0),
               total_smoker = 1)

  # Aggregate data by FIPS code

  agg_data <- data_dummy %>%
    group_by(FIPS) %>%
    summarise(cdc_mean_bmi = mean(bmi),
              count_cusmoker = sum(current_smoker),
              count_sdsmoker = sum(somedays_smoker),
              count_fmsmoker = sum(former_smoker),
              count_nvsmoker = sum(never_smoker),
              count_nnsmoker = sum(notknown_smoker),
              count_tlsmoker = sum(total_smoker))

  # compute percentage of different smokers

  agg_data_pct <- agg_data %>%
    add_column(
      cdc_pct_cusmoker = .$count_cusmoker / .$count_tlsmoker,
      cdc_pct_sdsmoker = .$count_sdsmoker / .$count_tlsmoker,
      cdc_pct_fmsmoker = .$count_fmsmoker / .$count_tlsmoker,
      cdc_pct_nvsmoker = .$count_nvsmoker / .$count_tlsmoker,
      cdc_pct_nnsmoker = .$count_nnsmoker / .$count_tlsmoker,
    )

  data <- agg_data_pct[, c("FIPS", "cdc_mean_bmi", "cdc_pct_cusmoker",
                           "cdc_pct_sdsmoker", "cdc_pct_fmsmoker",
                           "cdc_pct_nvsmoker", "cdc_pct_nnsmoker")]

  return(data)
}

impute_cdc <- function(data, param_list){

  for (param in param_list){

    # compute mean and std for each state
    state_mean_std <- data %>%
      group_by(STATE) %>%
      summarise(mean = mean(.data[[param]], na.rm = TRUE),
                std = sd(.data[[param]], na.rm = TRUE))

    # assign a random value to the missing county

    param_index <- which(colnames(data) == param)

    for (i in seq(1, nrow(data))){

      if (is.na(data[i, param_index])){

        mean_std_val <- state_mean_std[state_mean_std$STATE == data[i, c("STATE")], c("mean", "std")]

        if (is.na(mean_std_val[2])){
          data[i, param_index] <- mean_std_val[1]
        } else {
          val <- rnorm(1, mean=mean_std_val[1][[1]],
                       sd = mean_std_val[2][[1]])
          # If random value is a negative number, use the mean value.
          data[i, param_index] <- ifelse(val < 0, mean_std_val[1][[1]], val)
        }
      }
    }
  }
  return(data)
}

## PM2.5 Functions -------------------------------------------------------------

#' @title
#' Match Di et al 2019 exposure data to site code
#'
#' @param site_code A data.frame with Lon, Lat, and SiteCode fields.
#' @param exp_data  Di et al 2019, exposure data.
#' @param exp_name  The exposure parameter name.
#'
#' @return
#' A data.frame with Lon, Lat, and exposure data.
#'
#' @export
#'
match_exposure_to_sitecode <- function(site_code, exp_data, exp_name){
  data <- data.frame(t(exp_data))
  names(data) <- exp_name
  m_data <- cbind(site_code,data)
  m_data$SiteCode <- NULL
  return(m_data)
}
