##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Processing CDC Data 

## requirements: 

##   parameters:
##        nthreads: adjust according to availability
##          indir: path to the input directory

##   output:
##      CSV file, 



library("tidyverse")
library("haven")


# Read CDC files
brfss_2010 <- read_xpt(file.path(get_options("input_dir"),
                                 "public/CDC_data","CDBRFS10.XPT"))

varlist <- c("_STATE", "CTYCODE", "_BMI4","_SMOKER3")

brfss_2010_subset <- extract_brfss_vars(brfss_2010, varlist = varlist)

brfss_2010 <- NULL


# Modify column names
names(brfss_2010_subset) <- c("state","county","bmi","smoker")

# Polish variables
brfss_data_2010 <- polish_bfrss_vars(brfss_2010_subset)

# merge the new values with the shape file.
merged_obj <- merge(cs_inland, brfss_data_2010, by=c("FIPS"))
merged_obj$missing <- ifelse(is.na(merged_obj$mean_bmi), 1, 0)
merged_obj$missing <- as.factor(merged_obj$missing)

brfss_data_2010_merged <- data.frame(
           merged_obj[, c("FIPS", "STATE", "COUNTY", "mean_bmi", "pct_cusmoker", 
                          "pct_sdsmoker", "pct_fmsmoker", "pct_nvsmoker", 
                          "pct_nnsmoker")])

brfss_data_2010_merged <- impute_cdc(
  data = brfss_data_2010_merged,
  param_list = c("mean_bmi", "pct_cusmoker", 
                 "pct_sdsmoker","pct_fmsmoker",
                 "pct_nvsmoker",
                 "pct_nnsmoker"))

brfss_data_2010_merged <- brfss_data_2010_merged[, !colnames(brfss_data_2010_merged) %in% c("STATE", "COUNTY")]

write.csv(brfss_data_2010_merged,
          file.path(pr_results, "brfss_data_2010.csv"),
          row.names = FALSE)

# Plot of missing values
spplot(merged_obj, zcol = "missing",
       col.regions=c("snow","red"),
       col = "grey61",
       xlab="Longitude", ylab="Latitude",
       main="Counties with Missing CDC Data")

# Plot of current smokers
merged_obj <- merge(cs_inland, brfss_data_2010_merged, by=c("FIPS"))
spplot(merged_obj, zcol = "pct_cusmoker",
       col.regions=heat.colors(51, rev = TRUE),
       xlab="Longitude", ylab="Latitude",
       main="Percentage of Current Smokers in the Contiguous United States (2010)")





