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

brfss_2008 <- read_xpt(file.path(get_options("input_dir"),
                                 "public/CDC_data","CDBRFS08.XPT"))
brfss_2009 <- read_xpt(file.path(get_options("input_dir"),
                                 "public/CDC_data","CDBRFS09.XPT"))
brfss_2010 <- read_xpt(file.path(get_options("input_dir"),
                                 "public/CDC_data","CDBRFS10.XPT"))

varlist <- c("_STATE", "CTYCODE", "_BMI4","_SMOKER3")

brfss_2008_subset <- extract_brfss_vars(brfss_2008, varlist = varlist)
brfss_2009_subset <- extract_brfss_vars(brfss_2009, varlist = varlist)
brfss_2010_subset <- extract_brfss_vars(brfss_2010, varlist = varlist)

brfss_2008 <- NULL
brfss_2009 <- NULL
brfss_2010 <- NULL


# Modify column names
names(brfss_2008_subset) <- c("state","county","bmi","smoker")
names(brfss_2009_subset) <- c("state","county","bmi","smoker")
names(brfss_2010_subset) <- c("state","county","bmi","smoker")


brfss_data_2008 <- polish_bfrss_vars(brfss_2008_subset)
brfss_data_2009 <- polish_bfrss_vars(brfss_2009_subset)
brfss_data_2010 <- polish_bfrss_vars(brfss_2010_subset)

write.csv(brfss_data_2008,
          file.path(pr_results, "aggregated_brfss_data_2008.csv"),
          row.names = FALSE)

write.csv(brfss_data_2009,
          file.path(pr_results, "aggregated_brfss_data_2009.csv"),
          row.names = FALSE)

write.csv(brfss_data_2010,
          file.path(pr_results, "aggregated_brfss_data_2010.csv"),
          row.names = FALSE)

# The rest is just for representation purposes
# merge the new values with the shape file.
merged_obj <- merge(cs_inland, brfss_data_2010, by=c("FIPS"))

spplot(merged_obj, zcol = "pct_cusmoker",
       col.regions=heat.colors(51, rev = TRUE),
       xlab="Longitude", ylab="Latitude",
       main="Percentage of Current Smokers in the Contiguous United States (2010)")





