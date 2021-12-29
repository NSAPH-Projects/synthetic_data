##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Aggregating Synthetic CMS data 

# load libraries

library(fst)
library(lubridate)
library(data.table)
library(memoise)


# Add functions to memoization
cd <- cachem::cache_disk(pr_cache)
m_extract_CMS_data <- memoise(extract_CMS_data, cache = cd)
m_aggregate_cms_data <- memoise(aggregate_cms_data, cache = cd)

# Combine fst data.
CMS_DATA_DIR <- file.path(get_options("input_dir"),"public/cms_data")

# read SSA to FIPS crosswalk
# https://www.nber.org/research/data/ssa-federal-information-processing-series-fips-state-and-county-crosswalk
ssa2fips_data <- read.csv(file.path(get_options("input_dir"),
                                    "public","cms_data",
                                    "ssa_fips_state_county2011.csv"))

ssa2fips_data$cbsa <- NULL
ssa2fips_data$cbsaname <- NULL

# remove empty rows
ssa2fips_data <- ssa2fips_data[!is.na(ssa2fips_data$ssacounty),]

ssa2fips_data$FIPS <- sprintf("%05d", ssa2fips_data$fipscounty)
ssa2fips_data$SSA <- sprintf("%05d", ssa2fips_data$ssacounty)

ssa2fips_cross <- ssa2fips_data[, c("SSA","FIPS")]


cms_2010 <- m_extract_CMS_data(CMS_DATA_DIR, 2010)


cms_2010_fips <- merge(cms_2010, ssa2fips_cross, by = "SSA")

agg_data_cms_2010 <- m_aggregate_cms_data(cms_2010_fips)


add_state_county <- function(data){
  data$STATE <- substr(data$FIPS, 1, 2)
  data$COUNTY <- substr(data$FIPS, 3, 5)
  return(data)
}


agg_data_cms_2010 <- add_state_county(agg_data_cms_2010)

# select inland data
inland_states <- county_per_state$STATE
agg_data_cms_2010 <- agg_data_cms_2010[agg_data_cms_2010$STATE %in% inland_states, ]

#county_per_state_cms <- agg_data_cms_2010 %>% group_by(STATE) %>% count()


# Drop State and County fields
agg_data_cms_2010 <- agg_data_cms_2010[, !colnames(agg_data_cms_2010) %in%
                                         c("STATE", "COUNTY")]

merged_obj <- merge(cs_inland, agg_data_cms_2010, by=c("FIPS"))
merged_obj$missing <- ifelse(is.na(merged_obj$cms_mortality_pct), 1, 0)
merged_obj$missing <- as.factor(merged_obj$missing)

# Imputing missing data 
cms_attr <- c("cms_mortality_pct", "cms_white_pct", "cms_black_pct",
              "cms_others_pct", "cms_hispanic_pct", "cms_female_pct")

cms_obj_val <- data.frame(merged_obj[, c("FIPS","STATE","COUNTY","missing",
                                         cms_attr)])

for (item in cms_attr){
  cms_obj_val <- cms_obj_val %>% 
    group_by(STATE) %>% 
    mutate(new_val=ifelse(is.na(.data[[item]]),
                          median(.data[[item]], na.rm=TRUE),
                          .data[[item]]))
  
  cnames <- colnames(cms_obj_val)
  cms_obj_val <- cms_obj_val[ , !(names(cms_obj_val) %in% c(item))]
  colnames(cms_obj_val)[which(cnames=="new_val")-1] <- item
}


spplot(merged_obj, zcol = "missing",
       col.regions=c("snow","red"),
       col = "grey61",
       xlab="Longitude", ylab="Latitude",
       main="Counties with Missing CMS Data.")

cms_obj_val <- cms_obj_val[, !colnames(cms_obj_val) %in% 
                             c("STATE","COUNTY","missing")]


write.csv(cms_obj_val,
          file.path(pr_results, "CMS_data_2010.csv"),
          row.names = FALSE)

