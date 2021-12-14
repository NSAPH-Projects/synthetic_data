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


cms_2008 <- m_extract_CMS_data(CMS_DATA_DIR, 2008)
cms_2009 <- m_extract_CMS_data(CMS_DATA_DIR, 2009)
cms_2010 <- m_extract_CMS_data(CMS_DATA_DIR, 2010)

cms_2008_fips <- merge(cms_2008, ssa2fips_cross, by = "SSA")
cms_2009_fips <- merge(cms_2009, ssa2fips_cross, by = "SSA")
cms_2010_fips <- merge(cms_2010, ssa2fips_cross, by = "SSA")

agg_data_cms_2008 <- m_aggregate_cms_data(cms_2008_fips)
agg_data_cms_2009 <- m_aggregate_cms_data(cms_2009_fips)
agg_data_cms_2010 <- m_aggregate_cms_data(cms_2010_fips)


write.csv(agg_data_cms_2008,
          file.path(pr_results, "CMS_2008.csv"),
          row.names = FALSE)

write.csv(agg_data_cms_2009,
          file.path(pr_results, "CMS_2009.csv"),
          row.names = FALSE)

write.csv(agg_data_cms_2010,
          file.path(pr_results, "CMS_2010.csv"),
          row.names = FALSE)

