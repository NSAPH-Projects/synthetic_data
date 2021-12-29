##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Generating synthetic data for healthcare studies

# Make sure that your project environment is correctly set up. To do that:
# You need to update project_path_info.md file. Then
# Run: initialize_project.sh

set.seed(1274)

# Setup up folders' path
source("setup_env_vals.R")

# Load utility functions
source("r0_utility_functions.R")

# Process exposure data
source("r1_exposure_data.R")

# Porcess census data
source("r2_census_data.R")

# Process CDC data 
source("r3_brfss_data.R")

# Process gridMET data
source("r4_gridmet_data.R")

# Process Synthetic CMS data
source("r5_syn_CMS_data.R")

# Join Data Together
# This file uses saved files. You can run it separately if you have run the 
# the previous files, before.
source("r6_join_data.R")


