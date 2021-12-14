##       author: Naeem Khoshnevis
##      created: December 2021
##      purpose: Set up environmental variables and path

##  Requirements
##       Before running this R script, the project_path_info.md file should be 
##       updated for the project and initialize_project.sh should be run. 


###  Passing data between functions --------------------------------------------
if (!exists("my_options", mode="environment")){
  my_options <- new.env(parent = emptyenv())
  message("A new key-value environment has been created.")
}

get_options <- function(k){
  my_options[[k]]
}

set_options <- function(k, v){
  my_options[[k]] <- v
}

list_options <- function(){
  names(my_options)
}

### Set up path

pr_path <- read.table("../pr_hard_path_name.txt", header=FALSE)
pr_dir <- pr_path$V1[1]
pr_name <- pr_path$V1[2]
pr_input_dir <- file.path(pr_dir,"data")
pr_output_dir <- file.path(pr_dir,"output")
pr_code_dir <- file.path(pr_dir, "code")

set_options("input_dir",pr_input_dir)
set_options("output_dir",pr_output_dir)
set_options("code_dir",pr_code_dir)
set_options("project_name", pr_name)

### Create Folders

pr_results <- file.path(get_options("output_dir"),
                        "results")
pr_cache <- file.path(get_options("output_dir"),
                      "cache")

ifelse(!dir.exists(pr_results), dir.create(pr_results),
       "Folder exists already")

ifelse(!dir.exists(pr_cache),dir.create(pr_cache),
       "Folder exists already")