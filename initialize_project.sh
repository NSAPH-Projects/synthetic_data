#!/usr/local/bin/bash

# Author: Naeem Khoshnevis
# Last update: Dec 10, 2021

# Note:
#       shebang for linux system  #!/bin/bash
#       shebang for macOS system  #!/usr/local/bin/bash
#
#  You may need to install bash on new macOS (brew install bash)
#  This script requires bash version 4.0 or higher.

# Read Parameters

declare -A v=( )

filename="project_path_info.md"
 
while read line
do
    # $line variable contains current line read from the file
    # display $line text on the screen or do something with it.
    
    key=`echo $line | cut -d "=" -f1`
    val=`echo $line | cut -d "=" -f2`
 
    if [ -z "$key" ]
    then
        echo $key
    else
        v[$key]=$val
    fi
    
done < $filename


echo "         *** Warning ***           "
echo " "
echo "This script should be run once per project per system."
echo "It is important to understand its purpose. Although running this script will update all linkers, It is important to make sure that you know which project you are wokring on. Please enter the project name and hit enter button: "
read pr_name

echo ${v[PROJECT_NAME]}
echo $pr_name

if [ `echo ${v[PROJECT_NAME]}` = `echo $pr_name` ]; then
    echo "project name is correct, updating linkers ... "
else
    echo "project name is not correct."
    exit 1
fi;


if [ -d data ]
then
     echo "data folder exists, removing old symbolic links (if any) ... "
     rm data/public
     rm data/private
     echo "Done with removing old links for data folder."
else
     echo "Creating data folder ... "
     mkdir data
fi

if [ -d output ]
then
     echo "output folder exists, removing old symbolic links (if any) ... "
     rm output
     echo "Done with removing old links for output folder."
fi


ln -s `echo ${v[PUBLIC_DATA_DIR]}` data/public
ln -s `echo ${v[PRIVATE_DATA_DIR]}` data/private
ln -s `echo ${v[OUTPUT_DATA_DIR]}` output

echo $(pwd) > pr_hard_path_name.txt
echo ${v[PROJECT_NAME]} >> pr_hard_path_name.txt

echo "Done!"
