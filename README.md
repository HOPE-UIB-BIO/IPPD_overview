# Indo-Pacific overview
Developed by HOPE team Ondrej Mottl, Suzette Flantua, Vivian Felde, Kuber Bhatta

# General info
It is a recommended structure for R projects which consists of modular codes with individual functions and purposes. 
The **Config file** is the single file where all variables, packages, criteria are defined. Operations are singled out in folders and R-codes.

# Default Main structure 
## R
* ´___Init_project___.R´
* ´00_Config_file´
* ´00_Master´
* ´R/01_Data_processing´: Folder for data sourcing, cleaning, filtering, organizing
* ´R/02_Supplementary_analyses´: Folder for data exploratory analyses, code to create temporal outputs
* ´R/03_Main_analyses´: Folder where main analyses of project/paper are done
* ´R/Functions´: Each single R code represents an R function. See HOPE convention for suggestions for function descriptions

## Outputs
- ### Data 
- ### Figures
- ### Tables

## Data
- ### Input
- ### Processed

# HOPE code convention
Based on reviews of multiple sources of code conventions, HOPE designed a code convention where different recommendations are merged:
https://docs.google.com/document/d/1MFYi-VLiBAMsFvaTd5Z9qkrXV8TxFzhXnzlkc1VjJ3g/edit?usp=sharing
Examples are:
* Each R code (with exception of the functions) comes with the same header with a short note on the aims of the specific .R
* Use subheaders throughout the code
* Comment consistently with good descriptions

# Setup
* ´___Init_project___.R´ set up project on each machine. **EACH USER SHOULD RUN THIS SCRIPT FIRST**
* Config file is the master file in terms of setting all criteria used throughout the repo, loading the required packages and saving all settings throughout the repo.
* in order to make any change to code, we recoment making a new branch, and merge it
