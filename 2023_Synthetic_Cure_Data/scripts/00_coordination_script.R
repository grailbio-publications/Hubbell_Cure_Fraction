#generate synthetic life table data for cure fraction analysis
#the idea is to ensure privacy by using synthetic data to demonstrate code
#where the synthetic data captures the real data well
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-022-01654-1 for reference


library(tidyverse)
library(readxl)
library(flexsurv)

date_code<-"20230616"


source("scripts/prep_inputs/a0_read_main_table.R")
source("scripts/prep_inputs/a1_read_subtype_table.R")
source("scripts/prep_inputs/a2_read_surgery_table.R")

source("scripts/tree_structured_scheme.R")
source("scripts/generate_one_life_table.R")

#generate synthetic version of main life table
source("scripts/01_synthesize_main_life_table.R")
source("scripts/02_synthesize_subtype_life_table.R")
source("scripts/03_synthesize_surgery_life_table.R")