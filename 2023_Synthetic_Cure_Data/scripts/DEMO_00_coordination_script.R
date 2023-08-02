#demonstrate synthetic data generation on a single cancer type for speed
#starting from synthetic data to avoid privacy violations

library(tidyverse)
library(readxl)
library(flexsurv)

#ensure that it is obvious that this is demonstration data
date_code<-"20230727DEMO"
#original synthetic data comes from here
input_date_code<-"20230616"

#setwd("2023_Synthetic_Cure_Data")  #if you are not in the right directory

#we will be reading from generated synthetic data which is already in table form
#so no data prep is done

#need the routines

source("scripts/tree_structured_scheme.R")
source("scripts/generate_one_life_table.R")

#now generate data for a single demonstration cancer, i.e. gallbladder

source("scripts/DEMO_01_synthesize_demo_life_table.R")

#do not worry about surgery/subtypes as they use the same technique on different life tables

source("scripts/DEMO_02_plot_demo_data.R")