library(tidyverse)
library(readxl)
library(ggrepel)
library(feather)
library(rstan)
library(patchwork)

date_code<-"20230727DEMO"  #mark demo data run
input_date_code<-"20230727DEMO" #this uses the demo data

#what data source am I using?
original_flag<-FALSE
synthetic_flag<-!original_flag

#speed up stan fits a little
options(mc.cores=4)
set.seed(20160307)

#setwd("2023_Cure_Fraction_Syn")


#general function to fit cure fraction to a single data set
source("R/fit_staged_cure_fraction.R")


#only using gallbladder
#only using demo data
#therefore no DEMO_01 script
#only doing main analysis
source("scripts/DEMO_02_collapse_life_tables.R")
source("scripts/DEMO_03_fit_main_collapsed.R")
source("scripts/DEMO_04_summarize_main_fits.R")
source("scripts/DEMO_05_figure_one.R")
#skip figure two as single-cancer correlation not interesting
source("scripts/DEMO_07_figure_three.R")

#everything else follows similarly
#surgery, age etc skipped for this demo, but use parallel code