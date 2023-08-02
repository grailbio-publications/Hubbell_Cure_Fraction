library(tidyverse)
library(readxl)
library(ggrepel)
library(feather)
library(rstan)
library(patchwork)

date_code<-"20230621SYN"  #mark synthetic data run
input_date_code<-"20230616"

#what data source am I using?
original_flag<-FALSE
synthetic_flag<-!original_flag

#speed up stan fits a little
options(mc.cores=4)
set.seed(20160307)


#general function to fit cure fraction to a single data set
source("R/fit_staged_cure_fraction.R")

#run once to extract and clean data from excel file
source("scripts/01_extract_inputs.R")

#first analysis for main paper
#basic cancer types, all ages
source("scripts/02_collapse_life_tables.R")
source("scripts/03_fit_main_collapsed.R")
source("scripts/04_summarize_main_fits.R")
source("scripts/05_figure_one.R")
source("scripts/06_figure_two.R")
source("scripts/07_figure_three.R")

#supplemental figures and analyses
source("scripts/101_supplemental_figure_one.R")
source("scripts/102_supplemental_surgery_collapse.R")
source("scripts/103_fit_surgery_collapsed.R")
source("scripts/104_summarize_surgery_fits.R")
source("scripts/105_surgery_plots.R")
source("scripts/106_extra_subtype_plots.R")

#age bands
source("scripts/201_collapse_age_band.R")
source("scripts/202_fit_age_band.R")
source("scripts/203_summarize_age.R")
source("scripts/204_plot_age_summary.R")