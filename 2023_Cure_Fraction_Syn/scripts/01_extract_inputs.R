
#fancy extraction
if (original_flag){
  source("scripts/prep_inputs/a0_read_main_table.R")
  source("scripts/prep_inputs/a1_read_subtype_table.R")
  source("scripts/prep_inputs/a2_read_surgery_table.R")
}

if (synthetic_flag){
  #do nothing: input files were generated from synthetic data code
}