#fit surgery collapsed data

#set up

#transform data to an easily writeable form
local_collapsed_one<-surgery_table_collapsed_one %>%
  mutate(SEER_Draw=gsub(" ","_",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub("-","_",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub("/","_",SEER_Draw))

scan_choice_df<-local_collapsed_one %>%
  filter(Stage!="Unknown/missing") %>%
  select(SEER_Draw) %>%
  distinct() %>% 
  filter(SEER_Draw!="Brain",
         SEER_Draw!="CUP",
         SEER_Draw!="Lymphoid_Leukemia",
         SEER_Draw!="Myeloid_Neoplasm",
         SEER_Draw!="Plasma_Cell_Neoplasm",
         SEER_Draw!="[OTHER]") %>%
  arrange(SEER_Draw)

tStage=c("I","II","III","IV")

my_stan_model<-stan_model(file="scripts/mixture_cure_stage_constrained.stan")

for (ii in 1:length(scan_choice_df$SEER_Draw)){
  
  cancer_choice<-scan_choice_df$SEER_Draw[ii]
  
  print(paste(ii,cancer_choice))
  
  #filter down to just this cancer across stages
  my_local_data<-local_collapsed_one %>% 
    filter(SEER_Draw==cancer_choice,Stage!="Unknown/missing") %>% 
    select(ax=Index,alive_at_start=alive_start,died,lost,css_c,Stage,SEER_Draw) %>%
    arrange(Stage,ax) 
  
  #pass model in to avoid recompilation each time
  fit_staged_cure_fraction(my_stan_model, my_local_data,date_code,cancer_choice,"surgery")
  
}