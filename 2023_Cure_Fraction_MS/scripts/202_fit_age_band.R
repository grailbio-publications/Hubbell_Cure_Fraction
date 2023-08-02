#transform data to an easily writeable form
local_collapsed_age<-total_collapsed_age %>%
  mutate(SEER_Draw=gsub(" ","_",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub("-","_",SEER_Draw))

scan_choice_df<-local_collapsed_age %>%
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

#temporary only regenerate changed values
#scan_choice_df<-scan_choice_df %>%
#  filter(SEER_Draw %in% c("Breast_HR_unknown","Lung_other"))


tStage=c("I","II","III","IV")

age_set<-local_collapsed_age %>% select(age_band) %>% distinct() %>% pull(age_band)

my_stan_model<-stan_model(file="scripts/mixture_cure_stage_constrained.stan")

#speed up a little
options(mc.cores=4)

#loop over all my choices
for (ii in 1:length(scan_choice_df$SEER_Draw)){
  
  cancer_choice<-scan_choice_df$SEER_Draw[ii]
  
  
  #loop over my age bands
  for (age_choice in age_set){
    print(paste(ii,cancer_choice,age_choice))
    
    #filter down to just this cancer across stages
    my_local_data<-local_collapsed_age %>% 
      filter(age_band==age_choice,SEER_Draw==cancer_choice,Stage!="Unknown_missing") %>% 
      select(ax=Index,alive_at_start=alive_start,died,lost,css_c,Stage,SEER_Draw) %>%
      arrange(Stage,ax) 
    
    #pass model in to avoid recompilation each time
    fit_staged_cure_fraction(my_stan_model, my_local_data,date_code,cancer_choice,age_choice)
    
  }
}