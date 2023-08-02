#these are already loaded if run in order

life_table_collapsed_age<-life_table_data_two %>%
  mutate(age_band=case_when(Upper<55 ~ "40-54",
                            Upper<65 ~ "55-64",
                            Upper<75 ~ "65-74",
                            Upper<85~ "75-84")) %>%
  group_by(age_band,SEER_Draw,Stage,Index) %>% 
  summarize(alive_start=sum(alive_start),
            died=sum(died),
            lost=sum(lost)) %>%
  ungroup() %>%
  arrange(age_band,SEER_Draw,Stage,Index) %>%
  group_by(age_band,SEER_Draw,Stage) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

life_table_subtype_collapsed_age<-life_table_subtype_data_two %>%
  mutate(age_band=case_when(Upper<55 ~ "40-54",
                            Upper<65 ~ "55-64",
                            Upper<75 ~ "65-74",
                            Upper<85~ "75-84")) %>%
  group_by(age_band,SEER_Draw,Stage,Index) %>% 
  summarize(alive_start=sum(alive_start),
            died=sum(died),
            lost=sum(lost)) %>%
  ungroup() %>%
  arrange(age_band,SEER_Draw,Stage,Index) %>%
  group_by(age_band,SEER_Draw,Stage) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

#basic: across all cancer ages reasonable to screen for, how does it all go?
total_collapsed_age<-bind_rows(full=life_table_collapsed_age,subtype=life_table_subtype_collapsed_age)

write_tsv(total_collapsed_age,sprintf("generated_data/%s_age_collapsed_life_table_data.tsv",date_code))