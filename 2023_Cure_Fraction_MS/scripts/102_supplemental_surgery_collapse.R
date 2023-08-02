surgery_table_collapsed_one<-surgery_table_data_two %>%
  filter(Lower<80) %>%
  group_by(SEER_Draw,Stage,Index) %>% 
  summarize(alive_start=sum(alive_start),
            died=sum(died),
            lost=sum(lost)) %>%
  ungroup() %>%
  arrange(SEER_Draw,Stage,Index) %>%
  group_by(SEER_Draw,Stage) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

write_tsv(surgery_table_collapsed_one,sprintf("generated_data/%s_surgery_collapsed_life_table_data.tsv",date_code))
