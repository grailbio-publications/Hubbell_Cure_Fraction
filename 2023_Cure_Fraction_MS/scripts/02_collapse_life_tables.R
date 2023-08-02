#collapse life tables for main analysis

#should read from generated_data

#looking at 40-80 here

life_table_collapsed_one<-life_table_data_two %>%
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

life_table_subtype_collapsed_one<-life_table_subtype_data_two %>%
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

#basic: across all cancer ages reasonable to screen for, how does it all go?
total_collapsed_one<-bind_rows(full=life_table_collapsed_one,subtype=life_table_subtype_collapsed_one)

write_tsv(total_collapsed_one,sprintf("generated_data/%s_total_collapsed_life_table_data.tsv",date_code))
