#note recycling same variable names
#shouldn't have any worries
life_table_original<-read_tsv(sprintf("generated_data/%s_surgery_life_table_data.tsv",date_code))

#get rid of known bad data cases
life_table_raw<-life_table_original %>%
  filter(!(Sex=="Male" & SEER_Draw=="Ovary")) %>%
  filter(!(SEER_Draw=="Plasma Cell Neoplasm" & !(Stage=="Unknown/missing"))) %>%
  filter(!(SEER_Draw=="Lymphoid Leukemia" & !(Stage=="Unknown/missing"))) %>%
  filter(!(SEER_Draw=="Brain")) %>%
  filter(!(SEER_Draw=="CUP")) %>% 
  filter(!(Stage=="Unknown/missing")) %>%  #not going to be used
  type_convert() 

#minimum of 25 deaths
life_table_schema<-generate_life_table_scheme(life_table_raw,25)


my_list<-list()
k<-1
set.seed(20230623)
nblocks<-length(life_table_schema$SEER_Draw)

#now try looping over blocks
for (ival in 1:nblocks){
  tmp<-life_table_schema[ival,]
  tmp<-tmp %>% 
    separate_longer_delim(Lower,delim=",") %>%
    select(SEER_Draw,Stage,Sex,Lower) %>%
    type_convert()
  
  print(paste(ival,nblocks, tmp$SEER_Draw,tmp$Stage,tmp$Sex,tmp$Lower,collapse=","))
  
  local_data_pull<-life_table_raw %>%
    semi_join(tmp)
  
  fake_table<-generate_one_life_table(local_data_pull)
  
  my_list[[k]]<-fake_table
  k<-k+1
}

#then can make synthetic data

synthetic_one<-bind_rows(my_list)

#now this is synthetic data of identical size to the original life table in the file
#but now all entries are fake
synthetic_two<-life_table_original %>%
  select(SEER_Draw,Stage,Sex,Lower,Upper,Index) %>%
  left_join(synthetic_one) %>% 
  mutate(alive_start=replace_na(alive_start,replace=0)) %>%
  mutate(died=replace_na(died,replace=0)) %>%
  mutate(lost=replace_na(lost,replace=0)) %>%
  group_by(SEER_Draw,Stage,Sex,Lower,Upper) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

#the replacement for "basic" is synthetic
write_tsv(synthetic_two,sprintf("generated_data/%s_synthetic_surgery_life_table_data.tsv",date_code))
