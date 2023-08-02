#synthetic data

#for demonstration
#we are reading in the synthetic data table
life_table_original<-read_tsv(sprintf("generated_data/%s_synthetic_life_table_data.tsv",input_date_code))

#get rid of known bad data cases
life_table_raw<-life_table_original %>%
  filter(!(Sex=="Male" & SEER_Draw=="Ovary")) %>%
  filter(!(SEER_Draw=="Plasma Cell Neoplasm" & !(Stage=="Unknown/missing"))) %>%
  filter(!(SEER_Draw=="Lymphoid Leukemia" & !(Stage=="Unknown/missing"))) %>%
  filter(!(SEER_Draw=="Brain")) %>%
  filter(!(SEER_Draw=="CUP")) %>% 
  filter(!(Stage=="Unknown/missing")) %>%  #not going to be used
  type_convert() %>%
  filter(SEER_Draw=="Gallbladder")  #single demonstration cancer to run through

#minimum of 25 deaths
life_table_schema<-generate_life_table_scheme(life_table_raw,25)


my_list<-list()
k<-1
set.seed(20230621)
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
#note that we are only using gallbladder to demonstrate
#and therefore all non-gallbladder entries will be zero instead of the synthetic data
#so we filter to gallbladder only for this demo
synthetic_two<-life_table_original %>%
  filter(SEER_Draw=="Gallbladder") %>% 
  select(SEER_Draw,Stage,Sex,Lower,Upper,Index) %>%
  left_join(synthetic_one) %>% 
  mutate(alive_start=replace_na(alive_start,replace=0)) %>%
  mutate(died=replace_na(died,replace=0)) %>%
  mutate(lost=replace_na(lost,replace=0)) %>%
  group_by(SEER_Draw,Stage,Sex,Lower,Upper) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

#we output here the demonstration life table containing only gallbladder
write_tsv(synthetic_two,sprintf("generated_data/%s_demo_life_table_data.tsv",date_code))


