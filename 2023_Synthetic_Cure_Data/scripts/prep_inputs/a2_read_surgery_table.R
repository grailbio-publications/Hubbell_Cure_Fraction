surgery_table_all<-read_excel(excel_data_name,
                              sheet="CSS by site-stage-sex-age w sgy",
                              #range="A1:I341580", #bug in cellranger for large rectangles
                              range=cell_limits(c(1,1),c(349186,9)), #note reversal
                              col_names=FALSE,col_types="text")

names(surgery_table_all)<-c("Index","time","alive_start","died","lost","css_i","css_c","se_i","se_c")

surgery_table_data<-surgery_table_all %>%
  mutate(Istar=case_when(grepl("/",Index)~Index,
                         TRUE ~ NA_character_)) %>%
  fill(Istar) %>%
  filter(grepl("mo",time) & !grepl("more",time) & !grepl("edian",time)) %>%
  type_convert() %>%
  mutate(css_i=parse_number(css_i),
         css_c=parse_number(css_c),
         se_i=parse_number(se_i),
         se_c=parse_number(se_c)) %>%
  select(Istar,Index,time,alive_start,died,lost,css_i,css_c,se_i,se_c) %>%
  mutate(Istar=gsub("n/R","n_R",Istar)) %>%  #fix Colon/Rectum
  mutate(Istar=gsub("r/B","r_B",Istar)) %>% #fix Liver/Bile-duct
  mutate(Istar=gsub("wn/mi","wn_mi",Istar)) %>%  #fix unknown/missing
  separate(Istar,into=c("SEER_Draw","Stage","Sex","Age"),sep="/",extra="merge") %>%
  mutate(SEER_Draw=gsub("n_R","n/R",SEER_Draw,fixed=TRUE)) %>%
  mutate(SEER_Draw=gsub("r_B","r/B",SEER_Draw,fixed=TRUE)) %>%
  mutate(Stage=gsub("wn_mi","wn/mi",Stage,fixed=TRUE))  #undo the substitutions make for parsing

surgery_table_data_two<-surgery_table_data %>%
  select(SEER_Draw,Stage,Sex,Age,Index,alive_start,died,lost) %>%
  complete(SEER_Draw,Stage,Sex,Age,Index,fill=list(alive_start=0,died=0,lost=0)) %>%
  mutate(Age=gsub(" years","",Age)) %>%
  separate(Age,sep="-",into=c("Lower","Upper")) %>%
  arrange(SEER_Draw,Stage,Sex,Lower,Upper,Index) %>%
  group_by(SEER_Draw,Stage,Sex,Lower,Upper) %>%
  mutate(css_i=1-died/pmax(1,alive_start)) %>%
  mutate(css_c=cumprod(css_i)) %>%  #generate KM estimate
  ungroup() 

write_tsv(surgery_table_data_two,sprintf("generated_data/%s_surgery_life_table_data.tsv",date_code))


