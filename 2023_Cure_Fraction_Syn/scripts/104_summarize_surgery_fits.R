#summarize fits

fthr_surgery_list<-system("ls generated_data/feather/*.feather",intern=T)

#get only the fits we need
#need both categories to contrast
fthr_surgery_list<-fthr_surgery_list[grepl("total",fthr_surgery_list) | grepl("surgery",fthr_surgery_list)]
#get only the most current version
fthr_surgery_list<-fthr_surgery_list[grepl(date_code,fthr_surgery_list)]


fthr_summary_surgery<-sapply(fthr_surgery_list,function(nz){
  coef_tmp_df<-read_feather(nz)
  coef_summary_df<-coef_tmp_df %>% 
    group_by(Stage) %>%
    summarize(m_a=median(a_weibull),
              sd_a=sd(a_weibull),
              m_b=median(b_weibull),
              sd_b=sd(b_weibull),
              m_cf=median(c_fraction),
              sd_cf=sd(c_fraction),
              l_cf=quantile(c_fraction,probs=0.025),
              u_cf=quantile(c_fraction,probs=0.975),
              m_df=median(d_decay),
              sd_df=sd(d_decay),
              l_df=quantile(d_decay,probs=0.025),
              u_df=quantile(d_decay,probs=0.975)) %>%
    ungroup()  %>%
    mutate(flag=grepl("surgery",nz),
           surgery=case_when(flag ~ "Surgery",
                             TRUE ~ "Total")) %>%
    select(-flag) %>%
    mutate(id=gsub("generated_data/feather/","",gsub(".feather","",nz,fixed=TRUE),fixed=TRUE)) %>%
    mutate(id=gsub("_total_coef","",id)) %>%
    mutate(id=gsub("_surgery_coef","",id)) %>%
    separate(id,into=c("date_code","SEER_Draw"),sep="_",extra="merge")
  coef_summary_df
},simplify=FALSE)

surgery_summary_df<-bind_rows(fthr_summary_surgery,.id="filenum")

surgery_cure_fraction<-surgery_summary_df %>%
  select(status=surgery,SEER_Draw,Stage,m_cf,l_cf,u_cf,m_df) %>%
  mutate(cure_fraction=sprintf("%s(%s-%s)",round(100*m_cf),round(100*l_cf),round(100*u_cf))) %>%
  mutate(excess_risk=round(exp(-12*m_df),3)) %>% #not sure the ci matters here, but can provide if we like
  select(status, SEER_Draw,Stage,cure_fraction,excess_risk) %>%
  pivot_wider(names_from=Stage,values_from=c("cure_fraction","excess_risk")) %>%
  arrange(SEER_Draw,status) #for easy comparison

write_tsv(surgery_cure_fraction,sprintf("reports/%s_text_surgery_cure_fraction.tsv",date_code))