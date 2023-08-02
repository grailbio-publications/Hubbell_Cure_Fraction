#extract main data

main_fthr_list<-system("ls generated_data/feather/*.feather",intern=T)

#restrict to non-supplemental
main_fthr_list<-main_fthr_list[grepl("total",main_fthr_list)]
#restrict to current date code
main_fthr_list<-main_fthr_list[grepl(date_code,main_fthr_list)]

main_fthr_summary_list<-sapply(main_fthr_list,function(nz){
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
    mutate(id=gsub("generated_data/feather/","",gsub(".feather","",nz,fixed=TRUE),fixed=TRUE)) %>%
    mutate(id=gsub("_total_coef","",id)) %>%
    separate(id,into=c("date_code","SEER_Draw"),sep="_",extra="merge")
  coef_summary_df
},simplify=FALSE)

main_fthr_summary_df<-bind_rows(main_fthr_summary_list,.id="filenum")


#makes table one
cure_fraction<-main_fthr_summary_df %>%
  select(SEER_Draw,Stage,m_cf,l_cf,u_cf,m_df) %>%
  mutate(cure_fraction=sprintf("%s(%s-%s)",round(100*m_cf),round(100*l_cf),round(100*u_cf))) %>%
  mutate(excess_risk=round(exp(-12*m_df),3)) %>% #not sure the ci matters here, but can provide if we like
  select(SEER_Draw,Stage,cure_fraction,excess_risk) %>%
  pivot_wider(names_from=Stage,values_from=c("cure_fraction","excess_risk"))

write_tsv(cure_fraction,sprintf("reports/%s_text_cure_fraction.tsv",date_code))