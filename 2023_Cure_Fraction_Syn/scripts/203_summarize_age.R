fthr_age_all<-system("ls generated_data/feather/*.feather",intern=T)

fthr_age_all<-fthr_age_all[grepl("-",fthr_age_all)]

#restrict to current date code
fthr_age_all<-fthr_age_all[grepl(date_code,fthr_age_all)]


fthr_summary_age_all<-sapply(fthr_age_all,function(nz){
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
    mutate(id=gsub("_coef","",id)) %>%
    separate(id,into=c("date_code","SEER_Draw"),sep="_",extra="merge")
  coef_summary_df
},simplify=FALSE)

fthr_summary_age_all_df<-bind_rows(fthr_summary_age_all,.id="filenum")

fthr_summary_age_all_df<-fthr_summary_age_all_df %>%
  mutate(status=str_extract(SEER_Draw, "(?<=_)[^_]*$")) %>%
  mutate(final_draw=str_sub(SEER_Draw,1,-(nchar(status)+2))) %>%
  mutate(SEER_Draw=final_draw)


#write a full data table
age_data_summary<-fthr_summary_age_all_df %>%
  select(status, SEER_Draw,Stage,
         coef_a_weibull=m_a,sd_a,
         coef_b_weibull=m_b,sd_b,
         coef_lts_fraction=m_cf,sd_lts=sd_cf,lb_lts=l_cf,ub_lts=u_cf,
         coef_recur_month=m_df,sd_recur=sd_df,lb_recur=l_df,ub_recur=u_df)

write_tsv(age_data_summary,sprintf("reports/%s_age_data_summary.tsv",date_code))

#makes human-readable table
age_cure_fraction<-fthr_summary_age_all_df %>%
  select(status,SEER_Draw,Stage,m_cf,l_cf,u_cf,m_df) %>%
  mutate(cure_fraction=sprintf("%s(%s-%s)",round(100*m_cf),round(100*l_cf),round(100*u_cf))) %>%
  mutate(excess_risk=round(exp(-12*m_df),3)) %>% #not sure the ci matters here, but can provide if we like
  select(status,SEER_Draw,Stage,cure_fraction,excess_risk) %>%
  pivot_wider(names_from=Stage,values_from=c("cure_fraction","excess_risk"))

write_tsv(age_cure_fraction,sprintf("reports/%s_age_cure_fraction.tsv",date_code))
