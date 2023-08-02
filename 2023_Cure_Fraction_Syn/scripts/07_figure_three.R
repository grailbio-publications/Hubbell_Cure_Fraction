#figure 3
#inset example gallbladder

#make an illustrative example of mixture model


cancer_choice<-"Gallbladder"
my_local_data<-raw_life_table %>% 
  filter(SEER_Draw==cancer_choice,Stage!="Unknown/missing") %>% 
  select(ax=Index,alive_at_start=alive_start,died,lost,css_c,Stage,SEER_Draw)

local_coef_all_data<-main_fthr_summary_df %>%
  filter(SEER_Draw==cancer_choice)

#just use the summary value for illustration
all_outcomes<-my_local_data %>% 
  select(SEER_Draw,Stage,ax,css_c) %>%
  left_join(local_coef_all_data) %>%
  mutate(not_cured=exp(-exp(log(ax)*m_a+m_b)),
         cured=exp(-m_df*ax),
         survival=not_cured*(1-m_cf)+m_cf*cured,
         ext_ax=2.5*ax,
         ext_not_cured=exp(-exp(log(ext_ax)*m_a+m_b)),
         ext_cured=exp(-m_df*ext_ax),
         ext_survival=ext_not_cured*(1-m_cf)+m_cf*ext_cured,
         SEER_Draw=cancer_choice)

minimal_outcomes<-all_outcomes %>%
  select(SEER_Draw,Stage,ax,css_c,not_cured,cured,survival,m_cf) 

minimal_ref<-minimal_outcomes %>% 
  select(SEER_Draw,Stage) %>%
  distinct() %>%
  mutate(ax=0,css_c=1.0,not_cured=1.0,cured=1.0,survival=1.0,m_cf=NA)

minimal_outcomes<-bind_rows(minimal_outcomes,minimal_ref) %>%
  arrange(SEER_Draw,Stage,ax) %>%
  mutate(years=ax/12) %>%
  group_by(SEER_Draw,Stage) %>%
  fill(m_cf,.direction="up") %>%
  ungroup()

#illustrate fit
stage_i_outcomes<-minimal_outcomes %>% filter(Stage=="I")

main_stage_i_plot<-stage_i_outcomes %>%
  mutate(almost_cured=m_cf*cured) %>%
  ggplot(aes(x=years))+
  geom_point(aes(y=css_c),data=stage_i_outcomes %>% filter(ax%%3==0))+
  geom_line(aes(y=survival),color="blue")+
  geom_line(aes(y=almost_cured),color="blue",lty="dashed")+
  coord_cartesian(ylim=c(0,1),xlim=c(0,14))+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    title=element_text(size=18),
    plot.margin=margin(25,25,25,25))+
  labs(y="Survival/all",x="Years since diagnosis")+
  ggtitle(sprintf("Stage I %s",cancer_choice))

m_cf<-stage_i_outcomes %>% filter(ax==0) %>% pull(m_cf)

not_cured_stage_i_plot<-stage_i_outcomes %>%
  mutate(almost_cured=m_cf*cured) %>%
  ggplot(aes(x=years))+
  #geom_point(aes(y=css_c),data=stage_i_outcomes %>% filter(ax%%3==0))+
  geom_line(aes(y=not_cured),color="blue")+
  #geom_line(aes(y=almost_cured,color="fitted"),lty="dashed")+
  coord_cartesian(ylim=c(0,1),xlim=c(0,14))+
  theme_bw()+
  theme(
    axis.text = element_text(size=12),
    title=element_text(size=14),
    plot.margin=margin(15,15,5,5),
    plot.background=element_rect(color="black",fill="white"))+
  labs(y="Survival",x="Years since diagnosis")+
  ggtitle(sprintf("Not Cured (%s%%)",100-round(100*m_cf)))

cured_stage_i_plot<-stage_i_outcomes %>%
  mutate(almost_cured=m_cf*cured) %>%
  ggplot(aes(x=years))+
  #geom_point(aes(y=css_c),data=stage_i_outcomes %>% filter(ax%%3==0))+
  geom_line(aes(y=cured),color="blue")+
  #geom_line(aes(y=almost_cured,color="fitted"),lty="dashed")+
  coord_cartesian(ylim=c(0,1),xlim=c(0,14))+
  theme_bw()+
  theme(
    axis.text = element_text(size=12),
    title=element_text(size=14),
    plot.margin=margin(15,15,5,5),
    plot.background=element_rect(color="black",fill="white"))+
  labs(y="Survival",x="Years since diagnosis")+
  ggtitle(sprintf("Cured (%s%%)",round(100*m_cf)))

mixture_illustration_plot<-main_stage_i_plot+
  inset_element(not_cured_stage_i_plot,left=0.6,bottom=0.6,right=1,top=1)+
  inset_element(cured_stage_i_plot,left=0.6,bottom=0,right=1,top=0.4)

ggsave(sprintf("figs/%s_%s_figure_three_mixture_illustration_plot.pdf",date_code,cancer_choice),
       mixture_illustration_plot,
       width=11,height=11)

#how about a richer plot with ribbons?

#need to read in the full gallbladder sample file
#fthr_list should contain only total aggregate files at this point
my_target_file<-grep(cancer_choice,main_fthr_list,value=TRUE)

coef_tmp_df<-read_feather(my_target_file)

all_stage_outcomes<-my_local_data %>% 
  select(ax,css_c,Stage) %>%
  left_join(coef_tmp_df,multiple="all") %>%
  mutate(not_cured=exp(-exp(log(ax)*a_weibull+b_weibull)),
         cured=exp(-d_decay*ax),
         survival=not_cured*(1-c_fraction)+c_fraction*cured,
         SEER_Draw=cancer_choice)



all_stage_focus<-all_stage_outcomes %>% 
  group_by(Stage,ax) %>%
  summarize(#smax=max(survival),
    #smin=min(survival),
    smed=median(survival),
    stop=quantile(survival,probs=0.975),
    sbot=quantile(survival,probs=0.025),
    almost_cure=median(c_fraction*cured),
    css=median(css_c)) %>%
  ungroup() %>%
  mutate(year=ax/12)

minimal_all_ref<-all_stage_focus %>% 
  select(Stage) %>%
  distinct() %>%
  mutate(ax=0,smed=1.0,stop=1.0,sbot=1.0,almost_cure=NA,css=1.0,year=0)

all_stage_focus<-bind_rows(all_stage_focus,minimal_all_ref) %>%
  arrange(Stage,ax) %>%
  group_by(Stage) %>%
  fill(almost_cure,.direction="up") %>%
  ungroup()


plot_with_cure<-all_stage_focus %>%
  ggplot(aes(x=year))+
  #geom_ribbon(aes(ymin=smin,ymax=smax),fill="grey70")+
  geom_ribbon(aes(ymin=sbot,ymax=stop),fill="lightblue")+
  geom_line(aes(y=smed,color=Stage),lwd=1,color="blue")+
  geom_point(aes(y=css),data=all_stage_focus %>% filter(ax%%6==0))+
  geom_line(aes(y=almost_cure),color="blue", lty="dashed")+
  facet_wrap(~Stage)+
  coord_cartesian(ylim=c(0,1),xlim=c(0,14))+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    strip.text = element_text(size=14),
    title=element_text(size=18))+
  labs(y="Survival",x="Years since diagnosis")+
  ggtitle(cancer_choice)

ggsave(sprintf("figs/%s_%s_figure_three_multiple_stage_plot.pdf",date_code,cancer_choice),
       plot_with_cure,
       width=11,height=11)


