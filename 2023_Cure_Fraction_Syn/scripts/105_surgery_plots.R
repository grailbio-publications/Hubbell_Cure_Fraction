#supplemental surgery plots

selected_surgery_cure_fraction_plot<-surgery_summary_df %>% 
  filter(SEER_Draw %in% c("Breast","Colon_Rectum","Esophagus",
                          "Gallbladder","Liver_Bile_duct","Lung",
                          "Ovary","Pancreas","Stomach")) %>%
  mutate(pct_df=round(100-100*exp(-m_df*12),1)) %>%
  ggplot(aes(x=Stage,y=100*m_cf))+
  geom_errorbar(aes(ymin=100*l_cf,ymax=100*u_cf,color=surgery),position=position_dodge(0.5),width=0.3)+
  geom_point(aes(color=surgery),position=position_dodge(0.5))+
  facet_wrap(~SEER_Draw)+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position="top",
    legend.title=element_blank())+
  labs(x="Stage",y="Percent LTS")+
  coord_cartesian(y=c(0,100))+
  ggtitle("Selected Long Term Survivor Fraction by Stage SEER17(Nov 2022) 40-79")

ggsave(sprintf("figs/%s_supp_surgery_selected_cure.pdf",date_code),
       selected_surgery_cure_fraction_plot,
       width=12,height=12)

all_surgery_cure_fraction_plot<-surgery_summary_df %>% 
  mutate(pct_df=round(100-100*exp(-m_df*12),1)) %>%
  ggplot(aes(x=Stage,y=100*m_cf))+
  geom_errorbar(aes(ymin=100*l_cf,ymax=100*u_cf,color=surgery),position=position_dodge(0.5),width=0.3)+
  geom_point(aes(color=surgery),position=position_dodge(0.5))+
  facet_wrap(~SEER_Draw)+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position="top",
    legend.title=element_blank())+
  labs(x="Stage",y="Percent LTS")+
  coord_cartesian(y=c(0,100))+
  ggtitle("All Long Term Survivor Fraction by Stage SEER17(Nov 2022) 40-79")

ggsave(sprintf("figs/%s_supp_surgery_all_cure.pdf",date_code),
       all_surgery_cure_fraction_plot,
       width=16,height=16)

