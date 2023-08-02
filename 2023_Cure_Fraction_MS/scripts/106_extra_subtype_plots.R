#contrast some specific subtypes of cancer

extrasubtype_summary_df<-main_fthr_summary_df %>%
  filter(SEER_Draw %in% c("Cervix_adeno","Cervix_squamous",
                          "Esophagus_adeno","Esophagus_squamous",
                          "Hodgkin_lymphoma","Non_Hodgkin_lymphoma",
                          "Renal_parenchyma","Renal_pelvis"))

all_extrasubtype_cure_fraction_plot<-extrasubtype_summary_df %>% 
  mutate(pct_df=round(100-100*exp(-m_df*12),1)) %>%
  ggplot(aes(x=Stage,y=100*m_cf))+
  geom_errorbar(aes(ymin=100*l_cf,ymax=100*u_cf),position=position_dodge(0.5),width=0.3)+
  geom_point(aes(),position=position_dodge(0.5))+
  facet_wrap(~SEER_Draw,ncol=2)+
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

ggsave(sprintf("figs/%s_supp_extrasubtype_all_cure.pdf",date_code),
       all_extrasubtype_cure_fraction_plot,
       width=16,height=16)