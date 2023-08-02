selected_cure_fraction_plot<-main_fthr_summary_df %>% 
  filter(SEER_Draw %in% c("Breast","Colon_Rectum","Esophagus",
                          "Gallbladder","Liver_Bile_duct","Lung",
                          "Ovary","Pancreas","Stomach")) %>%
  mutate(pct_df=round(100-100*exp(-m_df*12),1)) %>%
  ggplot(aes(x=Stage,y=100*m_cf))+
  geom_errorbar(aes(ymin=100*l_cf,ymax=100*u_cf))+
  geom_point(color="black")+
  facet_wrap(~SEER_Draw)+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=16),
    legend.position=c(0.2,0.9),
    legend.title=element_blank())+
  labs(x="Stage",y="Percent LTS")+
  coord_cartesian(y=c(0,100))+
  ggtitle("Selected Long Term Survivor Fraction by Stage SEER17(Nov 2022) 40-79")

ggsave(sprintf("figs/%s_figure_one.pdf",date_code),
       selected_cure_fraction_plot,
       width=12,height=12)