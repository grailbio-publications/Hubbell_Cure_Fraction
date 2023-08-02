#put together my happy place

age_lts_fraction_plot<-fthr_summary_age_all_df %>%
  ggplot(aes(x=Stage,y=m_cf,group=status))+
  geom_point(aes(shape=status,color=status))+
  geom_line(aes(color=status),lty="dashed")+
  facet_wrap(~SEER_Draw)+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=16),
    legend.position="top",
    legend.title=element_blank())+
  labs(x="Stage",y="Proportion LTS")+
  coord_cartesian(y=c(0,1))+
  ggtitle("Long Term Survivor Fraction by Stage SEER17(Nov 2022)")

ggsave(sprintf("figs/%s_supp_figure_age_lts_fraction.pdf",date_code),
       age_lts_fraction_plot,
       width=16,height=16)

age_lts_recur_plot<-fthr_summary_age_all_df %>%
  ggplot(aes(x=Stage,y=1-exp(-m_df*12),group=status))+
  geom_point(aes(shape=status,color=status))+
  geom_line(aes(color=status),lty="dashed")+
  facet_wrap(~SEER_Draw)+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=16),
    legend.position="top",
    legend.title=element_blank())+
  labs(x="Stage",y="Proportion death per year in LTS")+
  #coord_cartesian(y=c(0,1))+
  ggtitle("Long Term Survivor Death by Stage SEER17(Nov 2022)")


ggsave(sprintf("figs/%s_supp_figure_age_lts_recur.pdf",date_code),
       age_lts_recur_plot,
       width=16,height=16)
