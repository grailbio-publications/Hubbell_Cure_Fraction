#figure 2: correlation with 5 year survival differences
#demonstrate use of surrogate differences

#get css at 5 years from data
#this is from the collapsed data
local_input_date_code<-date_code
raw_life_table<-read_tsv(sprintf("generated_data/%s_total_collapsed_life_table_data.tsv",local_input_date_code))

#get the basic survival measures
summary_survival<-raw_life_table %>%
  mutate(ax=Index) %>%
  group_by(SEER_Draw,Stage) %>%
  summarize(five_css=css_c[ax==60],
            ten_css=css_c[ax==120]) %>%
  ungroup() %>%
  filter(Stage!="Unknown/missing") %>%
  mutate(SEER_Draw=gsub(" ","_",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub("-","_",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub("/","_",SEER_Draw))

delta_survival_table<-main_fthr_summary_df %>%
  left_join(summary_survival) %>%
  group_by(SEER_Draw) %>%
  mutate(delta_cf=m_cf-m_cf[Stage=="IV"],
         delta_five=five_css-five_css[Stage=="IV"],
         delta_ten=ten_css-ten_css[Stage=="IV"]) %>%
  ungroup() %>%
  filter(Stage!="IV") %>%
  select(SEER_Draw,Stage,delta_cf,delta_five,delta_ten)

cor_fit<-delta_survival_table %>% 
  summarize(corr_five=cor(delta_five,delta_cf),corr_ten=cor(delta_ten,delta_cf))

#story 2: differences in 5 year survival represent differences in cure nicely
delta_survival_five_plot<-delta_survival_table %>%
  ggplot(aes(x=delta_five,y=delta_cf))+
  geom_point(aes(shape=Stage))+
  geom_abline(intercept=0,slope=1)+
  geom_label(aes(label=SEER_Draw,color=Stage))+
  coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
  labs(x="Improvement in 5-year survival from metastasis",
       y="Improvement in cure from metastasis")+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position=c(0.2,0.9),
    legend.title=element_blank())+
  ggtitle(sprintf("Differences in 5-year survival\ncorrelate(%s) with difference in LTS",round(cor_fit$corr_five[1],3)))

ggsave(sprintf("figs/%s_figure_two_plot.pdf",date_code),
       delta_survival_five_plot,
       width=11,height=11)

#story 2: differences in 5 year survival represent differences in cure nicely
delta_survival_five_repel_plot<-delta_survival_table %>%
  ggplot(aes(x=delta_five,y=delta_cf))+
  geom_point(aes(shape=Stage))+
  geom_abline(intercept=0,slope=1)+
  geom_label_repel(aes(label=SEER_Draw,color=Stage),seed=20160307,max.overlaps=Inf,force=2)+
  coord_cartesian(xlim=c(-0.1,1),ylim=c(-0.1,1))+
  labs(x="Improvement in 5-year survival from metastasis",
       y="Improvement in cure from metastasis")+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position=c(0.2,0.9),
    legend.title=element_blank())+
  ggtitle(sprintf("Differences in 5-year survival\ncorrelate(%s) with difference in LTS",round(cor_fit$corr_five[1],3)))

ggsave(sprintf("figs/%s_figure_two_repel_plot.pdf",date_code),
       delta_survival_five_repel_plot,
       width=14,height=14)

simple_survival_five_plot<-delta_survival_table %>%
  ggplot(aes(x=delta_five,y=delta_cf))+
  geom_point(aes(shape=Stage),size=3)+
  geom_abline(intercept=0,slope=1)+
  #geom_label(aes(label=SEER_Draw,color=Stage))+
  coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
  labs(x="Improvement in 5-year survival from metastasis",
       y="Improvement in long-term surviovrs from metastasis")+
  theme_bw()+
  theme(
    axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position=c(0.2,0.9),
    legend.title=element_blank())+
  ggtitle(sprintf("Differences in 5-year survival\ncorrelate(%s) with long-term survivors",round(cor_fit$corr_five[1],3)))

ggsave(sprintf("figs/%s_figure_two_simple_plot.pdf",date_code),
       simple_survival_five_plot,
       width=11,height=11)
