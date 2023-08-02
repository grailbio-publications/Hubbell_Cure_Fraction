#plot comparison data for demonstration


#plot both data - synthetic original vs demo data for gallbladder

demo_css_plot<-bind_rows(original=life_table_original,demo=synthetic_two,.id="tmp") %>% 
  filter(Stage!="Unknown/missing",SEER_Draw=="Gallbladder") %>% 
  select(SEER_Draw,Stage,Sex,Lower,Index,css_c,tmp) %>%
  ggplot(aes(x=Index,y=css_c))+
  geom_point(aes(color=tmp,group=tmp)) +
  facet_wrap(facets=c("Sex","Stage","Lower"))+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10,hjust=0,face="bold"),
    axis.text = element_text(size=14),
    axis.text.y=element_text(size=18),
    legend.text = element_text(size=14),
    title=element_text(size=18),
    legend.position="top")+
  labs(x="Time",y="CSS",color="Data")+
  ggtitle("Survival curves for privacy-preserving data synthesis")

ggsave(sprintf("figs/%s_demo_survival_data.pdf",date_code),
       demo_css_plot,
       width=20,height=20)
