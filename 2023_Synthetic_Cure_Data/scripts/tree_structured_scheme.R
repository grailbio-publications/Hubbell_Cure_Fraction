
#collapses age-specific blocks until we have enough events to usefully fit most of the time
generate_life_table_scheme<-function(life_table_raw, died_min){
  
  #things that can be usefully fit at all
  start_stat<-life_table_raw %>% 
    group_by(SEER_Draw,Stage,Sex,Lower) %>%
    summarize(died=sum(died),
              lost=sum(lost)) %>%
    ungroup() %>%
    filter(died>0 | lost>0)
  
  stat_loop=start_stat
  
  #no need to check, must terminate in small number of iterations so a few extra won't matter
  for (ii in 1:20){
    
    stat_loop<-stat_loop %>% 
      group_by(SEER_Draw,Stage,Sex) %>%
      mutate(target=pmin(died,lost)) %>%
      mutate(need_update=(died_min-target)*(died_min>target)) %>%
      mutate(happy_update=need_update+lag(need_update,default=-1)) %>%
      mutate(gp=1:length(happy_update)) %>%
      mutate(update_me=(max(happy_update)==happy_update) & happy_update>0) %>%
      mutate(check_unique=cumsum(update_me)) %>%
      mutate(update_me=update_me*(check_unique<2)) %>%
      mutate(x_gp=case_when(update_me==TRUE ~ lag(gp),
                            TRUE ~ gp)) %>% 
      ungroup() %>%
      group_by(SEER_Draw,Stage,Sex,x_gp) %>%
      summarize(fused=length(died),
                Lower=paste(Lower,collapse=","),
                died=sum(died),
                lost=sum(lost)) %>%
      ungroup() %>%
      select(SEER_Draw,Stage,Sex,Lower,died,lost,fused)
  }
  
  stat_loop
}