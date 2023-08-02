
generate_one_life_table<-function(local_data_pull){

local_data_ratio<-local_data_pull %>%
  group_by(Lower) %>%
  summarize(a=max(alive_start)) %>%
  ungroup() %>%
  mutate(ra=pmax(a,5)) %>%  
  mutate(ratio=ra/sum(ra)) %>%
  mutate(cratio=cumsum(ratio))

local_data_tmp<-local_data_pull %>%
  group_by(Index) %>%
  summarize(alive_start=sum(alive_start),
            died=sum(died),
            lost=sum(lost)) %>%
  ungroup()

life_rep_tmp<-local_data_tmp %>% 
  select(Index,died,lost) %>% 
  pivot_longer(-Index) %>%
  uncount(value,.id="tmp") %>%
  mutate(flag=1*(name=="died")) %>%
  mutate(time=Index-runif(length(flag))) %>%
  select(time,flag) %>%
  arrange(time)

life_rep_org<-life_rep_tmp

#get scale of thiings to generate
n=length(life_rep_tmp$time)
nret<-rpois(1,lambda=max(15,n))+5

#reduce life-rep-tmp if very large
fit_max<-5000
if (length(life_rep_tmp$time)>fit_max){
  #downsample to 5000
  life_rep_tmp<-life_rep_tmp %>% 
    slice_sample(n=fit_max)
}

#now compute

max_knots<-12
index_max<-max(local_data_pull$Index)

o_death<-sum(local_data_tmp$died)
o_lost<-sum(local_data_tmp$lost)
ktop<-min(max(floor(sqrt(o_death+4))-1,0),max_knots)
ltop=min(max(floor(sqrt(o_lost+4))-1,1),max_knots)
kl=log(quantile(life_rep_tmp$time[life_rep_tmp$flag==0],probs=(1:ltop)/(ltop+1))) #linear scale but between knots

fake_table<-NULL #make sure null in case of error
w_count<-0
while(is.null(fake_table) & w_count<6){
  #tryCatch is there because the fit to survival curves sometimes fails
  #even although we assure a minimum number of events
  tryCatch({
    #stabilize?
    
    tfit<-flexsurvspline(Surv(time,flag)~1,life_rep_tmp,k=ktop)
    #know the special structure of this table has yearly cadence with a change at 5-6 years
    tmax<-sort(life_rep_tmp$time,decreasing=TRUE)[2] #allow one point to stabilize fit
    lfit<-flexsurvspline(Surv(time,1-flag)~1, life_rep_tmp,knots=kl)
    

    
    xdata<-simulate(tfit,newdata=tibble(index=1:nret), tidy=TRUE,censtime=index_max, seed=20160307)
    #generate lost with its own hazard function
    ydata<-simulate(lfit,newdata=tibble(index=1:nret),tidy=TRUE,censtime=index_max,seed=20160930) #make sure different seeds!
    
    #fix my event table
    fake_events<-tibble(time=xdata$time,event=xdata$event,ctime=ydata$time) %>%
      mutate(xevent=event*(ctime>time)) %>% #censorship
      mutate(xtime=pmin(time,ctime)) %>%
      select(time=xtime,event=xevent)
    
    
    #need to downsample back to appropriate ratios for the subcategories
    #so can fill out table
    fake_events_stratified<-fake_events %>% 
      mutate(z=runif(length(time))) %>% 
      mutate(Lower=cut(z,c(0,local_data_ratio$cratio),labels=local_data_ratio$Lower)) %>%
      mutate(Lower=as.numeric(as.character(Lower))) #undo the factor from cut
    
    collapse_censored<-fake_events_stratified %>%
      mutate(etop=ceiling(time)) %>%
      group_by(Lower,etop,event) %>%
      summarize(total=length(time)) %>%
      ungroup()
    
    #make into a complete life table
    #with entries for every step
    fake_table<-collapse_censored %>% 
      mutate(status=case_when(event==0 ~ "lost",
                              TRUE ~ "died")) %>%
      select(Lower,etop,status,total) %>%
      complete(Lower=local_data_ratio$Lower,etop=1:index_max,status=c("died","lost"),fill=list(total=0)) %>%
      pivot_wider(names_from=status,values_from=total) %>%
      mutate(tt=lost+died) %>%
      group_by(Lower) %>%
      mutate(alive_start=sum(tt)-cumsum(tt)+tt) %>%
      ungroup() %>%
      select(Lower,Index=etop,alive_start=alive_start,died,lost) %>%
      left_join(tmp) %>%
      select(SEER_Draw,Stage,Sex,Lower,Index,alive_start,died,lost)
    #really, don't do anything
    
  },
  error=function(e){
    #don't do anything if error out
    fake_table<<-NULL
    print(paste("Fail to generate: reducing complexity of fit ",tmp$SEER_Draw,tmp$Stage,tmp$Sex,tmp$Lower,collapse=","))
  },
  warning=function(w){
    fake_table<<-NULL
    print(paste("Warn in generate: reducing complexity of fit ",tmp$SEER_Draw,tmp$Stage,tmp$Sex,tmp$Lower,collapse=","))
  })
  if (is.null(fake_table)){
    #reduce knot complexity, see if it fits
    ktop=max(floor(ktop*0.5),0)
    ltop=max(floor(ltop*0.5),1)
    #because using explicit knots, ltop always at least 1
    kl=log(quantile(life_rep_tmp$time[life_rep_tmp$flag==0],probs=(1:ltop)/(ltop+1))) #linear scale but between knots
  }
  w_count<-w_count+1 #make sure the loop is finite
}
print(paste("Generated ",w_count, tmp$SEER_Draw,tmp$Stage,tmp$Sex,tmp$Lower,collapse=","))

#the output is the fake table
fake_table
}