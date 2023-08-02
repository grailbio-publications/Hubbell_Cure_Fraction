
initial_guess<-function(my_local_data,upper_decay_limit){
  #guess an approximate fit to get near the data
  #while respecting constraints
  #use linearized weibull formula on log-log scale
  chain_guess<-my_local_data %>% 
    group_by(Stage) %>% 
    summarize(css=min(css_c[css_c>0]),mm=max(ax)) %>% #must be non-zero
    ungroup() %>%
    mutate(sc=sort(css,decreasing=TRUE)*c(0.99,0.98,0.97,0.96)) %>%  #tiebreaker
    mutate(cf=0.8*sc, 
           y_x=(sc-cf)/(1-cf),
           y_weibull=exp(log(y_x)-lag(log(y_x),default=0)),
           a_weibull=1.0,
           #d_decay=0.005*(1-cf),
           xc_fraction=exp(log(cf)-lag(log(cf),default=0)),
           xd_decay=rev(xc_fraction)*0.5) %>%
    mutate(c_fraction=cumprod(xc_fraction),
           b_weibull=log(-log(y_x))-a_weibull*log(mm),
           d_decay=upper_decay_limit[1]*rev(cumprod(xd_decay)))
  
  chain_guess
}

output_cure_diagnostics<-function(date_code,cancer_choice,keyword,chain_guess,x_df,y_df){
  pdf(sprintf("reports/pdf/%s_%s_%s_pix.pdf",date_code,cancer_choice,keyword),
      width=9,height=9)
  
  all_surv_plot<-y_df %>%
    mutate(survival=pred_css) %>%
    group_by(Stage,ax) %>%
    summarize(smax=max(survival),
              smin=min(survival),
              smed=median(survival),
              stop=quantile(survival,probs=0.95),
              sbot=quantile(survival,probs=0.05),
              css=median(css_c),
              pred_cf=median(pred_c*c_fraction)) %>%
    ungroup() %>%
    mutate(year=ax/12) %>%
    ggplot(aes(x=year))+
    geom_ribbon(aes(ymin=smin,ymax=smax),fill="grey70")+
    geom_ribbon(aes(ymin=sbot,ymax=stop),fill="grey40")+
    geom_line(aes(y=smed,color="predict"),lwd=1)+
    geom_point(aes(y=css,color="raw"),alpha=0.3)+
    geom_line(aes(y=pred_cf),lty="dashed")+
    coord_cartesian(ylim=c(0,1))+
    facet_wrap(~Stage)+
    labs(y="Survival/all",x="Years since diagnosis")+
    ggtitle(paste(cancer_choice))
  
  #not cured
  nc_surv_plot<-y_df %>%
    mutate(survival=pred_nc) %>%
    group_by(Stage,ax) %>%
    summarize(smax=max(survival),
              smin=min(survival),
              smed=median(survival),
              stop=quantile(survival,probs=0.95),
              sbot=quantile(survival,probs=0.05),
              css=median((css_c-pred_c*c_fraction)/(1-c_fraction))) %>%
    ungroup() %>%
    mutate(year=ax/12) %>%
    ggplot(aes(x=year))+
    geom_ribbon(aes(ymin=smin,ymax=smax),fill="grey70")+
    geom_ribbon(aes(ymin=sbot,ymax=stop),fill="grey40")+
    geom_line(aes(y=smed,color="predict"),lwd=1)+
    geom_point(aes(y=css,color="raw"),alpha=0.3)+
    coord_cartesian(ylim=c(0,1))+
    facet_wrap(~Stage)+
    labs(y="Survival/nc",x="Years since diagnosis")+
    ggtitle(paste(cancer_choice))
  
  #fix limits using geom_blank trick
  
  blank_limits_coef_df<-tribble(~name,~ymin,~ymax,
                                "a_weibull",0.5,2,
                                "b_weibull",-9,2,
                                "c_fraction",0,1,
                                "d_decay",0,0.04) %>%
    cross_join(tibble(Stage=tStage))
  
  basic_coef_plot<-x_df %>% 
    mutate(d_decay=d_decay*12) %>% #put in years
    pivot_longer(cols=c("a_weibull","b_weibull","c_fraction","d_decay")) %>% 
    ggplot(aes(x=Stage,y=value))+
    geom_boxplot()+
    facet_wrap(~name,scale="free_y")+
    geom_blank(aes(y=ymin),data=blank_limits_coef_df)+
    geom_blank(aes(y=ymax),data=blank_limits_coef_df)+
    ggtitle(paste(cancer_choice,"Coefficient ranges"))
  
  #predict from the guess
  chain_df<-chain_guess %>% 
    left_join(my_local_data %>% select(Stage,ax,css_c),multiple="all") %>%
    mutate(pred_nc=exp(-exp(a_weibull*log(ax)+b_weibull)),
           pred_c=exp(-d_decay*ax),
           pred_css=c_fraction*pred_c+(1-c_fraction)*pred_nc) 
  
  #show me my initial guess to I can see how crazy it is
  first_guess_plot<-chain_df %>% 
    ggplot(aes(x=ax/12,group=Stage))+
    geom_line(aes(y=css_c,color="raw"))+
    geom_line(aes(y=pred_css,color="pred"))+
    facet_wrap(~Stage)+
    coord_cartesian(y=c(0,1))+
    labs(y="survival",x="years")+
    ggtitle(paste(cancer_choice,"initial guess"))
   
  
  
  
  print(all_surv_plot)
  print(nc_surv_plot)
  print(basic_coef_plot)
  print(first_guess_plot)
  
  dev.off()
  
}




#function: fit cure fraction model to data frame of four stages
#output diagnostics

#keyword ensures that items are distinct when saved
#even if they have the same cancer type
fit_staged_cure_fraction<-function(my_stan_model, my_local_data,date_code,cancer_choice,keyword){
  #set table for stan
  #code currently assumes order
  local_data_for_stan<-list(N=length(my_local_data$ax),
                            Q=length(my_local_data$ax)/4,  #supply length of months per stage to avoid stan warning
                            J=4,
                            upper_decay=0.04/12, # upper limit for decay per month
                            stage=match(my_local_data$Stage,tStage),
                            ax=my_local_data$ax,
                            alive=my_local_data$alive_at_start,
                            died=my_local_data$died,
                            lost=my_local_data$lost,
                            css=my_local_data$css_c)
  

  chain_guess<-initial_guess(my_local_data,local_data_for_stan$upper_decay)
  
  #initialize across all chains
  #trust in wandering around the space
  init_chain=list(a_weibull=chain_guess$a_weibull,
                  y_weibull=chain_guess$y_weibull,
                  xc_fraction=chain_guess$xc_fraction,
                  xd_decay=chain_guess$xd_decay)
  
  per_chain=list()
  for (i in 1:4){per_chain[[i]]=init_chain}
  
  #fit using log-likelihood of life tables given a set of parameters
  #parameters generated from transformed underlying values
  #to enforce constraints on the natural scale
  #fit_four<-stan(file="scripts/mixture_cure_stage_constrained.stan",
  fit_four<-rstan::sampling(object=my_stan_model,
                 init=per_chain,
                 seed=20160307,  #fix random seed for reproducible stan results
                 data=local_data_for_stan)
  
  #extract variables
  x_list<-sapply(c("a_weibull","b_weibull","c_fraction","d_decay"),function(nx){
    tmp<-rstan::extract(fit_four,pars=nx)
    colnames(tmp[[1]])<-tStage
    as_tibble(tmp[[1]]) %>%
      mutate(var_name=nx,row=1:length(I)) %>%
      pivot_longer(all_of(tStage))
  },simplify=FALSE)
  
  x_df<-bind_rows(x_list)
  
  x_df<-x_df %>%
    pivot_wider(names_from=c("var_name"),values_from=c("value")) %>%
    rename(Stage=name)
  
  #write coefficients for fit to output
  write_feather(x_df,sprintf("generated_data/feather/%s_%s_%s_coef.feather",date_code,cancer_choice,keyword))
  
  #calculate predictions
  y_df<-x_df %>% 
    left_join(my_local_data %>% select(Stage,ax,css_c),multiple="all") %>%
    mutate(pred_nc=exp(-exp(a_weibull*log(ax)+b_weibull)),
           pred_c=exp(-d_decay*ax),
           pred_css=c_fraction*pred_c+(1-c_fraction)*pred_nc) 
  
  output_cure_diagnostics(date_code,cancer_choice,keyword, chain_guess,x_df,y_df)
  
}