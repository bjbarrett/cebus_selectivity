library(rethinking)

m_coiba <- ulam(
  alist(
    as_wt ~ dgamma2(mu_as_wt,scale_as_wt), #weight shape and scale for astro tools
    log(mu_as_wt) <- a_as_wt , 
    
    fs_wt ~dgamma2(mu_fs_wt, scale_fs_wt), #weight shape/scale freshw. snail tools
    log(mu_fs_wt) <- a_fs_wt,
    
    rm_wt ~ dgamma2(mu_rm_wt,scale_rm_wt) , #weight shape and scale for RM
    log(mu_rm_wt) <- a_rm_wt ,
    
    c(a_as_wt, a_fs_wt, a_rm_wt) ~ normal(5.5,2), #prior for mean
    c(scale_as_wt,scale_fs_wt, scale_rm_wt) ~ exponential(0.01),
    
    
    as_th ~ dgamma2(mu_as_th,scale_as_th), #Thickness astro tools
    log(mu_as_th) <- a_as_th , 
    
    fs_th ~dgamma2(mu_fs_th, scale_fs_th), #thickn shape/scale freshw. snail tools
    log(mu_fs_th) <- a_fs_th,
    
    rm_th ~ dgamma2(mu_rm_th,scale_rm_th) , #thickn shape and scale for RM
    log(mu_rm_th) <- a_rm_th ,
    
    c(a_as_th,a_fs_th, a_rm_th) ~ normal(3,2), 
    c(scale_as_th,scale_fs_th, scale_rm_th) ~ exponential(0.05),
    
    
    as_l ~ dgamma2(mu_as_l,scale_as_l), #length astro tools
    log(mu_as_l) <- a_as_l , 
    
    fs_l ~dgamma2(mu_fs_l, scale_fs_l), #length shape/scale freshw. snail tools
    log(mu_fs_l) <- a_fs_l,
    
    rm_l ~ dgamma2(mu_rm_l,scale_rm_l) , #length shape and scale for RM
    log(mu_rm_l) <- a_rm_l ,
    
    c(a_as_l, a_fs_l, a_rm_l) ~ normal(3,2), 
    c(scale_as_l,scale_fs_l, scale_rm_l) ~ exponential(0.05), 
    
    
    as_wd ~ dgamma2(mu_as_wd,scale_as_wd), #width astro tools
    log(mu_as_wd) <- a_as_wd , 
    
    fs_wd ~dgamma2(mu_fs_wd, scale_fs_wd), #width shape/scale freshw. snail tools
    log(mu_fs_wd) <- a_fs_wd,
    
    rm_wd ~ dgamma2(mu_rm_wd,scale_rm_wd) , #width shape and scale for RM
    log(mu_rm_wd) <- a_rm_wd ,
    
    c(a_as_wd,a_fs_wd, a_rm_wd) ~ normal(3,2), 
    c(scale_rm_wd,scale_as_wd, scale_fs_wd) ~ exponential(0.05) 
    
  ),
  
  data=data_c, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

precis(m_coiba)
traceplot(m_coiba)

m_jic <- ulam(
  alist(
    tc_wt ~ dgamma2(mu_tc_wt,scale_tc_wt), #weight shape and scale for astro tools
    log(mu_tc_wt) <- a_tc_wt , 
    
    cc_wt ~dgamma2(mu_cc_wt, scale_cc_wt), #weight shape/scale freshw. snail tools
    log(mu_cc_wt) <- a_cc_wt,
    
    rm_wt ~ dgamma2(mu_rm_wt,scale_rm_wt) , #weight shape and scale for RM
    log(mu_rm_wt) <- a_rm_wt ,
  
    c(a_tc_wt, a_cc_wt, a_rm_wt) ~ normal(5.5,2), #prior for mean
    c(scale_tc_wt,scale_cc_wt, scale_rm_wt) ~ exponential(0.01),
    
    
    tc_th ~ dgamma2(mu_tc_th,scale_tc_th), #Thickness astro tools
    log(mu_tc_th) <- a_tc_th , 
    
    cc_th ~dgamma2(mu_cc_th, scale_cc_th), #thickn shape/scale freshw. snail tools
    log(mu_cc_th) <- a_cc_th,
    
    rm_th ~ dgamma2(mu_rm_th,scale_rm_th) , #thickn shape and scale for RM
    log(mu_rm_th) <- a_rm_th ,
    
    c(a_tc_th,a_cc_th,a_rm_th) ~ normal(3,2), 
    c(scale_tc_th,scale_cc_th, scale_rm_th) ~ exponential(0.05),
    
    
    tc_l ~ dgamma2(mu_tc_l,scale_tc_l), #length astro tools
    log(mu_tc_l) <- a_tc_l , 
    
    cc_l ~dgamma2(mu_cc_l, scale_cc_l), #length shape/scale freshw. snail tools
    log(mu_cc_l) <- a_cc_l,
    
    rm_l ~ dgamma2(mu_rm_l,scale_rm_l) , #length shape and scale for RM
    log(mu_rm_l) <- a_rm_l ,
    
    c(a_tc_l, a_cc_l, a_rm_l) ~ normal(3,2), 
    c(scale_tc_l,scale_cc_l, scale_rm_l) ~ exponential(0.05), 
    
    
    tc_wd ~ dgamma2(mu_tc_wd,scale_tc_wd), #width astro tools
    log(mu_tc_wd) <- a_tc_wd , 
    
    cc_wd ~dgamma2(mu_cc_wd, scale_cc_wd), #width shape/scale freshw. snail tools
    log(mu_cc_wd) <- a_cc_wd,
    
    rm_wd ~ dgamma2(mu_rm_wd,scale_rm_wd) , #width shape and scale for RM
    log(mu_rm_wd) <- a_rm_wd ,
    
    c(a_tc_wd,a_cc_wd,a_rm_wd) ~ normal(3,2), 
    c(scale_rm_wd,scale_tc_wd, scale_cc_wd) ~ exponential(0.05) 
    
  ),
  
  data=data_j, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

precis(m_jic)
traceplot(m_jic)

