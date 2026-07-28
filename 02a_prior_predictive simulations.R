m_coiba_prior_pred <- ulam(
  alist(
    as_wt ~ dgamma2(mu_as_wt,scale_as_wt), #weight shape and scale for astro tools
    log(mu_as_wt) <- a_as_wt , 
    
    fs_wt ~dgamma2(mu_fs_wt, scale_fs_wt), #weight shape/scale freshw. snail tools
    log(mu_fs_wt) <- a_fs_wt,
    
    rm_wt ~ dgamma2(mu_rm_wt,scale_rm_wt) , #weight shape and scale for RM
    log(mu_rm_wt) <- a_rm_wt ,
    
    c(a_as_wt, a_fs_wt, a_rm_wt) ~ normal(5.5,2), #prior for mean
    c(scale_as_wt,scale_fs_wt, scale_rm_wt) ~ exponential(.01),
    
    
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
  
  data=data_c, cores=4 , warmup=1000 , iter=2000 , sample_prior=TRUE, chains=4,
)


precis(m_coiba_prior_pred)
post <- extract.samples(m_coiba_prior_pred)

pdf(file="figures/gamma_select_coiba_4dims_prior_pred.pdf" , height=8 , width=8)


par(mfrow = c(2, 2))
par(cex = 0.9)
par(mar = c(4, 0, 0, 0) + 0.25, oma = c(3, 0,0,0) +0.25)


### weight
dens(data_c$rm_wt, col="white" , xlim=c(0 , max(data_c$as_wt)) , ylim=c(-.0004 , 0.0045), lwd=1 , yaxt="n" , ylab="", xlab="weight (g)" , cex.lab=1.5)
#title("a. weight (g)")

#posterior samples for raw
for ( i in 1:100 ) {
        curve(dgamma2(x , mu=exp(post$a_rm_wt[i]) , scale=post$scale_rm_wt[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for snail
for ( i in 1:100 ) {
        curve(dgamma2(x , mu=exp(post$a_fs_wt[i]) , scale=post$scale_fs_wt[i])  , add=TRUE ,  col=col.alpha("coral3",0.05))
}
#posterior samples for astro
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_as_wt[i]) , scale=post$scale_as_wt[i])  , add=TRUE ,  col=col.alpha("#430062",0.05))
}

#plot raw data
points(data_c$fs_wt , rep(-.00008, length(data_c$fs_wt) ) , pch="|"  , col=col.alpha("coral3",0.25) , cex=0.5)
points(data_c$as_wt , rep(-.00024, length(data_c$as_wt) ) , pch="|"  , col=col.alpha("#430062",0.25) , cex=0.5)
points(data_c$rm_wt , rep(-.0004, length(data_c$rm_wt) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###sastrocaryum
sharks <- mean(exp(post$a_as_wt))
stingrays <- HPDI(exp(post$a_as_wt))
points(sharks , -.00024 , col="#430062" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.00024 , y1=-.00024 , col="#430062",lw=2)
###snails
sharks <- mean(exp(post$a_fs_wt))
stingrays <- HPDI(exp(post$a_fs_wt))
points(sharks , -.00008 , col="coral3" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.00008 , y1=-.00008 , col="coral3",lw=2)
#raw below
sharks <- mean(exp(post$a_rm_wt))
stingrays <- HPDI(exp(post$a_rm_wt))
points(sharks , -.0004 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0004 , y1=-.0004 , col="gray20" ,lw=2)

legend("topleft" ,legend = "a." , bty="n")
################thickness######
dens(data_c$rm_th, col="white" , xlim=c(0 , 125) , ylim=c(-.004 , 0.05), lwd=1 , yaxt="n" , ylab="", xlab="thickness (mm)", cex.lab=1.5)
#title("b. thickness (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_th[i]) , scale=post$scale_rm_th[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for snail
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_fs_th[i]) , scale=post$scale_fs_th[i])  , add=TRUE ,  col=col.alpha("coral3",0.05))
}
#posterior samples for astro
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_as_th[i]) , scale=post$scale_as_th[i])  , add=TRUE ,  col=col.alpha("#430062",0.05))
}

#plot raw data
points(data_c$fs_th , rep(-.0008, length(data_c$fs_th) ) , pch="|"  , col=col.alpha("coral3",0.25) , cex=0.5)
points(data_c$as_th , rep(-.0024, length(data_c$as_th) ) , pch="|"  , col=col.alpha("#430062",0.25) , cex=0.5)
points(data_c$rm_th , rep(-.004, length(data_c$rm_th) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###sastrocaryum
sharks <- mean(exp(post$a_as_th))
stingrays <- HPDI(exp(post$a_as_th))
points(sharks , -.0024  , col="#430062" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0024 , y1=-.0024 , col="#430062" , lw=2)
###snails
sharks <- mean(exp(post$a_fs_th))
stingrays <- HPDI(exp(post$a_fs_th))
points(sharks , -.0008 , col="coral3" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0008 , y1=-.0008 , col="coral3" , lw=2)
#raw below
sharks <- mean(exp(post$a_rm_th))
stingrays <- HPDI(exp(post$a_rm_th))
points(sharks , -.004 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.004 , y1=-.004 , col="gray20" , lw=2)

legend("topleft" ,legend = "b." , bty="n")
############length###########################

dens(data_c$rm_l, col="white" , xlim=c(0 , 220) , ylim=c(-.002 , 0.025), lwd=1 , yaxt="n" , ylab="", xlab="length (mm)", cex.lab=1.5)
#title("c. length (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_l[i]) , scale=post$scale_rm_l[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for snail
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_fs_l[i]) , scale=post$scale_fs_l[i])  , add=TRUE ,  col=col.alpha("coral3",0.05))
}
#posterior samples for astro
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_as_l[i]) , scale=post$scale_as_l[i])  , add=TRUE ,  col=col.alpha("#430062",0.05))
}

#plot raw data
points(data_c$fs_l , rep(-.0004, length(data_c$fs_l) ) , pch="|"  , col=col.alpha("coral3",0.25) , cex=0.5)
points(data_c$as_l , rep(-.0012, length(data_c$as_l) ) , pch="|"  , col=col.alpha("#430062",0.25) , cex=0.5)
points(data_c$rm_l , rep(-.002, length(data_c$rm_l) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###sastrocaryum
sharks <- mean(exp(post$a_as_l))
stingrays <- HPDI(exp(post$a_as_l))
points(sharks , -.0012 , col="#430062" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0012 , y1=-.0012 , col="#430062",lw=2)
###snails
sharks <- mean(exp(post$a_fs_l))
stingrays <- HPDI(exp(post$a_fs_l))
points(sharks , -.0004 , col="coral3" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0004 , y1=-.0004 , col="coral3",lw=2)
#raw below
sharks <- mean(exp(post$a_rm_l))
stingrays <- HPDI(exp(post$a_rm_l))
points(sharks , -.002 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.002 , y1=-.002 , col="gray20",lw=2)

legend("topleft" ,legend = "c." , bty="n")

############width###########################

dens(data_c$rm_wd, col="white" , xlim=c(0 , 180) , ylim=c(-.002 , 0.03), lwd=1 , yaxt="n" , ylab="", xlab="width (mm)" , cex.lab=1.5)
#title("c. length (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_wd[i]) , scale=post$scale_rm_wd[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for snail
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_fs_wd[i]) , scale=post$scale_fs_wd[i])  , add=TRUE ,  col=col.alpha("coral3",0.05))
}
#posterior samples for astro
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_as_wd[i]) , scale=post$scale_as_wd[i])  , add=TRUE ,  col=col.alpha("#430062",0.05))
}
#plot raw data
points(data_c$fs_wd , rep(-.0004, length(data_c$fs_wd) ) , pch="|"  , col=col.alpha("coral3",0.25) , cex=0.5)
points(data_c$as_wd , rep(-.0012, length(data_c$as_wd) ) , pch="|"  , col=col.alpha("#430062",0.25) , cex=0.5)
points(data_c$rm_wd , rep(-.002, length(data_c$rm_wd) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###sastrocaryum
sharks <- mean(exp(post$a_as_wd))
stingrays <- HPDI(exp(post$a_as_wd))
points(sharks , -.0012 , col="#430062" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0012 , y1=-.0012 , col="#430062" , lw=2)
###snails
sharks <- mean(exp(post$a_fs_wd))
stingrays <- HPDI(exp(post$a_fs_wd))
points(sharks , -.0004 , col="coral3" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0004 , y1=-.0004 , col="coral3" , lw=2)
#raw below
sharks <- mean(exp(post$a_rm_wd))
stingrays <- HPDI(exp(post$a_rm_wd))
points(sharks , -.002 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.002 , y1=-.002 , col="gray20" , lw=2)

legend("topleft" ,legend = "d." , bty="n")
legend(-180,-0.014, fill=c("gray20" , "coral3","#430062") , legend=c("raw material" , "Clypeolum latissimum", "Astrocaryum standleyanum") , bty="n" , horiz=TRUE , xpd=NA )
dev.off()

