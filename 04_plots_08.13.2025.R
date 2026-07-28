##Coiba selectivity
##
###MULTIPANEL
post <- extract.samples(m_coiba)

pdf(file="figures/gamma_select_coiba_4dims.pdf" , height=8 , width=8)


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


#################JICARON########
post <- extract.samples(m_jic)

pdf(file="figures/gamma_select_jic_4dims.pdf" , height=8 , width=8)

par(mfrow = c(2, 2))
par(cex = 0.9)
par(mar = c(4, 0, 0, 0) + 0.25, oma = c(3, 0,0,0) +0.25)


### weight
dens(data_j$rm_wt, col="white" , xlim=c(0 , max(data_j$rm_wt)) , ylim=c(-.0004 , 0.003), lwd=1 , yaxt="n" , ylab="", xlab="weight (g)" , cex.lab=1.5)
#title("a. weight (g)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_wt[i]) , scale=post$scale_rm_wt[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for hermit
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_cc_wt[i]) , scale=post$scale_cc_wt[i])  , add=TRUE ,  col=col.alpha("orange2",0.05))
}
#posterior samples for almendra
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_tc_wt[i]) , scale=post$scale_tc_wt[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(data_j$cc_wt , rep(-.00008, length(data_j$cc_wt) ) , pch="|"  , col=col.alpha("orange2",0.25) , cex=0.5)
points(data_j$tc_wt , rep(-.00024, length(data_j$tc_wt) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) , cex=0.5)
points(data_j$rm_wt , rep(-.0004, length(data_j$rm_wt) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###almendra
sharks <- mean(exp(post$a_tc_wt))
stingrays <- HPDI(exp(post$a_tc_wt))
points(sharks , -.00024 , col="cornflowerblue" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.00024 , y1=-.00024 , col="cornflowerblue" , lw=2)
###hermit
sharks <- mean(exp(post$a_cc_wt))
stingrays <- HPDI(exp(post$a_cc_wt))
points(sharks , -.00008 , col="orange2" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.00008 , y1=-.00008 , col="orange2", lw=2)
#raw below
sharks <- mean(exp(post$a_rm_wt))
stingrays <- HPDI(exp(post$a_rm_wt))
points(sharks , -.0004 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0004 , y1=-.0004 , col="gray20" , lw=2)

legend("topleft" ,legend = "a." , bty="n")

################thickness######
dens(data_c$rm_th, col="white" , xlim=c(0 , max(data_j$rm_th)) , ylim=c(-.004 , 0.035), lwd=1 , yaxt="n" , ylab="", xlab="thickness (mm)", cex.lab=1.5)
#title("b. thickness (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_th[i]) , scale=post$scale_rm_th[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for hermit
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_cc_th[i]) , scale=post$scale_cc_th[i])  , add=TRUE ,  col=col.alpha("orange2",0.05))
}
#posterior samples for halmendra
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_tc_th[i]) , scale=post$scale_tc_th[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(data_j$cc_th , rep(-.0008, length(data_j$cc_th) ) , pch="|"  , col=col.alpha("orange2",0.25) , cex=0.5)
points(data_j$tc_th , rep(-.0024, length(data_j$tc_th) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) , cex=0.5)
points(data_j$rm_th , rep(-.004, length(data_j$rm_th) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###almendra
sharks <- mean(exp(post$a_tc_th))
stingrays <- HPDI(exp(post$a_tc_th))
points(sharks , -.0024 , col="cornflowerblue" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0024 , y1=-.0024 , col="cornflowerblue" , lw=2)
###hermit
sharks <- mean(exp(post$a_cc_th))
stingrays <- HPDI(exp(post$a_cc_th))
points(sharks , -.0008 , col="orange2" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0008  , y1=-.0008  , col="orange2" , lw=2)
#raw below
sharks <- mean(exp(post$a_rm_th))
stingrays <- HPDI(exp(post$a_rm_th))
points(sharks , -.004 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.004 , y1=-.004 , col="gray20" , lw=2)

legend("topleft" ,legend = "b." , bty="n")
############length###########################

dens(data_c$rm_l, col="white" , xlim=c(0 , max(data_j$tc_l)) , ylim=c(-.002 , 0.015), lwd=1 , yaxt="n" , ylab="", xlab="length (mm)", cex.lab=1.5)
#title("c. length (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_l[i]) , scale=post$scale_rm_l[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for hermit
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_cc_l[i]) , scale=post$scale_cc_l[i])  , add=TRUE ,  col=col.alpha("orange2",0.05))
}
#posterior samples for almendra
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_tc_l[i]) , scale=post$scale_tc_l[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}

#plot raw data
points(data_j$cc_l , rep(-.0004, length(data_j$cc_l) ) , pch="|"  , col=col.alpha("orange2",0.25) , cex=0.5)
points(data_j$tc_l , rep(-.0012, length(data_j$tc_l) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) , cex=0.5)
points(data_j$rm_l , rep(-.002, length(data_j$rm_l) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###almendra
sharks <- mean(exp(post$a_tc_l))
stingrays <- HPDI(exp(post$a_tc_l))
points(sharks , -.0012, col="cornflowerblue" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0012 , y1=-.0012 , col="cornflowerblue" , lw=2)
###hermit
sharks <- mean(exp(post$a_cc_l))
stingrays <- HPDI(exp(post$a_cc_l))
points(sharks , -0.0004 , col="orange2" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-0.0004 , y1=-0.0004 , col="orange2" , lw=2)
#raw below
sharks <- mean(exp(post$a_rm_l))
stingrays <- HPDI(exp(post$a_rm_l))
points(sharks , -.002, col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.002 , y1=-.002 , col="gray20" , lw=2)

legend("topleft" ,legend = "c." , bty="n")

###########width###########################

dens(data_c$rm_wd, col="white" , xlim=c(0 , 180) , ylim=c(-.002 , 0.02), lwd=1 , yaxt="n" , ylab="", xlab="width (mm)" , cex.lab=1.5)
#title("c. length (mm)")

#posterior samples for raw
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_rm_wd[i]) , scale=post$scale_rm_wd[i])  , add=TRUE ,  col=col.alpha("gray20",0.05))
}
#posterior samples for hermits
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_cc_wd[i]) , scale=post$scale_cc_wd[i])  , add=TRUE ,  col=col.alpha("orange2",0.05))
}
#posterior samples for almendra
for ( i in 1:100 ) {
  curve(dgamma2(x , mu=exp(post$a_tc_wd[i]) , scale=post$scale_tc_wd[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}

#plot raw data
points(data_j$cc_wd , rep(-.0004, length(data_j$cc_wd) ) , pch="|"  , col=col.alpha("orange2",0.25) , cex=0.5)
points(data_j$tc_wd , rep(-.0012, length(data_j$tc_wd) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) , cex=0.5)
points(data_j$rm_wd , rep(-.002, length(data_j$rm_wd) ) , pch="|"  , col=col.alpha("gray20",0.25) , cex=0.5)

###almendra
sharks <- mean(exp(post$a_tc_wd))
stingrays <- HPDI(exp(post$a_tc_wd))
points(sharks , -.0012, , col="cornflowerblue" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0012 , y1=-.0012 , col="cornflowerblue" , lw=2)
###hermit
sharks <- mean(exp(post$a_cc_wd))
stingrays <- HPDI(exp(post$a_cc_wd))
points(sharks , -.0004 , col="orange2" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.0004 , y1=-.0004 , col="orange2" , lw=2)
#raw below
sharks <- mean(exp(post$a_rm_wd))
stingrays <- HPDI(exp(post$a_rm_wd))
points(sharks , -.002 , col="gray20" , pch=18 , cex=1.1)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=-.002 , y1=-.002 , col="gray20" , lw=2)  

legend("topleft" ,legend = "d." , bty="n")
legend(-160,-0.01, fill=c("gray20" ,"cornflowerblue", "orange2") , legend=c("raw material" , "Terminalia catappa","Coenobita compressus" ) , bty="n" , horiz=TRUE , xpd=NA ,border="NA" )
dev.off()


###contrasts
post_j <- extract.samples(m_jic)
post_j$con_ccrm_wt <- post_j$a_cc_wt - post_j$a_rm_wt
post_j$con_tcrm_wt <- post_j$a_tc_wt - post_j$a_rm_wt
post_j$con_ccrm_th <- post_j$a_cc_th - post_j$a_rm_th
post_j$con_tcrm_th <- post_j$a_tc_th - post_j$a_rm_th
post_j$con_ccrm_l <- post_j$a_cc_l - post_j$a_rm_l
post_j$con_tcrm_l <- post_j$a_tc_l - post_j$a_rm_l
post_j$con_ccrm_wd <- post_j$a_cc_wd - post_j$a_rm_wd
post_j$con_tcrm_wd <- post_j$a_tc_wd - post_j$a_rm_wd

post_c <- extract.samples(m_coiba)
post_c$con_fsrm_wt <- post_c$a_fs_wt - post_c$a_rm_wt
post_c$con_asrm_wt <- post_c$a_as_wt - post_c$a_rm_wt
post_c$con_fsrm_th <- post_c$a_fs_th - post_c$a_rm_th
post_c$con_asrm_th <- post_c$a_as_th - post_c$a_rm_th
post_c$con_fsrm_l <- post_c$a_fs_l - post_c$a_rm_l
post_c$con_asrm_l <- post_c$a_as_l - post_c$a_rm_l
post_c$con_fsrm_wd <- post_c$a_fs_wd - post_c$a_rm_wd
post_c$con_asrm_wd <- post_c$a_as_wd - post_c$a_rm_wd


pdf(file="figures/dimension_mean_contrasts.pdf" , height=6 , width=8)

par(mfrow = c(2, 4))
par(cex = 0.9)
par(mar = c(3.9, 0, 0, 0) + 0.1, oma = c(2,2,0,0) +0.25)

dens(post_j$con_ccrm_wt, xlim=c(-1.5,1) , ylim=c(0,6) , col="NA", cex.axis=0.7 , ylab="" , yaxt='n' ,  xlab='')
shade(density(post_j$con_ccrm_wt) , PCI(post_j$con_ccrm_wt , prob=0.9999) , col=col.alpha("orange2" , 0.8) )
shade(density(post_j$con_tcrm_wt) , PCI(post_j$con_tcrm_wt , prob=0.9999) , col=col.alpha("cornflowerblue" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="a." , bty="n")
dens(post_j$con_ccrm_th, xlim=c(-1.5,1) , ylim=c(0,11) , col="NA" , ylab='' , yaxt='n', cex.axis=0.7,  xlab='')
shade(density(post_j$con_ccrm_th) , PCI(post_j$con_ccrm_th , prob=0.9999) , col=col.alpha("orange2" , 0.8) )
shade(density(post_j$con_tcrm_th) , PCI(post_j$con_tcrm_th , prob=0.9999) , col=col.alpha("cornflowerblue" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="b." , bty="n")

dens(post_j$con_ccrm_l, xlim=c(-1.5,1) , ylim=c(0,11) , col="NA", ylab='' , yaxt='n', cex.axis=0.7,  xlab='')
shade(density(post_j$con_ccrm_l) , PCI(post_j$con_ccrm_l , prob=0.9999) , col=col.alpha("orange2" , 0.8) )
shade(density(post_j$con_tcrm_l) , PCI(post_j$con_tcrm_l , prob=0.9999) , col=col.alpha("cornflowerblue" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="c." , bty="n")

dens(post_j$con_ccrm_wd, xlim=c(-1.5,1) , ylim=c(0,11) , col="NA", ylab='' , yaxt='n', cex.axis=0.7,  xlab='')
shade(density(post_j$con_ccrm_wd) , PCI(post_j$con_ccrm_wd , prob=0.9999) , col=col.alpha("orange2" , 0.8) )
shade(density(post_j$con_tcrm_wd) , PCI(post_j$con_tcrm_wd , prob=0.9999) , col=col.alpha("cornflowerblue" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="d." , bty="n")

dens(post_c$con_fsrm_wt, xlim=c(-1.5,1) , ylim=c(0,4) , col="NA", ylab="Coiba" , yaxt='n', cex.axis=0.7 , xlab="contrast (weight)")
shade(density(post_c$con_fsrm_wt) , PCI(post_c$con_fsrm_wt , prob=0.9999) , col=col.alpha("coral3" , 0.8) )
shade(density(post_c$con_asrm_wt) , PCI(post_c$con_asrm_wt , prob=0.9999) , col=col.alpha("#430062" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="e." , bty="n")

dens(post_c$con_fsrm_th, xlim=c(-1.5,1) , ylim=c(0,8) , col="NA", ylab='' , yaxt='n', cex.axis=0.7 , xlab="contrast (thickness)")
shade(density(post_c$con_fsrm_th) , PCI(post_c$con_fsrm_th , prob=0.9999) , col=col.alpha("coral3" , 0.8) )
shade(density(post_c$con_asrm_th) , PCI(post_c$con_asrm_th , prob=0.9999) , col=col.alpha("#430062" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="f." , bty="n")

dens(post_c$con_fsrm_l, xlim=c(-1.5,1) , ylim=c(0,8) , col="NA", ylab='' , yaxt='n', cex.axis=0.7 , xlab="contrast (length)")
shade(density(post_c$con_fsrm_l) , PCI(post_c$con_fsrm_l , prob=0.9999) , col=col.alpha("coral3" , 0.8) )
shade(density(post_c$con_asrm_l) , PCI(post_c$con_asrm_l , prob=0.9999) , col=col.alpha("#430062" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="g." , bty="n")

dens(post_c$con_fsrm_wd, xlim=c(-1.5,1) , ylim=c(0,8) , col="NA", ylab='' , yaxt='n', cex.axis=0.7, xlab="contrast (width)")
shade(density(post_c$con_fsrm_wd) , PCI(post_c$con_fsrm_wd , prob=0.9999) , col=col.alpha("coral3" , 0.8) )
shade(density(post_c$con_asrm_wd) , PCI(post_c$con_asrm_wd , prob=0.9999) , col=col.alpha("#430062" , 0.8) )
abline(v=0, lty=3)
legend("topleft" , legend="h." , bty="n")
legend(-10,-3.8, fill=c("cornflowerblue", "orange2","coral3","#430062") , legend=c( "Terminalia catappa","Coenobita compressus" ,"Clypeolum latissimum", "Astrocaryum standleyanum") , bty="n" , horiz=TRUE , xpd=NA ,border="NA" , cex=0.8 )

mtext("Jicarón", side = 2, line = 1, outer = TRUE , at=0.8)
mtext("Coiba", side = 2, line = 1, outer = TRUE , at=0.3)
mtext("Coiba", side = 2, line = 1, outer = TRUE , at=0.3)


dev.off()