#Coibatooldata
#Model interpretation
#August 2025

####important numbers
nrow(dk[dk$used_tool==1,])
nrow(dk[dk$used_tool==1 & dk$island=="coiba",])
nrow(dk[dk$used_tool==1 & dk$island=="jicaron",])

temp <- precis(m_jic , digits=2)
write.csv(round(temp[1:4] , 2) , file="tables/m_jic_params.csv")

temp <- precis(m_coiba , digits=2)
write.csv(round(temp[1:4] , 2) , file="tables/m_coiba_params.csv")

###mean raw material weight 
median(exp(post_j$a_rm_wt))
HPDI(exp(post_j$a_rm_wt))
median(exp(post_j$a_cc_wt))
median(exp(post_j$a_tc_wt))

median(exp(post_c$a_rm_wt))
median(exp(post_j$a_cc_wt))
median(exp(post_j$a_tc_wt))

median(data_j$rm_wt)
median(data_j$cc_wt)
median(data_j$tc_wt)

median(data_j$rm_wd)
median(data_j$cc_wd)
median(data_j$tc_wd)

median(data_j$rm_th)
median(data_j$cc_th)
median(data_j$tc_th)

median(data_j$rm_l)
median(data_j$cc_l)
median(data_j$tc_l)

length(data_j$rm_l)
length(data_j$cc_l)
length(data_j$tc_l)

#coiba

median(data_c$rm_wt)
median(data_c$fs_wt)
median(data_c$as_wt)

median(data_c$rm_wd)
median(data_c$fs_wd)
median(data_c$as_wd)

median(data_c$rm_th)
median(data_c$fs_th)
median(data_c$as_th)

median(data_c$rm_l)
median(data_c$fs_l)
median(data_c$as_l)

length(data_c$rm_l)
length(data_c$fs_l)
length(data_c$as_l)

