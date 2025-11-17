#"White-faced capuchins exhibit selectivity for stone tools"
#Data management
#November 17, 2025

library(janitor)
library(stringr)
library(rethinking)
library(cmdstanr)
# install.packages("brms")
#fcns
numbers_only <- function(x) !grepl("\\D", x)

####load data
dk_raw <- read.csv("Capuchin_Tool_Surveying__2025-01.csv", sep=";", header=T)
dk <-dk_raw

####subset by island, dates visited
# coiba is all with "location" containing "7.6"
# jicaron is all with "location" containing "7.2"

sort(dk$location)
lat <- as.numeric(substr(dk$location, 1, 6))
dk$island <- ifelse(lat > 7.3 , "coiba" , "jicaron")
which(dk$island=="coiba")
coiba_days <- sort(unique(dk$today[which(dk$island=="coiba")]))
jicaron_days <- sort(unique(dk$today[which(dk$island=="jicaron")]))

xx <- as.vector(sapply(dk_raw, function(x)all(any(is.na(x))))) #makes a vector of T/F if all NA
dk <- dk_raw[,xx==FALSE] #creat dk which removes all columns with all NAs
colnames(dk)
dk <- clean_names(dk)

dk <- dk[numbers_only(dk$site_id)==FALSE,]
dk$site_id

#differentiate used tools from raw material
criteria <- str_detect(dk$site_id, "T")  # Extract matching rows with str_detect
dk$used_tool <- ifelse(criteria==TRUE , 1 , 0)

criteria <- str_detect(dk$site_id, "Clff")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}
sum(dk$used_tool)

criteria <- str_detect(dk$site_id, "Fridge")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "Wtu")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "ex")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "Cebus")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}


dk <- dk[which(dk$weight_g!=0 & dk$length_mm_max!=0 &  dk$width_mm_max!=0 & dk$thickness!=0),]
dk <- dk[!(dk$weight_g == 4.8), ]  #remove row 670 (collection error, lists weight as 4.8g)

#by trip 
unique(dk$today) #(jul21,jan22, jul22, jan23, mar23, jul23, sep23, jan24, dec24)

criteria <- str_detect(dk$today, "2021-07")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 1 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2022-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 2 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2022-07") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 3 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 4 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-03") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 5 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-07") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 6 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-09")  #mislabeled as september
for( i in 1:nrow(dk)){
        dk$today [i] <- ifelse(criteria[i]==TRUE , "2023-03" , dk$today[i])
}
criteria <- str_detect(dk$today, "2023-03") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 7 , dk$trip[i])
}


criteria <- str_detect(dk$today, "2024-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 8 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2024-12") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 9 , dk$trip[i])
}

dk$trip_id <- as.integer(dk$trip)

##Identify presence of astrocaryum based on comments
sort(unique(dk$comments))

astro  <- c("Astrocaryum spp, rio esc" , "Astrocaryum spp, rio esc" , "Astrocaryum spp., weathered, Rio Esc" , 
            "Astrocaryum, unknown round thin-shell fruit" , "Asttocaryum," , "Debris: unknown nut" ,
            "Debris: unknown nut, same as others found today" , "Debris: unknown nut. Adding camera here now." , 
            "Debris: unknown nut. See pictures." , "In forest, across from mudslide. Unknown fruit/nut. Collected for ID" ,
            "Other debris: astrocaryum" , "Palm of astrocaryum.Taken back for Meredith to measure. Pedro has it. I have debris in left pocket." , 
            "Rio escondido, fruit/nut unknown" , "Unidentified nut" , "Unknown nut: round with thin shell")

#new variable for astrocaryum presence
dk$astro <- 0
for (i in 1 : nrow(dk)){
  dk$astro[i] <- ifelse(dk$comments[i] %in% astro , 1 , 0)
}
#add entries from debris
dk$astro[which(  dk$debris_at_site %in% "astrocaryum" )] <- 1

## rio snails
dk$riosnail <-0
dk$riosnail[which(  dk$debris_at_site %in% "river_snail" )]  <- 1

str(dk)
max(dk$length_mm_max)
dk[which(dk$length_mm_max>300),]
#create separate data frames for each island
dk$island <- ifelse( dk$today %in% coiba_days , "coiba" , "jicaron")
dk$island_index <- as.integer(as.factor(dk$island ))

##coiba selection criteria for datalist

c_raw <- which(dk$island=="coiba" & dk$used_tool==0)
c_ast <- which(dk$island=="coiba" & dk$used_tool==1 & dk$astro==1)
c_cly <- which(dk$island=="coiba" & dk$used_tool==1 & dk$riosnail==1)

data_c <- list (
  rm_wt = dk$weight_g[c_raw],
  rm_th = dk$thickness[c_raw],
  rm_l = dk$length_mm_max[c_raw],
  rm_wd = dk$width_mm_max[c_raw],
  as_wt = dk$weight_g[c_ast],
  as_th = dk$thickness[c_ast],
  as_l = dk$length_mm_max[c_ast],
  as_wd = dk$width_mm_max[c_ast],
  fs_wt = dk$weight_g[c_cly],
  fs_th = dk$thickness[c_cly],
  fs_l = dk$length_mm_max[c_cly],
  fs_wd = dk$width_mm_max[c_cly]
)

#jicaron datalist
j_raw <- which(dk$island=="jicaron" & dk$used_tool==0)
j_tc <- which(dk$island=="jicaron" & dk$used_tool==1 & dk$debris_at_site_almendra==1)
j_cc <- which(dk$island=="jicaron" & dk$used_tool==1 & dk$debris_at_site_hermit_crabs==1)

data_j <- list (
  rm_wt = dk$weight_g[j_raw],
  rm_th = dk$thickness[j_raw],
  rm_l = dk$length_mm_max[j_raw],
  rm_wd = dk$width_mm_max[j_raw],
  tc_wt = dk$weight_g[j_tc],
  tc_th = dk$thickness[j_tc],
  tc_l = dk$length_mm_max[j_tc],
  tc_wd = dk$width_mm_max[j_tc],
  cc_wt = dk$weight_g[j_cc],
  cc_th = dk$thickness[j_cc],
  cc_l = dk$length_mm_max[j_cc],
  cc_wd = dk$width_mm_max[j_cc]
)