power <- read.csv(file = "rawdata.csv",encoding = "UTF-8")
nonpower <- read.csv(file = "ra2.csv",encoding = "UTF-8")

library(dplyr)
library(magrittr)
library(ggplot2)


###### FIRST PART : ######

# power rider by city 
# offpeak traffic usage by occasion
c11 <- list()
c12 <- list()
c13 <- list()
c14 <- list()
c15 <- list()
cc11 <- list()
cc12 <- list()
cc13 <- list()
cc14 <- list()
cc15 <- list()
cc1 <- list()

c21 <- list()
c22 <- list()
c23 <- list()
c24 <- list()
c25 <- list() 
cc21 <- list()
cc22 <- list()
cc23 <- list()
cc24 <- list()
cc25 <- list()
cc2 <- list()

c31 <- list()
c32 <- list()
c33 <- list()
c34 <- list()
c35 <- list()
cc31 <- list()
cc32 <- list()
cc33 <- list()
cc34 <- list()
cc35 <- list()
cc3 <- list()

c41 <- list()
c42 <- list()
c43 <- list()
c44 <- list()
c45 <- list()
cc41 <- list()
cc42 <- list()
cc43 <- list()
cc44 <- list()
cc45 <- list()
cc4 <- list()

c51 <- list()
c52 <- list()
c53 <- list()
c54 <- list()
c55 <- list()
cc51 <- list()
cc52 <- list()
cc53 <- list()
cc54 <- list()
cc55 <- list()
cc5 <- list()

df <- list()

for(i in 0:4){
  c11[[i+1]] <- power %>% filter(power[i+30] > 3,power$Home == "tpe" ) %>% nrow() 
  c12[[i+1]] <- power %>% filter(power[i+35] > 3,power$Home == "tpe" ) %>% nrow()
  c13[[i+1]] <- power %>% filter(power[i+40] > 3,power$Home == "tpe" ) %>% nrow()
  c14[[i+1]] <- power %>% filter(power[i+45] > 3,power$Home == "tpe" ) %>% nrow()
  c15[[i+1]] <- power %>% filter(power[i+50] > 3,power$Home == "tpe" ) %>% nrow()
  cc11[[i+1]] <- c11[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc12[[i+1]] <- c12[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc13[[i+1]] <- c13[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc14[[i+1]] <- c14[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc15[[i+1]] <- c15[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc1[[i+1]] <- c(cc11[[i+1]],cc12[[i+1]],cc13[[i+1]],cc14[[i+1]],cc15[[i+1]])
  
  
  c21[[i+1]] <- power %>% filter(power[30+i] > 3,power$Home == "ty" ) %>% nrow()
  c22[[i+1]] <- power %>% filter(power[35+i] > 3,power$Home == "ty" ) %>% nrow()
  c23[[i+1]] <- power %>% filter(power[40+i] > 3,power$Home == "ty" ) %>% nrow()
  c24[[i+1]] <- power %>% filter(power[45+i] > 3,power$Home == "ty" ) %>% nrow()
  c25[[i+1]] <- power %>% filter(power[50+i] > 3,power$Home == "ty" ) %>% nrow()
  cc21[[i+1]] <- c21[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc22[[i+1]] <- c22[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc23[[i+1]] <- c23[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc24[[i+1]] <- c24[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc25[[i+1]] <- c25[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc2[[i+1]] <- c(cc21[[i+1]],cc22[[i+1]],cc23[[i+1]],cc24[[i+1]],cc25[[i+1]])
  
  
  c31[[i+1]] <- power %>% filter(power[30+i] > 3,power$Home == "hc" ) %>% nrow()
  c32[[i+1]] <- power %>% filter(power[35+i] > 3,power$Home == "hc" ) %>% nrow()
  c33[[i+1]] <- power %>% filter(power[40+i] > 3,power$Home == "hc" ) %>% nrow()
  c34[[i+1]] <- power %>% filter(power[45+i] > 3,power$Home == "hc" ) %>% nrow()
  c35[[i+1]] <- power %>% filter(power[50+i] > 3,power$Home == "hc" ) %>% nrow()
  cc31[[i+1]] <- c31[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc32[[i+1]] <- c32[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc33[[i+1]] <- c33[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc34[[i+1]] <- c34[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc35[[i+1]] <- c35[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc3[[i+1]] <- c(cc31[[i+1]],cc32[[i+1]],cc33[[i+1]],cc34[[i+1]],cc35[[i+1]])
  
  
  c41[[i+1]] <- power %>% filter(power[30+i] > 3,power$Home == "tc" ) %>% nrow()
  c42[[i+1]] <- power %>% filter(power[35+i] > 3,power$Home == "tc" ) %>% nrow()
  c43[[i+1]] <- power %>% filter(power[40+i] > 3,power$Home == "tc" ) %>% nrow()
  c44[[i+1]] <- power %>% filter(power[45+i] > 3,power$Home == "tc" ) %>% nrow()
  c45[[i+1]] <- power %>% filter(power[50+i] > 3,power$Home == "tc" ) %>% nrow()
  cc41[[i+1]] <- c41[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc42[[i+1]] <- c42[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc43[[i+1]] <- c43[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc44[[i+1]] <- c44[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc45[[i+1]] <- c45[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc4[[i+1]] <- c(cc41[[i+1]],cc42[[i+1]],cc43[[i+1]],cc44[[i+1]],cc45[[i+1]])
  
  
  c51[[i+1]] <- power %>% filter(power[30+i] > 3,power$Home == "kh" ) %>% nrow() 
  c52[[i+1]] <- power %>% filter(power[35+i] > 3,power$Home == "kh" ) %>% nrow() 
  c53[[i+1]] <- power %>% filter(power[40+i] > 3,power$Home == "kh" ) %>% nrow() 
  c54[[i+1]] <- power %>% filter(power[45+i] > 3,power$Home == "kh" ) %>% nrow() 
  c55[[i+1]] <- power %>% filter(power[50+i] > 3,power$Home == "kh" ) %>% nrow() 
  cc51[[i+1]] <- c51[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc52[[i+1]] <- c52[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc53[[i+1]] <- c53[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc54[[i+1]] <- c54[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc55[[i+1]] <- c55[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc5[[i+1]] <- c(cc51[[i+1]],cc52[[i+1]],cc53[[i+1]],cc54[[i+1]],cc55[[i+1]])
  
  df[[i+1]] <- data.frame(cc1[[i+1]],cc2[[i+1]],cc3[[i+1]],cc4[[i+1]],cc5[[i+1]]) 
}

# df[[1]] commute
power_city_commute <- df[[1]]
# df[[2]] weather
power_city_weather <- df[[2]]
# df[[3]] shopping
power_city_shopping <- df[[3]]
# df[[5]] business trip
power_city_business <- df[[5]]



###### SECOND PART : ######

# nonpower rider by city 
# offpeak traffic usage by occasion
n11 <- list()
n12 <- list()
n13 <- list()
n14 <- list()
n15 <- list()
nc11 <- list()
nc12 <- list()
nc13 <- list()
nc14 <- list()
nc15 <- list()
nc1 <- list()

n21 <- list()
n22 <- list()
n23 <- list()
n24 <- list()
n25 <- list() 
nc21 <- list()
nc22 <- list()
nc23 <- list()
nc24 <- list()
nc25 <- list()
nc2 <- list()

n31 <- list()
n32 <- list()
n33 <- list()
n34 <- list()
n35 <- list()
nc31 <- list()
nc32 <- list()
nc33 <- list()
nc34 <- list()
nc35 <- list()
nc3 <- list()

n41 <- list()
n42 <- list()
n43 <- list()
n44 <- list()
n45 <- list()
nc41 <- list()
nc42 <- list()
nc43 <- list()
nc44 <- list()
nc45 <- list()
nc4 <- list()

n51 <- list()
n52 <- list()
n53 <- list()
n54 <- list()
n55 <- list()
nc51 <- list()
nc52 <- list()
nc53 <- list()
nc54 <- list()
nc55 <- list()
nc5 <- list()

ndf <- list()

for(i in 0:4){
  n11[[i+1]] <- nonpower %>% filter(nonpower[i+30] > 3,nonpower$Home == "tpe" ) %>% nrow() 
  n12[[i+1]] <- nonpower %>% filter(nonpower[i+35] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n13[[i+1]] <- nonpower %>% filter(nonpower[i+40] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n14[[i+1]] <- nonpower %>% filter(nonpower[i+45] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n15[[i+1]] <- nonpower %>% filter(nonpower[i+50] > 3,nonpower$Home == "tpe" ) %>% nrow()
  nc11[[i+1]] <- n11[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc12[[i+1]] <- n12[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc13[[i+1]] <- n13[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc14[[i+1]] <- n14[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc15[[i+1]] <- n15[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc1[[i+1]] <- c(nc11[[i+1]],nc12[[i+1]],nc13[[i+1]],nc14[[i+1]],nc15[[i+1]])
  
  
  n21[[i+1]] <- nonpower %>% filter(nonpower[30+i] > 3,nonpower$Home == "ty" ) %>% nrow()
  n22[[i+1]] <- nonpower %>% filter(nonpower[35+i] > 3,nonpower$Home == "ty" ) %>% nrow()
  n23[[i+1]] <- nonpower %>% filter(nonpower[40+i] > 3,nonpower$Home == "ty" ) %>% nrow()
  n24[[i+1]] <- nonpower %>% filter(nonpower[45+i] > 3,nonpower$Home == "ty" ) %>% nrow()
  n25[[i+1]] <- nonpower %>% filter(nonpower[50+i] > 3,nonpower$Home == "ty" ) %>% nrow()
  nc21[[i+1]] <- n21[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc22[[i+1]] <- n22[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc23[[i+1]] <- n23[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc24[[i+1]] <- n24[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc25[[i+1]] <- n25[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc2[[i+1]] <- c(nc21[[i+1]],nc22[[i+1]],nc23[[i+1]],nc24[[i+1]],nc25[[i+1]])
  
  
  n31[[i+1]] <- nonpower %>% filter(nonpower[30+i] > 3,nonpower$Home == "hc" ) %>% nrow()
  n32[[i+1]] <- nonpower %>% filter(nonpower[35+i] > 3,nonpower$Home == "hc" ) %>% nrow()
  n33[[i+1]] <- nonpower %>% filter(nonpower[40+i] > 3,nonpower$Home == "hc" ) %>% nrow()
  n34[[i+1]] <- nonpower %>% filter(nonpower[45+i] > 3,nonpower$Home == "hc" ) %>% nrow()
  n35[[i+1]] <- nonpower %>% filter(nonpower[50+i] > 3,nonpower$Home == "hc" ) %>% nrow()
  nc31[[i+1]] <- n31[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc32[[i+1]] <- n32[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc33[[i+1]] <- n33[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc34[[i+1]] <- n34[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc35[[i+1]] <- n35[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc3[[i+1]] <- c(nc31[[i+1]],nc32[[i+1]],nc33[[i+1]],nc34[[i+1]],nc35[[i+1]])
  
  
  n41[[i+1]] <- nonpower %>% filter(nonpower[30+i] > 3,nonpower$Home == "tc" ) %>% nrow()
  n42[[i+1]] <- nonpower %>% filter(nonpower[35+i] > 3,nonpower$Home == "tc" ) %>% nrow()
  n43[[i+1]] <- nonpower %>% filter(nonpower[40+i] > 3,nonpower$Home == "tc" ) %>% nrow()
  n44[[i+1]] <- nonpower %>% filter(nonpower[45+i] > 3,nonpower$Home == "tc" ) %>% nrow()
  n45[[i+1]] <- nonpower %>% filter(nonpower[50+i] > 3,nonpower$Home == "tc" ) %>% nrow()
  nc41[[i+1]] <- n41[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc42[[i+1]] <- n42[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc43[[i+1]] <- n43[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc44[[i+1]] <- n44[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc45[[i+1]] <- n45[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc4[[i+1]] <- c(nc41[[i+1]],nc42[[i+1]],nc43[[i+1]],nc44[[i+1]],nc45[[i+1]])
  
  
  n51[[i+1]] <- nonpower %>% filter(nonpower[30+i] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n52[[i+1]] <- nonpower %>% filter(nonpower[35+i] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n53[[i+1]] <- nonpower %>% filter(nonpower[40+i] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n54[[i+1]] <- nonpower %>% filter(nonpower[45+i] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n55[[i+1]] <- nonpower %>% filter(nonpower[50+i] > 3,nonpower$Home == "kh" ) %>% nrow() 
  nc51[[i+1]] <- n51[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc52[[i+1]] <- n52[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc53[[i+1]] <- n53[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc54[[i+1]] <- n54[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc55[[i+1]] <- n55[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc5[[i+1]] <- c(nc51[[i+1]],nc52[[i+1]],nc53[[i+1]],nc54[[i+1]],nc55[[i+1]])
  
  ndf[[i+1]] <- data.frame(nc1[[i+1]],nc2[[i+1]],nc3[[i+1]],nc4[[i+1]],nc5[[i+1]]) 
}

# df[[1]] commute
nonpower_city_commute <- ndf[[1]]
# df[[2]] weather
nonpower_city_weather <- ndf[[2]]
# df[[3]] shopping
nonpower_city_shopping <- ndf[[3]]
# df[[5]] business trip
nonpower_city_business <- ndf[[5]]



###### THIRD PART : ######

# power rider by city 
# all-time traffic usage by occasion

c11 <- list()
c12 <- list()
c13 <- list()
c14 <- list()
c15 <- list()
cc11 <- list()
cc12 <- list()
cc13 <- list()
cc14 <- list()
cc15 <- list()
cc1 <- list()

c21 <- list()
c22 <- list()
c23 <- list()
c24 <- list()
c25 <- list() 
cc21 <- list()
cc22 <- list()
cc23 <- list()
cc24 <- list()
cc25 <- list()
cc2 <- list()

c31 <- list()
c32 <- list()
c33 <- list()
c34 <- list()
c35 <- list()
cc31 <- list()
cc32 <- list()
cc33 <- list()
cc34 <- list()
cc35 <- list()
cc3 <- list()

c41 <- list()
c42 <- list()
c43 <- list()
c44 <- list()
c45 <- list()
cc41 <- list()
cc42 <- list()
cc43 <- list()
cc44 <- list()
cc45 <- list()
cc4 <- list()

c51 <- list()
c52 <- list()
c53 <- list()
c54 <- list()
c55 <- list()
cc51 <- list()
cc52 <- list()
cc53 <- list()
cc54 <- list()
cc55 <- list()
cc5 <- list()

df <- list()

for(i in 0:4){
  c11[[i+1]] <- power %>% filter(power[i*5+10] > 3,power$Home == "tpe" ) %>% nrow() 
  c12[[i+1]] <- power %>% filter(power[i*5+11] > 3,power$Home == "tpe" ) %>% nrow()
  c13[[i+1]] <- power %>% filter(power[i*5+12] > 3,power$Home == "tpe" ) %>% nrow()
  c14[[i+1]] <- power %>% filter(power[i*5+13] > 3,power$Home == "tpe" ) %>% nrow()
  c15[[i+1]] <- power %>% filter(power[i*5+14] > 3,power$Home == "tpe" ) %>% nrow()
  cc11[[i+1]] <- c11[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc12[[i+1]] <- c12[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc13[[i+1]] <- c13[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc14[[i+1]] <- c14[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc15[[i+1]] <- c15[[i+1]]/(c11[[i+1]]+c12[[i+1]]+c13[[i+1]]+c14[[i+1]]+c15[[i+1]])
  cc1[[i+1]] <- c(cc11[[i+1]],cc12[[i+1]],cc13[[i+1]],cc14[[i+1]],cc15[[i+1]])
  
  
  c21[[i+1]] <- power %>% filter(power[i*5+10] > 3,power$Home == "ty" ) %>% nrow()
  c22[[i+1]] <- power %>% filter(power[i*5+11] > 3,power$Home == "ty" ) %>% nrow()
  c23[[i+1]] <- power %>% filter(power[i*5+12] > 3,power$Home == "ty" ) %>% nrow()
  c24[[i+1]] <- power %>% filter(power[i*5+13] > 3,power$Home == "ty" ) %>% nrow()
  c25[[i+1]] <- power %>% filter(power[i*5+14] > 3,power$Home == "ty" ) %>% nrow()
  cc21[[i+1]] <- c21[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc22[[i+1]] <- c22[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc23[[i+1]] <- c23[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc24[[i+1]] <- c24[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc25[[i+1]] <- c25[[i+1]]/(c21[[i+1]]+c22[[i+1]]+c23[[i+1]]+c24[[i+1]]+c25[[i+1]])
  cc2[[i+1]] <- c(cc21[[i+1]],cc22[[i+1]],cc23[[i+1]],cc24[[i+1]],cc25[[i+1]])
  
  
  c31[[i+1]] <- power %>% filter(power[i*5+10] > 3,power$Home == "hc" ) %>% nrow()
  c32[[i+1]] <- power %>% filter(power[i*5+11] > 3,power$Home == "hc" ) %>% nrow()
  c33[[i+1]] <- power %>% filter(power[i*5+12] > 3,power$Home == "hc" ) %>% nrow()
  c34[[i+1]] <- power %>% filter(power[i*5+13] > 3,power$Home == "hc" ) %>% nrow()
  c35[[i+1]] <- power %>% filter(power[i*5+14] > 3,power$Home == "hc" ) %>% nrow()
  cc31[[i+1]] <- c31[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc32[[i+1]] <- c32[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc33[[i+1]] <- c33[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc34[[i+1]] <- c34[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc35[[i+1]] <- c35[[i+1]]/(c31[[i+1]]+c32[[i+1]]+c33[[i+1]]+c34[[i+1]]+c35[[i+1]])
  cc3[[i+1]] <- c(cc31[[i+1]],cc32[[i+1]],cc33[[i+1]],cc34[[i+1]],cc35[[i+1]])
  
  
  c41[[i+1]] <- power %>% filter(power[i*5+10] > 3,power$Home == "tc" ) %>% nrow()
  c42[[i+1]] <- power %>% filter(power[i*5+11] > 3,power$Home == "tc" ) %>% nrow()
  c43[[i+1]] <- power %>% filter(power[i*5+12] > 3,power$Home == "tc" ) %>% nrow()
  c44[[i+1]] <- power %>% filter(power[i*5+13] > 3,power$Home == "tc" ) %>% nrow()
  c45[[i+1]] <- power %>% filter(power[i*5+14] > 3,power$Home == "tc" ) %>% nrow()
  cc41[[i+1]] <- c41[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc42[[i+1]] <- c42[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc43[[i+1]] <- c43[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc44[[i+1]] <- c44[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc45[[i+1]] <- c45[[i+1]]/(c41[[i+1]]+c42[[i+1]]+c43[[i+1]]+c44[[i+1]]+c45[[i+1]])
  cc4[[i+1]] <- c(cc41[[i+1]],cc42[[i+1]],cc43[[i+1]],cc44[[i+1]],cc45[[i+1]])
  
  
  c51[[i+1]] <- power %>% filter(power[i*5+10] > 3,power$Home == "kh" ) %>% nrow() 
  c52[[i+1]] <- power %>% filter(power[i*5+11] > 3,power$Home == "kh" ) %>% nrow() 
  c53[[i+1]] <- power %>% filter(power[i*5+12] > 3,power$Home == "kh" ) %>% nrow() 
  c54[[i+1]] <- power %>% filter(power[i*5+13] > 3,power$Home == "kh" ) %>% nrow() 
  c55[[i+1]] <- power %>% filter(power[i*5+14] > 3,power$Home == "kh" ) %>% nrow() 
  cc51[[i+1]] <- c51[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc52[[i+1]] <- c52[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc53[[i+1]] <- c53[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc54[[i+1]] <- c54[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc55[[i+1]] <- c55[[i+1]]/(c51[[i+1]]+c52[[i+1]]+c53[[i+1]]+c54[[i+1]]+c55[[i+1]])
  cc5[[i+1]] <- c(cc51[[i+1]],cc52[[i+1]],cc53[[i+1]],cc54[[i+1]],cc55[[i+1]])
  
  df[[i+1]] <- data.frame(cc1[[i+1]],cc2[[i+1]],cc3[[i+1]],cc4[[i+1]],cc5[[i+1]]) 
}

# df[[1]] commute
power_city_commute_alltime <- df[[1]]
# df[[2]] weather
power_city_weather_alltime <- df[[2]]
# df[[3]] shopping
power_city_shopping_alltime <- df[[3]]
# df[[5]] business trip
power_city_business_alltime <- df[[5]]




###### FOURTH PART : ######

# nonpower rider by city 
# all-time traffic usage by occasion
n11 <- list()
n12 <- list()
n13 <- list()
n14 <- list()
n15 <- list()
nc11 <- list()
nc12 <- list()
nc13 <- list()
nc14 <- list()
nc15 <- list()
nc1 <- list()

n21 <- list()
n22 <- list()
n23 <- list()
n24 <- list()
n25 <- list() 
nc21 <- list()
nc22 <- list()
nc23 <- list()
nc24 <- list()
nc25 <- list()
nc2 <- list()

n31 <- list()
n32 <- list()
n33 <- list()
n34 <- list()
n35 <- list()
nc31 <- list()
nc32 <- list()
nc33 <- list()
nc34 <- list()
nc35 <- list()
nc3 <- list()

n41 <- list()
n42 <- list()
n43 <- list()
n44 <- list()
n45 <- list()
nc41 <- list()
nc42 <- list()
nc43 <- list()
nc44 <- list()
nc45 <- list()
nc4 <- list()

n51 <- list()
n52 <- list()
n53 <- list()
n54 <- list()
n55 <- list()
nc51 <- list()
nc52 <- list()
nc53 <- list()
nc54 <- list()
nc55 <- list()
nc5 <- list()

ndf <- list()

for(i in 0:4){
  n11[[i+1]] <- nonpower %>% filter(nonpower[i*5+10] > 3,nonpower$Home == "tpe" ) %>% nrow() 
  n12[[i+1]] <- nonpower %>% filter(nonpower[i*5+11] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n13[[i+1]] <- nonpower %>% filter(nonpower[i*5+12] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n14[[i+1]] <- nonpower %>% filter(nonpower[i*5+13] > 3,nonpower$Home == "tpe" ) %>% nrow()
  n15[[i+1]] <- nonpower %>% filter(nonpower[i*5+14] > 3,nonpower$Home == "tpe" ) %>% nrow()
  nc11[[i+1]] <- n11[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc12[[i+1]] <- n12[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc13[[i+1]] <- n13[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc14[[i+1]] <- n14[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc15[[i+1]] <- n15[[i+1]]/(n11[[i+1]]+n12[[i+1]]+n13[[i+1]]+n14[[i+1]]+n15[[i+1]])
  nc1[[i+1]] <- c(nc11[[i+1]],nc12[[i+1]],nc13[[i+1]],nc14[[i+1]],nc15[[i+1]])
  
  
  n21[[i+1]] <- nonpower %>% filter(nonpower[i*5+10] > 3,nonpower$Home == "ty" ) %>% nrow()
  n22[[i+1]] <- nonpower %>% filter(nonpower[i*5+11] > 3,nonpower$Home == "ty" ) %>% nrow()
  n23[[i+1]] <- nonpower %>% filter(nonpower[i*5+12] > 3,nonpower$Home == "ty" ) %>% nrow()
  n24[[i+1]] <- nonpower %>% filter(nonpower[i*5+13] > 3,nonpower$Home == "ty" ) %>% nrow()
  n25[[i+1]] <- nonpower %>% filter(nonpower[i*5+14] > 3,nonpower$Home == "ty" ) %>% nrow()
  nc21[[i+1]] <- n21[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc22[[i+1]] <- n22[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc23[[i+1]] <- n23[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc24[[i+1]] <- n24[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc25[[i+1]] <- n25[[i+1]]/(n21[[i+1]]+n22[[i+1]]+n23[[i+1]]+n24[[i+1]]+n25[[i+1]])
  nc2[[i+1]] <- c(nc21[[i+1]],nc22[[i+1]],nc23[[i+1]],nc24[[i+1]],nc25[[i+1]])
  
  
  n31[[i+1]] <- nonpower %>% filter(nonpower[i*5+10] > 3,nonpower$Home == "hc" ) %>% nrow()
  n32[[i+1]] <- nonpower %>% filter(nonpower[i*5+11] > 3,nonpower$Home == "hc" ) %>% nrow()
  n33[[i+1]] <- nonpower %>% filter(nonpower[i*5+12] > 3,nonpower$Home == "hc" ) %>% nrow()
  n34[[i+1]] <- nonpower %>% filter(nonpower[i*5+13] > 3,nonpower$Home == "hc" ) %>% nrow()
  n35[[i+1]] <- nonpower %>% filter(nonpower[i*5+14] > 3,nonpower$Home == "hc" ) %>% nrow()
  nc31[[i+1]] <- n31[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc32[[i+1]] <- n32[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc33[[i+1]] <- n33[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc34[[i+1]] <- n34[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc35[[i+1]] <- n35[[i+1]]/(n31[[i+1]]+n32[[i+1]]+n33[[i+1]]+n34[[i+1]]+n35[[i+1]])
  nc3[[i+1]] <- c(nc31[[i+1]],nc32[[i+1]],nc33[[i+1]],nc34[[i+1]],nc35[[i+1]])
  
  
  n41[[i+1]] <- nonpower %>% filter(nonpower[i*5+10] > 3,nonpower$Home == "tc" ) %>% nrow()
  n42[[i+1]] <- nonpower %>% filter(nonpower[i*5+11] > 3,nonpower$Home == "tc" ) %>% nrow()
  n43[[i+1]] <- nonpower %>% filter(nonpower[i*5+12] > 3,nonpower$Home == "tc" ) %>% nrow()
  n44[[i+1]] <- nonpower %>% filter(nonpower[i*5+13] > 3,nonpower$Home == "tc" ) %>% nrow()
  n45[[i+1]] <- nonpower %>% filter(nonpower[i*5+14] > 3,nonpower$Home == "tc" ) %>% nrow()
  nc41[[i+1]] <- n41[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc42[[i+1]] <- n42[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc43[[i+1]] <- n43[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc44[[i+1]] <- n44[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc45[[i+1]] <- n45[[i+1]]/(n41[[i+1]]+n42[[i+1]]+n43[[i+1]]+n44[[i+1]]+n45[[i+1]])
  nc4[[i+1]] <- c(nc41[[i+1]],nc42[[i+1]],nc43[[i+1]],nc44[[i+1]],nc45[[i+1]])
  
  
  n51[[i+1]] <- nonpower %>% filter(nonpower[i*5+10] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n52[[i+1]] <- nonpower %>% filter(nonpower[i*5+11] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n53[[i+1]] <- nonpower %>% filter(nonpower[i*5+12] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n54[[i+1]] <- nonpower %>% filter(nonpower[i*5+13] > 3,nonpower$Home == "kh" ) %>% nrow() 
  n55[[i+1]] <- nonpower %>% filter(nonpower[i*5+14] > 3,nonpower$Home == "kh" ) %>% nrow() 
  nc51[[i+1]] <- n51[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc52[[i+1]] <- n52[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc53[[i+1]] <- n53[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc54[[i+1]] <- n54[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc55[[i+1]] <- n55[[i+1]]/(n51[[i+1]]+n52[[i+1]]+n53[[i+1]]+n54[[i+1]]+n55[[i+1]])
  nc5[[i+1]] <- c(nc51[[i+1]],nc52[[i+1]],nc53[[i+1]],nc54[[i+1]],nc55[[i+1]])
  
  ndf[[i+1]] <- data.frame(nc1[[i+1]],nc2[[i+1]],nc3[[i+1]],nc4[[i+1]],nc5[[i+1]]) 
}

# df[[1]] commute
nonpower_city_commute_alltime <- ndf[[1]]
# df[[2]] weather
nonpower_city_weather_alltime <- ndf[[2]]
# df[[3]] shopping
nonpower_city_shopping_alltime <- ndf[[3]]
# df[[5]] business trip
nonpower_city_business_alltime <- ndf[[5]]



###### FIFTH PART : ######

# % of persona/hobby/social media usage 
# offpeak power rider 
c11 <- list()
c12 <- list()
c13 <- list()
c14 <- list()
c15 <- list()

c21 <- list()
c22 <- list()
c23 <- list()
c24 <- list()
c25 <- list() 

c31 <- list()
c32 <- list()
c33 <- list()
c34 <- list()
c35 <- list()

c41 <- list()
c42 <- list()
c43 <- list()
c44 <- list()
c45 <- list()

c51 <- list()
c52 <- list()
c53 <- list()
c54 <- list()
c55 <- list()


for(i in 0:4){   
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  c11[[i+1]] <- power %>% filter(power[5*i+30] > 3,power$Home == "tpe" )  # commute
  c12[[i+1]] <- power %>% filter(power[5*i+31] > 3,power$Home == "tpe" )  # weather
  c13[[i+1]] <- power %>% filter(power[5*i+32] > 3,power$Home == "tpe" )  # shopping
  c14[[i+1]] <- power %>% filter(power[5*i+33] > 3,power$Home == "tpe" )  # dining
  c15[[i+1]] <- power %>% filter(power[5*i+34] > 3,power$Home == "tpe" )  # business trip

  c21[[i+1]] <- power %>% filter(power[5*i+30] > 3,power$Home == "ty" ) 
  c22[[i+1]] <- power %>% filter(power[5*i+31] > 3,power$Home == "ty" ) 
  c23[[i+1]] <- power %>% filter(power[5*i+32] > 3,power$Home == "ty" ) 
  c24[[i+1]] <- power %>% filter(power[5*i+33] > 3,power$Home == "ty" ) 
  c25[[i+1]] <- power %>% filter(power[5*i+34] > 3,power$Home == "ty" )

  c31[[i+1]] <- power %>% filter(power[5*i+30] > 3,power$Home == "hc" ) 
  c32[[i+1]] <- power %>% filter(power[5*i+31] > 3,power$Home == "hc" ) 
  c33[[i+1]] <- power %>% filter(power[5*i+32] > 3,power$Home == "hc" ) 
  c34[[i+1]] <- power %>% filter(power[5*i+33] > 3,power$Home == "hc" ) 
  c35[[i+1]] <- power %>% filter(power[5*i+34] > 3,power$Home == "hc" ) 
  
  c41[[i+1]] <- power %>% filter(power[5*i+30] > 3,power$Home == "tc" ) 
  c42[[i+1]] <- power %>% filter(power[5*i+31] > 3,power$Home == "tc" ) 
  c43[[i+1]] <- power %>% filter(power[5*i+32] > 3,power$Home == "tc" ) 
  c44[[i+1]] <- power %>% filter(power[5*i+33] > 3,power$Home == "tc" )
  c45[[i+1]] <- power %>% filter(power[5*i+34] > 3,power$Home == "tc" ) 

  c51[[i+1]] <- power %>% filter(power[5*i+30] > 3,power$Home == "kh" ) 
  c52[[i+1]] <- power %>% filter(power[5*i+31] > 3,power$Home == "kh" ) 
  c53[[i+1]] <- power %>% filter(power[5*i+32] > 3,power$Home == "kh" )
  c54[[i+1]] <- power %>% filter(power[5*i+33] > 3,power$Home == "kh" ) 
  c55[[i+1]] <- power %>% filter(power[5*i+34] > 3,power$Home == "kh" ) 

}


x11 <- list()
x12 <- list()
x13 <- list()
x14 <- list()
x15 <- list()
x11x <- list()
x12x <- list()
x13x <- list()
x14x <- list()
x15x <- list()
xx1 <- list()

x21 <- list()
x22 <- list()
x23 <- list()
x24 <- list()
x25 <- list() 
x21x <- list()
x22x <- list()
x23x <- list()
x24x <- list()
x25x <- list()
xx2 <- list()

x31 <- list()
x32 <- list()
x33 <- list()
x34 <- list()
x35 <- list()
x31x <- list()
x32x <- list()
x33x <- list()
x34x <- list()
x35x <- list()
xx3 <- list()

x41 <- list()
x42 <- list()
x43 <- list()
x44 <- list()
x45 <- list()
x41x <- list()
x42x <- list()
x43x <- list()
x44x <- list()
x45x <- list()
xx4 <- list()

x51 <- list()
x52 <- list()
x53 <- list()
x54 <- list()
x55 <- list()
x51x <- list()
x52x <- list()
x53x <- list()
x54x <- list()
x55x <- list()
xx5 <- list()

for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # Taipei & commute
  x11[[1+j*7]] <- filter(c11[[j+1]], c11[[j+1]][74] > 3) %>% nrow()
  x11[[2+j*7]] <- filter(c11[[j+1]], c11[[j+1]][75] > 3) %>% nrow()
  x11[[3+j*7]] <- filter(c11[[j+1]], c11[[j+1]][76] > 3) %>% nrow()
  x11[[4+j*7]] <- filter(c11[[j+1]], c11[[j+1]][77] > 3) %>% nrow()
  x11[[5+j*7]] <- filter(c11[[j+1]], c11[[j+1]][78] > 3) %>% nrow()
  x11[[6+j*7]] <- filter(c11[[j+1]], c11[[j+1]][79] > 3) %>% nrow()
  x11[[7+j*7]] <- filter(c11[[j+1]], c11[[j+1]][80] > 3) %>% nrow()
  
  x11[[36+j*10]] <- filter(c11[[j+1]], c11[[j+1]][55] > 3) %>% nrow()
  x11[[37+j*10]] <- filter(c11[[j+1]], c11[[j+1]][56] > 3) %>% nrow()
  x11[[38+j*10]] <- filter(c11[[j+1]], c11[[j+1]][57] > 3) %>% nrow()
  x11[[39+j*10]] <- filter(c11[[j+1]], c11[[j+1]][58] > 3) %>% nrow()
  x11[[40+j*10]] <- filter(c11[[j+1]], c11[[j+1]][59] > 3) %>% nrow()
  x11[[41+j*10]] <- filter(c11[[j+1]], c11[[j+1]][60] > 3) %>% nrow()
  x11[[42+j*10]] <- filter(c11[[j+1]], c11[[j+1]][61] > 3) %>% nrow()
  x11[[43+j*10]] <- filter(c11[[j+1]], c11[[j+1]][62] > 3) %>% nrow()
  x11[[44+j*10]] <- filter(c11[[j+1]], c11[[j+1]][63] > 3) %>% nrow()
  x11[[45+j*10]] <- filter(c11[[j+1]], c11[[j+1]][64] > 3) %>% nrow()

  x11[[86+j*9]] <- filter(c11[[j+1]], c11[[j+1]][65] > 3) %>% nrow()
  x11[[87+j*9]] <- filter(c11[[j+1]], c11[[j+1]][66] > 3) %>% nrow()
  x11[[88+j*9]] <- filter(c11[[j+1]], c11[[j+1]][67] > 3) %>% nrow()
  x11[[89+j*9]] <- filter(c11[[j+1]], c11[[j+1]][68] > 3) %>% nrow()
  x11[[90+j*9]] <- filter(c11[[j+1]], c11[[j+1]][69] > 3) %>% nrow()
  x11[[91+j*9]] <- filter(c11[[j+1]], c11[[j+1]][70] > 3) %>% nrow()
  x11[[92+j*9]] <- filter(c11[[j+1]], c11[[j+1]][71] > 3) %>% nrow()
  x11[[93+j*9]] <- filter(c11[[j+1]], c11[[j+1]][72] > 3) %>% nrow()
  x11[[94+j*9]] <- filter(c11[[j+1]], c11[[j+1]][73] > 3) %>% nrow()
    
  x11x[[j+1]] <- data.frame(a = x11[[1+j*7]], b = x11[[2+j*7]],c = x11[[3+j*7]],d = x11[[4+j*7]],e = x11[[5+j*7]],f = x11[[6+j*7]],g = x11[[7+j*7]],
                            h = x11[[36+j*10]],i = x11[[37+j*10]],k = x11[[38+j*10]],l = x11[[39+j*10]],m = x11[[40+j*10]],n = x11[[41+j*10]],o = x11[[42+j*10]],p = x11[[43+j*10]],q = x11[[44+j*10]],r = x11[[45+j*10]],
                            s = x11[[86+j*9]], t = x11[[87+j*9]],u = x11[[88+j*9]],v = x11[[89+j*9]], w = x11[[90+j*9]], x = x11[[91+j*9]],y = x11[[92+j*9]],z = x11[[93+j*9]],zz = x11[[94+j*9]]) 
  
  # Taipei & weather
  x12[[1+j*7]] <- filter(c12[[j+1]], c12[[j+1]][74] > 3) %>% nrow()
  x12[[2+j*7]] <- filter(c12[[j+1]], c12[[j+1]][75] > 3) %>% nrow()
  x12[[3+j*7]] <- filter(c12[[j+1]], c12[[j+1]][76] > 3) %>% nrow()
  x12[[4+j*7]] <- filter(c12[[j+1]], c12[[j+1]][77] > 3) %>% nrow()
  x12[[5+j*7]] <- filter(c12[[j+1]], c12[[j+1]][78] > 3) %>% nrow()
  x12[[6+j*7]] <- filter(c12[[j+1]], c12[[j+1]][79] > 3) %>% nrow()
  x12[[7+j*7]] <- filter(c12[[j+1]], c12[[j+1]][80] > 3) %>% nrow()
  
  x12[[36+j*10]] <- filter(c12[[j+1]], c12[[j+1]][55] > 3) %>% nrow()
  x12[[37+j*10]] <- filter(c12[[j+1]], c12[[j+1]][56] > 3) %>% nrow()
  x12[[38+j*10]] <- filter(c12[[j+1]], c12[[j+1]][57] > 3) %>% nrow()
  x12[[39+j*10]] <- filter(c12[[j+1]], c12[[j+1]][58] > 3) %>% nrow()
  x12[[40+j*10]] <- filter(c12[[j+1]], c12[[j+1]][59] > 3) %>% nrow()
  x12[[41+j*10]] <- filter(c12[[j+1]], c12[[j+1]][60] > 3) %>% nrow()
  x12[[42+j*10]] <- filter(c12[[j+1]], c12[[j+1]][61] > 3) %>% nrow()
  x12[[43+j*10]] <- filter(c12[[j+1]], c12[[j+1]][62] > 3) %>% nrow()
  x12[[44+j*10]] <- filter(c12[[j+1]], c12[[j+1]][63] > 3) %>% nrow()
  x12[[45+j*10]] <- filter(c12[[j+1]], c12[[j+1]][64] > 3) %>% nrow()
  
  x12[[86+j*9]] <- filter(c12[[j+1]], c12[[j+1]][65] > 3) %>% nrow()
  x12[[87+j*9]] <- filter(c12[[j+1]], c12[[j+1]][66] > 3) %>% nrow()
  x12[[88+j*9]] <- filter(c12[[j+1]], c12[[j+1]][67] > 3) %>% nrow()
  x12[[89+j*9]] <- filter(c12[[j+1]], c12[[j+1]][68] > 3) %>% nrow()
  x12[[90+j*9]] <- filter(c12[[j+1]], c12[[j+1]][69] > 3) %>% nrow()
  x12[[91+j*9]] <- filter(c12[[j+1]], c12[[j+1]][70] > 3) %>% nrow()
  x12[[92+j*9]] <- filter(c12[[j+1]], c12[[j+1]][71] > 3) %>% nrow()
  x12[[93+j*9]] <- filter(c12[[j+1]], c12[[j+1]][72] > 3) %>% nrow()
  x12[[94+j*9]] <- filter(c12[[j+1]], c12[[j+1]][73] > 3) %>% nrow()

  x12x[[j+1]] <- data.frame(a = x12[[1+j*7]],b = x12[[2+j*7]],c = x12[[3+j*7]],d = x12[[4+j*7]],e = x12[[5+j*7]],f = x12[[6+j*7]],g = x12[[7+j*7]],
                            h = x12[[36+j*10]],i = x12[[37+j*10]],k = x12[[38+j*10]],l = x12[[39+j*10]],m = x12[[40+j*10]],n = x12[[41+j*10]],o = x12[[42+j*10]],p = x12[[43+j*10]],q = x12[[44+j*10]],r = x12[[45+j*10]],
                            s = x12[[86+j*9]], t = x12[[87+j*9]], u = x12[[88+j*9]], v = x12[[89+j*9]], w = x12[[90+j*9]], x = x12[[91+j*9]], y = x12[[92+j*9]], z = x12[[93+j*9]],zz = x12[[94+j*9]])
  
  # Taipei & shopping
  x13[[1+j*7]] <- filter(c13[[j+1]], c13[[j+1]][74] > 3) %>% nrow()
  x13[[2+j*7]] <- filter(c13[[j+1]], c13[[j+1]][75] > 3) %>% nrow()
  x13[[3+j*7]] <- filter(c13[[j+1]], c13[[j+1]][76] > 3) %>% nrow()
  x13[[4+j*7]] <- filter(c13[[j+1]], c13[[j+1]][77] > 3) %>% nrow()
  x13[[5+j*7]] <- filter(c13[[j+1]], c13[[j+1]][78] > 3) %>% nrow()
  x13[[6+j*7]] <- filter(c13[[j+1]], c13[[j+1]][79] > 3) %>% nrow()
  x13[[7+j*7]] <- filter(c13[[j+1]], c13[[j+1]][80] > 3) %>% nrow()
  
  x13[[36+j*10]] <- filter(c13[[j+1]], c13[[j+1]][55] > 3) %>% nrow()
  x13[[37+j*10]] <- filter(c13[[j+1]], c13[[j+1]][56] > 3) %>% nrow()
  x13[[38+j*10]] <- filter(c13[[j+1]], c13[[j+1]][57] > 3) %>% nrow()
  x13[[39+j*10]] <- filter(c13[[j+1]], c13[[j+1]][58] > 3) %>% nrow()
  x13[[40+j*10]] <- filter(c13[[j+1]], c13[[j+1]][59] > 3) %>% nrow()
  x13[[41+j*10]] <- filter(c13[[j+1]], c13[[j+1]][60] > 3) %>% nrow()
  x13[[42+j*10]] <- filter(c13[[j+1]], c13[[j+1]][61] > 3) %>% nrow()
  x13[[43+j*10]] <- filter(c13[[j+1]], c13[[j+1]][62] > 3) %>% nrow()
  x13[[44+j*10]] <- filter(c13[[j+1]], c13[[j+1]][63] > 3) %>% nrow()
  x13[[45+j*10]] <- filter(c13[[j+1]], c13[[j+1]][64] > 3) %>% nrow()
  
  x13[[86+j*9]] <- filter(c13[[j+1]], c13[[j+1]][65] > 3) %>% nrow()
  x13[[87+j*9]] <- filter(c13[[j+1]], c13[[j+1]][66] > 3) %>% nrow()
  x13[[88+j*9]] <- filter(c13[[j+1]], c13[[j+1]][67] > 3) %>% nrow()
  x13[[89+j*9]] <- filter(c13[[j+1]], c13[[j+1]][68] > 3) %>% nrow()
  x13[[90+j*9]] <- filter(c13[[j+1]], c13[[j+1]][69] > 3) %>% nrow()
  x13[[91+j*9]] <- filter(c13[[j+1]], c13[[j+1]][70] > 3) %>% nrow()
  x13[[92+j*9]] <- filter(c13[[j+1]], c13[[j+1]][71] > 3) %>% nrow()
  x13[[93+j*9]] <- filter(c13[[j+1]], c13[[j+1]][72] > 3) %>% nrow()
  x13[[94+j*9]] <- filter(c13[[j+1]], c13[[j+1]][73] > 3) %>% nrow()
  
  x13x[[j+1]] <- data.frame(a = x13[[1+j*7]],b = x13[[2+j*7]],c = x13[[3+j*7]], d = x13[[4+j*7]],e = x13[[5+j*7]],f = x13[[6+j*7]],g = x13[[7+j*7]],
                            h = x13[[36+j*10]],i = x13[[37+j*10]],k = x13[[38+j*10]],l = x13[[39+j*10]],m = x13[[40+j*10]],n = x13[[41+j*10]],o = x13[[42+j*10]],p = x13[[43+j*10]],q = x13[[44+j*10]],r = x13[[45+j*10]],
                            s = x13[[86+j*9]], t = x13[[87+j*9]], u = x13[[88+j*9]], v = x13[[89+j*9]], w = x13[[90+j*9]], x = x13[[91+j*9]], y = x13[[92+j*9]], z = x13[[93+j*9]],zz = x13[[94+j*9]])
  
  
  # Taipei & dining
  x14[[1+j*7]] <- filter(c14[[j+1]], c14[[j+1]][74] > 3) %>% nrow()
  x14[[2+j*7]] <- filter(c14[[j+1]], c14[[j+1]][75] > 3) %>% nrow()
  x14[[3+j*7]] <- filter(c14[[j+1]], c14[[j+1]][76] > 3) %>% nrow()
  x14[[4+j*7]] <- filter(c14[[j+1]], c14[[j+1]][77] > 3) %>% nrow()
  x14[[5+j*7]] <- filter(c14[[j+1]], c14[[j+1]][78] > 3) %>% nrow()
  x14[[6+j*7]] <- filter(c14[[j+1]], c14[[j+1]][79] > 3) %>% nrow()
  x14[[7+j*7]] <- filter(c14[[j+1]], c14[[j+1]][80] > 3) %>% nrow()
  
  x14[[36+j*10]] <- filter(c14[[j+1]], c14[[j+1]][55] > 3) %>% nrow()
  x14[[37+j*10]] <- filter(c14[[j+1]], c14[[j+1]][56] > 3) %>% nrow()
  x14[[38+j*10]] <- filter(c14[[j+1]], c14[[j+1]][57] > 3) %>% nrow()
  x14[[39+j*10]] <- filter(c14[[j+1]], c14[[j+1]][58] > 3) %>% nrow()
  x14[[40+j*10]] <- filter(c14[[j+1]], c14[[j+1]][59] > 3) %>% nrow()
  x14[[41+j*10]] <- filter(c14[[j+1]], c14[[j+1]][60] > 3) %>% nrow()
  x14[[42+j*10]] <- filter(c14[[j+1]], c14[[j+1]][61] > 3) %>% nrow()
  x14[[43+j*10]] <- filter(c14[[j+1]], c14[[j+1]][62] > 3) %>% nrow()
  x14[[44+j*10]] <- filter(c14[[j+1]], c14[[j+1]][63] > 3) %>% nrow()
  x14[[45+j*10]] <- filter(c14[[j+1]], c14[[j+1]][64] > 3) %>% nrow()
  
  x14[[86+j*9]] <- filter(c14[[j+1]], c14[[j+1]][65] > 3) %>% nrow()
  x14[[87+j*9]] <- filter(c14[[j+1]], c14[[j+1]][66] > 3) %>% nrow()
  x14[[88+j*9]] <- filter(c14[[j+1]], c14[[j+1]][67] > 3) %>% nrow()
  x14[[89+j*9]] <- filter(c14[[j+1]], c14[[j+1]][68] > 3) %>% nrow()
  x14[[90+j*9]] <- filter(c14[[j+1]], c14[[j+1]][69] > 3) %>% nrow()
  x14[[91+j*9]] <- filter(c14[[j+1]], c14[[j+1]][70] > 3) %>% nrow()
  x14[[92+j*9]] <- filter(c14[[j+1]], c14[[j+1]][71] > 3) %>% nrow()
  x14[[93+j*9]] <- filter(c14[[j+1]], c14[[j+1]][72] > 3) %>% nrow()
  x14[[94+j*9]] <- filter(c14[[j+1]], c14[[j+1]][73] > 3) %>% nrow()
  
  x14x[[j+1]] <- data.frame(a = x14[[1+j*7]],b = x14[[2+j*7]],c = x14[[3+j*7]],d = x14[[4+j*7]],e = x14[[5+j*7]],f = x14[[6+j*7]],g = x14[[7+j*7]],
                            h = x14[[36+j*10]],i = x14[[37+j*10]],k = x14[[38+j*10]],l = x14[[39+j*10]],m = x14[[40+j*10]],n = x14[[41+j*10]],o = x14[[42+j*10]],p = x14[[43+j*10]],q = x14[[44+j*10]],r = x14[[45+j*10]],
                            s = x14[[86+j*9]], t = x14[[87+j*9]], u = x14[[88+j*9]], v = x14[[89+j*9]], w = x14[[90+j*9]], x = x14[[91+j*9]], y = x14[[92+j*9]], z = x14[[93+j*9]],zz = x14[[94+j*9]])
  
  
  # Taipei & business trip
  x15[[1+j*7]] <- filter(c15[[j+1]], c15[[j+1]][74] > 3) %>% nrow()
  x15[[2+j*7]] <- filter(c15[[j+1]], c15[[j+1]][75] > 3) %>% nrow()
  x15[[3+j*7]] <- filter(c15[[j+1]], c15[[j+1]][76] > 3) %>% nrow()
  x15[[4+j*7]] <- filter(c15[[j+1]], c15[[j+1]][77] > 3) %>% nrow()
  x15[[5+j*7]] <- filter(c15[[j+1]], c15[[j+1]][78] > 3) %>% nrow()
  x15[[6+j*7]] <- filter(c15[[j+1]], c15[[j+1]][79] > 3) %>% nrow()
  x15[[7+j*7]] <- filter(c15[[j+1]], c15[[j+1]][80] > 3) %>% nrow()
  
  x15[[36+j*10]] <- filter(c15[[j+1]], c15[[j+1]][55] > 3) %>% nrow()
  x15[[37+j*10]] <- filter(c15[[j+1]], c15[[j+1]][56] > 3) %>% nrow()
  x15[[38+j*10]] <- filter(c15[[j+1]], c15[[j+1]][57] > 3) %>% nrow()
  x15[[39+j*10]] <- filter(c15[[j+1]], c15[[j+1]][58] > 3) %>% nrow()
  x15[[40+j*10]] <- filter(c15[[j+1]], c15[[j+1]][59] > 3) %>% nrow()
  x15[[41+j*10]] <- filter(c15[[j+1]], c15[[j+1]][60] > 3) %>% nrow()
  x15[[42+j*10]] <- filter(c15[[j+1]], c15[[j+1]][61] > 3) %>% nrow()
  x15[[43+j*10]] <- filter(c15[[j+1]], c15[[j+1]][62] > 3) %>% nrow()
  x15[[44+j*10]] <- filter(c15[[j+1]], c15[[j+1]][63] > 3) %>% nrow()
  x15[[45+j*10]] <- filter(c15[[j+1]], c15[[j+1]][64] > 3) %>% nrow()
  
  x15[[86+j*9]] <- filter(c15[[j+1]], c15[[j+1]][65] > 3) %>% nrow()
  x15[[87+j*9]] <- filter(c15[[j+1]], c15[[j+1]][66] > 3) %>% nrow()
  x15[[88+j*9]] <- filter(c15[[j+1]], c15[[j+1]][67] > 3) %>% nrow()
  x15[[89+j*9]] <- filter(c15[[j+1]], c15[[j+1]][68] > 3) %>% nrow()
  x15[[90+j*9]] <- filter(c15[[j+1]], c15[[j+1]][69] > 3) %>% nrow()
  x15[[91+j*9]] <- filter(c15[[j+1]], c15[[j+1]][70] > 3) %>% nrow()
  x15[[92+j*9]] <- filter(c15[[j+1]], c15[[j+1]][71] > 3) %>% nrow()
  x15[[93+j*9]] <- filter(c15[[j+1]], c15[[j+1]][72] > 3) %>% nrow()
  x15[[94+j*9]] <- filter(c15[[j+1]], c15[[j+1]][73] > 3) %>% nrow()
  
  x15x[[j+1]] <- data.frame(a = x15[[1+j*7]],b = x15[[2+j*7]],c = x15[[3+j*7]],d = x15[[4+j*7]],e = x15[[5+j*7]],f = x15[[6+j*7]],g = x15[[7+j*7]],
                            h = x15[[36+j*10]],i = x15[[37+j*10]],k = x15[[38+j*10]],l = x15[[39+j*10]],m = x15[[40+j*10]],n = x15[[41+j*10]],o = x15[[42+j*10]],p = x15[[43+j*10]],q = x15[[44+j*10]],r = x15[[45+j*10]],
                            s = x15[[86+j*9]], t = x15[[87+j*9]], u = x15[[88+j*9]], v = x15[[89+j*9]], w = x15[[90+j*9]], x = x15[[91+j*9]], y = x15[[92+j*9]], z = x15[[93+j*9]],zz = x15[[94+j*9]])
  
  xx1[[j+1]] <- rbind(x11x[[j+1]],x12x[[j+1]],x13x[[j+1]],x14x[[j+1]],x15x[[j+1]])
  
}


for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # TY & commute
  x21[[1+j*7]] <- filter(c21[[j+1]], c21[[j+1]][74] > 3) %>% nrow()
  x21[[2+j*7]] <- filter(c21[[j+1]], c21[[j+1]][75] > 3) %>% nrow()
  x21[[3+j*7]] <- filter(c21[[j+1]], c21[[j+1]][76] > 3) %>% nrow()
  x21[[4+j*7]] <- filter(c21[[j+1]], c21[[j+1]][77] > 3) %>% nrow()
  x21[[5+j*7]] <- filter(c21[[j+1]], c21[[j+1]][78] > 3) %>% nrow()
  x21[[6+j*7]] <- filter(c21[[j+1]], c21[[j+1]][79] > 3) %>% nrow()
  x21[[7+j*7]] <- filter(c21[[j+1]], c21[[j+1]][80] > 3) %>% nrow()
  
  x21[[36+j*10]] <- filter(c21[[j+1]], c21[[j+1]][55] > 3) %>% nrow()
  x21[[37+j*10]] <- filter(c21[[j+1]], c21[[j+1]][56] > 3) %>% nrow()
  x21[[38+j*10]] <- filter(c21[[j+1]], c21[[j+1]][57] > 3) %>% nrow()
  x21[[39+j*10]] <- filter(c21[[j+1]], c21[[j+1]][58] > 3) %>% nrow()
  x21[[40+j*10]] <- filter(c21[[j+1]], c21[[j+1]][59] > 3) %>% nrow()
  x21[[41+j*10]] <- filter(c21[[j+1]], c21[[j+1]][60] > 3) %>% nrow()
  x21[[42+j*10]] <- filter(c21[[j+1]], c21[[j+1]][61] > 3) %>% nrow()
  x21[[43+j*10]] <- filter(c21[[j+1]], c21[[j+1]][62] > 3) %>% nrow()
  x21[[44+j*10]] <- filter(c21[[j+1]], c21[[j+1]][63] > 3) %>% nrow()
  x21[[45+j*10]] <- filter(c21[[j+1]], c21[[j+1]][64] > 3) %>% nrow()
  
  x21[[86+j*9]] <- filter(c21[[j+1]], c21[[j+1]][65] > 3) %>% nrow()
  x21[[87+j*9]] <- filter(c21[[j+1]], c21[[j+1]][66] > 3) %>% nrow()
  x21[[88+j*9]] <- filter(c21[[j+1]], c21[[j+1]][67] > 3) %>% nrow()
  x21[[89+j*9]] <- filter(c21[[j+1]], c21[[j+1]][68] > 3) %>% nrow()
  x21[[90+j*9]] <- filter(c21[[j+1]], c21[[j+1]][69] > 3) %>% nrow()
  x21[[91+j*9]] <- filter(c21[[j+1]], c21[[j+1]][70] > 3) %>% nrow()
  x21[[92+j*9]] <- filter(c21[[j+1]], c21[[j+1]][71] > 3) %>% nrow()
  x21[[93+j*9]] <- filter(c21[[j+1]], c21[[j+1]][72] > 3) %>% nrow()
  x21[[94+j*9]] <- filter(c21[[j+1]], c21[[j+1]][73] > 3) %>% nrow()
  
  x21x[[j+1]] <- data.frame(a = x21[[1+j*7]],b = x21[[2+j*7]],c = x21[[3+j*7]],d = x21[[4+j*7]],e = x21[[5+j*7]],f = x21[[6+j*7]],g = x21[[7+j*7]],
                            h = x21[[36+j*10]],i = x21[[37+j*10]],k = x21[[38+j*10]],l = x21[[39+j*10]],m = x21[[40+j*10]],n = x21[[41+j*10]],o = x21[[42+j*10]],p = x21[[43+j*10]],q = x21[[44+j*10]],r = x21[[45+j*10]],
                            s = x21[[86+j*9]], t = x21[[87+j*9]], u = x21[[88+j*9]], v = x21[[89+j*9]], w = x21[[90+j*9]], x = x21[[91+j*9]], y = x21[[92+j*9]], z = x21[[93+j*9]],zz = x21[[94+j*9]]) 
  
  # TY & weather
  x22[[1+j*7]] <- filter(c22[[j+1]], c22[[j+1]][74] > 3) %>% nrow()
  x22[[2+j*7]] <- filter(c22[[j+1]], c22[[j+1]][75] > 3) %>% nrow()
  x22[[3+j*7]] <- filter(c22[[j+1]], c22[[j+1]][76] > 3) %>% nrow()
  x22[[4+j*7]] <- filter(c22[[j+1]], c22[[j+1]][77] > 3) %>% nrow()
  x22[[5+j*7]] <- filter(c22[[j+1]], c22[[j+1]][78] > 3) %>% nrow()
  x22[[6+j*7]] <- filter(c22[[j+1]], c22[[j+1]][79] > 3) %>% nrow()
  x22[[7+j*7]] <- filter(c22[[j+1]], c22[[j+1]][80] > 3) %>% nrow()
  
  x22[[36+j*10]] <- filter(c22[[j+1]], c22[[j+1]][55] > 3) %>% nrow()
  x22[[37+j*10]] <- filter(c22[[j+1]], c22[[j+1]][56] > 3) %>% nrow()
  x22[[38+j*10]] <- filter(c22[[j+1]], c22[[j+1]][57] > 3) %>% nrow()
  x22[[39+j*10]] <- filter(c22[[j+1]], c22[[j+1]][58] > 3) %>% nrow()
  x22[[40+j*10]] <- filter(c22[[j+1]], c22[[j+1]][59] > 3) %>% nrow()
  x22[[41+j*10]] <- filter(c22[[j+1]], c22[[j+1]][60] > 3) %>% nrow()
  x22[[42+j*10]] <- filter(c22[[j+1]], c22[[j+1]][61] > 3) %>% nrow()
  x22[[43+j*10]] <- filter(c22[[j+1]], c22[[j+1]][62] > 3) %>% nrow()
  x22[[44+j*10]] <- filter(c22[[j+1]], c22[[j+1]][63] > 3) %>% nrow()
  x22[[45+j*10]] <- filter(c22[[j+1]], c22[[j+1]][64] > 3) %>% nrow()
  
  x22[[86+j*9]] <- filter(c22[[j+1]], c22[[j+1]][65] > 3) %>% nrow()
  x22[[87+j*9]] <- filter(c22[[j+1]], c22[[j+1]][66] > 3) %>% nrow()
  x22[[88+j*9]] <- filter(c22[[j+1]], c22[[j+1]][67] > 3) %>% nrow()
  x22[[89+j*9]] <- filter(c22[[j+1]], c22[[j+1]][68] > 3) %>% nrow()
  x22[[90+j*9]] <- filter(c22[[j+1]], c22[[j+1]][69] > 3) %>% nrow()
  x22[[91+j*9]] <- filter(c22[[j+1]], c22[[j+1]][70] > 3) %>% nrow()
  x22[[92+j*9]] <- filter(c22[[j+1]], c22[[j+1]][71] > 3) %>% nrow()
  x22[[93+j*9]] <- filter(c22[[j+1]], c22[[j+1]][72] > 3) %>% nrow()
  x22[[94+j*9]] <- filter(c22[[j+1]], c22[[j+1]][73] > 3) %>% nrow()

  x22x[[j+1]] <- data.frame(a = x22[[1+j*7]],b = x22[[2+j*7]],c = x22[[3+j*7]],d = x22[[4+j*7]],e = x22[[5+j*7]],f = x22[[6+j*7]],g = x22[[7+j*7]],
                            h = x22[[36+j*10]],i = x22[[37+j*10]],k = x22[[38+j*10]],l = x22[[39+j*10]],m = x22[[40+j*10]],n = x22[[41+j*10]],o = x22[[42+j*10]],p = x22[[43+j*10]],q = x22[[44+j*10]],r = x22[[45+j*10]],
                            s = x22[[86+j*9]], t = x22[[87+j*9]], u = x22[[88+j*9]], v = x22[[89+j*9]], w = x22[[90+j*9]], x = x22[[91+j*9]], y = x22[[92+j*9]], z = x22[[93+j*9]],zz = x22[[94+j*9]])
  
  # TY & shopping
  x23[[1+j*7]] <- filter(c23[[j+1]], c23[[j+1]][74] > 3) %>% nrow()
  x23[[2+j*7]] <- filter(c23[[j+1]], c23[[j+1]][75] > 3) %>% nrow()
  x23[[3+j*7]] <- filter(c23[[j+1]], c23[[j+1]][76] > 3) %>% nrow()
  x23[[4+j*7]] <- filter(c23[[j+1]], c23[[j+1]][77] > 3) %>% nrow()
  x23[[5+j*7]] <- filter(c23[[j+1]], c23[[j+1]][78] > 3) %>% nrow()
  x23[[6+j*7]] <- filter(c23[[j+1]], c23[[j+1]][79] > 3) %>% nrow()
  x23[[7+j*7]] <- filter(c23[[j+1]], c23[[j+1]][80] > 3) %>% nrow()
  
  x23[[36+j*10]] <- filter(c23[[j+1]], c23[[j+1]][55] > 3) %>% nrow()
  x23[[37+j*10]] <- filter(c23[[j+1]], c23[[j+1]][56] > 3) %>% nrow()
  x23[[38+j*10]] <- filter(c23[[j+1]], c23[[j+1]][57] > 3) %>% nrow()
  x23[[39+j*10]] <- filter(c23[[j+1]], c23[[j+1]][58] > 3) %>% nrow()
  x23[[40+j*10]] <- filter(c23[[j+1]], c23[[j+1]][59] > 3) %>% nrow()
  x23[[41+j*10]] <- filter(c23[[j+1]], c23[[j+1]][60] > 3) %>% nrow()
  x23[[42+j*10]] <- filter(c23[[j+1]], c23[[j+1]][61] > 3) %>% nrow()
  x23[[43+j*10]] <- filter(c23[[j+1]], c23[[j+1]][62] > 3) %>% nrow()
  x23[[44+j*10]] <- filter(c23[[j+1]], c23[[j+1]][63] > 3) %>% nrow()
  x23[[45+j*10]] <- filter(c23[[j+1]], c23[[j+1]][64] > 3) %>% nrow()
  
  x23[[86+j*9]] <- filter(c23[[j+1]], c23[[j+1]][65] > 3) %>% nrow()
  x23[[87+j*9]] <- filter(c23[[j+1]], c23[[j+1]][66] > 3) %>% nrow()
  x23[[88+j*9]] <- filter(c23[[j+1]], c23[[j+1]][67] > 3) %>% nrow()
  x23[[89+j*9]] <- filter(c23[[j+1]], c23[[j+1]][68] > 3) %>% nrow()
  x23[[90+j*9]] <- filter(c23[[j+1]], c23[[j+1]][69] > 3) %>% nrow()
  x23[[91+j*9]] <- filter(c23[[j+1]], c23[[j+1]][70] > 3) %>% nrow()
  x23[[92+j*9]] <- filter(c23[[j+1]], c23[[j+1]][71] > 3) %>% nrow()
  x23[[93+j*9]] <- filter(c23[[j+1]], c23[[j+1]][72] > 3) %>% nrow()
  x23[[94+j*9]] <- filter(c23[[j+1]], c23[[j+1]][73] > 3) %>% nrow()
  
  x23x[[j+1]] <- data.frame(a = x23[[1+j*7]],b = x23[[2+j*7]],c = x23[[3+j*7]],d = x23[[4+j*7]],e = x23[[5+j*7]],f = x23[[6+j*7]],g = x23[[7+j*7]],
                            h = x23[[36+j*10]],i = x23[[37+j*10]],k = x23[[38+j*10]],l = x23[[39+j*10]],m = x23[[40+j*10]],n = x23[[41+j*10]],o = x23[[42+j*10]],p = x23[[43+j*10]],q = x23[[44+j*10]],r = x23[[45+j*10]],
                            s = x23[[86+j*9]], t = x23[[87+j*9]], u = x23[[88+j*9]], v = x23[[89+j*9]], w = x23[[90+j*9]], x = x23[[91+j*9]], y = x23[[92+j*9]], z = x23[[93+j*9]],zz = x23[[94+j*9]])
  
  
  # TY & dining
  x24[[1+j*7]] <- filter(c24[[j+1]], c24[[j+1]][74] > 3) %>% nrow()
  x24[[2+j*7]] <- filter(c24[[j+1]], c24[[j+1]][75] > 3) %>% nrow()
  x24[[3+j*7]] <- filter(c24[[j+1]], c24[[j+1]][76] > 3) %>% nrow()
  x24[[4+j*7]] <- filter(c24[[j+1]], c24[[j+1]][77] > 3) %>% nrow()
  x24[[5+j*7]] <- filter(c24[[j+1]], c24[[j+1]][78] > 3) %>% nrow()
  x24[[6+j*7]] <- filter(c24[[j+1]], c24[[j+1]][79] > 3) %>% nrow()
  x24[[7+j*7]] <- filter(c24[[j+1]], c24[[j+1]][80] > 3) %>% nrow()
  
  x24[[36+j*10]] <- filter(c24[[j+1]], c24[[j+1]][55] > 3) %>% nrow()
  x24[[37+j*10]] <- filter(c24[[j+1]], c24[[j+1]][56] > 3) %>% nrow()
  x24[[38+j*10]] <- filter(c24[[j+1]], c24[[j+1]][57] > 3) %>% nrow()
  x24[[39+j*10]] <- filter(c24[[j+1]], c24[[j+1]][58] > 3) %>% nrow()
  x24[[40+j*10]] <- filter(c24[[j+1]], c24[[j+1]][59] > 3) %>% nrow()
  x24[[41+j*10]] <- filter(c24[[j+1]], c24[[j+1]][60] > 3) %>% nrow()
  x24[[42+j*10]] <- filter(c24[[j+1]], c24[[j+1]][61] > 3) %>% nrow()
  x24[[43+j*10]] <- filter(c24[[j+1]], c24[[j+1]][62] > 3) %>% nrow()
  x24[[44+j*10]] <- filter(c24[[j+1]], c24[[j+1]][63] > 3) %>% nrow()
  x24[[45+j*10]] <- filter(c24[[j+1]], c24[[j+1]][64] > 3) %>% nrow()
  
  x24[[86+j*9]] <- filter(c24[[j+1]], c24[[j+1]][65] > 3) %>% nrow()
  x24[[87+j*9]] <- filter(c24[[j+1]], c24[[j+1]][66] > 3) %>% nrow()
  x24[[88+j*9]] <- filter(c24[[j+1]], c24[[j+1]][67] > 3) %>% nrow()
  x24[[89+j*9]] <- filter(c24[[j+1]], c24[[j+1]][68] > 3) %>% nrow()
  x24[[90+j*9]] <- filter(c24[[j+1]], c24[[j+1]][69] > 3) %>% nrow()
  x24[[91+j*9]] <- filter(c24[[j+1]], c24[[j+1]][70] > 3) %>% nrow()
  x24[[92+j*9]] <- filter(c24[[j+1]], c24[[j+1]][71] > 3) %>% nrow()
  x24[[93+j*9]] <- filter(c24[[j+1]], c24[[j+1]][72] > 3) %>% nrow()
  x24[[94+j*9]] <- filter(c24[[j+1]], c24[[j+1]][73] > 3) %>% nrow()
  
  x24x[[j+1]] <- data.frame(a = x24[[1+j*7]],b = x24[[2+j*7]],c = x24[[3+j*7]],d = x24[[4+j*7]],e = x24[[5+j*7]],f = x24[[6+j*7]],g = x24[[7+j*7]],
                            h = x24[[36+j*10]],i = x24[[37+j*10]],k = x24[[38+j*10]],l = x24[[39+j*10]],m = x24[[40+j*10]],n = x24[[41+j*10]],o = x24[[42+j*10]],p = x24[[43+j*10]],q = x24[[44+j*10]],r = x24[[45+j*10]],
                            s = x24[[86+j*9]], t = x24[[87+j*9]], u = x24[[88+j*9]], v = x24[[89+j*9]], w = x24[[90+j*9]], x = x24[[91+j*9]], y = x24[[92+j*9]], z = x24[[93+j*9]],zz = x24[[94+j*9]])
  
  
  # TY & business trip
  x25[[1+j*7]] <- filter(c25[[j+1]], c25[[j+1]][74] > 3) %>% nrow()
  x25[[2+j*7]] <- filter(c25[[j+1]], c25[[j+1]][75] > 3) %>% nrow()
  x25[[3+j*7]] <- filter(c25[[j+1]], c25[[j+1]][76] > 3) %>% nrow()
  x25[[4+j*7]] <- filter(c25[[j+1]], c25[[j+1]][77] > 3) %>% nrow()
  x25[[5+j*7]] <- filter(c25[[j+1]], c25[[j+1]][78] > 3) %>% nrow()
  x25[[6+j*7]] <- filter(c25[[j+1]], c25[[j+1]][79] > 3) %>% nrow()
  x25[[7+j*7]] <- filter(c25[[j+1]], c25[[j+1]][80] > 3) %>% nrow()
  
  x25[[36+j*10]] <- filter(c25[[j+1]], c25[[j+1]][55] > 3) %>% nrow()
  x25[[37+j*10]] <- filter(c25[[j+1]], c25[[j+1]][56] > 3) %>% nrow()
  x25[[38+j*10]] <- filter(c25[[j+1]], c25[[j+1]][57] > 3) %>% nrow()
  x25[[39+j*10]] <- filter(c25[[j+1]], c25[[j+1]][58] > 3) %>% nrow()
  x25[[40+j*10]] <- filter(c25[[j+1]], c25[[j+1]][59] > 3) %>% nrow()
  x25[[41+j*10]] <- filter(c25[[j+1]], c25[[j+1]][60] > 3) %>% nrow()
  x25[[42+j*10]] <- filter(c25[[j+1]], c25[[j+1]][61] > 3) %>% nrow()
  x25[[43+j*10]] <- filter(c25[[j+1]], c25[[j+1]][62] > 3) %>% nrow()
  x25[[44+j*10]] <- filter(c25[[j+1]], c25[[j+1]][63] > 3) %>% nrow()
  x25[[45+j*10]] <- filter(c25[[j+1]], c25[[j+1]][64] > 3) %>% nrow()
  
  x25[[86+j*9]] <- filter(c25[[j+1]], c25[[j+1]][65] > 3) %>% nrow()
  x25[[87+j*9]] <- filter(c25[[j+1]], c25[[j+1]][66] > 3) %>% nrow()
  x25[[88+j*9]] <- filter(c25[[j+1]], c25[[j+1]][67] > 3) %>% nrow()
  x25[[89+j*9]] <- filter(c25[[j+1]], c25[[j+1]][68] > 3) %>% nrow()
  x25[[90+j*9]] <- filter(c25[[j+1]], c25[[j+1]][69] > 3) %>% nrow()
  x25[[91+j*9]] <- filter(c25[[j+1]], c25[[j+1]][70] > 3) %>% nrow()
  x25[[92+j*9]] <- filter(c25[[j+1]], c25[[j+1]][71] > 3) %>% nrow()
  x25[[93+j*9]] <- filter(c25[[j+1]], c25[[j+1]][72] > 3) %>% nrow()
  x25[[94+j*9]] <- filter(c25[[j+1]], c25[[j+1]][73] > 3) %>% nrow()
  
  x25x[[j+1]] <- data.frame(a = x25[[1+j*7]],b = x25[[2+j*7]],c = x25[[3+j*7]],d = x25[[4+j*7]],e = x25[[5+j*7]],f = x25[[6+j*7]],g = x25[[7+j*7]],
                            h = x25[[36+j*10]],i = x25[[37+j*10]],k = x25[[38+j*10]],l = x25[[39+j*10]],m = x25[[40+j*10]],n = x25[[41+j*10]],o = x25[[42+j*10]],p = x25[[43+j*10]],q = x25[[44+j*10]],r = x25[[45+j*10]],
                            s = x25[[86+j*9]], t = x25[[87+j*9]], u = x25[[88+j*9]], v = x25[[89+j*9]], w = x25[[90+j*9]], x = x25[[91+j*9]], y = x25[[92+j*9]], z = x25[[93+j*9]],zz = x25[[94+j*9]])
  
  xx2[[j+1]] <- rbind(x21x[[j+1]],x22x[[j+1]],x23x[[j+1]],x24x[[j+1]],x25x[[j+1]])
  
}




for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # HC & commute
  x31[[1+j*7]] <- filter(c31[[j+1]], c31[[j+1]][74] > 3) %>% nrow()
  x31[[2+j*7]] <- filter(c31[[j+1]], c31[[j+1]][75] > 3) %>% nrow()
  x31[[3+j*7]] <- filter(c31[[j+1]], c31[[j+1]][76] > 3) %>% nrow()
  x31[[4+j*7]] <- filter(c31[[j+1]], c31[[j+1]][77] > 3) %>% nrow()
  x31[[5+j*7]] <- filter(c31[[j+1]], c31[[j+1]][78] > 3) %>% nrow()
  x31[[6+j*7]] <- filter(c31[[j+1]], c31[[j+1]][79] > 3) %>% nrow()
  x31[[7+j*7]] <- filter(c31[[j+1]], c31[[j+1]][80] > 3) %>% nrow()
  
  x31[[36+j*10]] <- filter(c31[[j+1]], c31[[j+1]][55] > 3) %>% nrow()
  x31[[37+j*10]] <- filter(c31[[j+1]], c31[[j+1]][56] > 3) %>% nrow()
  x31[[38+j*10]] <- filter(c31[[j+1]], c31[[j+1]][57] > 3) %>% nrow()
  x31[[39+j*10]] <- filter(c31[[j+1]], c31[[j+1]][58] > 3) %>% nrow()
  x31[[40+j*10]] <- filter(c31[[j+1]], c31[[j+1]][59] > 3) %>% nrow()
  x31[[41+j*10]] <- filter(c31[[j+1]], c31[[j+1]][60] > 3) %>% nrow()
  x31[[42+j*10]] <- filter(c31[[j+1]], c31[[j+1]][61] > 3) %>% nrow()
  x31[[43+j*10]] <- filter(c31[[j+1]], c31[[j+1]][62] > 3) %>% nrow()
  x31[[44+j*10]] <- filter(c31[[j+1]], c31[[j+1]][63] > 3) %>% nrow()
  x31[[45+j*10]] <- filter(c31[[j+1]], c31[[j+1]][64] > 3) %>% nrow()
  
  x31[[86+j*9]] <- filter(c31[[j+1]], c31[[j+1]][65] > 3) %>% nrow()
  x31[[87+j*9]] <- filter(c31[[j+1]], c31[[j+1]][66] > 3) %>% nrow()
  x31[[88+j*9]] <- filter(c31[[j+1]], c31[[j+1]][67] > 3) %>% nrow()
  x31[[89+j*9]] <- filter(c31[[j+1]], c31[[j+1]][68] > 3) %>% nrow()
  x31[[90+j*9]] <- filter(c31[[j+1]], c31[[j+1]][69] > 3) %>% nrow()
  x31[[91+j*9]] <- filter(c31[[j+1]], c31[[j+1]][70] > 3) %>% nrow()
  x31[[92+j*9]] <- filter(c31[[j+1]], c31[[j+1]][71] > 3) %>% nrow()
  x31[[93+j*9]] <- filter(c31[[j+1]], c31[[j+1]][72] > 3) %>% nrow()
  x31[[94+j*9]] <- filter(c31[[j+1]], c31[[j+1]][73] > 3) %>% nrow()
  
  x31x[[j+1]] <- data.frame(a = x31[[1+j*7]],b = x31[[2+j*7]],c = x31[[3+j*7]],d = x31[[4+j*7]],e = x31[[5+j*7]],f = x31[[6+j*7]],g = x31[[7+j*7]],
                            h = x31[[36+j*10]],i = x31[[37+j*10]],k = x31[[38+j*10]],l = x31[[39+j*10]],m = x31[[40+j*10]],n = x31[[41+j*10]],o = x31[[42+j*10]],p = x31[[43+j*10]],q = x31[[44+j*10]],r = x31[[45+j*10]],
                            s = x31[[86+j*9]], t = x31[[87+j*9]], u = x31[[88+j*9]], v = x31[[89+j*9]], w = x31[[90+j*9]], x = x31[[91+j*9]], y = x31[[92+j*9]], z = x31[[93+j*9]],zz = x31[[94+j*9]]) 
  
  # HC & weather
  x32[[1+j*7]] <- filter(c32[[j+1]], c32[[j+1]][74] > 3) %>% nrow()
  x32[[2+j*7]] <- filter(c32[[j+1]], c32[[j+1]][75] > 3) %>% nrow()
  x32[[3+j*7]] <- filter(c32[[j+1]], c32[[j+1]][76] > 3) %>% nrow()
  x32[[4+j*7]] <- filter(c32[[j+1]], c32[[j+1]][77] > 3) %>% nrow()
  x32[[5+j*7]] <- filter(c32[[j+1]], c32[[j+1]][78] > 3) %>% nrow()
  x32[[6+j*7]] <- filter(c32[[j+1]], c32[[j+1]][79] > 3) %>% nrow()
  x32[[7+j*7]] <- filter(c32[[j+1]], c32[[j+1]][80] > 3) %>% nrow()
  
  x32[[36+j*10]] <- filter(c32[[j+1]], c32[[j+1]][55] > 3) %>% nrow()
  x32[[37+j*10]] <- filter(c32[[j+1]], c32[[j+1]][56] > 3) %>% nrow()
  x32[[38+j*10]] <- filter(c32[[j+1]], c32[[j+1]][57] > 3) %>% nrow()
  x32[[39+j*10]] <- filter(c32[[j+1]], c32[[j+1]][58] > 3) %>% nrow()
  x32[[40+j*10]] <- filter(c32[[j+1]], c32[[j+1]][59] > 3) %>% nrow()
  x32[[41+j*10]] <- filter(c32[[j+1]], c32[[j+1]][60] > 3) %>% nrow()
  x32[[42+j*10]] <- filter(c32[[j+1]], c32[[j+1]][61] > 3) %>% nrow()
  x32[[43+j*10]] <- filter(c32[[j+1]], c32[[j+1]][62] > 3) %>% nrow()
  x32[[44+j*10]] <- filter(c32[[j+1]], c32[[j+1]][63] > 3) %>% nrow()
  x32[[45+j*10]] <- filter(c32[[j+1]], c32[[j+1]][64] > 3) %>% nrow()
  
  x32[[86+j*9]] <- filter(c32[[j+1]], c32[[j+1]][65] > 3) %>% nrow()
  x32[[87+j*9]] <- filter(c32[[j+1]], c32[[j+1]][66] > 3) %>% nrow()
  x32[[88+j*9]] <- filter(c32[[j+1]], c32[[j+1]][67] > 3) %>% nrow()
  x32[[89+j*9]] <- filter(c32[[j+1]], c32[[j+1]][68] > 3) %>% nrow()
  x32[[90+j*9]] <- filter(c32[[j+1]], c32[[j+1]][69] > 3) %>% nrow()
  x32[[91+j*9]] <- filter(c32[[j+1]], c32[[j+1]][70] > 3) %>% nrow()
  x32[[92+j*9]] <- filter(c32[[j+1]], c32[[j+1]][71] > 3) %>% nrow()
  x32[[93+j*9]] <- filter(c32[[j+1]], c32[[j+1]][72] > 3) %>% nrow()
  x32[[94+j*9]] <- filter(c32[[j+1]], c32[[j+1]][73] > 3) %>% nrow()
  
  x32x[[j+1]] <- data.frame(a = x32[[1+j*7]],b = x32[[2+j*7]],c = x32[[3+j*7]],d = x32[[4+j*7]],e = x32[[5+j*7]],f = x32[[6+j*7]],g = x32[[7+j*7]],
                            h = x32[[36+j*10]],i = x32[[37+j*10]],k = x32[[38+j*10]],l = x32[[39+j*10]],m = x32[[40+j*10]],n = x32[[41+j*10]],o = x32[[42+j*10]],p = x32[[43+j*10]],q = x32[[44+j*10]],r = x32[[45+j*10]],
                            s = x32[[86+j*9]], t = x32[[87+j*9]], u = x32[[88+j*9]], v = x32[[89+j*9]], w = x32[[90+j*9]], x = x32[[91+j*9]], y = x32[[92+j*9]], z = x32[[93+j*9]],zz = x32[[94+j*9]])
  
  # HC & shopping
  x33[[1+j*7]] <- filter(c33[[j+1]], c33[[j+1]][74] > 3) %>% nrow()
  x33[[2+j*7]] <- filter(c33[[j+1]], c33[[j+1]][75] > 3) %>% nrow()
  x33[[3+j*7]] <- filter(c33[[j+1]], c33[[j+1]][76] > 3) %>% nrow()
  x33[[4+j*7]] <- filter(c33[[j+1]], c33[[j+1]][77] > 3) %>% nrow()
  x33[[5+j*7]] <- filter(c33[[j+1]], c33[[j+1]][78] > 3) %>% nrow()
  x33[[6+j*7]] <- filter(c33[[j+1]], c33[[j+1]][79] > 3) %>% nrow()
  x33[[7+j*7]] <- filter(c33[[j+1]], c33[[j+1]][80] > 3) %>% nrow()
  
  x33[[36+j*10]] <- filter(c33[[j+1]], c33[[j+1]][55] > 3) %>% nrow()
  x33[[37+j*10]] <- filter(c33[[j+1]], c33[[j+1]][56] > 3) %>% nrow()
  x33[[38+j*10]] <- filter(c33[[j+1]], c33[[j+1]][57] > 3) %>% nrow()
  x33[[39+j*10]] <- filter(c33[[j+1]], c33[[j+1]][58] > 3) %>% nrow()
  x33[[40+j*10]] <- filter(c33[[j+1]], c33[[j+1]][59] > 3) %>% nrow()
  x33[[41+j*10]] <- filter(c33[[j+1]], c33[[j+1]][60] > 3) %>% nrow()
  x33[[42+j*10]] <- filter(c33[[j+1]], c33[[j+1]][61] > 3) %>% nrow()
  x33[[43+j*10]] <- filter(c33[[j+1]], c33[[j+1]][62] > 3) %>% nrow()
  x33[[44+j*10]] <- filter(c33[[j+1]], c33[[j+1]][63] > 3) %>% nrow()
  x33[[45+j*10]] <- filter(c33[[j+1]], c33[[j+1]][64] > 3) %>% nrow()
  
  x33[[86+j*9]] <- filter(c33[[j+1]], c33[[j+1]][65] > 3) %>% nrow()
  x33[[87+j*9]] <- filter(c33[[j+1]], c33[[j+1]][66] > 3) %>% nrow()
  x33[[88+j*9]] <- filter(c33[[j+1]], c33[[j+1]][67] > 3) %>% nrow()
  x33[[89+j*9]] <- filter(c33[[j+1]], c33[[j+1]][68] > 3) %>% nrow()
  x33[[90+j*9]] <- filter(c33[[j+1]], c33[[j+1]][69] > 3) %>% nrow()
  x33[[91+j*9]] <- filter(c33[[j+1]], c33[[j+1]][70] > 3) %>% nrow()
  x33[[92+j*9]] <- filter(c33[[j+1]], c33[[j+1]][71] > 3) %>% nrow()
  x33[[93+j*9]] <- filter(c33[[j+1]], c33[[j+1]][72] > 3) %>% nrow()
  x33[[94+j*9]] <- filter(c33[[j+1]], c33[[j+1]][73] > 3) %>% nrow()
  
  x33x[[j+1]] <- data.frame(a = x33[[1+j*7]],b = x33[[2+j*7]],c = x33[[3+j*7]],d = x33[[4+j*7]],e = x33[[5+j*7]],f = x33[[6+j*7]],g = x33[[7+j*7]],
                            h = x33[[36+j*10]],i = x33[[37+j*10]],k = x33[[38+j*10]],l = x33[[39+j*10]],m = x33[[40+j*10]],n = x33[[41+j*10]],o = x33[[42+j*10]],p = x33[[43+j*10]],q = x33[[44+j*10]],r = x33[[45+j*10]],
                            s = x33[[86+j*9]], t = x33[[87+j*9]], u = x33[[88+j*9]], v = x33[[89+j*9]], w = x33[[90+j*9]], x = x33[[91+j*9]], y = x33[[92+j*9]], z = x33[[93+j*9]],zz = x33[[94+j*9]])
  
  
  # HC & dining
  x34[[1+j*7]] <- filter(c34[[j+1]], c34[[j+1]][74] > 3) %>% nrow()
  x34[[2+j*7]] <- filter(c34[[j+1]], c34[[j+1]][75] > 3) %>% nrow()
  x34[[3+j*7]] <- filter(c34[[j+1]], c34[[j+1]][76] > 3) %>% nrow()
  x34[[4+j*7]] <- filter(c34[[j+1]], c34[[j+1]][77] > 3) %>% nrow()
  x34[[5+j*7]] <- filter(c34[[j+1]], c34[[j+1]][78] > 3) %>% nrow()
  x34[[6+j*7]] <- filter(c34[[j+1]], c34[[j+1]][79] > 3) %>% nrow()
  x34[[7+j*7]] <- filter(c34[[j+1]], c34[[j+1]][80] > 3) %>% nrow()
  
  x34[[36+j*10]] <- filter(c34[[j+1]], c34[[j+1]][55] > 3) %>% nrow()
  x34[[37+j*10]] <- filter(c34[[j+1]], c34[[j+1]][56] > 3) %>% nrow()
  x34[[38+j*10]] <- filter(c34[[j+1]], c34[[j+1]][57] > 3) %>% nrow()
  x34[[39+j*10]] <- filter(c34[[j+1]], c34[[j+1]][58] > 3) %>% nrow()
  x34[[40+j*10]] <- filter(c34[[j+1]], c34[[j+1]][59] > 3) %>% nrow()
  x34[[41+j*10]] <- filter(c34[[j+1]], c34[[j+1]][60] > 3) %>% nrow()
  x34[[42+j*10]] <- filter(c34[[j+1]], c34[[j+1]][61] > 3) %>% nrow()
  x34[[43+j*10]] <- filter(c34[[j+1]], c34[[j+1]][62] > 3) %>% nrow()
  x34[[44+j*10]] <- filter(c34[[j+1]], c34[[j+1]][63] > 3) %>% nrow()
  x34[[45+j*10]] <- filter(c34[[j+1]], c34[[j+1]][64] > 3) %>% nrow()
  
  x34[[86+j*9]] <- filter(c34[[j+1]], c34[[j+1]][65] > 3) %>% nrow()
  x34[[87+j*9]] <- filter(c34[[j+1]], c34[[j+1]][66] > 3) %>% nrow()
  x34[[88+j*9]] <- filter(c34[[j+1]], c34[[j+1]][67] > 3) %>% nrow()
  x34[[89+j*9]] <- filter(c34[[j+1]], c34[[j+1]][68] > 3) %>% nrow()
  x34[[90+j*9]] <- filter(c34[[j+1]], c34[[j+1]][69] > 3) %>% nrow()
  x34[[91+j*9]] <- filter(c34[[j+1]], c34[[j+1]][70] > 3) %>% nrow()
  x34[[92+j*9]] <- filter(c34[[j+1]], c34[[j+1]][71] > 3) %>% nrow()
  x34[[93+j*9]] <- filter(c34[[j+1]], c34[[j+1]][72] > 3) %>% nrow()
  x34[[94+j*9]] <- filter(c34[[j+1]], c34[[j+1]][73] > 3) %>% nrow()
  
  x34x[[j+1]] <- data.frame(a = x34[[1+j*7]],b = x34[[2+j*7]],c = x34[[3+j*7]],d = x34[[4+j*7]],e = x34[[5+j*7]],f = x34[[6+j*7]],g = x34[[7+j*7]],
                            h = x34[[36+j*10]],i = x34[[37+j*10]],k = x34[[38+j*10]],l = x34[[39+j*10]],m = x34[[40+j*10]],n = x34[[41+j*10]],o = x34[[42+j*10]],p = x34[[43+j*10]],q = x34[[44+j*10]],r = x34[[45+j*10]],
                            s = x34[[86+j*9]], t = x34[[87+j*9]], u = x34[[88+j*9]], v = x34[[89+j*9]], w = x34[[90+j*9]], x = x34[[91+j*9]], y = x34[[92+j*9]], z = x34[[93+j*9]],zz = x34[[94+j*9]])
  
  
  # HC & business trip
  x35[[1+j*7]] <- filter(c35[[j+1]], c35[[j+1]][74] > 3) %>% nrow()
  x35[[2+j*7]] <- filter(c35[[j+1]], c35[[j+1]][75] > 3) %>% nrow()
  x35[[3+j*7]] <- filter(c35[[j+1]], c35[[j+1]][76] > 3) %>% nrow()
  x35[[4+j*7]] <- filter(c35[[j+1]], c35[[j+1]][77] > 3) %>% nrow()
  x35[[5+j*7]] <- filter(c35[[j+1]], c35[[j+1]][78] > 3) %>% nrow()
  x35[[6+j*7]] <- filter(c35[[j+1]], c35[[j+1]][79] > 3) %>% nrow()
  x35[[7+j*7]] <- filter(c35[[j+1]], c35[[j+1]][80] > 3) %>% nrow()
  
  x35[[36+j*10]] <- filter(c35[[j+1]], c35[[j+1]][55] > 3) %>% nrow()
  x35[[37+j*10]] <- filter(c35[[j+1]], c35[[j+1]][56] > 3) %>% nrow()
  x35[[38+j*10]] <- filter(c35[[j+1]], c35[[j+1]][57] > 3) %>% nrow()
  x35[[39+j*10]] <- filter(c35[[j+1]], c35[[j+1]][58] > 3) %>% nrow()
  x35[[40+j*10]] <- filter(c35[[j+1]], c35[[j+1]][59] > 3) %>% nrow()
  x35[[41+j*10]] <- filter(c35[[j+1]], c35[[j+1]][60] > 3) %>% nrow()
  x35[[42+j*10]] <- filter(c35[[j+1]], c35[[j+1]][61] > 3) %>% nrow()
  x35[[43+j*10]] <- filter(c35[[j+1]], c35[[j+1]][62] > 3) %>% nrow()
  x35[[44+j*10]] <- filter(c35[[j+1]], c35[[j+1]][63] > 3) %>% nrow()
  x35[[45+j*10]] <- filter(c35[[j+1]], c35[[j+1]][64] > 3) %>% nrow()
  
  x35[[86+j*9]] <- filter(c35[[j+1]], c35[[j+1]][65] > 3) %>% nrow()
  x35[[87+j*9]] <- filter(c35[[j+1]], c35[[j+1]][66] > 3) %>% nrow()
  x35[[88+j*9]] <- filter(c35[[j+1]], c35[[j+1]][67] > 3) %>% nrow()
  x35[[89+j*9]] <- filter(c35[[j+1]], c35[[j+1]][68] > 3) %>% nrow()
  x35[[90+j*9]] <- filter(c35[[j+1]], c35[[j+1]][69] > 3) %>% nrow()
  x35[[91+j*9]] <- filter(c35[[j+1]], c35[[j+1]][70] > 3) %>% nrow()
  x35[[92+j*9]] <- filter(c35[[j+1]], c35[[j+1]][71] > 3) %>% nrow()
  x35[[93+j*9]] <- filter(c35[[j+1]], c35[[j+1]][72] > 3) %>% nrow()
  x35[[94+j*9]] <- filter(c35[[j+1]], c35[[j+1]][73] > 3) %>% nrow()
  
  x35x[[j+1]] <- data.frame(a = x35[[1+j*7]],b = x35[[2+j*7]],c = x35[[3+j*7]],d = x35[[4+j*7]],e = x35[[5+j*7]],f = x35[[6+j*7]],g = x35[[7+j*7]],
                            h = x35[[36+j*10]],i = x35[[37+j*10]],k = x35[[38+j*10]],l = x35[[39+j*10]],m = x35[[40+j*10]],n = x35[[41+j*10]],o = x35[[42+j*10]],p = x35[[43+j*10]],q = x35[[44+j*10]],r = x35[[45+j*10]],
                            s = x35[[86+j*9]], t = x35[[87+j*9]], u = x35[[88+j*9]], v = x35[[89+j*9]], w = x35[[90+j*9]], x = x35[[91+j*9]], y = x35[[92+j*9]], z = x35[[93+j*9]],zz = x35[[94+j*9]])
  
  xx3[[j+1]] <- rbind(x31x[[j+1]],x32x[[j+1]],x33x[[j+1]],x34x[[j+1]],x35x[[j+1]])
  
}



for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # TC & commute
  x41[[1+j*7]] <- filter(c41[[j+1]], c41[[j+1]][74] > 3) %>% nrow()
  x41[[2+j*7]] <- filter(c41[[j+1]], c41[[j+1]][75] > 3) %>% nrow()
  x41[[3+j*7]] <- filter(c41[[j+1]], c41[[j+1]][76] > 3) %>% nrow()
  x41[[4+j*7]] <- filter(c41[[j+1]], c41[[j+1]][77] > 3) %>% nrow()
  x41[[5+j*7]] <- filter(c41[[j+1]], c41[[j+1]][78] > 3) %>% nrow()
  x41[[6+j*7]] <- filter(c41[[j+1]], c41[[j+1]][79] > 3) %>% nrow()
  x41[[7+j*7]] <- filter(c41[[j+1]], c41[[j+1]][80] > 3) %>% nrow()
  
  x41[[36+j*10]] <- filter(c41[[j+1]], c41[[j+1]][55] > 3) %>% nrow()
  x41[[37+j*10]] <- filter(c41[[j+1]], c41[[j+1]][56] > 3) %>% nrow()
  x41[[38+j*10]] <- filter(c41[[j+1]], c41[[j+1]][57] > 3) %>% nrow()
  x41[[39+j*10]] <- filter(c41[[j+1]], c41[[j+1]][58] > 3) %>% nrow()
  x41[[40+j*10]] <- filter(c41[[j+1]], c41[[j+1]][59] > 3) %>% nrow()
  x41[[41+j*10]] <- filter(c41[[j+1]], c41[[j+1]][60] > 3) %>% nrow()
  x41[[42+j*10]] <- filter(c41[[j+1]], c41[[j+1]][61] > 3) %>% nrow()
  x41[[43+j*10]] <- filter(c41[[j+1]], c41[[j+1]][62] > 3) %>% nrow()
  x41[[44+j*10]] <- filter(c41[[j+1]], c41[[j+1]][63] > 3) %>% nrow()
  x41[[45+j*10]] <- filter(c41[[j+1]], c41[[j+1]][64] > 3) %>% nrow()
  
  x41[[86+j*9]] <- filter(c41[[j+1]], c41[[j+1]][65] > 3) %>% nrow()
  x41[[87+j*9]] <- filter(c41[[j+1]], c41[[j+1]][66] > 3) %>% nrow()
  x41[[88+j*9]] <- filter(c41[[j+1]], c41[[j+1]][67] > 3) %>% nrow()
  x41[[89+j*9]] <- filter(c41[[j+1]], c41[[j+1]][68] > 3) %>% nrow()
  x41[[90+j*9]] <- filter(c41[[j+1]], c41[[j+1]][69] > 3) %>% nrow()
  x41[[91+j*9]] <- filter(c41[[j+1]], c41[[j+1]][70] > 3) %>% nrow()
  x41[[92+j*9]] <- filter(c41[[j+1]], c41[[j+1]][71] > 3) %>% nrow()
  x41[[93+j*9]] <- filter(c41[[j+1]], c41[[j+1]][72] > 3) %>% nrow()
  x41[[94+j*9]] <- filter(c41[[j+1]], c41[[j+1]][73] > 3) %>% nrow()
  
  x41x[[j+1]] <- data.frame(a = x41[[1+j*7]],b = x41[[2+j*7]],c = x41[[3+j*7]],d = x41[[4+j*7]],e = x41[[5+j*7]],f = x41[[6+j*7]],g = x41[[7+j*7]],
                            h = x41[[36+j*10]],i = x41[[37+j*10]],k = x41[[38+j*10]],l = x41[[39+j*10]],m = x41[[40+j*10]],n = x41[[41+j*10]],o = x41[[42+j*10]],p = x41[[43+j*10]],q = x41[[44+j*10]],r = x41[[45+j*10]],
                            s = x41[[86+j*9]], t = x41[[87+j*9]], u = x41[[88+j*9]], v = x41[[89+j*9]], w = x41[[90+j*9]], x = x41[[91+j*9]], y = x41[[92+j*9]], z = x41[[93+j*9]],zz = x41[[94+j*9]]) 
  
  # TC & weather
  x42[[1+j*7]] <- filter(c42[[j+1]], c42[[j+1]][74] > 3) %>% nrow()
  x42[[2+j*7]] <- filter(c42[[j+1]], c42[[j+1]][75] > 3) %>% nrow()
  x42[[3+j*7]] <- filter(c42[[j+1]], c42[[j+1]][76] > 3) %>% nrow()
  x42[[4+j*7]] <- filter(c42[[j+1]], c42[[j+1]][77] > 3) %>% nrow()
  x42[[5+j*7]] <- filter(c42[[j+1]], c42[[j+1]][78] > 3) %>% nrow()
  x42[[6+j*7]] <- filter(c42[[j+1]], c42[[j+1]][79] > 3) %>% nrow()
  x42[[7+j*7]] <- filter(c42[[j+1]], c42[[j+1]][80] > 3) %>% nrow()
  
  x42[[36+j*10]] <- filter(c42[[j+1]], c42[[j+1]][55] > 3) %>% nrow()
  x42[[37+j*10]] <- filter(c42[[j+1]], c42[[j+1]][56] > 3) %>% nrow()
  x42[[38+j*10]] <- filter(c42[[j+1]], c42[[j+1]][57] > 3) %>% nrow()
  x42[[39+j*10]] <- filter(c42[[j+1]], c42[[j+1]][58] > 3) %>% nrow()
  x42[[40+j*10]] <- filter(c42[[j+1]], c42[[j+1]][59] > 3) %>% nrow()
  x42[[41+j*10]] <- filter(c42[[j+1]], c42[[j+1]][60] > 3) %>% nrow()
  x42[[42+j*10]] <- filter(c42[[j+1]], c42[[j+1]][61] > 3) %>% nrow()
  x42[[43+j*10]] <- filter(c42[[j+1]], c42[[j+1]][62] > 3) %>% nrow()
  x42[[44+j*10]] <- filter(c42[[j+1]], c42[[j+1]][63] > 3) %>% nrow()
  x42[[45+j*10]] <- filter(c42[[j+1]], c42[[j+1]][64] > 3) %>% nrow()
  
  x42[[86+j*9]] <- filter(c42[[j+1]], c42[[j+1]][65] > 3) %>% nrow()
  x42[[87+j*9]] <- filter(c42[[j+1]], c42[[j+1]][66] > 3) %>% nrow()
  x42[[88+j*9]] <- filter(c42[[j+1]], c42[[j+1]][67] > 3) %>% nrow()
  x42[[89+j*9]] <- filter(c42[[j+1]], c42[[j+1]][68] > 3) %>% nrow()
  x42[[90+j*9]] <- filter(c42[[j+1]], c42[[j+1]][69] > 3) %>% nrow()
  x42[[91+j*9]] <- filter(c42[[j+1]], c42[[j+1]][70] > 3) %>% nrow()
  x42[[92+j*9]] <- filter(c42[[j+1]], c42[[j+1]][71] > 3) %>% nrow()
  x42[[93+j*9]] <- filter(c42[[j+1]], c42[[j+1]][72] > 3) %>% nrow()
  x42[[94+j*9]] <- filter(c42[[j+1]], c42[[j+1]][73] > 3) %>% nrow()
  
  x42x[[j+1]] <- data.frame(a = x42[[1+j*7]],b = x42[[2+j*7]],c = x42[[3+j*7]],d = x42[[4+j*7]],e = x42[[5+j*7]],f = x42[[6+j*7]],g = x42[[7+j*7]],
                            h = x42[[36+j*10]],i = x42[[37+j*10]],k = x42[[38+j*10]],l = x42[[39+j*10]],m = x42[[40+j*10]],n = x42[[41+j*10]],o = x42[[42+j*10]],p = x42[[43+j*10]],q = x42[[44+j*10]],r = x42[[45+j*10]],
                            s = x42[[86+j*9]], t = x42[[87+j*9]], u = x42[[88+j*9]], v = x42[[89+j*9]], w = x42[[90+j*9]], x = x42[[91+j*9]], y = x42[[92+j*9]], z = x42[[93+j*9]],zz = x42[[94+j*9]])
  
  # TC & shopping
  x43[[1+j*7]] <- filter(c43[[j+1]], c43[[j+1]][74] > 3) %>% nrow()
  x43[[2+j*7]] <- filter(c43[[j+1]], c43[[j+1]][75] > 3) %>% nrow()
  x43[[3+j*7]] <- filter(c43[[j+1]], c43[[j+1]][76] > 3) %>% nrow()
  x43[[4+j*7]] <- filter(c43[[j+1]], c43[[j+1]][77] > 3) %>% nrow()
  x43[[5+j*7]] <- filter(c43[[j+1]], c43[[j+1]][78] > 3) %>% nrow()
  x43[[6+j*7]] <- filter(c43[[j+1]], c43[[j+1]][79] > 3) %>% nrow()
  x43[[7+j*7]] <- filter(c43[[j+1]], c43[[j+1]][80] > 3) %>% nrow()
  
  x43[[36+j*10]] <- filter(c43[[j+1]], c43[[j+1]][55] > 3) %>% nrow()
  x43[[37+j*10]] <- filter(c43[[j+1]], c43[[j+1]][56] > 3) %>% nrow()
  x43[[38+j*10]] <- filter(c43[[j+1]], c43[[j+1]][57] > 3) %>% nrow()
  x43[[39+j*10]] <- filter(c43[[j+1]], c43[[j+1]][58] > 3) %>% nrow()
  x43[[40+j*10]] <- filter(c43[[j+1]], c43[[j+1]][59] > 3) %>% nrow()
  x43[[41+j*10]] <- filter(c43[[j+1]], c43[[j+1]][60] > 3) %>% nrow()
  x43[[42+j*10]] <- filter(c43[[j+1]], c43[[j+1]][61] > 3) %>% nrow()
  x43[[43+j*10]] <- filter(c43[[j+1]], c43[[j+1]][62] > 3) %>% nrow()
  x43[[44+j*10]] <- filter(c43[[j+1]], c43[[j+1]][63] > 3) %>% nrow()
  x43[[45+j*10]] <- filter(c43[[j+1]], c43[[j+1]][64] > 3) %>% nrow()
  
  x43[[86+j*9]] <- filter(c43[[j+1]], c43[[j+1]][65] > 3) %>% nrow()
  x43[[87+j*9]] <- filter(c43[[j+1]], c43[[j+1]][66] > 3) %>% nrow()
  x43[[88+j*9]] <- filter(c43[[j+1]], c43[[j+1]][67] > 3) %>% nrow()
  x43[[89+j*9]] <- filter(c43[[j+1]], c43[[j+1]][68] > 3) %>% nrow()
  x43[[90+j*9]] <- filter(c43[[j+1]], c43[[j+1]][69] > 3) %>% nrow()
  x43[[91+j*9]] <- filter(c43[[j+1]], c43[[j+1]][70] > 3) %>% nrow()
  x43[[92+j*9]] <- filter(c43[[j+1]], c43[[j+1]][71] > 3) %>% nrow()
  x43[[93+j*9]] <- filter(c43[[j+1]], c43[[j+1]][72] > 3) %>% nrow()
  x43[[94+j*9]] <- filter(c43[[j+1]], c43[[j+1]][73] > 3) %>% nrow()

  x43x[[j+1]] <- data.frame(a = x43[[1+j*7]],b = x43[[2+j*7]],c = x43[[3+j*7]],d = x43[[4+j*7]],e = x43[[5+j*7]],f = x43[[6+j*7]],g = x43[[7+j*7]],
                            h = x43[[36+j*10]],i = x43[[37+j*10]],k = x43[[38+j*10]],l = x43[[39+j*10]],m = x43[[40+j*10]],n = x43[[41+j*10]],o = x43[[42+j*10]],p = x43[[43+j*10]],q = x43[[44+j*10]],r = x43[[45+j*10]],
                            s = x43[[86+j*9]], t = x43[[87+j*9]], u = x43[[88+j*9]], v = x43[[89+j*9]], w = x43[[90+j*9]], x = x43[[91+j*9]], y = x43[[92+j*9]], z = x43[[93+j*9]],zz = x43[[94+j*9]])
  
  
  # TC & dining
  x44[[1+j*7]] <- filter(c44[[j+1]], c44[[j+1]][74] > 3) %>% nrow()
  x44[[2+j*7]] <- filter(c44[[j+1]], c44[[j+1]][75] > 3) %>% nrow()
  x44[[3+j*7]] <- filter(c44[[j+1]], c44[[j+1]][76] > 3) %>% nrow()
  x44[[4+j*7]] <- filter(c44[[j+1]], c44[[j+1]][77] > 3) %>% nrow()
  x44[[5+j*7]] <- filter(c44[[j+1]], c44[[j+1]][78] > 3) %>% nrow()
  x44[[6+j*7]] <- filter(c44[[j+1]], c44[[j+1]][79] > 3) %>% nrow()
  x44[[7+j*7]] <- filter(c44[[j+1]], c44[[j+1]][80] > 3) %>% nrow()
  
  x44[[36+j*10]] <- filter(c44[[j+1]], c44[[j+1]][55] > 3) %>% nrow()
  x44[[37+j*10]] <- filter(c44[[j+1]], c44[[j+1]][56] > 3) %>% nrow()
  x44[[38+j*10]] <- filter(c44[[j+1]], c44[[j+1]][57] > 3) %>% nrow()
  x44[[39+j*10]] <- filter(c44[[j+1]], c44[[j+1]][58] > 3) %>% nrow()
  x44[[40+j*10]] <- filter(c44[[j+1]], c44[[j+1]][59] > 3) %>% nrow()
  x44[[41+j*10]] <- filter(c44[[j+1]], c44[[j+1]][60] > 3) %>% nrow()
  x44[[42+j*10]] <- filter(c44[[j+1]], c44[[j+1]][61] > 3) %>% nrow()
  x44[[43+j*10]] <- filter(c44[[j+1]], c44[[j+1]][62] > 3) %>% nrow()
  x44[[44+j*10]] <- filter(c44[[j+1]], c44[[j+1]][63] > 3) %>% nrow()
  x44[[45+j*10]] <- filter(c44[[j+1]], c44[[j+1]][64] > 3) %>% nrow()

  x44[[86+j*9]] <- filter(c44[[j+1]], c44[[j+1]][65] > 3) %>% nrow()
  x44[[87+j*9]] <- filter(c44[[j+1]], c44[[j+1]][66] > 3) %>% nrow()
  x44[[88+j*9]] <- filter(c44[[j+1]], c44[[j+1]][67] > 3) %>% nrow()
  x44[[89+j*9]] <- filter(c44[[j+1]], c44[[j+1]][68] > 3) %>% nrow()
  x44[[90+j*9]] <- filter(c44[[j+1]], c44[[j+1]][69] > 3) %>% nrow()
  x44[[91+j*9]] <- filter(c44[[j+1]], c44[[j+1]][70] > 3) %>% nrow()
  x44[[92+j*9]] <- filter(c44[[j+1]], c44[[j+1]][71] > 3) %>% nrow()
  x44[[93+j*9]] <- filter(c44[[j+1]], c44[[j+1]][72] > 3) %>% nrow()
  x44[[94+j*9]] <- filter(c44[[j+1]], c44[[j+1]][73] > 3) %>% nrow()

  x44x[[j+1]] <- data.frame(a = x44[[1+j*7]],b = x44[[2+j*7]],c = x44[[3+j*7]],d = x44[[4+j*7]],e = x44[[5+j*7]],f = x44[[6+j*7]],g = x44[[7+j*7]],
                            h = x44[[36+j*10]],i = x44[[37+j*10]],k = x44[[38+j*10]],l = x44[[39+j*10]],m = x44[[40+j*10]],n = x44[[41+j*10]],o = x44[[42+j*10]],p = x44[[43+j*10]],q = x44[[44+j*10]],r = x44[[45+j*10]],
                            s = x44[[86+j*9]], t = x44[[87+j*9]], u = x44[[88+j*9]], v = x44[[89+j*9]], w = x44[[90+j*9]], x = x44[[91+j*9]], y = x44[[92+j*9]], z = x44[[93+j*9]],zz = x44[[94+j*9]])
  
  
  # TC & business trip
  x45[[1+j*7]] <- filter(c45[[j+1]], c45[[j+1]][74] > 3) %>% nrow()
  x45[[2+j*7]] <- filter(c45[[j+1]], c45[[j+1]][75] > 3) %>% nrow()
  x45[[3+j*7]] <- filter(c45[[j+1]], c45[[j+1]][76] > 3) %>% nrow()
  x45[[4+j*7]] <- filter(c45[[j+1]], c45[[j+1]][77] > 3) %>% nrow()
  x45[[5+j*7]] <- filter(c45[[j+1]], c45[[j+1]][78] > 3) %>% nrow()
  x45[[6+j*7]] <- filter(c45[[j+1]], c45[[j+1]][79] > 3) %>% nrow()
  x45[[7+j*7]] <- filter(c45[[j+1]], c45[[j+1]][80] > 3) %>% nrow()
  
  x45[[36+j*10]] <- filter(c45[[j+1]], c45[[j+1]][55] > 3) %>% nrow()
  x45[[37+j*10]] <- filter(c45[[j+1]], c45[[j+1]][56] > 3) %>% nrow()
  x45[[38+j*10]] <- filter(c45[[j+1]], c45[[j+1]][57] > 3) %>% nrow()
  x45[[39+j*10]] <- filter(c45[[j+1]], c45[[j+1]][58] > 3) %>% nrow()
  x45[[40+j*10]] <- filter(c45[[j+1]], c45[[j+1]][59] > 3) %>% nrow()
  x45[[41+j*10]] <- filter(c45[[j+1]], c45[[j+1]][60] > 3) %>% nrow()
  x45[[42+j*10]] <- filter(c45[[j+1]], c45[[j+1]][61] > 3) %>% nrow()
  x45[[43+j*10]] <- filter(c45[[j+1]], c45[[j+1]][62] > 3) %>% nrow()
  x45[[44+j*10]] <- filter(c45[[j+1]], c45[[j+1]][63] > 3) %>% nrow()
  x45[[45+j*10]] <- filter(c45[[j+1]], c45[[j+1]][64] > 3) %>% nrow()
  
  x45[[86+j*9]] <- filter(c45[[j+1]], c45[[j+1]][65] > 3) %>% nrow()
  x45[[87+j*9]] <- filter(c45[[j+1]], c45[[j+1]][66] > 3) %>% nrow()
  x45[[88+j*9]] <- filter(c45[[j+1]], c45[[j+1]][67] > 3) %>% nrow()
  x45[[89+j*9]] <- filter(c45[[j+1]], c45[[j+1]][68] > 3) %>% nrow()
  x45[[90+j*9]] <- filter(c45[[j+1]], c45[[j+1]][69] > 3) %>% nrow()
  x45[[91+j*9]] <- filter(c45[[j+1]], c45[[j+1]][70] > 3) %>% nrow()
  x45[[92+j*9]] <- filter(c45[[j+1]], c45[[j+1]][71] > 3) %>% nrow()
  x45[[93+j*9]] <- filter(c45[[j+1]], c45[[j+1]][72] > 3) %>% nrow()
  x45[[94+j*9]] <- filter(c45[[j+1]], c45[[j+1]][73] > 3) %>% nrow()
  
  x45x[[j+1]] <- data.frame(a = x45[[1+j*7]],b = x45[[2+j*7]],c = x45[[3+j*7]],d = x45[[4+j*7]],e = x45[[5+j*7]],f = x45[[6+j*7]],g = x45[[7+j*7]],
                            h = x45[[36+j*10]],i = x45[[37+j*10]],k = x45[[38+j*10]],l = x45[[39+j*10]],m = x45[[40+j*10]],n = x45[[41+j*10]],o = x45[[42+j*10]],p = x45[[43+j*10]],q = x45[[44+j*10]],r = x45[[45+j*10]],
                            s = x45[[86+j*9]], t = x45[[87+j*9]], u = x45[[88+j*9]], v = x45[[89+j*9]], w = x45[[90+j*9]], x = x45[[91+j*9]], y = x45[[92+j*9]], z = x45[[93+j*9]],zz = x45[[94+j*9]])
  
  xx4[[j+1]] <- rbind(x41x[[j+1]],x42x[[j+1]],x43x[[j+1]],x44x[[j+1]],x45x[[j+1]])
  
}



for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # KH & commute
  x51[[1+j*7]] <- filter(c51[[j+1]], c51[[j+1]][74] > 3) %>% nrow()
  x51[[2+j*7]] <- filter(c51[[j+1]], c51[[j+1]][75] > 3) %>% nrow()
  x51[[3+j*7]] <- filter(c51[[j+1]], c51[[j+1]][76] > 3) %>% nrow()
  x51[[4+j*7]] <- filter(c51[[j+1]], c51[[j+1]][77] > 3) %>% nrow()
  x51[[5+j*7]] <- filter(c51[[j+1]], c51[[j+1]][78] > 3) %>% nrow()
  x51[[6+j*7]] <- filter(c51[[j+1]], c51[[j+1]][79] > 3) %>% nrow()
  x51[[7+j*7]] <- filter(c51[[j+1]], c51[[j+1]][80] > 3) %>% nrow()
  
  x51[[36+j*10]] <- filter(c51[[j+1]], c51[[j+1]][55] > 3) %>% nrow()
  x51[[37+j*10]] <- filter(c51[[j+1]], c51[[j+1]][56] > 3) %>% nrow()
  x51[[38+j*10]] <- filter(c51[[j+1]], c51[[j+1]][57] > 3) %>% nrow()
  x51[[39+j*10]] <- filter(c51[[j+1]], c51[[j+1]][58] > 3) %>% nrow()
  x51[[40+j*10]] <- filter(c51[[j+1]], c51[[j+1]][59] > 3) %>% nrow()
  x51[[41+j*10]] <- filter(c51[[j+1]], c51[[j+1]][60] > 3) %>% nrow()
  x51[[42+j*10]] <- filter(c51[[j+1]], c51[[j+1]][61] > 3) %>% nrow()
  x51[[43+j*10]] <- filter(c51[[j+1]], c51[[j+1]][62] > 3) %>% nrow()
  x51[[44+j*10]] <- filter(c51[[j+1]], c51[[j+1]][63] > 3) %>% nrow()
  x51[[45+j*10]] <- filter(c51[[j+1]], c51[[j+1]][64] > 3) %>% nrow()
  
  x51[[86+j*9]] <- filter(c51[[j+1]], c51[[j+1]][65] > 3) %>% nrow()
  x51[[87+j*9]] <- filter(c51[[j+1]], c51[[j+1]][66] > 3) %>% nrow()
  x51[[88+j*9]] <- filter(c51[[j+1]], c51[[j+1]][67] > 3) %>% nrow()
  x51[[89+j*9]] <- filter(c51[[j+1]], c51[[j+1]][68] > 3) %>% nrow()
  x51[[90+j*9]] <- filter(c51[[j+1]], c51[[j+1]][69] > 3) %>% nrow()
  x51[[91+j*9]] <- filter(c51[[j+1]], c51[[j+1]][70] > 3) %>% nrow()
  x51[[92+j*9]] <- filter(c51[[j+1]], c51[[j+1]][71] > 3) %>% nrow()
  x51[[93+j*9]] <- filter(c51[[j+1]], c51[[j+1]][72] > 3) %>% nrow()
  x51[[94+j*9]] <- filter(c51[[j+1]], c51[[j+1]][73] > 3) %>% nrow()
  
  x51x[[j+1]] <- data.frame(a = x51[[1+j*7]],b = x51[[2+j*7]],c = x51[[3+j*7]],d = x51[[4+j*7]],e = x51[[5+j*7]],f = x51[[6+j*7]],g = x51[[7+j*7]],
                            h = x51[[36+j*10]],i = x51[[37+j*10]],k = x51[[38+j*10]],l = x51[[39+j*10]],m = x51[[40+j*10]],n = x51[[41+j*10]],o = x51[[42+j*10]],p = x51[[43+j*10]],q = x51[[44+j*10]],r = x51[[45+j*10]],
                            s = x51[[86+j*9]], t = x51[[87+j*9]], u = x51[[88+j*9]], v = x51[[89+j*9]], w = x51[[90+j*9]], x = x51[[91+j*9]], y = x51[[92+j*9]], z = x51[[93+j*9]],zz = x51[[94+j*9]]) 
  
  # KH & weather
  x52[[1+j*7]] <- filter(c52[[j+1]], c52[[j+1]][74] > 3) %>% nrow()
  x52[[2+j*7]] <- filter(c52[[j+1]], c52[[j+1]][75] > 3) %>% nrow()
  x52[[3+j*7]] <- filter(c52[[j+1]], c52[[j+1]][76] > 3) %>% nrow()
  x52[[4+j*7]] <- filter(c52[[j+1]], c52[[j+1]][77] > 3) %>% nrow()
  x52[[5+j*7]] <- filter(c52[[j+1]], c52[[j+1]][78] > 3) %>% nrow()
  x52[[6+j*7]] <- filter(c52[[j+1]], c52[[j+1]][79] > 3) %>% nrow()
  x52[[7+j*7]] <- filter(c52[[j+1]], c52[[j+1]][80] > 3) %>% nrow()

  x52[[36+j*10]] <- filter(c52[[j+1]], c52[[j+1]][55] > 3) %>% nrow()
  x52[[37+j*10]] <- filter(c52[[j+1]], c52[[j+1]][56] > 3) %>% nrow()
  x52[[38+j*10]] <- filter(c52[[j+1]], c52[[j+1]][57] > 3) %>% nrow()
  x52[[39+j*10]] <- filter(c52[[j+1]], c52[[j+1]][58] > 3) %>% nrow()
  x52[[40+j*10]] <- filter(c52[[j+1]], c52[[j+1]][59] > 3) %>% nrow()
  x52[[41+j*10]] <- filter(c52[[j+1]], c52[[j+1]][60] > 3) %>% nrow()
  x52[[42+j*10]] <- filter(c52[[j+1]], c52[[j+1]][61] > 3) %>% nrow()
  x52[[43+j*10]] <- filter(c52[[j+1]], c52[[j+1]][62] > 3) %>% nrow()
  x52[[44+j*10]] <- filter(c52[[j+1]], c52[[j+1]][63] > 3) %>% nrow()
  x52[[45+j*10]] <- filter(c52[[j+1]], c52[[j+1]][64] > 3) %>% nrow()
  
  x52[[86+j*9]] <- filter(c52[[j+1]], c52[[j+1]][65] > 3) %>% nrow()
  x52[[87+j*9]] <- filter(c52[[j+1]], c52[[j+1]][66] > 3) %>% nrow()
  x52[[88+j*9]] <- filter(c52[[j+1]], c52[[j+1]][67] > 3) %>% nrow()
  x52[[89+j*9]] <- filter(c52[[j+1]], c52[[j+1]][68] > 3) %>% nrow()
  x52[[90+j*9]] <- filter(c52[[j+1]], c52[[j+1]][69] > 3) %>% nrow()
  x52[[91+j*9]] <- filter(c52[[j+1]], c52[[j+1]][70] > 3) %>% nrow()
  x52[[92+j*9]] <- filter(c52[[j+1]], c52[[j+1]][71] > 3) %>% nrow()
  x52[[93+j*9]] <- filter(c52[[j+1]], c52[[j+1]][72] > 3) %>% nrow()
  x52[[94+j*9]] <- filter(c52[[j+1]], c52[[j+1]][73] > 3) %>% nrow()
  
  x52x[[j+1]] <- data.frame(a = x52[[1+j*7]],b = x52[[2+j*7]],c = x52[[3+j*7]],d = x52[[4+j*7]],e = x52[[5+j*7]],f = x52[[6+j*7]],g = x52[[7+j*7]],
                            h = x52[[36+j*10]],i = x52[[37+j*10]],k = x52[[38+j*10]],l = x52[[39+j*10]],m = x52[[40+j*10]],n = x52[[41+j*10]],o = x52[[42+j*10]],p = x52[[43+j*10]],q = x52[[44+j*10]],r = x52[[45+j*10]],
                            s = x52[[86+j*9]], t = x52[[87+j*9]], u = x52[[88+j*9]], v = x52[[89+j*9]], w = x52[[90+j*9]], x = x52[[91+j*9]], y = x52[[92+j*9]], z = x52[[93+j*9]],zz = x52[[94+j*9]])
  
  # KH & shopping
  x53[[1+j*7]] <- filter(c53[[j+1]], c53[[j+1]][74] > 3) %>% nrow()
  x53[[2+j*7]] <- filter(c53[[j+1]], c53[[j+1]][75] > 3) %>% nrow()
  x53[[3+j*7]] <- filter(c53[[j+1]], c53[[j+1]][76] > 3) %>% nrow()
  x53[[4+j*7]] <- filter(c53[[j+1]], c53[[j+1]][77] > 3) %>% nrow()
  x53[[5+j*7]] <- filter(c53[[j+1]], c53[[j+1]][78] > 3) %>% nrow()
  x53[[6+j*7]] <- filter(c53[[j+1]], c53[[j+1]][79] > 3) %>% nrow()
  x53[[7+j*7]] <- filter(c53[[j+1]], c53[[j+1]][80] > 3) %>% nrow()
  
  x53[[36+j*10]] <- filter(c53[[j+1]], c53[[j+1]][55] > 3) %>% nrow()
  x53[[37+j*10]] <- filter(c53[[j+1]], c53[[j+1]][56] > 3) %>% nrow()
  x53[[38+j*10]] <- filter(c53[[j+1]], c53[[j+1]][57] > 3) %>% nrow()
  x53[[39+j*10]] <- filter(c53[[j+1]], c53[[j+1]][58] > 3) %>% nrow()
  x53[[40+j*10]] <- filter(c53[[j+1]], c53[[j+1]][59] > 3) %>% nrow()
  x53[[41+j*10]] <- filter(c53[[j+1]], c53[[j+1]][60] > 3) %>% nrow()
  x53[[42+j*10]] <- filter(c53[[j+1]], c53[[j+1]][61] > 3) %>% nrow()
  x53[[43+j*10]] <- filter(c53[[j+1]], c53[[j+1]][62] > 3) %>% nrow()
  x53[[44+j*10]] <- filter(c53[[j+1]], c53[[j+1]][63] > 3) %>% nrow()
  x53[[45+j*10]] <- filter(c53[[j+1]], c53[[j+1]][64] > 3) %>% nrow()
  
  x53[[86+j*9]] <- filter(c53[[j+1]], c53[[j+1]][65] > 3) %>% nrow()
  x53[[87+j*9]] <- filter(c53[[j+1]], c53[[j+1]][66] > 3) %>% nrow()
  x53[[88+j*9]] <- filter(c53[[j+1]], c53[[j+1]][67] > 3) %>% nrow()
  x53[[89+j*9]] <- filter(c53[[j+1]], c53[[j+1]][68] > 3) %>% nrow()
  x53[[90+j*9]] <- filter(c53[[j+1]], c53[[j+1]][69] > 3) %>% nrow()
  x53[[91+j*9]] <- filter(c53[[j+1]], c53[[j+1]][70] > 3) %>% nrow()
  x53[[92+j*9]] <- filter(c53[[j+1]], c53[[j+1]][71] > 3) %>% nrow()
  x53[[93+j*9]] <- filter(c53[[j+1]], c53[[j+1]][72] > 3) %>% nrow()
  x53[[94+j*9]] <- filter(c53[[j+1]], c53[[j+1]][73] > 3) %>% nrow()

  x53x[[j+1]] <- data.frame(a = x53[[1+j*7]],b = x53[[2+j*7]],c = x53[[3+j*7]],d = x53[[4+j*7]],e = x53[[5+j*7]],f = x53[[6+j*7]],g = x53[[7+j*7]],
                            h = x53[[36+j*10]],i = x53[[37+j*10]],k = x53[[38+j*10]],l = x53[[39+j*10]],m = x53[[40+j*10]],n = x53[[41+j*10]],o = x53[[42+j*10]],p = x53[[43+j*10]],q = x53[[44+j*10]],r = x53[[45+j*10]],
                            s = x53[[86+j*9]], t = x53[[87+j*9]], u = x53[[88+j*9]], v = x53[[89+j*9]], w = x53[[90+j*9]], x = x53[[91+j*9]], y = x53[[92+j*9]], z = x53[[93+j*9]],zz = x53[[94+j*9]])
  
  
  # KH & dining
  x54[[1+j*7]] <- filter(c54[[j+1]], c54[[j+1]][74] > 3) %>% nrow()
  x54[[2+j*7]] <- filter(c54[[j+1]], c54[[j+1]][75] > 3) %>% nrow()
  x54[[3+j*7]] <- filter(c54[[j+1]], c54[[j+1]][76] > 3) %>% nrow()
  x54[[4+j*7]] <- filter(c54[[j+1]], c54[[j+1]][77] > 3) %>% nrow()
  x54[[5+j*7]] <- filter(c54[[j+1]], c54[[j+1]][78] > 3) %>% nrow()
  x54[[6+j*7]] <- filter(c54[[j+1]], c54[[j+1]][79] > 3) %>% nrow()
  x54[[7+j*7]] <- filter(c54[[j+1]], c54[[j+1]][80] > 3) %>% nrow()
  
  x54[[36+j*10]] <- filter(c54[[j+1]], c54[[j+1]][55] > 3) %>% nrow()
  x54[[37+j*10]] <- filter(c54[[j+1]], c54[[j+1]][56] > 3) %>% nrow()
  x54[[38+j*10]] <- filter(c54[[j+1]], c54[[j+1]][57] > 3) %>% nrow()
  x54[[39+j*10]] <- filter(c54[[j+1]], c54[[j+1]][58] > 3) %>% nrow()
  x54[[40+j*10]] <- filter(c54[[j+1]], c54[[j+1]][59] > 3) %>% nrow()
  x54[[41+j*10]] <- filter(c54[[j+1]], c54[[j+1]][60] > 3) %>% nrow()
  x54[[42+j*10]] <- filter(c54[[j+1]], c54[[j+1]][61] > 3) %>% nrow()
  x54[[43+j*10]] <- filter(c54[[j+1]], c54[[j+1]][62] > 3) %>% nrow()
  x54[[44+j*10]] <- filter(c54[[j+1]], c54[[j+1]][63] > 3) %>% nrow()
  x54[[45+j*10]] <- filter(c54[[j+1]], c54[[j+1]][64] > 3) %>% nrow()
  
  x54[[86+j*9]] <- filter(c54[[j+1]], c54[[j+1]][65] > 3) %>% nrow()
  x54[[87+j*9]] <- filter(c54[[j+1]], c54[[j+1]][66] > 3) %>% nrow()
  x54[[88+j*9]] <- filter(c54[[j+1]], c54[[j+1]][67] > 3) %>% nrow()
  x54[[89+j*9]] <- filter(c54[[j+1]], c54[[j+1]][68] > 3) %>% nrow()
  x54[[90+j*9]] <- filter(c54[[j+1]], c54[[j+1]][69] > 3) %>% nrow()
  x54[[91+j*9]] <- filter(c54[[j+1]], c54[[j+1]][70] > 3) %>% nrow()
  x54[[92+j*9]] <- filter(c54[[j+1]], c54[[j+1]][71] > 3) %>% nrow()
  x54[[93+j*9]] <- filter(c54[[j+1]], c54[[j+1]][72] > 3) %>% nrow()
  x54[[94+j*9]] <- filter(c54[[j+1]], c54[[j+1]][73] > 3) %>% nrow()
  
  x54x[[j+1]] <- data.frame(a = x54[[1+j*7]],b = x54[[2+j*7]],c = x54[[3+j*7]],d = x54[[4+j*7]],e = x54[[5+j*7]],f = x54[[6+j*7]],g = x54[[7+j*7]],
                            h = x54[[36+j*10]],i = x54[[37+j*10]],k = x54[[38+j*10]],l = x54[[39+j*10]],m = x54[[40+j*10]],n = x54[[41+j*10]],o = x54[[42+j*10]],p = x54[[43+j*10]],q = x54[[44+j*10]],r = x54[[45+j*10]],
                            s = x54[[86+j*9]], t = x54[[87+j*9]], u = x54[[88+j*9]], v = x54[[89+j*9]], w = x54[[90+j*9]], x = x54[[91+j*9]], y = x54[[92+j*9]], z = x54[[93+j*9]],zz = x54[[94+j*9]])
  
  
  # KH & business trip
  x55[[1+j*7]] <- filter(c55[[j+1]], c55[[j+1]][74] > 3) %>% nrow()
  x55[[2+j*7]] <- filter(c55[[j+1]], c55[[j+1]][75] > 3) %>% nrow()
  x55[[3+j*7]] <- filter(c55[[j+1]], c55[[j+1]][76] > 3) %>% nrow()
  x55[[4+j*7]] <- filter(c55[[j+1]], c55[[j+1]][77] > 3) %>% nrow()
  x55[[5+j*7]] <- filter(c55[[j+1]], c55[[j+1]][78] > 3) %>% nrow()
  x55[[6+j*7]] <- filter(c55[[j+1]], c55[[j+1]][79] > 3) %>% nrow()
  x55[[7+j*7]] <- filter(c55[[j+1]], c55[[j+1]][80] > 3) %>% nrow()
  
  x55[[36+j*10]] <- filter(c55[[j+1]], c55[[j+1]][55] > 3) %>% nrow()
  x55[[37+j*10]] <- filter(c55[[j+1]], c55[[j+1]][56] > 3) %>% nrow()
  x55[[38+j*10]] <- filter(c55[[j+1]], c55[[j+1]][57] > 3) %>% nrow()
  x55[[39+j*10]] <- filter(c55[[j+1]], c55[[j+1]][58] > 3) %>% nrow()
  x55[[40+j*10]] <- filter(c55[[j+1]], c55[[j+1]][59] > 3) %>% nrow()
  x55[[41+j*10]] <- filter(c55[[j+1]], c55[[j+1]][60] > 3) %>% nrow()
  x55[[42+j*10]] <- filter(c55[[j+1]], c55[[j+1]][61] > 3) %>% nrow()
  x55[[43+j*10]] <- filter(c55[[j+1]], c55[[j+1]][62] > 3) %>% nrow()
  x55[[44+j*10]] <- filter(c55[[j+1]], c55[[j+1]][63] > 3) %>% nrow()
  x55[[45+j*10]] <- filter(c55[[j+1]], c55[[j+1]][64] > 3) %>% nrow()
  
  x55[[86+j*9]] <- filter(c55[[j+1]], c55[[j+1]][65] > 3) %>% nrow()
  x55[[87+j*9]] <- filter(c55[[j+1]], c55[[j+1]][66] > 3) %>% nrow()
  x55[[88+j*9]] <- filter(c55[[j+1]], c55[[j+1]][67] > 3) %>% nrow()
  x55[[89+j*9]] <- filter(c55[[j+1]], c55[[j+1]][68] > 3) %>% nrow()
  x55[[90+j*9]] <- filter(c55[[j+1]], c55[[j+1]][69] > 3) %>% nrow()
  x55[[91+j*9]] <- filter(c55[[j+1]], c55[[j+1]][70] > 3) %>% nrow()
  x55[[92+j*9]] <- filter(c55[[j+1]], c55[[j+1]][71] > 3) %>% nrow()
  x55[[93+j*9]] <- filter(c55[[j+1]], c55[[j+1]][72] > 3) %>% nrow()
  x55[[94+j*9]] <- filter(c55[[j+1]], c55[[j+1]][73] > 3) %>% nrow()
  
  x55x[[j+1]] <- data.frame(a = x55[[1+j*7]],b = x55[[2+j*7]],c = x55[[3+j*7]],d = x55[[4+j*7]],e = x55[[5+j*7]],f = x55[[6+j*7]],g = x55[[7+j*7]],
                            h = x55[[36+j*10]],i = x55[[37+j*10]],k = x55[[38+j*10]],l = x55[[39+j*10]],m = x55[[40+j*10]],n = x55[[41+j*10]],o = x55[[42+j*10]],p = x55[[43+j*10]],q = x55[[44+j*10]],r = x55[[45+j*10]],
                            s = x55[[86+j*9]], t = x55[[87+j*9]], u = x55[[88+j*9]], v = x55[[89+j*9]], w = x55[[90+j*9]], x = x55[[91+j*9]], y = x55[[92+j*9]], z = x55[[93+j*9]],zz = x55[[94+j*9]])
  
  xx5[[j+1]] <- rbind(x51x[[j+1]],x52x[[j+1]],x53x[[j+1]],x54x[[j+1]],x55x[[j+1]])
  
}

# tpe : own car usage in different occasion
xx1[[1]] <- xx1[[1]][-4,]
xx1[[1]]
# tpe : public transportation usage in different occasion
xx1[[2]] <- xx1[[2]][-4,]
xx1[[2]]
# tpe : sharebike usage in different occasion
xx1[[3]] <- xx1[[3]][-4,]
xx1[[3]]
# tpe : app dispatcher usage in different occasion
xx1[[4]] <- xx1[[4]][-4,]
xx1[[4]]
# tpe : road-hail taxi usage in different occasion
xx1[[5]] <- xx1[[5]][-4,]
xx1[[5]]


# ty : own car usage in different occasion
xx2[[1]] <- xx2[[1]][-4,]
xx2[[1]]
# ty : public transportation usage in different occasion
xx2[[2]] <- xx2[[2]][-4,]
xx2[[2]]
# ty : sharebike usage in different occasion
xx2[[3]] <- xx2[[3]][-4,]
xx2[[3]]
# ty : app dispatcher usage in different occasion
xx2[[4]] <- xx2[[4]][-4,]
xx2[[4]]
# ty : road-hail taxi usage in different occasion
xx2[[5]] <- xx2[[5]][-4,]
xx2[[5]]


# hc : own car usage in different occasion
xx3[[1]] <- xx3[[1]][-4,]
xx3[[1]]
# hc : public transportation usage in different occasion
xx3[[2]] <- xx3[[2]][-4,]
xx3[[2]]
# hc : sharebike usage in different occasion
xx3[[3]] <- xx3[[3]][-4,]
xx3[[3]]
# hc : app dispatcher usage in different occasion
xx3[[4]] <- xx3[[4]][-4,]
xx3[[4]]
# hc : road-hail taxi usage in different occasion
xx3[[5]] <- xx3[[5]][-4,]
xx3[[5]]


# tc : own car usage in different occasion
xx4[[1]] <- xx4[[1]][-4,]
xx4[[1]]
# tc : public transportation usage in different occasion
xx4[[2]] <- xx4[[2]][-4,]
xx4[[2]]
# tc : sharebike usage in different occasion
xx4[[3]] <- xx4[[3]][-4,]
xx4[[3]]
# tc : app dispatcher usage in different occasion
xx4[[4]] <- xx4[[4]][-4,]
xx4[[4]]
# tc : road-hail taxi usage in different occasion
xx4[[5]] <- xx4[[5]][-4,]
xx4[[5]]


# kh : own car usage in different occasion
xx5[[1]] <- xx5[[1]][-4,]
xx5[[1]]
# kh : public transportation usage in different occasion
xx5[[2]] <- xx5[[2]][-4,]
xx5[[2]]
# kh : sharebike usage in different occasion
xx5[[3]] <- xx5[[3]][-4,]
xx5[[3]]
# kh : app dispatcher usage in different occasion
xx5[[4]] <- xx5[[4]][-4,]
xx5[[4]]
# kh : road-hail taxi usage in different occasion
xx5[[5]] <- xx5[[5]][-4,]
xx5[[5]]


# change to percentage
for(k in 1:5){
  xx1[[k]][1,] <- xx1[[k]][1,]/nrow(c11[[k]])
  xx1[[k]][2,] <- xx1[[k]][2,]/nrow(c12[[k]])
  xx1[[k]][3,] <- xx1[[k]][3,]/nrow(c13[[k]])
  xx1[[k]][4,] <- xx1[[k]][4,]/nrow(c15[[k]])
  
  xx2[[k]][1,] <- xx2[[k]][1,]/nrow(c21[[k]])
  xx2[[k]][2,] <- xx2[[k]][2,]/nrow(c22[[k]])
  xx2[[k]][3,] <- xx2[[k]][3,]/nrow(c23[[k]])
  xx2[[k]][4,] <- xx2[[k]][4,]/nrow(c25[[k]])
  
  xx3[[k]][1,] <- xx3[[k]][1,]/nrow(c31[[k]])
  xx3[[k]][2,] <- xx3[[k]][2,]/nrow(c32[[k]])
  xx3[[k]][3,] <- xx3[[k]][3,]/nrow(c33[[k]])
  xx3[[k]][4,] <- xx3[[k]][4,]/nrow(c35[[k]])
  
  xx4[[k]][1,] <- xx4[[k]][1,]/nrow(c41[[k]])
  xx4[[k]][2,] <- xx4[[k]][2,]/nrow(c42[[k]])
  xx4[[k]][3,] <- xx4[[k]][3,]/nrow(c43[[k]])
  xx4[[k]][4,] <- xx4[[k]][4,]/nrow(c45[[k]])
  
  xx5[[k]][1,] <- xx5[[k]][1,]/nrow(c51[[k]])
  xx5[[k]][2,] <- xx5[[k]][2,]/nrow(c52[[k]])
  xx5[[k]][3,] <- xx5[[k]][3,]/nrow(c53[[k]])
  xx5[[k]][4,] <- xx5[[k]][4,]/nrow(c55[[k]])
}

# change column name
for(p in 1:5){
  colnames(xx1[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx2[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx3[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx4[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx5[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
}

# individualize
# tpe : own car usage in different occasion
power_tpe_own_persona <- xx1[[1]]
# tpe : public transportation usage in different occasion
power_tpe_pub_persona <- xx1[[2]]
# tpe : sharebike usage in different occasion
power_tpe_sb_persona <- xx1[[3]]
# tpe : app dispatcher usage in different occasion
power_tpe_app_persona <- xx1[[4]]
# tpe : road-hail taxi usage in different occasion
power_tpe_tx_persona <- xx1[[5]]


# ty : own car usage in different occasion
power_ty_own_persona <- xx2[[1]]
# ty : public transportation usage in different occasion
power_ty_pub_persona <- xx2[[2]]
# ty : sharebike usage in different occasion
power_ty_sb_persona <- xx2[[3]]
# ty : app dispatcher usage in different occasion
power_ty_app_persona <- xx2[[4]]
# ty : road-hail taxi usage in different occasion
power_ty_tx_persona <- xx2[[5]]


# hc : own car usage in different occasion
power_hc_own_persona <- xx3[[1]]
# hc : public transportation usage in different occasion
power_hc_pub_persona <- xx3[[2]]
# hc : sharebike usage in different occasion
power_hc_sb_persona <- xx3[[3]]
# hc : app dispatcher usage in different occasion
power_hc_app_persona <- xx3[[4]]
# hc : road-hail taxi usage in different occasion
power_hc_tx_persona <- xx3[[5]]


# tc : own car usage in different occasion
power_tc_own_persona <- xx4[[1]]
# tc : public transportation usage in different occasion
power_tc_pub_persona <- xx4[[2]]
# tc : sharebike usage in different occasion
power_tc_sb_persona <- xx4[[3]]
# tc : app dispatcher usage in different occasion
power_tc_app_persona <- xx4[[4]]
# tc : road-hail taxi usage in different occasion
power_tc_tx_persona <- xx4[[5]]


# kh : own car usage in different occasion
power_kh_own_persona <- xx5[[1]]
# kh : public transportation usage in different occasion
power_kh_pub_persona <- xx5[[2]]
# kh : sharebike usage in different occasion
power_kh_sb_persona <- xx5[[3]]
# kh : app dispatcher usage in different occasion
power_kh_app_persona <- xx5[[4]]
# kh : road-hail taxi usage in different occasion
power_kh_tx_persona <- xx5[[5]]





###### SIXTH PART : ######

# % of persona/hobby/social media usage 
# offpeak nonpower rider 
c11 <- list()
c12 <- list()
c13 <- list()
c14 <- list()
c15 <- list()

c21 <- list()
c22 <- list()
c23 <- list()
c24 <- list()
c25 <- list() 

c31 <- list()
c32 <- list()
c33 <- list()
c34 <- list()
c35 <- list()

c41 <- list()
c42 <- list()
c43 <- list()
c44 <- list()
c45 <- list()

c51 <- list()
c52 <- list()
c53 <- list()
c54 <- list()
c55 <- list()


for(i in 0:4){   
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  c11[[i+1]] <- nonpower %>% filter(nonpower[5*i+30] > 3,nonpower$Home == "tpe" )  # commute
  c12[[i+1]] <- nonpower %>% filter(nonpower[5*i+31] > 3,nonpower$Home == "tpe" )  # weather
  c13[[i+1]] <- nonpower %>% filter(nonpower[5*i+32] > 3,nonpower$Home == "tpe" )  # shopping
  c14[[i+1]] <- nonpower %>% filter(nonpower[5*i+33] > 3,nonpower$Home == "tpe" )  # dining
  c15[[i+1]] <- nonpower %>% filter(nonpower[5*i+34] > 3,nonpower$Home == "tpe" )  # business trip
  
  c21[[i+1]] <- nonpower %>% filter(nonpower[5*i+30] > 3,nonpower$Home == "ty" ) 
  c22[[i+1]] <- nonpower %>% filter(nonpower[5*i+31] > 3,nonpower$Home == "ty" ) 
  c23[[i+1]] <- nonpower %>% filter(nonpower[5*i+32] > 3,nonpower$Home == "ty" ) 
  c24[[i+1]] <- nonpower %>% filter(nonpower[5*i+33] > 3,nonpower$Home == "ty" ) 
  c25[[i+1]] <- nonpower %>% filter(nonpower[5*i+34] > 3,nonpower$Home == "ty" )
  
  c31[[i+1]] <- nonpower %>% filter(nonpower[5*i+30] > 3,nonpower$Home == "hc" ) 
  c32[[i+1]] <- nonpower %>% filter(nonpower[5*i+31] > 3,nonpower$Home == "hc" ) 
  c33[[i+1]] <- nonpower %>% filter(nonpower[5*i+32] > 3,nonpower$Home == "hc" ) 
  c34[[i+1]] <- nonpower %>% filter(nonpower[5*i+33] > 3,nonpower$Home == "hc" ) 
  c35[[i+1]] <- nonpower %>% filter(nonpower[5*i+34] > 3,nonpower$Home == "hc" ) 
  
  c41[[i+1]] <- nonpower %>% filter(nonpower[5*i+30] > 3,nonpower$Home == "tc" ) 
  c42[[i+1]] <- nonpower %>% filter(nonpower[5*i+31] > 3,nonpower$Home == "tc" ) 
  c43[[i+1]] <- nonpower %>% filter(nonpower[5*i+32] > 3,nonpower$Home == "tc" ) 
  c44[[i+1]] <- nonpower %>% filter(nonpower[5*i+33] > 3,nonpower$Home == "tc" )
  c45[[i+1]] <- nonpower %>% filter(nonpower[5*i+34] > 3,nonpower$Home == "tc" ) 
  
  c51[[i+1]] <- nonpower %>% filter(nonpower[5*i+30] > 3,nonpower$Home == "kh" ) 
  c52[[i+1]] <- nonpower %>% filter(nonpower[5*i+31] > 3,nonpower$Home == "kh" ) 
  c53[[i+1]] <- nonpower %>% filter(nonpower[5*i+32] > 3,nonpower$Home == "kh" )
  c54[[i+1]] <- nonpower %>% filter(nonpower[5*i+33] > 3,nonpower$Home == "kh" ) 
  c55[[i+1]] <- nonpower %>% filter(nonpower[5*i+34] > 3,nonpower$Home == "kh" ) 
  
}


x11 <- list()
x12 <- list()
x13 <- list()
x14 <- list()
x15 <- list()
x11x <- list()
x12x <- list()
x13x <- list()
x14x <- list()
x15x <- list()
xx1 <- list()

x21 <- list()
x22 <- list()
x23 <- list()
x24 <- list()
x25 <- list() 
x21x <- list()
x22x <- list()
x23x <- list()
x24x <- list()
x25x <- list()
xx2 <- list()

x31 <- list()
x32 <- list()
x33 <- list()
x34 <- list()
x35 <- list()
x31x <- list()
x32x <- list()
x33x <- list()
x34x <- list()
x35x <- list()
xx3 <- list()

x41 <- list()
x42 <- list()
x43 <- list()
x44 <- list()
x45 <- list()
x41x <- list()
x42x <- list()
x43x <- list()
x44x <- list()
x45x <- list()
xx4 <- list()

x51 <- list()
x52 <- list()
x53 <- list()
x54 <- list()
x55 <- list()
x51x <- list()
x52x <- list()
x53x <- list()
x54x <- list()
x55x <- list()
xx5 <- list()

for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # Taipei & commute
  x11[[1+j*7]] <- filter(c11[[j+1]], c11[[j+1]][74] > 3) %>% nrow()
  x11[[2+j*7]] <- filter(c11[[j+1]], c11[[j+1]][75] > 3) %>% nrow()
  x11[[3+j*7]] <- filter(c11[[j+1]], c11[[j+1]][76] > 3) %>% nrow()
  x11[[4+j*7]] <- filter(c11[[j+1]], c11[[j+1]][77] > 3) %>% nrow()
  x11[[5+j*7]] <- filter(c11[[j+1]], c11[[j+1]][78] > 3) %>% nrow()
  x11[[6+j*7]] <- filter(c11[[j+1]], c11[[j+1]][79] > 3) %>% nrow()
  x11[[7+j*7]] <- filter(c11[[j+1]], c11[[j+1]][80] > 3) %>% nrow()
  
  x11[[36+j*10]] <- filter(c11[[j+1]], c11[[j+1]][55] > 3) %>% nrow()
  x11[[37+j*10]] <- filter(c11[[j+1]], c11[[j+1]][56] > 3) %>% nrow()
  x11[[38+j*10]] <- filter(c11[[j+1]], c11[[j+1]][57] > 3) %>% nrow()
  x11[[39+j*10]] <- filter(c11[[j+1]], c11[[j+1]][58] > 3) %>% nrow()
  x11[[40+j*10]] <- filter(c11[[j+1]], c11[[j+1]][59] > 3) %>% nrow()
  x11[[41+j*10]] <- filter(c11[[j+1]], c11[[j+1]][60] > 3) %>% nrow()
  x11[[42+j*10]] <- filter(c11[[j+1]], c11[[j+1]][61] > 3) %>% nrow()
  x11[[43+j*10]] <- filter(c11[[j+1]], c11[[j+1]][62] > 3) %>% nrow()
  x11[[44+j*10]] <- filter(c11[[j+1]], c11[[j+1]][63] > 3) %>% nrow()
  x11[[45+j*10]] <- filter(c11[[j+1]], c11[[j+1]][64] > 3) %>% nrow()
  
  x11[[86+j*9]] <- filter(c11[[j+1]], c11[[j+1]][65] > 3) %>% nrow()
  x11[[87+j*9]] <- filter(c11[[j+1]], c11[[j+1]][66] > 3) %>% nrow()
  x11[[88+j*9]] <- filter(c11[[j+1]], c11[[j+1]][67] > 3) %>% nrow()
  x11[[89+j*9]] <- filter(c11[[j+1]], c11[[j+1]][68] > 3) %>% nrow()
  x11[[90+j*9]] <- filter(c11[[j+1]], c11[[j+1]][69] > 3) %>% nrow()
  x11[[91+j*9]] <- filter(c11[[j+1]], c11[[j+1]][70] > 3) %>% nrow()
  x11[[92+j*9]] <- filter(c11[[j+1]], c11[[j+1]][71] > 3) %>% nrow()
  x11[[93+j*9]] <- filter(c11[[j+1]], c11[[j+1]][72] > 3) %>% nrow()
  x11[[94+j*9]] <- filter(c11[[j+1]], c11[[j+1]][73] > 3) %>% nrow()
  
  x11x[[j+1]] <- data.frame(a = x11[[1+j*7]], b = x11[[2+j*7]],c = x11[[3+j*7]],d = x11[[4+j*7]],e = x11[[5+j*7]],f = x11[[6+j*7]],g = x11[[7+j*7]],
                            h = x11[[36+j*10]],i = x11[[37+j*10]],k = x11[[38+j*10]],l = x11[[39+j*10]],m = x11[[40+j*10]],n = x11[[41+j*10]],o = x11[[42+j*10]],p = x11[[43+j*10]],q = x11[[44+j*10]],r = x11[[45+j*10]],
                            s = x11[[86+j*9]], t = x11[[87+j*9]],u = x11[[88+j*9]],v = x11[[89+j*9]], w = x11[[90+j*9]], x = x11[[91+j*9]],y = x11[[92+j*9]],z = x11[[93+j*9]],zz = x11[[94+j*9]]) 
  
  # Taipei & weather
  x12[[1+j*7]] <- filter(c12[[j+1]], c12[[j+1]][74] > 3) %>% nrow()
  x12[[2+j*7]] <- filter(c12[[j+1]], c12[[j+1]][75] > 3) %>% nrow()
  x12[[3+j*7]] <- filter(c12[[j+1]], c12[[j+1]][76] > 3) %>% nrow()
  x12[[4+j*7]] <- filter(c12[[j+1]], c12[[j+1]][77] > 3) %>% nrow()
  x12[[5+j*7]] <- filter(c12[[j+1]], c12[[j+1]][78] > 3) %>% nrow()
  x12[[6+j*7]] <- filter(c12[[j+1]], c12[[j+1]][79] > 3) %>% nrow()
  x12[[7+j*7]] <- filter(c12[[j+1]], c12[[j+1]][80] > 3) %>% nrow()
  
  x12[[36+j*10]] <- filter(c12[[j+1]], c12[[j+1]][55] > 3) %>% nrow()
  x12[[37+j*10]] <- filter(c12[[j+1]], c12[[j+1]][56] > 3) %>% nrow()
  x12[[38+j*10]] <- filter(c12[[j+1]], c12[[j+1]][57] > 3) %>% nrow()
  x12[[39+j*10]] <- filter(c12[[j+1]], c12[[j+1]][58] > 3) %>% nrow()
  x12[[40+j*10]] <- filter(c12[[j+1]], c12[[j+1]][59] > 3) %>% nrow()
  x12[[41+j*10]] <- filter(c12[[j+1]], c12[[j+1]][60] > 3) %>% nrow()
  x12[[42+j*10]] <- filter(c12[[j+1]], c12[[j+1]][61] > 3) %>% nrow()
  x12[[43+j*10]] <- filter(c12[[j+1]], c12[[j+1]][62] > 3) %>% nrow()
  x12[[44+j*10]] <- filter(c12[[j+1]], c12[[j+1]][63] > 3) %>% nrow()
  x12[[45+j*10]] <- filter(c12[[j+1]], c12[[j+1]][64] > 3) %>% nrow()
  
  x12[[86+j*9]] <- filter(c12[[j+1]], c12[[j+1]][65] > 3) %>% nrow()
  x12[[87+j*9]] <- filter(c12[[j+1]], c12[[j+1]][66] > 3) %>% nrow()
  x12[[88+j*9]] <- filter(c12[[j+1]], c12[[j+1]][67] > 3) %>% nrow()
  x12[[89+j*9]] <- filter(c12[[j+1]], c12[[j+1]][68] > 3) %>% nrow()
  x12[[90+j*9]] <- filter(c12[[j+1]], c12[[j+1]][69] > 3) %>% nrow()
  x12[[91+j*9]] <- filter(c12[[j+1]], c12[[j+1]][70] > 3) %>% nrow()
  x12[[92+j*9]] <- filter(c12[[j+1]], c12[[j+1]][71] > 3) %>% nrow()
  x12[[93+j*9]] <- filter(c12[[j+1]], c12[[j+1]][72] > 3) %>% nrow()
  x12[[94+j*9]] <- filter(c12[[j+1]], c12[[j+1]][73] > 3) %>% nrow()
  
  x12x[[j+1]] <- data.frame(a = x12[[1+j*7]],b = x12[[2+j*7]],c = x12[[3+j*7]],d = x12[[4+j*7]],e = x12[[5+j*7]],f = x12[[6+j*7]],g = x12[[7+j*7]],
                            h = x12[[36+j*10]],i = x12[[37+j*10]],k = x12[[38+j*10]],l = x12[[39+j*10]],m = x12[[40+j*10]],n = x12[[41+j*10]],o = x12[[42+j*10]],p = x12[[43+j*10]],q = x12[[44+j*10]],r = x12[[45+j*10]],
                            s = x12[[86+j*9]], t = x12[[87+j*9]], u = x12[[88+j*9]], v = x12[[89+j*9]], w = x12[[90+j*9]], x = x12[[91+j*9]], y = x12[[92+j*9]], z = x12[[93+j*9]],zz = x12[[94+j*9]])
  
  # Taipei & shopping
  x13[[1+j*7]] <- filter(c13[[j+1]], c13[[j+1]][74] > 3) %>% nrow()
  x13[[2+j*7]] <- filter(c13[[j+1]], c13[[j+1]][75] > 3) %>% nrow()
  x13[[3+j*7]] <- filter(c13[[j+1]], c13[[j+1]][76] > 3) %>% nrow()
  x13[[4+j*7]] <- filter(c13[[j+1]], c13[[j+1]][77] > 3) %>% nrow()
  x13[[5+j*7]] <- filter(c13[[j+1]], c13[[j+1]][78] > 3) %>% nrow()
  x13[[6+j*7]] <- filter(c13[[j+1]], c13[[j+1]][79] > 3) %>% nrow()
  x13[[7+j*7]] <- filter(c13[[j+1]], c13[[j+1]][80] > 3) %>% nrow()
  
  x13[[36+j*10]] <- filter(c13[[j+1]], c13[[j+1]][55] > 3) %>% nrow()
  x13[[37+j*10]] <- filter(c13[[j+1]], c13[[j+1]][56] > 3) %>% nrow()
  x13[[38+j*10]] <- filter(c13[[j+1]], c13[[j+1]][57] > 3) %>% nrow()
  x13[[39+j*10]] <- filter(c13[[j+1]], c13[[j+1]][58] > 3) %>% nrow()
  x13[[40+j*10]] <- filter(c13[[j+1]], c13[[j+1]][59] > 3) %>% nrow()
  x13[[41+j*10]] <- filter(c13[[j+1]], c13[[j+1]][60] > 3) %>% nrow()
  x13[[42+j*10]] <- filter(c13[[j+1]], c13[[j+1]][61] > 3) %>% nrow()
  x13[[43+j*10]] <- filter(c13[[j+1]], c13[[j+1]][62] > 3) %>% nrow()
  x13[[44+j*10]] <- filter(c13[[j+1]], c13[[j+1]][63] > 3) %>% nrow()
  x13[[45+j*10]] <- filter(c13[[j+1]], c13[[j+1]][64] > 3) %>% nrow()
  
  x13[[86+j*9]] <- filter(c13[[j+1]], c13[[j+1]][65] > 3) %>% nrow()
  x13[[87+j*9]] <- filter(c13[[j+1]], c13[[j+1]][66] > 3) %>% nrow()
  x13[[88+j*9]] <- filter(c13[[j+1]], c13[[j+1]][67] > 3) %>% nrow()
  x13[[89+j*9]] <- filter(c13[[j+1]], c13[[j+1]][68] > 3) %>% nrow()
  x13[[90+j*9]] <- filter(c13[[j+1]], c13[[j+1]][69] > 3) %>% nrow()
  x13[[91+j*9]] <- filter(c13[[j+1]], c13[[j+1]][70] > 3) %>% nrow()
  x13[[92+j*9]] <- filter(c13[[j+1]], c13[[j+1]][71] > 3) %>% nrow()
  x13[[93+j*9]] <- filter(c13[[j+1]], c13[[j+1]][72] > 3) %>% nrow()
  x13[[94+j*9]] <- filter(c13[[j+1]], c13[[j+1]][73] > 3) %>% nrow()
  
  x13x[[j+1]] <- data.frame(a = x13[[1+j*7]],b = x13[[2+j*7]],c = x13[[3+j*7]], d = x13[[4+j*7]],e = x13[[5+j*7]],f = x13[[6+j*7]],g = x13[[7+j*7]],
                            h = x13[[36+j*10]],i = x13[[37+j*10]],k = x13[[38+j*10]],l = x13[[39+j*10]],m = x13[[40+j*10]],n = x13[[41+j*10]],o = x13[[42+j*10]],p = x13[[43+j*10]],q = x13[[44+j*10]],r = x13[[45+j*10]],
                            s = x13[[86+j*9]], t = x13[[87+j*9]], u = x13[[88+j*9]], v = x13[[89+j*9]], w = x13[[90+j*9]], x = x13[[91+j*9]], y = x13[[92+j*9]], z = x13[[93+j*9]],zz = x13[[94+j*9]])
  
  
  # Taipei & dining
  x14[[1+j*7]] <- filter(c14[[j+1]], c14[[j+1]][74] > 3) %>% nrow()
  x14[[2+j*7]] <- filter(c14[[j+1]], c14[[j+1]][75] > 3) %>% nrow()
  x14[[3+j*7]] <- filter(c14[[j+1]], c14[[j+1]][76] > 3) %>% nrow()
  x14[[4+j*7]] <- filter(c14[[j+1]], c14[[j+1]][77] > 3) %>% nrow()
  x14[[5+j*7]] <- filter(c14[[j+1]], c14[[j+1]][78] > 3) %>% nrow()
  x14[[6+j*7]] <- filter(c14[[j+1]], c14[[j+1]][79] > 3) %>% nrow()
  x14[[7+j*7]] <- filter(c14[[j+1]], c14[[j+1]][80] > 3) %>% nrow()
  
  x14[[36+j*10]] <- filter(c14[[j+1]], c14[[j+1]][55] > 3) %>% nrow()
  x14[[37+j*10]] <- filter(c14[[j+1]], c14[[j+1]][56] > 3) %>% nrow()
  x14[[38+j*10]] <- filter(c14[[j+1]], c14[[j+1]][57] > 3) %>% nrow()
  x14[[39+j*10]] <- filter(c14[[j+1]], c14[[j+1]][58] > 3) %>% nrow()
  x14[[40+j*10]] <- filter(c14[[j+1]], c14[[j+1]][59] > 3) %>% nrow()
  x14[[41+j*10]] <- filter(c14[[j+1]], c14[[j+1]][60] > 3) %>% nrow()
  x14[[42+j*10]] <- filter(c14[[j+1]], c14[[j+1]][61] > 3) %>% nrow()
  x14[[43+j*10]] <- filter(c14[[j+1]], c14[[j+1]][62] > 3) %>% nrow()
  x14[[44+j*10]] <- filter(c14[[j+1]], c14[[j+1]][63] > 3) %>% nrow()
  x14[[45+j*10]] <- filter(c14[[j+1]], c14[[j+1]][64] > 3) %>% nrow()
  
  x14[[86+j*9]] <- filter(c14[[j+1]], c14[[j+1]][65] > 3) %>% nrow()
  x14[[87+j*9]] <- filter(c14[[j+1]], c14[[j+1]][66] > 3) %>% nrow()
  x14[[88+j*9]] <- filter(c14[[j+1]], c14[[j+1]][67] > 3) %>% nrow()
  x14[[89+j*9]] <- filter(c14[[j+1]], c14[[j+1]][68] > 3) %>% nrow()
  x14[[90+j*9]] <- filter(c14[[j+1]], c14[[j+1]][69] > 3) %>% nrow()
  x14[[91+j*9]] <- filter(c14[[j+1]], c14[[j+1]][70] > 3) %>% nrow()
  x14[[92+j*9]] <- filter(c14[[j+1]], c14[[j+1]][71] > 3) %>% nrow()
  x14[[93+j*9]] <- filter(c14[[j+1]], c14[[j+1]][72] > 3) %>% nrow()
  x14[[94+j*9]] <- filter(c14[[j+1]], c14[[j+1]][73] > 3) %>% nrow()
  
  x14x[[j+1]] <- data.frame(a = x14[[1+j*7]],b = x14[[2+j*7]],c = x14[[3+j*7]],d = x14[[4+j*7]],e = x14[[5+j*7]],f = x14[[6+j*7]],g = x14[[7+j*7]],
                            h = x14[[36+j*10]],i = x14[[37+j*10]],k = x14[[38+j*10]],l = x14[[39+j*10]],m = x14[[40+j*10]],n = x14[[41+j*10]],o = x14[[42+j*10]],p = x14[[43+j*10]],q = x14[[44+j*10]],r = x14[[45+j*10]],
                            s = x14[[86+j*9]], t = x14[[87+j*9]], u = x14[[88+j*9]], v = x14[[89+j*9]], w = x14[[90+j*9]], x = x14[[91+j*9]], y = x14[[92+j*9]], z = x14[[93+j*9]],zz = x14[[94+j*9]])
  
  
  # Taipei & business trip
  x15[[1+j*7]] <- filter(c15[[j+1]], c15[[j+1]][74] > 3) %>% nrow()
  x15[[2+j*7]] <- filter(c15[[j+1]], c15[[j+1]][75] > 3) %>% nrow()
  x15[[3+j*7]] <- filter(c15[[j+1]], c15[[j+1]][76] > 3) %>% nrow()
  x15[[4+j*7]] <- filter(c15[[j+1]], c15[[j+1]][77] > 3) %>% nrow()
  x15[[5+j*7]] <- filter(c15[[j+1]], c15[[j+1]][78] > 3) %>% nrow()
  x15[[6+j*7]] <- filter(c15[[j+1]], c15[[j+1]][79] > 3) %>% nrow()
  x15[[7+j*7]] <- filter(c15[[j+1]], c15[[j+1]][80] > 3) %>% nrow()
  
  x15[[36+j*10]] <- filter(c15[[j+1]], c15[[j+1]][55] > 3) %>% nrow()
  x15[[37+j*10]] <- filter(c15[[j+1]], c15[[j+1]][56] > 3) %>% nrow()
  x15[[38+j*10]] <- filter(c15[[j+1]], c15[[j+1]][57] > 3) %>% nrow()
  x15[[39+j*10]] <- filter(c15[[j+1]], c15[[j+1]][58] > 3) %>% nrow()
  x15[[40+j*10]] <- filter(c15[[j+1]], c15[[j+1]][59] > 3) %>% nrow()
  x15[[41+j*10]] <- filter(c15[[j+1]], c15[[j+1]][60] > 3) %>% nrow()
  x15[[42+j*10]] <- filter(c15[[j+1]], c15[[j+1]][61] > 3) %>% nrow()
  x15[[43+j*10]] <- filter(c15[[j+1]], c15[[j+1]][62] > 3) %>% nrow()
  x15[[44+j*10]] <- filter(c15[[j+1]], c15[[j+1]][63] > 3) %>% nrow()
  x15[[45+j*10]] <- filter(c15[[j+1]], c15[[j+1]][64] > 3) %>% nrow()
  
  x15[[86+j*9]] <- filter(c15[[j+1]], c15[[j+1]][65] > 3) %>% nrow()
  x15[[87+j*9]] <- filter(c15[[j+1]], c15[[j+1]][66] > 3) %>% nrow()
  x15[[88+j*9]] <- filter(c15[[j+1]], c15[[j+1]][67] > 3) %>% nrow()
  x15[[89+j*9]] <- filter(c15[[j+1]], c15[[j+1]][68] > 3) %>% nrow()
  x15[[90+j*9]] <- filter(c15[[j+1]], c15[[j+1]][69] > 3) %>% nrow()
  x15[[91+j*9]] <- filter(c15[[j+1]], c15[[j+1]][70] > 3) %>% nrow()
  x15[[92+j*9]] <- filter(c15[[j+1]], c15[[j+1]][71] > 3) %>% nrow()
  x15[[93+j*9]] <- filter(c15[[j+1]], c15[[j+1]][72] > 3) %>% nrow()
  x15[[94+j*9]] <- filter(c15[[j+1]], c15[[j+1]][73] > 3) %>% nrow()
  
  x15x[[j+1]] <- data.frame(a = x15[[1+j*7]],b = x15[[2+j*7]],c = x15[[3+j*7]],d = x15[[4+j*7]],e = x15[[5+j*7]],f = x15[[6+j*7]],g = x15[[7+j*7]],
                            h = x15[[36+j*10]],i = x15[[37+j*10]],k = x15[[38+j*10]],l = x15[[39+j*10]],m = x15[[40+j*10]],n = x15[[41+j*10]],o = x15[[42+j*10]],p = x15[[43+j*10]],q = x15[[44+j*10]],r = x15[[45+j*10]],
                            s = x15[[86+j*9]], t = x15[[87+j*9]], u = x15[[88+j*9]], v = x15[[89+j*9]], w = x15[[90+j*9]], x = x15[[91+j*9]], y = x15[[92+j*9]], z = x15[[93+j*9]],zz = x15[[94+j*9]])
  
  xx1[[j+1]] <- rbind(x11x[[j+1]],x12x[[j+1]],x13x[[j+1]],x14x[[j+1]],x15x[[j+1]])
  
}


for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # TY & commute
  x21[[1+j*7]] <- filter(c21[[j+1]], c21[[j+1]][74] > 3) %>% nrow()
  x21[[2+j*7]] <- filter(c21[[j+1]], c21[[j+1]][75] > 3) %>% nrow()
  x21[[3+j*7]] <- filter(c21[[j+1]], c21[[j+1]][76] > 3) %>% nrow()
  x21[[4+j*7]] <- filter(c21[[j+1]], c21[[j+1]][77] > 3) %>% nrow()
  x21[[5+j*7]] <- filter(c21[[j+1]], c21[[j+1]][78] > 3) %>% nrow()
  x21[[6+j*7]] <- filter(c21[[j+1]], c21[[j+1]][79] > 3) %>% nrow()
  x21[[7+j*7]] <- filter(c21[[j+1]], c21[[j+1]][80] > 3) %>% nrow()
  
  x21[[36+j*10]] <- filter(c21[[j+1]], c21[[j+1]][55] > 3) %>% nrow()
  x21[[37+j*10]] <- filter(c21[[j+1]], c21[[j+1]][56] > 3) %>% nrow()
  x21[[38+j*10]] <- filter(c21[[j+1]], c21[[j+1]][57] > 3) %>% nrow()
  x21[[39+j*10]] <- filter(c21[[j+1]], c21[[j+1]][58] > 3) %>% nrow()
  x21[[40+j*10]] <- filter(c21[[j+1]], c21[[j+1]][59] > 3) %>% nrow()
  x21[[41+j*10]] <- filter(c21[[j+1]], c21[[j+1]][60] > 3) %>% nrow()
  x21[[42+j*10]] <- filter(c21[[j+1]], c21[[j+1]][61] > 3) %>% nrow()
  x21[[43+j*10]] <- filter(c21[[j+1]], c21[[j+1]][62] > 3) %>% nrow()
  x21[[44+j*10]] <- filter(c21[[j+1]], c21[[j+1]][63] > 3) %>% nrow()
  x21[[45+j*10]] <- filter(c21[[j+1]], c21[[j+1]][64] > 3) %>% nrow()
  
  x21[[86+j*9]] <- filter(c21[[j+1]], c21[[j+1]][65] > 3) %>% nrow()
  x21[[87+j*9]] <- filter(c21[[j+1]], c21[[j+1]][66] > 3) %>% nrow()
  x21[[88+j*9]] <- filter(c21[[j+1]], c21[[j+1]][67] > 3) %>% nrow()
  x21[[89+j*9]] <- filter(c21[[j+1]], c21[[j+1]][68] > 3) %>% nrow()
  x21[[90+j*9]] <- filter(c21[[j+1]], c21[[j+1]][69] > 3) %>% nrow()
  x21[[91+j*9]] <- filter(c21[[j+1]], c21[[j+1]][70] > 3) %>% nrow()
  x21[[92+j*9]] <- filter(c21[[j+1]], c21[[j+1]][71] > 3) %>% nrow()
  x21[[93+j*9]] <- filter(c21[[j+1]], c21[[j+1]][72] > 3) %>% nrow()
  x21[[94+j*9]] <- filter(c21[[j+1]], c21[[j+1]][73] > 3) %>% nrow()
  
  x21x[[j+1]] <- data.frame(a = x21[[1+j*7]],b = x21[[2+j*7]],c = x21[[3+j*7]],d = x21[[4+j*7]],e = x21[[5+j*7]],f = x21[[6+j*7]],g = x21[[7+j*7]],
                            h = x21[[36+j*10]],i = x21[[37+j*10]],k = x21[[38+j*10]],l = x21[[39+j*10]],m = x21[[40+j*10]],n = x21[[41+j*10]],o = x21[[42+j*10]],p = x21[[43+j*10]],q = x21[[44+j*10]],r = x21[[45+j*10]],
                            s = x21[[86+j*9]], t = x21[[87+j*9]], u = x21[[88+j*9]], v = x21[[89+j*9]], w = x21[[90+j*9]], x = x21[[91+j*9]], y = x21[[92+j*9]], z = x21[[93+j*9]],zz = x21[[94+j*9]]) 
  
  # TY & weather
  x22[[1+j*7]] <- filter(c22[[j+1]], c22[[j+1]][74] > 3) %>% nrow()
  x22[[2+j*7]] <- filter(c22[[j+1]], c22[[j+1]][75] > 3) %>% nrow()
  x22[[3+j*7]] <- filter(c22[[j+1]], c22[[j+1]][76] > 3) %>% nrow()
  x22[[4+j*7]] <- filter(c22[[j+1]], c22[[j+1]][77] > 3) %>% nrow()
  x22[[5+j*7]] <- filter(c22[[j+1]], c22[[j+1]][78] > 3) %>% nrow()
  x22[[6+j*7]] <- filter(c22[[j+1]], c22[[j+1]][79] > 3) %>% nrow()
  x22[[7+j*7]] <- filter(c22[[j+1]], c22[[j+1]][80] > 3) %>% nrow()
  
  x22[[36+j*10]] <- filter(c22[[j+1]], c22[[j+1]][55] > 3) %>% nrow()
  x22[[37+j*10]] <- filter(c22[[j+1]], c22[[j+1]][56] > 3) %>% nrow()
  x22[[38+j*10]] <- filter(c22[[j+1]], c22[[j+1]][57] > 3) %>% nrow()
  x22[[39+j*10]] <- filter(c22[[j+1]], c22[[j+1]][58] > 3) %>% nrow()
  x22[[40+j*10]] <- filter(c22[[j+1]], c22[[j+1]][59] > 3) %>% nrow()
  x22[[41+j*10]] <- filter(c22[[j+1]], c22[[j+1]][60] > 3) %>% nrow()
  x22[[42+j*10]] <- filter(c22[[j+1]], c22[[j+1]][61] > 3) %>% nrow()
  x22[[43+j*10]] <- filter(c22[[j+1]], c22[[j+1]][62] > 3) %>% nrow()
  x22[[44+j*10]] <- filter(c22[[j+1]], c22[[j+1]][63] > 3) %>% nrow()
  x22[[45+j*10]] <- filter(c22[[j+1]], c22[[j+1]][64] > 3) %>% nrow()
  
  x22[[86+j*9]] <- filter(c22[[j+1]], c22[[j+1]][65] > 3) %>% nrow()
  x22[[87+j*9]] <- filter(c22[[j+1]], c22[[j+1]][66] > 3) %>% nrow()
  x22[[88+j*9]] <- filter(c22[[j+1]], c22[[j+1]][67] > 3) %>% nrow()
  x22[[89+j*9]] <- filter(c22[[j+1]], c22[[j+1]][68] > 3) %>% nrow()
  x22[[90+j*9]] <- filter(c22[[j+1]], c22[[j+1]][69] > 3) %>% nrow()
  x22[[91+j*9]] <- filter(c22[[j+1]], c22[[j+1]][70] > 3) %>% nrow()
  x22[[92+j*9]] <- filter(c22[[j+1]], c22[[j+1]][71] > 3) %>% nrow()
  x22[[93+j*9]] <- filter(c22[[j+1]], c22[[j+1]][72] > 3) %>% nrow()
  x22[[94+j*9]] <- filter(c22[[j+1]], c22[[j+1]][73] > 3) %>% nrow()
  
  x22x[[j+1]] <- data.frame(a = x22[[1+j*7]],b = x22[[2+j*7]],c = x22[[3+j*7]],d = x22[[4+j*7]],e = x22[[5+j*7]],f = x22[[6+j*7]],g = x22[[7+j*7]],
                            h = x22[[36+j*10]],i = x22[[37+j*10]],k = x22[[38+j*10]],l = x22[[39+j*10]],m = x22[[40+j*10]],n = x22[[41+j*10]],o = x22[[42+j*10]],p = x22[[43+j*10]],q = x22[[44+j*10]],r = x22[[45+j*10]],
                            s = x22[[86+j*9]], t = x22[[87+j*9]], u = x22[[88+j*9]], v = x22[[89+j*9]], w = x22[[90+j*9]], x = x22[[91+j*9]], y = x22[[92+j*9]], z = x22[[93+j*9]],zz = x22[[94+j*9]])
  
  # TY & shopping
  x23[[1+j*7]] <- filter(c23[[j+1]], c23[[j+1]][74] > 3) %>% nrow()
  x23[[2+j*7]] <- filter(c23[[j+1]], c23[[j+1]][75] > 3) %>% nrow()
  x23[[3+j*7]] <- filter(c23[[j+1]], c23[[j+1]][76] > 3) %>% nrow()
  x23[[4+j*7]] <- filter(c23[[j+1]], c23[[j+1]][77] > 3) %>% nrow()
  x23[[5+j*7]] <- filter(c23[[j+1]], c23[[j+1]][78] > 3) %>% nrow()
  x23[[6+j*7]] <- filter(c23[[j+1]], c23[[j+1]][79] > 3) %>% nrow()
  x23[[7+j*7]] <- filter(c23[[j+1]], c23[[j+1]][80] > 3) %>% nrow()
  
  x23[[36+j*10]] <- filter(c23[[j+1]], c23[[j+1]][55] > 3) %>% nrow()
  x23[[37+j*10]] <- filter(c23[[j+1]], c23[[j+1]][56] > 3) %>% nrow()
  x23[[38+j*10]] <- filter(c23[[j+1]], c23[[j+1]][57] > 3) %>% nrow()
  x23[[39+j*10]] <- filter(c23[[j+1]], c23[[j+1]][58] > 3) %>% nrow()
  x23[[40+j*10]] <- filter(c23[[j+1]], c23[[j+1]][59] > 3) %>% nrow()
  x23[[41+j*10]] <- filter(c23[[j+1]], c23[[j+1]][60] > 3) %>% nrow()
  x23[[42+j*10]] <- filter(c23[[j+1]], c23[[j+1]][61] > 3) %>% nrow()
  x23[[43+j*10]] <- filter(c23[[j+1]], c23[[j+1]][62] > 3) %>% nrow()
  x23[[44+j*10]] <- filter(c23[[j+1]], c23[[j+1]][63] > 3) %>% nrow()
  x23[[45+j*10]] <- filter(c23[[j+1]], c23[[j+1]][64] > 3) %>% nrow()
  
  x23[[86+j*9]] <- filter(c23[[j+1]], c23[[j+1]][65] > 3) %>% nrow()
  x23[[87+j*9]] <- filter(c23[[j+1]], c23[[j+1]][66] > 3) %>% nrow()
  x23[[88+j*9]] <- filter(c23[[j+1]], c23[[j+1]][67] > 3) %>% nrow()
  x23[[89+j*9]] <- filter(c23[[j+1]], c23[[j+1]][68] > 3) %>% nrow()
  x23[[90+j*9]] <- filter(c23[[j+1]], c23[[j+1]][69] > 3) %>% nrow()
  x23[[91+j*9]] <- filter(c23[[j+1]], c23[[j+1]][70] > 3) %>% nrow()
  x23[[92+j*9]] <- filter(c23[[j+1]], c23[[j+1]][71] > 3) %>% nrow()
  x23[[93+j*9]] <- filter(c23[[j+1]], c23[[j+1]][72] > 3) %>% nrow()
  x23[[94+j*9]] <- filter(c23[[j+1]], c23[[j+1]][73] > 3) %>% nrow()
  
  x23x[[j+1]] <- data.frame(a = x23[[1+j*7]],b = x23[[2+j*7]],c = x23[[3+j*7]],d = x23[[4+j*7]],e = x23[[5+j*7]],f = x23[[6+j*7]],g = x23[[7+j*7]],
                            h = x23[[36+j*10]],i = x23[[37+j*10]],k = x23[[38+j*10]],l = x23[[39+j*10]],m = x23[[40+j*10]],n = x23[[41+j*10]],o = x23[[42+j*10]],p = x23[[43+j*10]],q = x23[[44+j*10]],r = x23[[45+j*10]],
                            s = x23[[86+j*9]], t = x23[[87+j*9]], u = x23[[88+j*9]], v = x23[[89+j*9]], w = x23[[90+j*9]], x = x23[[91+j*9]], y = x23[[92+j*9]], z = x23[[93+j*9]],zz = x23[[94+j*9]])
  
  
  # TY & dining
  x24[[1+j*7]] <- filter(c24[[j+1]], c24[[j+1]][74] > 3) %>% nrow()
  x24[[2+j*7]] <- filter(c24[[j+1]], c24[[j+1]][75] > 3) %>% nrow()
  x24[[3+j*7]] <- filter(c24[[j+1]], c24[[j+1]][76] > 3) %>% nrow()
  x24[[4+j*7]] <- filter(c24[[j+1]], c24[[j+1]][77] > 3) %>% nrow()
  x24[[5+j*7]] <- filter(c24[[j+1]], c24[[j+1]][78] > 3) %>% nrow()
  x24[[6+j*7]] <- filter(c24[[j+1]], c24[[j+1]][79] > 3) %>% nrow()
  x24[[7+j*7]] <- filter(c24[[j+1]], c24[[j+1]][80] > 3) %>% nrow()
  
  x24[[36+j*10]] <- filter(c24[[j+1]], c24[[j+1]][55] > 3) %>% nrow()
  x24[[37+j*10]] <- filter(c24[[j+1]], c24[[j+1]][56] > 3) %>% nrow()
  x24[[38+j*10]] <- filter(c24[[j+1]], c24[[j+1]][57] > 3) %>% nrow()
  x24[[39+j*10]] <- filter(c24[[j+1]], c24[[j+1]][58] > 3) %>% nrow()
  x24[[40+j*10]] <- filter(c24[[j+1]], c24[[j+1]][59] > 3) %>% nrow()
  x24[[41+j*10]] <- filter(c24[[j+1]], c24[[j+1]][60] > 3) %>% nrow()
  x24[[42+j*10]] <- filter(c24[[j+1]], c24[[j+1]][61] > 3) %>% nrow()
  x24[[43+j*10]] <- filter(c24[[j+1]], c24[[j+1]][62] > 3) %>% nrow()
  x24[[44+j*10]] <- filter(c24[[j+1]], c24[[j+1]][63] > 3) %>% nrow()
  x24[[45+j*10]] <- filter(c24[[j+1]], c24[[j+1]][64] > 3) %>% nrow()
  
  x24[[86+j*9]] <- filter(c24[[j+1]], c24[[j+1]][65] > 3) %>% nrow()
  x24[[87+j*9]] <- filter(c24[[j+1]], c24[[j+1]][66] > 3) %>% nrow()
  x24[[88+j*9]] <- filter(c24[[j+1]], c24[[j+1]][67] > 3) %>% nrow()
  x24[[89+j*9]] <- filter(c24[[j+1]], c24[[j+1]][68] > 3) %>% nrow()
  x24[[90+j*9]] <- filter(c24[[j+1]], c24[[j+1]][69] > 3) %>% nrow()
  x24[[91+j*9]] <- filter(c24[[j+1]], c24[[j+1]][70] > 3) %>% nrow()
  x24[[92+j*9]] <- filter(c24[[j+1]], c24[[j+1]][71] > 3) %>% nrow()
  x24[[93+j*9]] <- filter(c24[[j+1]], c24[[j+1]][72] > 3) %>% nrow()
  x24[[94+j*9]] <- filter(c24[[j+1]], c24[[j+1]][73] > 3) %>% nrow()
  
  x24x[[j+1]] <- data.frame(a = x24[[1+j*7]],b = x24[[2+j*7]],c = x24[[3+j*7]],d = x24[[4+j*7]],e = x24[[5+j*7]],f = x24[[6+j*7]],g = x24[[7+j*7]],
                            h = x24[[36+j*10]],i = x24[[37+j*10]],k = x24[[38+j*10]],l = x24[[39+j*10]],m = x24[[40+j*10]],n = x24[[41+j*10]],o = x24[[42+j*10]],p = x24[[43+j*10]],q = x24[[44+j*10]],r = x24[[45+j*10]],
                            s = x24[[86+j*9]], t = x24[[87+j*9]], u = x24[[88+j*9]], v = x24[[89+j*9]], w = x24[[90+j*9]], x = x24[[91+j*9]], y = x24[[92+j*9]], z = x24[[93+j*9]],zz = x24[[94+j*9]])
  
  
  # TY & business trip
  x25[[1+j*7]] <- filter(c25[[j+1]], c25[[j+1]][74] > 3) %>% nrow()
  x25[[2+j*7]] <- filter(c25[[j+1]], c25[[j+1]][75] > 3) %>% nrow()
  x25[[3+j*7]] <- filter(c25[[j+1]], c25[[j+1]][76] > 3) %>% nrow()
  x25[[4+j*7]] <- filter(c25[[j+1]], c25[[j+1]][77] > 3) %>% nrow()
  x25[[5+j*7]] <- filter(c25[[j+1]], c25[[j+1]][78] > 3) %>% nrow()
  x25[[6+j*7]] <- filter(c25[[j+1]], c25[[j+1]][79] > 3) %>% nrow()
  x25[[7+j*7]] <- filter(c25[[j+1]], c25[[j+1]][80] > 3) %>% nrow()
  
  x25[[36+j*10]] <- filter(c25[[j+1]], c25[[j+1]][55] > 3) %>% nrow()
  x25[[37+j*10]] <- filter(c25[[j+1]], c25[[j+1]][56] > 3) %>% nrow()
  x25[[38+j*10]] <- filter(c25[[j+1]], c25[[j+1]][57] > 3) %>% nrow()
  x25[[39+j*10]] <- filter(c25[[j+1]], c25[[j+1]][58] > 3) %>% nrow()
  x25[[40+j*10]] <- filter(c25[[j+1]], c25[[j+1]][59] > 3) %>% nrow()
  x25[[41+j*10]] <- filter(c25[[j+1]], c25[[j+1]][60] > 3) %>% nrow()
  x25[[42+j*10]] <- filter(c25[[j+1]], c25[[j+1]][61] > 3) %>% nrow()
  x25[[43+j*10]] <- filter(c25[[j+1]], c25[[j+1]][62] > 3) %>% nrow()
  x25[[44+j*10]] <- filter(c25[[j+1]], c25[[j+1]][63] > 3) %>% nrow()
  x25[[45+j*10]] <- filter(c25[[j+1]], c25[[j+1]][64] > 3) %>% nrow()
  
  x25[[86+j*9]] <- filter(c25[[j+1]], c25[[j+1]][65] > 3) %>% nrow()
  x25[[87+j*9]] <- filter(c25[[j+1]], c25[[j+1]][66] > 3) %>% nrow()
  x25[[88+j*9]] <- filter(c25[[j+1]], c25[[j+1]][67] > 3) %>% nrow()
  x25[[89+j*9]] <- filter(c25[[j+1]], c25[[j+1]][68] > 3) %>% nrow()
  x25[[90+j*9]] <- filter(c25[[j+1]], c25[[j+1]][69] > 3) %>% nrow()
  x25[[91+j*9]] <- filter(c25[[j+1]], c25[[j+1]][70] > 3) %>% nrow()
  x25[[92+j*9]] <- filter(c25[[j+1]], c25[[j+1]][71] > 3) %>% nrow()
  x25[[93+j*9]] <- filter(c25[[j+1]], c25[[j+1]][72] > 3) %>% nrow()
  x25[[94+j*9]] <- filter(c25[[j+1]], c25[[j+1]][73] > 3) %>% nrow()
  
  x25x[[j+1]] <- data.frame(a = x25[[1+j*7]],b = x25[[2+j*7]],c = x25[[3+j*7]],d = x25[[4+j*7]],e = x25[[5+j*7]],f = x25[[6+j*7]],g = x25[[7+j*7]],
                            h = x25[[36+j*10]],i = x25[[37+j*10]],k = x25[[38+j*10]],l = x25[[39+j*10]],m = x25[[40+j*10]],n = x25[[41+j*10]],o = x25[[42+j*10]],p = x25[[43+j*10]],q = x25[[44+j*10]],r = x25[[45+j*10]],
                            s = x25[[86+j*9]], t = x25[[87+j*9]], u = x25[[88+j*9]], v = x25[[89+j*9]], w = x25[[90+j*9]], x = x25[[91+j*9]], y = x25[[92+j*9]], z = x25[[93+j*9]],zz = x25[[94+j*9]])
  
  xx2[[j+1]] <- rbind(x21x[[j+1]],x22x[[j+1]],x23x[[j+1]],x24x[[j+1]],x25x[[j+1]])
  
}




for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # HC & commute
  x31[[1+j*7]] <- filter(c31[[j+1]], c31[[j+1]][74] > 3) %>% nrow()
  x31[[2+j*7]] <- filter(c31[[j+1]], c31[[j+1]][75] > 3) %>% nrow()
  x31[[3+j*7]] <- filter(c31[[j+1]], c31[[j+1]][76] > 3) %>% nrow()
  x31[[4+j*7]] <- filter(c31[[j+1]], c31[[j+1]][77] > 3) %>% nrow()
  x31[[5+j*7]] <- filter(c31[[j+1]], c31[[j+1]][78] > 3) %>% nrow()
  x31[[6+j*7]] <- filter(c31[[j+1]], c31[[j+1]][79] > 3) %>% nrow()
  x31[[7+j*7]] <- filter(c31[[j+1]], c31[[j+1]][80] > 3) %>% nrow()
  
  x31[[36+j*10]] <- filter(c31[[j+1]], c31[[j+1]][55] > 3) %>% nrow()
  x31[[37+j*10]] <- filter(c31[[j+1]], c31[[j+1]][56] > 3) %>% nrow()
  x31[[38+j*10]] <- filter(c31[[j+1]], c31[[j+1]][57] > 3) %>% nrow()
  x31[[39+j*10]] <- filter(c31[[j+1]], c31[[j+1]][58] > 3) %>% nrow()
  x31[[40+j*10]] <- filter(c31[[j+1]], c31[[j+1]][59] > 3) %>% nrow()
  x31[[41+j*10]] <- filter(c31[[j+1]], c31[[j+1]][60] > 3) %>% nrow()
  x31[[42+j*10]] <- filter(c31[[j+1]], c31[[j+1]][61] > 3) %>% nrow()
  x31[[43+j*10]] <- filter(c31[[j+1]], c31[[j+1]][62] > 3) %>% nrow()
  x31[[44+j*10]] <- filter(c31[[j+1]], c31[[j+1]][63] > 3) %>% nrow()
  x31[[45+j*10]] <- filter(c31[[j+1]], c31[[j+1]][64] > 3) %>% nrow()
  
  x31[[86+j*9]] <- filter(c31[[j+1]], c31[[j+1]][65] > 3) %>% nrow()
  x31[[87+j*9]] <- filter(c31[[j+1]], c31[[j+1]][66] > 3) %>% nrow()
  x31[[88+j*9]] <- filter(c31[[j+1]], c31[[j+1]][67] > 3) %>% nrow()
  x31[[89+j*9]] <- filter(c31[[j+1]], c31[[j+1]][68] > 3) %>% nrow()
  x31[[90+j*9]] <- filter(c31[[j+1]], c31[[j+1]][69] > 3) %>% nrow()
  x31[[91+j*9]] <- filter(c31[[j+1]], c31[[j+1]][70] > 3) %>% nrow()
  x31[[92+j*9]] <- filter(c31[[j+1]], c31[[j+1]][71] > 3) %>% nrow()
  x31[[93+j*9]] <- filter(c31[[j+1]], c31[[j+1]][72] > 3) %>% nrow()
  x31[[94+j*9]] <- filter(c31[[j+1]], c31[[j+1]][73] > 3) %>% nrow()
  
  x31x[[j+1]] <- data.frame(a = x31[[1+j*7]],b = x31[[2+j*7]],c = x31[[3+j*7]],d = x31[[4+j*7]],e = x31[[5+j*7]],f = x31[[6+j*7]],g = x31[[7+j*7]],
                            h = x31[[36+j*10]],i = x31[[37+j*10]],k = x31[[38+j*10]],l = x31[[39+j*10]],m = x31[[40+j*10]],n = x31[[41+j*10]],o = x31[[42+j*10]],p = x31[[43+j*10]],q = x31[[44+j*10]],r = x31[[45+j*10]],
                            s = x31[[86+j*9]], t = x31[[87+j*9]], u = x31[[88+j*9]], v = x31[[89+j*9]], w = x31[[90+j*9]], x = x31[[91+j*9]], y = x31[[92+j*9]], z = x31[[93+j*9]],zz = x31[[94+j*9]]) 
  
  # HC & weather
  x32[[1+j*7]] <- filter(c32[[j+1]], c32[[j+1]][74] > 3) %>% nrow()
  x32[[2+j*7]] <- filter(c32[[j+1]], c32[[j+1]][75] > 3) %>% nrow()
  x32[[3+j*7]] <- filter(c32[[j+1]], c32[[j+1]][76] > 3) %>% nrow()
  x32[[4+j*7]] <- filter(c32[[j+1]], c32[[j+1]][77] > 3) %>% nrow()
  x32[[5+j*7]] <- filter(c32[[j+1]], c32[[j+1]][78] > 3) %>% nrow()
  x32[[6+j*7]] <- filter(c32[[j+1]], c32[[j+1]][79] > 3) %>% nrow()
  x32[[7+j*7]] <- filter(c32[[j+1]], c32[[j+1]][80] > 3) %>% nrow()
  
  x32[[36+j*10]] <- filter(c32[[j+1]], c32[[j+1]][55] > 3) %>% nrow()
  x32[[37+j*10]] <- filter(c32[[j+1]], c32[[j+1]][56] > 3) %>% nrow()
  x32[[38+j*10]] <- filter(c32[[j+1]], c32[[j+1]][57] > 3) %>% nrow()
  x32[[39+j*10]] <- filter(c32[[j+1]], c32[[j+1]][58] > 3) %>% nrow()
  x32[[40+j*10]] <- filter(c32[[j+1]], c32[[j+1]][59] > 3) %>% nrow()
  x32[[41+j*10]] <- filter(c32[[j+1]], c32[[j+1]][60] > 3) %>% nrow()
  x32[[42+j*10]] <- filter(c32[[j+1]], c32[[j+1]][61] > 3) %>% nrow()
  x32[[43+j*10]] <- filter(c32[[j+1]], c32[[j+1]][62] > 3) %>% nrow()
  x32[[44+j*10]] <- filter(c32[[j+1]], c32[[j+1]][63] > 3) %>% nrow()
  x32[[45+j*10]] <- filter(c32[[j+1]], c32[[j+1]][64] > 3) %>% nrow()
  
  x32[[86+j*9]] <- filter(c32[[j+1]], c32[[j+1]][65] > 3) %>% nrow()
  x32[[87+j*9]] <- filter(c32[[j+1]], c32[[j+1]][66] > 3) %>% nrow()
  x32[[88+j*9]] <- filter(c32[[j+1]], c32[[j+1]][67] > 3) %>% nrow()
  x32[[89+j*9]] <- filter(c32[[j+1]], c32[[j+1]][68] > 3) %>% nrow()
  x32[[90+j*9]] <- filter(c32[[j+1]], c32[[j+1]][69] > 3) %>% nrow()
  x32[[91+j*9]] <- filter(c32[[j+1]], c32[[j+1]][70] > 3) %>% nrow()
  x32[[92+j*9]] <- filter(c32[[j+1]], c32[[j+1]][71] > 3) %>% nrow()
  x32[[93+j*9]] <- filter(c32[[j+1]], c32[[j+1]][72] > 3) %>% nrow()
  x32[[94+j*9]] <- filter(c32[[j+1]], c32[[j+1]][73] > 3) %>% nrow()
  
  x32x[[j+1]] <- data.frame(a = x32[[1+j*7]],b = x32[[2+j*7]],c = x32[[3+j*7]],d = x32[[4+j*7]],e = x32[[5+j*7]],f = x32[[6+j*7]],g = x32[[7+j*7]],
                            h = x32[[36+j*10]],i = x32[[37+j*10]],k = x32[[38+j*10]],l = x32[[39+j*10]],m = x32[[40+j*10]],n = x32[[41+j*10]],o = x32[[42+j*10]],p = x32[[43+j*10]],q = x32[[44+j*10]],r = x32[[45+j*10]],
                            s = x32[[86+j*9]], t = x32[[87+j*9]], u = x32[[88+j*9]], v = x32[[89+j*9]], w = x32[[90+j*9]], x = x32[[91+j*9]], y = x32[[92+j*9]], z = x32[[93+j*9]],zz = x32[[94+j*9]])
  
  # HC & shopping
  x33[[1+j*7]] <- filter(c33[[j+1]], c33[[j+1]][74] > 3) %>% nrow()
  x33[[2+j*7]] <- filter(c33[[j+1]], c33[[j+1]][75] > 3) %>% nrow()
  x33[[3+j*7]] <- filter(c33[[j+1]], c33[[j+1]][76] > 3) %>% nrow()
  x33[[4+j*7]] <- filter(c33[[j+1]], c33[[j+1]][77] > 3) %>% nrow()
  x33[[5+j*7]] <- filter(c33[[j+1]], c33[[j+1]][78] > 3) %>% nrow()
  x33[[6+j*7]] <- filter(c33[[j+1]], c33[[j+1]][79] > 3) %>% nrow()
  x33[[7+j*7]] <- filter(c33[[j+1]], c33[[j+1]][80] > 3) %>% nrow()
  
  x33[[36+j*10]] <- filter(c33[[j+1]], c33[[j+1]][55] > 3) %>% nrow()
  x33[[37+j*10]] <- filter(c33[[j+1]], c33[[j+1]][56] > 3) %>% nrow()
  x33[[38+j*10]] <- filter(c33[[j+1]], c33[[j+1]][57] > 3) %>% nrow()
  x33[[39+j*10]] <- filter(c33[[j+1]], c33[[j+1]][58] > 3) %>% nrow()
  x33[[40+j*10]] <- filter(c33[[j+1]], c33[[j+1]][59] > 3) %>% nrow()
  x33[[41+j*10]] <- filter(c33[[j+1]], c33[[j+1]][60] > 3) %>% nrow()
  x33[[42+j*10]] <- filter(c33[[j+1]], c33[[j+1]][61] > 3) %>% nrow()
  x33[[43+j*10]] <- filter(c33[[j+1]], c33[[j+1]][62] > 3) %>% nrow()
  x33[[44+j*10]] <- filter(c33[[j+1]], c33[[j+1]][63] > 3) %>% nrow()
  x33[[45+j*10]] <- filter(c33[[j+1]], c33[[j+1]][64] > 3) %>% nrow()
  
  x33[[86+j*9]] <- filter(c33[[j+1]], c33[[j+1]][65] > 3) %>% nrow()
  x33[[87+j*9]] <- filter(c33[[j+1]], c33[[j+1]][66] > 3) %>% nrow()
  x33[[88+j*9]] <- filter(c33[[j+1]], c33[[j+1]][67] > 3) %>% nrow()
  x33[[89+j*9]] <- filter(c33[[j+1]], c33[[j+1]][68] > 3) %>% nrow()
  x33[[90+j*9]] <- filter(c33[[j+1]], c33[[j+1]][69] > 3) %>% nrow()
  x33[[91+j*9]] <- filter(c33[[j+1]], c33[[j+1]][70] > 3) %>% nrow()
  x33[[92+j*9]] <- filter(c33[[j+1]], c33[[j+1]][71] > 3) %>% nrow()
  x33[[93+j*9]] <- filter(c33[[j+1]], c33[[j+1]][72] > 3) %>% nrow()
  x33[[94+j*9]] <- filter(c33[[j+1]], c33[[j+1]][73] > 3) %>% nrow()
  
  x33x[[j+1]] <- data.frame(a = x33[[1+j*7]],b = x33[[2+j*7]],c = x33[[3+j*7]],d = x33[[4+j*7]],e = x33[[5+j*7]],f = x33[[6+j*7]],g = x33[[7+j*7]],
                            h = x33[[36+j*10]],i = x33[[37+j*10]],k = x33[[38+j*10]],l = x33[[39+j*10]],m = x33[[40+j*10]],n = x33[[41+j*10]],o = x33[[42+j*10]],p = x33[[43+j*10]],q = x33[[44+j*10]],r = x33[[45+j*10]],
                            s = x33[[86+j*9]], t = x33[[87+j*9]], u = x33[[88+j*9]], v = x33[[89+j*9]], w = x33[[90+j*9]], x = x33[[91+j*9]], y = x33[[92+j*9]], z = x33[[93+j*9]],zz = x33[[94+j*9]])
  
  
  # HC & dining
  x34[[1+j*7]] <- filter(c34[[j+1]], c34[[j+1]][74] > 3) %>% nrow()
  x34[[2+j*7]] <- filter(c34[[j+1]], c34[[j+1]][75] > 3) %>% nrow()
  x34[[3+j*7]] <- filter(c34[[j+1]], c34[[j+1]][76] > 3) %>% nrow()
  x34[[4+j*7]] <- filter(c34[[j+1]], c34[[j+1]][77] > 3) %>% nrow()
  x34[[5+j*7]] <- filter(c34[[j+1]], c34[[j+1]][78] > 3) %>% nrow()
  x34[[6+j*7]] <- filter(c34[[j+1]], c34[[j+1]][79] > 3) %>% nrow()
  x34[[7+j*7]] <- filter(c34[[j+1]], c34[[j+1]][80] > 3) %>% nrow()
  
  x34[[36+j*10]] <- filter(c34[[j+1]], c34[[j+1]][55] > 3) %>% nrow()
  x34[[37+j*10]] <- filter(c34[[j+1]], c34[[j+1]][56] > 3) %>% nrow()
  x34[[38+j*10]] <- filter(c34[[j+1]], c34[[j+1]][57] > 3) %>% nrow()
  x34[[39+j*10]] <- filter(c34[[j+1]], c34[[j+1]][58] > 3) %>% nrow()
  x34[[40+j*10]] <- filter(c34[[j+1]], c34[[j+1]][59] > 3) %>% nrow()
  x34[[41+j*10]] <- filter(c34[[j+1]], c34[[j+1]][60] > 3) %>% nrow()
  x34[[42+j*10]] <- filter(c34[[j+1]], c34[[j+1]][61] > 3) %>% nrow()
  x34[[43+j*10]] <- filter(c34[[j+1]], c34[[j+1]][62] > 3) %>% nrow()
  x34[[44+j*10]] <- filter(c34[[j+1]], c34[[j+1]][63] > 3) %>% nrow()
  x34[[45+j*10]] <- filter(c34[[j+1]], c34[[j+1]][64] > 3) %>% nrow()
  
  x34[[86+j*9]] <- filter(c34[[j+1]], c34[[j+1]][65] > 3) %>% nrow()
  x34[[87+j*9]] <- filter(c34[[j+1]], c34[[j+1]][66] > 3) %>% nrow()
  x34[[88+j*9]] <- filter(c34[[j+1]], c34[[j+1]][67] > 3) %>% nrow()
  x34[[89+j*9]] <- filter(c34[[j+1]], c34[[j+1]][68] > 3) %>% nrow()
  x34[[90+j*9]] <- filter(c34[[j+1]], c34[[j+1]][69] > 3) %>% nrow()
  x34[[91+j*9]] <- filter(c34[[j+1]], c34[[j+1]][70] > 3) %>% nrow()
  x34[[92+j*9]] <- filter(c34[[j+1]], c34[[j+1]][71] > 3) %>% nrow()
  x34[[93+j*9]] <- filter(c34[[j+1]], c34[[j+1]][72] > 3) %>% nrow()
  x34[[94+j*9]] <- filter(c34[[j+1]], c34[[j+1]][73] > 3) %>% nrow()
  
  x34x[[j+1]] <- data.frame(a = x34[[1+j*7]],b = x34[[2+j*7]],c = x34[[3+j*7]],d = x34[[4+j*7]],e = x34[[5+j*7]],f = x34[[6+j*7]],g = x34[[7+j*7]],
                            h = x34[[36+j*10]],i = x34[[37+j*10]],k = x34[[38+j*10]],l = x34[[39+j*10]],m = x34[[40+j*10]],n = x34[[41+j*10]],o = x34[[42+j*10]],p = x34[[43+j*10]],q = x34[[44+j*10]],r = x34[[45+j*10]],
                            s = x34[[86+j*9]], t = x34[[87+j*9]], u = x34[[88+j*9]], v = x34[[89+j*9]], w = x34[[90+j*9]], x = x34[[91+j*9]], y = x34[[92+j*9]], z = x34[[93+j*9]],zz = x34[[94+j*9]])
  
  
  # HC & business trip
  x35[[1+j*7]] <- filter(c35[[j+1]], c35[[j+1]][74] > 3) %>% nrow()
  x35[[2+j*7]] <- filter(c35[[j+1]], c35[[j+1]][75] > 3) %>% nrow()
  x35[[3+j*7]] <- filter(c35[[j+1]], c35[[j+1]][76] > 3) %>% nrow()
  x35[[4+j*7]] <- filter(c35[[j+1]], c35[[j+1]][77] > 3) %>% nrow()
  x35[[5+j*7]] <- filter(c35[[j+1]], c35[[j+1]][78] > 3) %>% nrow()
  x35[[6+j*7]] <- filter(c35[[j+1]], c35[[j+1]][79] > 3) %>% nrow()
  x35[[7+j*7]] <- filter(c35[[j+1]], c35[[j+1]][80] > 3) %>% nrow()
  
  x35[[36+j*10]] <- filter(c35[[j+1]], c35[[j+1]][55] > 3) %>% nrow()
  x35[[37+j*10]] <- filter(c35[[j+1]], c35[[j+1]][56] > 3) %>% nrow()
  x35[[38+j*10]] <- filter(c35[[j+1]], c35[[j+1]][57] > 3) %>% nrow()
  x35[[39+j*10]] <- filter(c35[[j+1]], c35[[j+1]][58] > 3) %>% nrow()
  x35[[40+j*10]] <- filter(c35[[j+1]], c35[[j+1]][59] > 3) %>% nrow()
  x35[[41+j*10]] <- filter(c35[[j+1]], c35[[j+1]][60] > 3) %>% nrow()
  x35[[42+j*10]] <- filter(c35[[j+1]], c35[[j+1]][61] > 3) %>% nrow()
  x35[[43+j*10]] <- filter(c35[[j+1]], c35[[j+1]][62] > 3) %>% nrow()
  x35[[44+j*10]] <- filter(c35[[j+1]], c35[[j+1]][63] > 3) %>% nrow()
  x35[[45+j*10]] <- filter(c35[[j+1]], c35[[j+1]][64] > 3) %>% nrow()
  
  x35[[86+j*9]] <- filter(c35[[j+1]], c35[[j+1]][65] > 3) %>% nrow()
  x35[[87+j*9]] <- filter(c35[[j+1]], c35[[j+1]][66] > 3) %>% nrow()
  x35[[88+j*9]] <- filter(c35[[j+1]], c35[[j+1]][67] > 3) %>% nrow()
  x35[[89+j*9]] <- filter(c35[[j+1]], c35[[j+1]][68] > 3) %>% nrow()
  x35[[90+j*9]] <- filter(c35[[j+1]], c35[[j+1]][69] > 3) %>% nrow()
  x35[[91+j*9]] <- filter(c35[[j+1]], c35[[j+1]][70] > 3) %>% nrow()
  x35[[92+j*9]] <- filter(c35[[j+1]], c35[[j+1]][71] > 3) %>% nrow()
  x35[[93+j*9]] <- filter(c35[[j+1]], c35[[j+1]][72] > 3) %>% nrow()
  x35[[94+j*9]] <- filter(c35[[j+1]], c35[[j+1]][73] > 3) %>% nrow()
  
  x35x[[j+1]] <- data.frame(a = x35[[1+j*7]],b = x35[[2+j*7]],c = x35[[3+j*7]],d = x35[[4+j*7]],e = x35[[5+j*7]],f = x35[[6+j*7]],g = x35[[7+j*7]],
                            h = x35[[36+j*10]],i = x35[[37+j*10]],k = x35[[38+j*10]],l = x35[[39+j*10]],m = x35[[40+j*10]],n = x35[[41+j*10]],o = x35[[42+j*10]],p = x35[[43+j*10]],q = x35[[44+j*10]],r = x35[[45+j*10]],
                            s = x35[[86+j*9]], t = x35[[87+j*9]], u = x35[[88+j*9]], v = x35[[89+j*9]], w = x35[[90+j*9]], x = x35[[91+j*9]], y = x35[[92+j*9]], z = x35[[93+j*9]],zz = x35[[94+j*9]])
  
  xx3[[j+1]] <- rbind(x31x[[j+1]],x32x[[j+1]],x33x[[j+1]],x34x[[j+1]],x35x[[j+1]])
  
}



for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # TC & commute
  x41[[1+j*7]] <- filter(c41[[j+1]], c41[[j+1]][74] > 3) %>% nrow()
  x41[[2+j*7]] <- filter(c41[[j+1]], c41[[j+1]][75] > 3) %>% nrow()
  x41[[3+j*7]] <- filter(c41[[j+1]], c41[[j+1]][76] > 3) %>% nrow()
  x41[[4+j*7]] <- filter(c41[[j+1]], c41[[j+1]][77] > 3) %>% nrow()
  x41[[5+j*7]] <- filter(c41[[j+1]], c41[[j+1]][78] > 3) %>% nrow()
  x41[[6+j*7]] <- filter(c41[[j+1]], c41[[j+1]][79] > 3) %>% nrow()
  x41[[7+j*7]] <- filter(c41[[j+1]], c41[[j+1]][80] > 3) %>% nrow()
  
  x41[[36+j*10]] <- filter(c41[[j+1]], c41[[j+1]][55] > 3) %>% nrow()
  x41[[37+j*10]] <- filter(c41[[j+1]], c41[[j+1]][56] > 3) %>% nrow()
  x41[[38+j*10]] <- filter(c41[[j+1]], c41[[j+1]][57] > 3) %>% nrow()
  x41[[39+j*10]] <- filter(c41[[j+1]], c41[[j+1]][58] > 3) %>% nrow()
  x41[[40+j*10]] <- filter(c41[[j+1]], c41[[j+1]][59] > 3) %>% nrow()
  x41[[41+j*10]] <- filter(c41[[j+1]], c41[[j+1]][60] > 3) %>% nrow()
  x41[[42+j*10]] <- filter(c41[[j+1]], c41[[j+1]][61] > 3) %>% nrow()
  x41[[43+j*10]] <- filter(c41[[j+1]], c41[[j+1]][62] > 3) %>% nrow()
  x41[[44+j*10]] <- filter(c41[[j+1]], c41[[j+1]][63] > 3) %>% nrow()
  x41[[45+j*10]] <- filter(c41[[j+1]], c41[[j+1]][64] > 3) %>% nrow()
  
  x41[[86+j*9]] <- filter(c41[[j+1]], c41[[j+1]][65] > 3) %>% nrow()
  x41[[87+j*9]] <- filter(c41[[j+1]], c41[[j+1]][66] > 3) %>% nrow()
  x41[[88+j*9]] <- filter(c41[[j+1]], c41[[j+1]][67] > 3) %>% nrow()
  x41[[89+j*9]] <- filter(c41[[j+1]], c41[[j+1]][68] > 3) %>% nrow()
  x41[[90+j*9]] <- filter(c41[[j+1]], c41[[j+1]][69] > 3) %>% nrow()
  x41[[91+j*9]] <- filter(c41[[j+1]], c41[[j+1]][70] > 3) %>% nrow()
  x41[[92+j*9]] <- filter(c41[[j+1]], c41[[j+1]][71] > 3) %>% nrow()
  x41[[93+j*9]] <- filter(c41[[j+1]], c41[[j+1]][72] > 3) %>% nrow()
  x41[[94+j*9]] <- filter(c41[[j+1]], c41[[j+1]][73] > 3) %>% nrow()
  
  x41x[[j+1]] <- data.frame(a = x41[[1+j*7]],b = x41[[2+j*7]],c = x41[[3+j*7]],d = x41[[4+j*7]],e = x41[[5+j*7]],f = x41[[6+j*7]],g = x41[[7+j*7]],
                            h = x41[[36+j*10]],i = x41[[37+j*10]],k = x41[[38+j*10]],l = x41[[39+j*10]],m = x41[[40+j*10]],n = x41[[41+j*10]],o = x41[[42+j*10]],p = x41[[43+j*10]],q = x41[[44+j*10]],r = x41[[45+j*10]],
                            s = x41[[86+j*9]], t = x41[[87+j*9]], u = x41[[88+j*9]], v = x41[[89+j*9]], w = x41[[90+j*9]], x = x41[[91+j*9]], y = x41[[92+j*9]], z = x41[[93+j*9]],zz = x41[[94+j*9]]) 
  
  # TC & weather
  x42[[1+j*7]] <- filter(c42[[j+1]], c42[[j+1]][74] > 3) %>% nrow()
  x42[[2+j*7]] <- filter(c42[[j+1]], c42[[j+1]][75] > 3) %>% nrow()
  x42[[3+j*7]] <- filter(c42[[j+1]], c42[[j+1]][76] > 3) %>% nrow()
  x42[[4+j*7]] <- filter(c42[[j+1]], c42[[j+1]][77] > 3) %>% nrow()
  x42[[5+j*7]] <- filter(c42[[j+1]], c42[[j+1]][78] > 3) %>% nrow()
  x42[[6+j*7]] <- filter(c42[[j+1]], c42[[j+1]][79] > 3) %>% nrow()
  x42[[7+j*7]] <- filter(c42[[j+1]], c42[[j+1]][80] > 3) %>% nrow()
  
  x42[[36+j*10]] <- filter(c42[[j+1]], c42[[j+1]][55] > 3) %>% nrow()
  x42[[37+j*10]] <- filter(c42[[j+1]], c42[[j+1]][56] > 3) %>% nrow()
  x42[[38+j*10]] <- filter(c42[[j+1]], c42[[j+1]][57] > 3) %>% nrow()
  x42[[39+j*10]] <- filter(c42[[j+1]], c42[[j+1]][58] > 3) %>% nrow()
  x42[[40+j*10]] <- filter(c42[[j+1]], c42[[j+1]][59] > 3) %>% nrow()
  x42[[41+j*10]] <- filter(c42[[j+1]], c42[[j+1]][60] > 3) %>% nrow()
  x42[[42+j*10]] <- filter(c42[[j+1]], c42[[j+1]][61] > 3) %>% nrow()
  x42[[43+j*10]] <- filter(c42[[j+1]], c42[[j+1]][62] > 3) %>% nrow()
  x42[[44+j*10]] <- filter(c42[[j+1]], c42[[j+1]][63] > 3) %>% nrow()
  x42[[45+j*10]] <- filter(c42[[j+1]], c42[[j+1]][64] > 3) %>% nrow()
  
  x42[[86+j*9]] <- filter(c42[[j+1]], c42[[j+1]][65] > 3) %>% nrow()
  x42[[87+j*9]] <- filter(c42[[j+1]], c42[[j+1]][66] > 3) %>% nrow()
  x42[[88+j*9]] <- filter(c42[[j+1]], c42[[j+1]][67] > 3) %>% nrow()
  x42[[89+j*9]] <- filter(c42[[j+1]], c42[[j+1]][68] > 3) %>% nrow()
  x42[[90+j*9]] <- filter(c42[[j+1]], c42[[j+1]][69] > 3) %>% nrow()
  x42[[91+j*9]] <- filter(c42[[j+1]], c42[[j+1]][70] > 3) %>% nrow()
  x42[[92+j*9]] <- filter(c42[[j+1]], c42[[j+1]][71] > 3) %>% nrow()
  x42[[93+j*9]] <- filter(c42[[j+1]], c42[[j+1]][72] > 3) %>% nrow()
  x42[[94+j*9]] <- filter(c42[[j+1]], c42[[j+1]][73] > 3) %>% nrow()
  
  x42x[[j+1]] <- data.frame(a = x42[[1+j*7]],b = x42[[2+j*7]],c = x42[[3+j*7]],d = x42[[4+j*7]],e = x42[[5+j*7]],f = x42[[6+j*7]],g = x42[[7+j*7]],
                            h = x42[[36+j*10]],i = x42[[37+j*10]],k = x42[[38+j*10]],l = x42[[39+j*10]],m = x42[[40+j*10]],n = x42[[41+j*10]],o = x42[[42+j*10]],p = x42[[43+j*10]],q = x42[[44+j*10]],r = x42[[45+j*10]],
                            s = x42[[86+j*9]], t = x42[[87+j*9]], u = x42[[88+j*9]], v = x42[[89+j*9]], w = x42[[90+j*9]], x = x42[[91+j*9]], y = x42[[92+j*9]], z = x42[[93+j*9]],zz = x42[[94+j*9]])
  
  # TC & shopping
  x43[[1+j*7]] <- filter(c43[[j+1]], c43[[j+1]][74] > 3) %>% nrow()
  x43[[2+j*7]] <- filter(c43[[j+1]], c43[[j+1]][75] > 3) %>% nrow()
  x43[[3+j*7]] <- filter(c43[[j+1]], c43[[j+1]][76] > 3) %>% nrow()
  x43[[4+j*7]] <- filter(c43[[j+1]], c43[[j+1]][77] > 3) %>% nrow()
  x43[[5+j*7]] <- filter(c43[[j+1]], c43[[j+1]][78] > 3) %>% nrow()
  x43[[6+j*7]] <- filter(c43[[j+1]], c43[[j+1]][79] > 3) %>% nrow()
  x43[[7+j*7]] <- filter(c43[[j+1]], c43[[j+1]][80] > 3) %>% nrow()
  
  x43[[36+j*10]] <- filter(c43[[j+1]], c43[[j+1]][55] > 3) %>% nrow()
  x43[[37+j*10]] <- filter(c43[[j+1]], c43[[j+1]][56] > 3) %>% nrow()
  x43[[38+j*10]] <- filter(c43[[j+1]], c43[[j+1]][57] > 3) %>% nrow()
  x43[[39+j*10]] <- filter(c43[[j+1]], c43[[j+1]][58] > 3) %>% nrow()
  x43[[40+j*10]] <- filter(c43[[j+1]], c43[[j+1]][59] > 3) %>% nrow()
  x43[[41+j*10]] <- filter(c43[[j+1]], c43[[j+1]][60] > 3) %>% nrow()
  x43[[42+j*10]] <- filter(c43[[j+1]], c43[[j+1]][61] > 3) %>% nrow()
  x43[[43+j*10]] <- filter(c43[[j+1]], c43[[j+1]][62] > 3) %>% nrow()
  x43[[44+j*10]] <- filter(c43[[j+1]], c43[[j+1]][63] > 3) %>% nrow()
  x43[[45+j*10]] <- filter(c43[[j+1]], c43[[j+1]][64] > 3) %>% nrow()
  
  x43[[86+j*9]] <- filter(c43[[j+1]], c43[[j+1]][65] > 3) %>% nrow()
  x43[[87+j*9]] <- filter(c43[[j+1]], c43[[j+1]][66] > 3) %>% nrow()
  x43[[88+j*9]] <- filter(c43[[j+1]], c43[[j+1]][67] > 3) %>% nrow()
  x43[[89+j*9]] <- filter(c43[[j+1]], c43[[j+1]][68] > 3) %>% nrow()
  x43[[90+j*9]] <- filter(c43[[j+1]], c43[[j+1]][69] > 3) %>% nrow()
  x43[[91+j*9]] <- filter(c43[[j+1]], c43[[j+1]][70] > 3) %>% nrow()
  x43[[92+j*9]] <- filter(c43[[j+1]], c43[[j+1]][71] > 3) %>% nrow()
  x43[[93+j*9]] <- filter(c43[[j+1]], c43[[j+1]][72] > 3) %>% nrow()
  x43[[94+j*9]] <- filter(c43[[j+1]], c43[[j+1]][73] > 3) %>% nrow()
  
  x43x[[j+1]] <- data.frame(a = x43[[1+j*7]],b = x43[[2+j*7]],c = x43[[3+j*7]],d = x43[[4+j*7]],e = x43[[5+j*7]],f = x43[[6+j*7]],g = x43[[7+j*7]],
                            h = x43[[36+j*10]],i = x43[[37+j*10]],k = x43[[38+j*10]],l = x43[[39+j*10]],m = x43[[40+j*10]],n = x43[[41+j*10]],o = x43[[42+j*10]],p = x43[[43+j*10]],q = x43[[44+j*10]],r = x43[[45+j*10]],
                            s = x43[[86+j*9]], t = x43[[87+j*9]], u = x43[[88+j*9]], v = x43[[89+j*9]], w = x43[[90+j*9]], x = x43[[91+j*9]], y = x43[[92+j*9]], z = x43[[93+j*9]],zz = x43[[94+j*9]])
  
  
  # TC & dining
  x44[[1+j*7]] <- filter(c44[[j+1]], c44[[j+1]][74] > 3) %>% nrow()
  x44[[2+j*7]] <- filter(c44[[j+1]], c44[[j+1]][75] > 3) %>% nrow()
  x44[[3+j*7]] <- filter(c44[[j+1]], c44[[j+1]][76] > 3) %>% nrow()
  x44[[4+j*7]] <- filter(c44[[j+1]], c44[[j+1]][77] > 3) %>% nrow()
  x44[[5+j*7]] <- filter(c44[[j+1]], c44[[j+1]][78] > 3) %>% nrow()
  x44[[6+j*7]] <- filter(c44[[j+1]], c44[[j+1]][79] > 3) %>% nrow()
  x44[[7+j*7]] <- filter(c44[[j+1]], c44[[j+1]][80] > 3) %>% nrow()
  
  x44[[36+j*10]] <- filter(c44[[j+1]], c44[[j+1]][55] > 3) %>% nrow()
  x44[[37+j*10]] <- filter(c44[[j+1]], c44[[j+1]][56] > 3) %>% nrow()
  x44[[38+j*10]] <- filter(c44[[j+1]], c44[[j+1]][57] > 3) %>% nrow()
  x44[[39+j*10]] <- filter(c44[[j+1]], c44[[j+1]][58] > 3) %>% nrow()
  x44[[40+j*10]] <- filter(c44[[j+1]], c44[[j+1]][59] > 3) %>% nrow()
  x44[[41+j*10]] <- filter(c44[[j+1]], c44[[j+1]][60] > 3) %>% nrow()
  x44[[42+j*10]] <- filter(c44[[j+1]], c44[[j+1]][61] > 3) %>% nrow()
  x44[[43+j*10]] <- filter(c44[[j+1]], c44[[j+1]][62] > 3) %>% nrow()
  x44[[44+j*10]] <- filter(c44[[j+1]], c44[[j+1]][63] > 3) %>% nrow()
  x44[[45+j*10]] <- filter(c44[[j+1]], c44[[j+1]][64] > 3) %>% nrow()
  
  x44[[86+j*9]] <- filter(c44[[j+1]], c44[[j+1]][65] > 3) %>% nrow()
  x44[[87+j*9]] <- filter(c44[[j+1]], c44[[j+1]][66] > 3) %>% nrow()
  x44[[88+j*9]] <- filter(c44[[j+1]], c44[[j+1]][67] > 3) %>% nrow()
  x44[[89+j*9]] <- filter(c44[[j+1]], c44[[j+1]][68] > 3) %>% nrow()
  x44[[90+j*9]] <- filter(c44[[j+1]], c44[[j+1]][69] > 3) %>% nrow()
  x44[[91+j*9]] <- filter(c44[[j+1]], c44[[j+1]][70] > 3) %>% nrow()
  x44[[92+j*9]] <- filter(c44[[j+1]], c44[[j+1]][71] > 3) %>% nrow()
  x44[[93+j*9]] <- filter(c44[[j+1]], c44[[j+1]][72] > 3) %>% nrow()
  x44[[94+j*9]] <- filter(c44[[j+1]], c44[[j+1]][73] > 3) %>% nrow()
  
  x44x[[j+1]] <- data.frame(a = x44[[1+j*7]],b = x44[[2+j*7]],c = x44[[3+j*7]],d = x44[[4+j*7]],e = x44[[5+j*7]],f = x44[[6+j*7]],g = x44[[7+j*7]],
                            h = x44[[36+j*10]],i = x44[[37+j*10]],k = x44[[38+j*10]],l = x44[[39+j*10]],m = x44[[40+j*10]],n = x44[[41+j*10]],o = x44[[42+j*10]],p = x44[[43+j*10]],q = x44[[44+j*10]],r = x44[[45+j*10]],
                            s = x44[[86+j*9]], t = x44[[87+j*9]], u = x44[[88+j*9]], v = x44[[89+j*9]], w = x44[[90+j*9]], x = x44[[91+j*9]], y = x44[[92+j*9]], z = x44[[93+j*9]],zz = x44[[94+j*9]])
  
  
  # TC & business trip
  x45[[1+j*7]] <- filter(c45[[j+1]], c45[[j+1]][74] > 3) %>% nrow()
  x45[[2+j*7]] <- filter(c45[[j+1]], c45[[j+1]][75] > 3) %>% nrow()
  x45[[3+j*7]] <- filter(c45[[j+1]], c45[[j+1]][76] > 3) %>% nrow()
  x45[[4+j*7]] <- filter(c45[[j+1]], c45[[j+1]][77] > 3) %>% nrow()
  x45[[5+j*7]] <- filter(c45[[j+1]], c45[[j+1]][78] > 3) %>% nrow()
  x45[[6+j*7]] <- filter(c45[[j+1]], c45[[j+1]][79] > 3) %>% nrow()
  x45[[7+j*7]] <- filter(c45[[j+1]], c45[[j+1]][80] > 3) %>% nrow()
  
  x45[[36+j*10]] <- filter(c45[[j+1]], c45[[j+1]][55] > 3) %>% nrow()
  x45[[37+j*10]] <- filter(c45[[j+1]], c45[[j+1]][56] > 3) %>% nrow()
  x45[[38+j*10]] <- filter(c45[[j+1]], c45[[j+1]][57] > 3) %>% nrow()
  x45[[39+j*10]] <- filter(c45[[j+1]], c45[[j+1]][58] > 3) %>% nrow()
  x45[[40+j*10]] <- filter(c45[[j+1]], c45[[j+1]][59] > 3) %>% nrow()
  x45[[41+j*10]] <- filter(c45[[j+1]], c45[[j+1]][60] > 3) %>% nrow()
  x45[[42+j*10]] <- filter(c45[[j+1]], c45[[j+1]][61] > 3) %>% nrow()
  x45[[43+j*10]] <- filter(c45[[j+1]], c45[[j+1]][62] > 3) %>% nrow()
  x45[[44+j*10]] <- filter(c45[[j+1]], c45[[j+1]][63] > 3) %>% nrow()
  x45[[45+j*10]] <- filter(c45[[j+1]], c45[[j+1]][64] > 3) %>% nrow()
  
  x45[[86+j*9]] <- filter(c45[[j+1]], c45[[j+1]][65] > 3) %>% nrow()
  x45[[87+j*9]] <- filter(c45[[j+1]], c45[[j+1]][66] > 3) %>% nrow()
  x45[[88+j*9]] <- filter(c45[[j+1]], c45[[j+1]][67] > 3) %>% nrow()
  x45[[89+j*9]] <- filter(c45[[j+1]], c45[[j+1]][68] > 3) %>% nrow()
  x45[[90+j*9]] <- filter(c45[[j+1]], c45[[j+1]][69] > 3) %>% nrow()
  x45[[91+j*9]] <- filter(c45[[j+1]], c45[[j+1]][70] > 3) %>% nrow()
  x45[[92+j*9]] <- filter(c45[[j+1]], c45[[j+1]][71] > 3) %>% nrow()
  x45[[93+j*9]] <- filter(c45[[j+1]], c45[[j+1]][72] > 3) %>% nrow()
  x45[[94+j*9]] <- filter(c45[[j+1]], c45[[j+1]][73] > 3) %>% nrow()
  
  x45x[[j+1]] <- data.frame(a = x45[[1+j*7]],b = x45[[2+j*7]],c = x45[[3+j*7]],d = x45[[4+j*7]],e = x45[[5+j*7]],f = x45[[6+j*7]],g = x45[[7+j*7]],
                            h = x45[[36+j*10]],i = x45[[37+j*10]],k = x45[[38+j*10]],l = x45[[39+j*10]],m = x45[[40+j*10]],n = x45[[41+j*10]],o = x45[[42+j*10]],p = x45[[43+j*10]],q = x45[[44+j*10]],r = x45[[45+j*10]],
                            s = x45[[86+j*9]], t = x45[[87+j*9]], u = x45[[88+j*9]], v = x45[[89+j*9]], w = x45[[90+j*9]], x = x45[[91+j*9]], y = x45[[92+j*9]], z = x45[[93+j*9]],zz = x45[[94+j*9]])
  
  xx4[[j+1]] <- rbind(x41x[[j+1]],x42x[[j+1]],x43x[[j+1]],x44x[[j+1]],x45x[[j+1]])
  
}



for(j in 0:4){
  # 0: OWN CAR / 1: PUBLIC TRANSPORTATION / 2: SHAREBIKE / 3: APP DISPATCHER / 4: ROAD-HAIL TAXI
  # KH & commute
  x51[[1+j*7]] <- filter(c51[[j+1]], c51[[j+1]][74] > 3) %>% nrow()
  x51[[2+j*7]] <- filter(c51[[j+1]], c51[[j+1]][75] > 3) %>% nrow()
  x51[[3+j*7]] <- filter(c51[[j+1]], c51[[j+1]][76] > 3) %>% nrow()
  x51[[4+j*7]] <- filter(c51[[j+1]], c51[[j+1]][77] > 3) %>% nrow()
  x51[[5+j*7]] <- filter(c51[[j+1]], c51[[j+1]][78] > 3) %>% nrow()
  x51[[6+j*7]] <- filter(c51[[j+1]], c51[[j+1]][79] > 3) %>% nrow()
  x51[[7+j*7]] <- filter(c51[[j+1]], c51[[j+1]][80] > 3) %>% nrow()
  
  x51[[36+j*10]] <- filter(c51[[j+1]], c51[[j+1]][55] > 3) %>% nrow()
  x51[[37+j*10]] <- filter(c51[[j+1]], c51[[j+1]][56] > 3) %>% nrow()
  x51[[38+j*10]] <- filter(c51[[j+1]], c51[[j+1]][57] > 3) %>% nrow()
  x51[[39+j*10]] <- filter(c51[[j+1]], c51[[j+1]][58] > 3) %>% nrow()
  x51[[40+j*10]] <- filter(c51[[j+1]], c51[[j+1]][59] > 3) %>% nrow()
  x51[[41+j*10]] <- filter(c51[[j+1]], c51[[j+1]][60] > 3) %>% nrow()
  x51[[42+j*10]] <- filter(c51[[j+1]], c51[[j+1]][61] > 3) %>% nrow()
  x51[[43+j*10]] <- filter(c51[[j+1]], c51[[j+1]][62] > 3) %>% nrow()
  x51[[44+j*10]] <- filter(c51[[j+1]], c51[[j+1]][63] > 3) %>% nrow()
  x51[[45+j*10]] <- filter(c51[[j+1]], c51[[j+1]][64] > 3) %>% nrow()
  
  x51[[86+j*9]] <- filter(c51[[j+1]], c51[[j+1]][65] > 3) %>% nrow()
  x51[[87+j*9]] <- filter(c51[[j+1]], c51[[j+1]][66] > 3) %>% nrow()
  x51[[88+j*9]] <- filter(c51[[j+1]], c51[[j+1]][67] > 3) %>% nrow()
  x51[[89+j*9]] <- filter(c51[[j+1]], c51[[j+1]][68] > 3) %>% nrow()
  x51[[90+j*9]] <- filter(c51[[j+1]], c51[[j+1]][69] > 3) %>% nrow()
  x51[[91+j*9]] <- filter(c51[[j+1]], c51[[j+1]][70] > 3) %>% nrow()
  x51[[92+j*9]] <- filter(c51[[j+1]], c51[[j+1]][71] > 3) %>% nrow()
  x51[[93+j*9]] <- filter(c51[[j+1]], c51[[j+1]][72] > 3) %>% nrow()
  x51[[94+j*9]] <- filter(c51[[j+1]], c51[[j+1]][73] > 3) %>% nrow()
  
  x51x[[j+1]] <- data.frame(a = x51[[1+j*7]],b = x51[[2+j*7]],c = x51[[3+j*7]],d = x51[[4+j*7]],e = x51[[5+j*7]],f = x51[[6+j*7]],g = x51[[7+j*7]],
                            h = x51[[36+j*10]],i = x51[[37+j*10]],k = x51[[38+j*10]],l = x51[[39+j*10]],m = x51[[40+j*10]],n = x51[[41+j*10]],o = x51[[42+j*10]],p = x51[[43+j*10]],q = x51[[44+j*10]],r = x51[[45+j*10]],
                            s = x51[[86+j*9]], t = x51[[87+j*9]], u = x51[[88+j*9]], v = x51[[89+j*9]], w = x51[[90+j*9]], x = x51[[91+j*9]], y = x51[[92+j*9]], z = x51[[93+j*9]],zz = x51[[94+j*9]]) 
  
  # KH & weather
  x52[[1+j*7]] <- filter(c52[[j+1]], c52[[j+1]][74] > 3) %>% nrow()
  x52[[2+j*7]] <- filter(c52[[j+1]], c52[[j+1]][75] > 3) %>% nrow()
  x52[[3+j*7]] <- filter(c52[[j+1]], c52[[j+1]][76] > 3) %>% nrow()
  x52[[4+j*7]] <- filter(c52[[j+1]], c52[[j+1]][77] > 3) %>% nrow()
  x52[[5+j*7]] <- filter(c52[[j+1]], c52[[j+1]][78] > 3) %>% nrow()
  x52[[6+j*7]] <- filter(c52[[j+1]], c52[[j+1]][79] > 3) %>% nrow()
  x52[[7+j*7]] <- filter(c52[[j+1]], c52[[j+1]][80] > 3) %>% nrow()
  
  x52[[36+j*10]] <- filter(c52[[j+1]], c52[[j+1]][55] > 3) %>% nrow()
  x52[[37+j*10]] <- filter(c52[[j+1]], c52[[j+1]][56] > 3) %>% nrow()
  x52[[38+j*10]] <- filter(c52[[j+1]], c52[[j+1]][57] > 3) %>% nrow()
  x52[[39+j*10]] <- filter(c52[[j+1]], c52[[j+1]][58] > 3) %>% nrow()
  x52[[40+j*10]] <- filter(c52[[j+1]], c52[[j+1]][59] > 3) %>% nrow()
  x52[[41+j*10]] <- filter(c52[[j+1]], c52[[j+1]][60] > 3) %>% nrow()
  x52[[42+j*10]] <- filter(c52[[j+1]], c52[[j+1]][61] > 3) %>% nrow()
  x52[[43+j*10]] <- filter(c52[[j+1]], c52[[j+1]][62] > 3) %>% nrow()
  x52[[44+j*10]] <- filter(c52[[j+1]], c52[[j+1]][63] > 3) %>% nrow()
  x52[[45+j*10]] <- filter(c52[[j+1]], c52[[j+1]][64] > 3) %>% nrow()
  
  x52[[86+j*9]] <- filter(c52[[j+1]], c52[[j+1]][65] > 3) %>% nrow()
  x52[[87+j*9]] <- filter(c52[[j+1]], c52[[j+1]][66] > 3) %>% nrow()
  x52[[88+j*9]] <- filter(c52[[j+1]], c52[[j+1]][67] > 3) %>% nrow()
  x52[[89+j*9]] <- filter(c52[[j+1]], c52[[j+1]][68] > 3) %>% nrow()
  x52[[90+j*9]] <- filter(c52[[j+1]], c52[[j+1]][69] > 3) %>% nrow()
  x52[[91+j*9]] <- filter(c52[[j+1]], c52[[j+1]][70] > 3) %>% nrow()
  x52[[92+j*9]] <- filter(c52[[j+1]], c52[[j+1]][71] > 3) %>% nrow()
  x52[[93+j*9]] <- filter(c52[[j+1]], c52[[j+1]][72] > 3) %>% nrow()
  x52[[94+j*9]] <- filter(c52[[j+1]], c52[[j+1]][73] > 3) %>% nrow()
  
  x52x[[j+1]] <- data.frame(a = x52[[1+j*7]],b = x52[[2+j*7]],c = x52[[3+j*7]],d = x52[[4+j*7]],e = x52[[5+j*7]],f = x52[[6+j*7]],g = x52[[7+j*7]],
                            h = x52[[36+j*10]],i = x52[[37+j*10]],k = x52[[38+j*10]],l = x52[[39+j*10]],m = x52[[40+j*10]],n = x52[[41+j*10]],o = x52[[42+j*10]],p = x52[[43+j*10]],q = x52[[44+j*10]],r = x52[[45+j*10]],
                            s = x52[[86+j*9]], t = x52[[87+j*9]], u = x52[[88+j*9]], v = x52[[89+j*9]], w = x52[[90+j*9]], x = x52[[91+j*9]], y = x52[[92+j*9]], z = x52[[93+j*9]],zz = x52[[94+j*9]])
  
  # KH & shopping
  x53[[1+j*7]] <- filter(c53[[j+1]], c53[[j+1]][74] > 3) %>% nrow()
  x53[[2+j*7]] <- filter(c53[[j+1]], c53[[j+1]][75] > 3) %>% nrow()
  x53[[3+j*7]] <- filter(c53[[j+1]], c53[[j+1]][76] > 3) %>% nrow()
  x53[[4+j*7]] <- filter(c53[[j+1]], c53[[j+1]][77] > 3) %>% nrow()
  x53[[5+j*7]] <- filter(c53[[j+1]], c53[[j+1]][78] > 3) %>% nrow()
  x53[[6+j*7]] <- filter(c53[[j+1]], c53[[j+1]][79] > 3) %>% nrow()
  x53[[7+j*7]] <- filter(c53[[j+1]], c53[[j+1]][80] > 3) %>% nrow()
  
  x53[[36+j*10]] <- filter(c53[[j+1]], c53[[j+1]][55] > 3) %>% nrow()
  x53[[37+j*10]] <- filter(c53[[j+1]], c53[[j+1]][56] > 3) %>% nrow()
  x53[[38+j*10]] <- filter(c53[[j+1]], c53[[j+1]][57] > 3) %>% nrow()
  x53[[39+j*10]] <- filter(c53[[j+1]], c53[[j+1]][58] > 3) %>% nrow()
  x53[[40+j*10]] <- filter(c53[[j+1]], c53[[j+1]][59] > 3) %>% nrow()
  x53[[41+j*10]] <- filter(c53[[j+1]], c53[[j+1]][60] > 3) %>% nrow()
  x53[[42+j*10]] <- filter(c53[[j+1]], c53[[j+1]][61] > 3) %>% nrow()
  x53[[43+j*10]] <- filter(c53[[j+1]], c53[[j+1]][62] > 3) %>% nrow()
  x53[[44+j*10]] <- filter(c53[[j+1]], c53[[j+1]][63] > 3) %>% nrow()
  x53[[45+j*10]] <- filter(c53[[j+1]], c53[[j+1]][64] > 3) %>% nrow()
  
  x53[[86+j*9]] <- filter(c53[[j+1]], c53[[j+1]][65] > 3) %>% nrow()
  x53[[87+j*9]] <- filter(c53[[j+1]], c53[[j+1]][66] > 3) %>% nrow()
  x53[[88+j*9]] <- filter(c53[[j+1]], c53[[j+1]][67] > 3) %>% nrow()
  x53[[89+j*9]] <- filter(c53[[j+1]], c53[[j+1]][68] > 3) %>% nrow()
  x53[[90+j*9]] <- filter(c53[[j+1]], c53[[j+1]][69] > 3) %>% nrow()
  x53[[91+j*9]] <- filter(c53[[j+1]], c53[[j+1]][70] > 3) %>% nrow()
  x53[[92+j*9]] <- filter(c53[[j+1]], c53[[j+1]][71] > 3) %>% nrow()
  x53[[93+j*9]] <- filter(c53[[j+1]], c53[[j+1]][72] > 3) %>% nrow()
  x53[[94+j*9]] <- filter(c53[[j+1]], c53[[j+1]][73] > 3) %>% nrow()
  
  x53x[[j+1]] <- data.frame(a = x53[[1+j*7]],b = x53[[2+j*7]],c = x53[[3+j*7]],d = x53[[4+j*7]],e = x53[[5+j*7]],f = x53[[6+j*7]],g = x53[[7+j*7]],
                            h = x53[[36+j*10]],i = x53[[37+j*10]],k = x53[[38+j*10]],l = x53[[39+j*10]],m = x53[[40+j*10]],n = x53[[41+j*10]],o = x53[[42+j*10]],p = x53[[43+j*10]],q = x53[[44+j*10]],r = x53[[45+j*10]],
                            s = x53[[86+j*9]], t = x53[[87+j*9]], u = x53[[88+j*9]], v = x53[[89+j*9]], w = x53[[90+j*9]], x = x53[[91+j*9]], y = x53[[92+j*9]], z = x53[[93+j*9]],zz = x53[[94+j*9]])
  
  
  # KH & dining
  x54[[1+j*7]] <- filter(c54[[j+1]], c54[[j+1]][74] > 3) %>% nrow()
  x54[[2+j*7]] <- filter(c54[[j+1]], c54[[j+1]][75] > 3) %>% nrow()
  x54[[3+j*7]] <- filter(c54[[j+1]], c54[[j+1]][76] > 3) %>% nrow()
  x54[[4+j*7]] <- filter(c54[[j+1]], c54[[j+1]][77] > 3) %>% nrow()
  x54[[5+j*7]] <- filter(c54[[j+1]], c54[[j+1]][78] > 3) %>% nrow()
  x54[[6+j*7]] <- filter(c54[[j+1]], c54[[j+1]][79] > 3) %>% nrow()
  x54[[7+j*7]] <- filter(c54[[j+1]], c54[[j+1]][80] > 3) %>% nrow()
  
  x54[[36+j*10]] <- filter(c54[[j+1]], c54[[j+1]][55] > 3) %>% nrow()
  x54[[37+j*10]] <- filter(c54[[j+1]], c54[[j+1]][56] > 3) %>% nrow()
  x54[[38+j*10]] <- filter(c54[[j+1]], c54[[j+1]][57] > 3) %>% nrow()
  x54[[39+j*10]] <- filter(c54[[j+1]], c54[[j+1]][58] > 3) %>% nrow()
  x54[[40+j*10]] <- filter(c54[[j+1]], c54[[j+1]][59] > 3) %>% nrow()
  x54[[41+j*10]] <- filter(c54[[j+1]], c54[[j+1]][60] > 3) %>% nrow()
  x54[[42+j*10]] <- filter(c54[[j+1]], c54[[j+1]][61] > 3) %>% nrow()
  x54[[43+j*10]] <- filter(c54[[j+1]], c54[[j+1]][62] > 3) %>% nrow()
  x54[[44+j*10]] <- filter(c54[[j+1]], c54[[j+1]][63] > 3) %>% nrow()
  x54[[45+j*10]] <- filter(c54[[j+1]], c54[[j+1]][64] > 3) %>% nrow()
  
  x54[[86+j*9]] <- filter(c54[[j+1]], c54[[j+1]][65] > 3) %>% nrow()
  x54[[87+j*9]] <- filter(c54[[j+1]], c54[[j+1]][66] > 3) %>% nrow()
  x54[[88+j*9]] <- filter(c54[[j+1]], c54[[j+1]][67] > 3) %>% nrow()
  x54[[89+j*9]] <- filter(c54[[j+1]], c54[[j+1]][68] > 3) %>% nrow()
  x54[[90+j*9]] <- filter(c54[[j+1]], c54[[j+1]][69] > 3) %>% nrow()
  x54[[91+j*9]] <- filter(c54[[j+1]], c54[[j+1]][70] > 3) %>% nrow()
  x54[[92+j*9]] <- filter(c54[[j+1]], c54[[j+1]][71] > 3) %>% nrow()
  x54[[93+j*9]] <- filter(c54[[j+1]], c54[[j+1]][72] > 3) %>% nrow()
  x54[[94+j*9]] <- filter(c54[[j+1]], c54[[j+1]][73] > 3) %>% nrow()
  
  x54x[[j+1]] <- data.frame(a = x54[[1+j*7]],b = x54[[2+j*7]],c = x54[[3+j*7]],d = x54[[4+j*7]],e = x54[[5+j*7]],f = x54[[6+j*7]],g = x54[[7+j*7]],
                            h = x54[[36+j*10]],i = x54[[37+j*10]],k = x54[[38+j*10]],l = x54[[39+j*10]],m = x54[[40+j*10]],n = x54[[41+j*10]],o = x54[[42+j*10]],p = x54[[43+j*10]],q = x54[[44+j*10]],r = x54[[45+j*10]],
                            s = x54[[86+j*9]], t = x54[[87+j*9]], u = x54[[88+j*9]], v = x54[[89+j*9]], w = x54[[90+j*9]], x = x54[[91+j*9]], y = x54[[92+j*9]], z = x54[[93+j*9]],zz = x54[[94+j*9]])
  
  
  # KH & business trip
  x55[[1+j*7]] <- filter(c55[[j+1]], c55[[j+1]][74] > 3) %>% nrow()
  x55[[2+j*7]] <- filter(c55[[j+1]], c55[[j+1]][75] > 3) %>% nrow()
  x55[[3+j*7]] <- filter(c55[[j+1]], c55[[j+1]][76] > 3) %>% nrow()
  x55[[4+j*7]] <- filter(c55[[j+1]], c55[[j+1]][77] > 3) %>% nrow()
  x55[[5+j*7]] <- filter(c55[[j+1]], c55[[j+1]][78] > 3) %>% nrow()
  x55[[6+j*7]] <- filter(c55[[j+1]], c55[[j+1]][79] > 3) %>% nrow()
  x55[[7+j*7]] <- filter(c55[[j+1]], c55[[j+1]][80] > 3) %>% nrow()
  
  x55[[36+j*10]] <- filter(c55[[j+1]], c55[[j+1]][55] > 3) %>% nrow()
  x55[[37+j*10]] <- filter(c55[[j+1]], c55[[j+1]][56] > 3) %>% nrow()
  x55[[38+j*10]] <- filter(c55[[j+1]], c55[[j+1]][57] > 3) %>% nrow()
  x55[[39+j*10]] <- filter(c55[[j+1]], c55[[j+1]][58] > 3) %>% nrow()
  x55[[40+j*10]] <- filter(c55[[j+1]], c55[[j+1]][59] > 3) %>% nrow()
  x55[[41+j*10]] <- filter(c55[[j+1]], c55[[j+1]][60] > 3) %>% nrow()
  x55[[42+j*10]] <- filter(c55[[j+1]], c55[[j+1]][61] > 3) %>% nrow()
  x55[[43+j*10]] <- filter(c55[[j+1]], c55[[j+1]][62] > 3) %>% nrow()
  x55[[44+j*10]] <- filter(c55[[j+1]], c55[[j+1]][63] > 3) %>% nrow()
  x55[[45+j*10]] <- filter(c55[[j+1]], c55[[j+1]][64] > 3) %>% nrow()
  
  x55[[86+j*9]] <- filter(c55[[j+1]], c55[[j+1]][65] > 3) %>% nrow()
  x55[[87+j*9]] <- filter(c55[[j+1]], c55[[j+1]][66] > 3) %>% nrow()
  x55[[88+j*9]] <- filter(c55[[j+1]], c55[[j+1]][67] > 3) %>% nrow()
  x55[[89+j*9]] <- filter(c55[[j+1]], c55[[j+1]][68] > 3) %>% nrow()
  x55[[90+j*9]] <- filter(c55[[j+1]], c55[[j+1]][69] > 3) %>% nrow()
  x55[[91+j*9]] <- filter(c55[[j+1]], c55[[j+1]][70] > 3) %>% nrow()
  x55[[92+j*9]] <- filter(c55[[j+1]], c55[[j+1]][71] > 3) %>% nrow()
  x55[[93+j*9]] <- filter(c55[[j+1]], c55[[j+1]][72] > 3) %>% nrow()
  x55[[94+j*9]] <- filter(c55[[j+1]], c55[[j+1]][73] > 3) %>% nrow()
  
  x55x[[j+1]] <- data.frame(a = x55[[1+j*7]],b = x55[[2+j*7]],c = x55[[3+j*7]],d = x55[[4+j*7]],e = x55[[5+j*7]],f = x55[[6+j*7]],g = x55[[7+j*7]],
                            h = x55[[36+j*10]],i = x55[[37+j*10]],k = x55[[38+j*10]],l = x55[[39+j*10]],m = x55[[40+j*10]],n = x55[[41+j*10]],o = x55[[42+j*10]],p = x55[[43+j*10]],q = x55[[44+j*10]],r = x55[[45+j*10]],
                            s = x55[[86+j*9]], t = x55[[87+j*9]], u = x55[[88+j*9]], v = x55[[89+j*9]], w = x55[[90+j*9]], x = x55[[91+j*9]], y = x55[[92+j*9]], z = x55[[93+j*9]],zz = x55[[94+j*9]])
  
  xx5[[j+1]] <- rbind(x51x[[j+1]],x52x[[j+1]],x53x[[j+1]],x54x[[j+1]],x55x[[j+1]])
  
}

# tpe : own car usage in different occasion
xx1[[1]] <- xx1[[1]][-4,]
xx1[[1]]
# tpe : public transportation usage in different occasion
xx1[[2]] <- xx1[[2]][-4,]
xx1[[2]]
# tpe : sharebike usage in different occasion
xx1[[3]] <- xx1[[3]][-4,]
xx1[[3]]
# tpe : app dispatcher usage in different occasion
xx1[[4]] <- xx1[[4]][-4,]
xx1[[4]]
# tpe : road-hail taxi usage in different occasion
xx1[[5]] <- xx1[[5]][-4,]
xx1[[5]]


# ty : own car usage in different occasion
xx2[[1]] <- xx2[[1]][-4,]
xx2[[1]]
# ty : public transportation usage in different occasion
xx2[[2]] <- xx2[[2]][-4,]
xx2[[2]]
# ty : sharebike usage in different occasion
xx2[[3]] <- xx2[[3]][-4,]
xx2[[3]]
# ty : app dispatcher usage in different occasion
xx2[[4]] <- xx2[[4]][-4,]
xx2[[4]]
# ty : road-hail taxi usage in different occasion
xx2[[5]] <- xx2[[5]][-4,]
xx2[[5]]


# hc : own car usage in different occasion
xx3[[1]] <- xx3[[1]][-4,]
xx3[[1]]
# hc : public transportation usage in different occasion
xx3[[2]] <- xx3[[2]][-4,]
xx3[[2]]
# hc : sharebike usage in different occasion
xx3[[3]] <- xx3[[3]][-4,]
xx3[[3]]
# hc : app dispatcher usage in different occasion
xx3[[4]] <- xx3[[4]][-4,]
xx3[[4]]
# hc : road-hail taxi usage in different occasion
xx3[[5]] <- xx3[[5]][-4,]
xx3[[5]]


# tc : own car usage in different occasion
xx4[[1]] <- xx4[[1]][-4,]
xx4[[1]]
# tc : public transportation usage in different occasion
xx4[[2]] <- xx4[[2]][-4,]
xx4[[2]]
# tc : sharebike usage in different occasion
xx4[[3]] <- xx4[[3]][-4,]
xx4[[3]]
# tc : app dispatcher usage in different occasion
xx4[[4]] <- xx4[[4]][-4,]
xx4[[4]]
# tc : road-hail taxi usage in different occasion
xx4[[5]] <- xx4[[5]][-4,]
xx4[[5]]


# kh : own car usage in different occasion
xx5[[1]] <- xx5[[1]][-4,]
xx5[[1]]
# kh : public transportation usage in different occasion
xx5[[2]] <- xx5[[2]][-4,]
xx5[[2]]
# kh : sharebike usage in different occasion
xx5[[3]] <- xx5[[3]][-4,]
xx5[[3]]
# kh : app dispatcher usage in different occasion
xx5[[4]] <- xx5[[4]][-4,]
xx5[[4]]
# kh : road-hail taxi usage in different occasion
xx5[[5]] <- xx5[[5]][-4,]
xx5[[5]]


# change to percentage
for(k in 1:5){
  xx1[[k]][1,] <- xx1[[k]][1,]/nrow(c11[[k]])
  xx1[[k]][2,] <- xx1[[k]][2,]/nrow(c12[[k]])
  xx1[[k]][3,] <- xx1[[k]][3,]/nrow(c13[[k]])
  xx1[[k]][4,] <- xx1[[k]][4,]/nrow(c15[[k]])
  
  xx2[[k]][1,] <- xx2[[k]][1,]/nrow(c21[[k]])
  xx2[[k]][2,] <- xx2[[k]][2,]/nrow(c22[[k]])
  xx2[[k]][3,] <- xx2[[k]][3,]/nrow(c23[[k]])
  xx2[[k]][4,] <- xx2[[k]][4,]/nrow(c25[[k]])
  
  xx3[[k]][1,] <- xx3[[k]][1,]/nrow(c31[[k]])
  xx3[[k]][2,] <- xx3[[k]][2,]/nrow(c32[[k]])
  xx3[[k]][3,] <- xx3[[k]][3,]/nrow(c33[[k]])
  xx3[[k]][4,] <- xx3[[k]][4,]/nrow(c35[[k]])
  
  xx4[[k]][1,] <- xx4[[k]][1,]/nrow(c41[[k]])
  xx4[[k]][2,] <- xx4[[k]][2,]/nrow(c42[[k]])
  xx4[[k]][3,] <- xx4[[k]][3,]/nrow(c43[[k]])
  xx4[[k]][4,] <- xx4[[k]][4,]/nrow(c45[[k]])
  
  xx5[[k]][1,] <- xx5[[k]][1,]/nrow(c51[[k]])
  xx5[[k]][2,] <- xx5[[k]][2,]/nrow(c52[[k]])
  xx5[[k]][3,] <- xx5[[k]][3,]/nrow(c53[[k]])
  xx5[[k]][4,] <- xx5[[k]][4,]/nrow(c55[[k]])
}

# change column name
for(p in 1:5){
  colnames(xx1[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx2[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx3[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx4[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
  colnames(xx5[[p]]) <- c("life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                          "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",	
                          "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard")
}

# individualize
# tpe : own car usage in different occasion
nonpower_tpe_own_persona <- xx1[[1]]
# tpe : public transportation usage in different occasion
nonpower_tpe_pub_persona <- xx1[[2]]
# tpe : sharebike usage in different occasion
nonpower_tpe_sb_persona <- xx1[[3]]
# tpe : app dispatcher usage in different occasion
nonpower_tpe_app_persona <- xx1[[4]]
# tpe : road-hail taxi usage in different occasion
nonpower_tpe_tx_persona <- xx1[[5]]


# ty : own car usage in different occasion
nonpower_ty_own_persona <- xx2[[1]]
# ty : public transportation usage in different occasion
nonpower_ty_pub_persona <- xx2[[2]]
# ty : sharebike usage in different occasion
nonpower_ty_sb_persona <- xx2[[3]]
# ty : app dispatcher usage in different occasion
nonpower_ty_app_persona <- xx2[[4]]
# ty : road-hail taxi usage in different occasion
nonpower_ty_tx_persona <- xx2[[5]]


# hc : own car usage in different occasion
nonpower_hc_own_persona <- xx3[[1]]
# hc : public transportation usage in different occasion
nonpower_hc_pub_persona <- xx3[[2]]
# hc : sharebike usage in different occasion
nonpower_hc_sb_persona <- xx3[[3]]
# hc : app dispatcher usage in different occasion
nonpower_hc_app_persona <- xx3[[4]]
# hc : road-hail taxi usage in different occasion
nonpower_hc_tx_persona <- xx3[[5]]


# tc : own car usage in different occasion
nonpower_tc_own_persona <- xx4[[1]]
# tc : public transportation usage in different occasion
nonpower_tc_pub_persona <- xx4[[2]]
# tc : sharebike usage in different occasion
nonpower_tc_sb_persona <- xx4[[3]]
# tc : app dispatcher usage in different occasion
nonpower_tc_app_persona <- xx4[[4]]
# tc : road-hail taxi usage in different occasion
nonpower_tc_tx_persona <- xx4[[5]]


# kh : own car usage in different occasion
nonpower_kh_own_persona <- xx5[[1]]
# kh : public transportation usage in different occasion
nonpower_kh_pub_persona <- xx5[[2]]
# kh : sharebike usage in different occasion
nonpower_kh_sb_persona <- xx5[[3]]
# kh : app dispatcher usage in different occasion
nonpower_kh_app_persona <- xx5[[4]]
# kh : road-hail taxi usage in different occasion
nonpower_kh_tx_persona <- xx5[[5]]




###### SEVENTH PART : ######
# Power Clustering

# 1.group by city
power_TPE <- power %>% filter(power$Home=="tpe") # size 3890
power_TY <- power %>% filter(power$Home=="ty")   # size 281
power_HC <- power %>% filter(power$Home=="hc")   # size 113
power_TC <- power %>% filter(power$Home=="tc")   # size 449
power_KH <- power %>% filter(power$Home=="kh")   # size 419


# 2.cluster tree 
# TPE
E.dist1 <- power_TPE[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster1 <- hclust(E.dist1, method="ward.D2")                  # 華德法
plot(h.cluster1)
cut.h.cluster1 <- cutree(h.cluster1, k=5)                        # 分成5群
power_TPE[81] <- paste("cluster",cut.h.cluster1)                 # 分群結果
filter(power_TPE,power_TPE$V81 == "cluster 1") %>% nrow()  # 647/3890 (16.6%)
filter(power_TPE,power_TPE$V81 == "cluster 2") %>% nrow()  # 684/3890 (17.6%)
filter(power_TPE,power_TPE$V81 == "cluster 3") %>% nrow()  # 1370/3890 (35.2%)
filter(power_TPE,power_TPE$V81 == "cluster 4") %>% nrow()  # 641/3890 (16.5%)
filter(power_TPE,power_TPE$V81 == "cluster 5") %>% nrow()  # 548/3890 (14.1%)

# TY
E.dist2 <- power_TY[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster2 <- hclust(E.dist2, method="ward.D2")                  # 華德法
plot(h.cluster2)
cut.h.cluster2 <- cutree(h.cluster2, k=6)                        # 分成6群
power_TY[81] <- paste("cluster",cut.h.cluster2)                  # 分群結果
filter(power_TY,power_TY$V81 == "cluster 1") %>% nrow()  # 33/281 (11.7%)
filter(power_TY,power_TY$V81 == "cluster 2") %>% nrow()  # 71/281 (27.4%)
filter(power_TY,power_TY$V81 == "cluster 3") %>% nrow()  # 80/281 (28.4%)
filter(power_TY,power_TY$V81 == "cluster 4") %>% nrow()  # 61/281 (21.7%)
filter(power_TY,power_TY$V81 == "cluster 5") %>% nrow()  # 28/281 (10.0%)
filter(power_TY,power_TY$V81 == "cluster 6") %>% nrow()  # 8/281 (2.8%)

# HC
E.dist3 <- power_HC[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster3 <- hclust(E.dist3, method="ward.D2")                  # 華德法
plot(h.cluster3)
cut.h.cluster3 <- cutree(h.cluster3, k=4)                        # 分成4群
power_HC[81] <- paste("cluster",cut.h.cluster3)                   # 分群結果
filter(power_HC,power_HC$V81 == "cluster 1") %>% nrow()   # 27/113 (23.9%)
filter(power_HC,power_HC$V81 == "cluster 2") %>% nrow()   # 33/113 (29.2%)
filter(power_HC,power_HC$V81 == "cluster 3") %>% nrow()   # 40/113 (35.4%)
filter(power_HC,power_HC$V81 == "cluster 4") %>% nrow()   # 13/113 (11.5%)

# TC
E.dist4 <- power_TC[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster4 <- hclust(E.dist4, method="ward.D2")                  # 華德法
plot(h.cluster4)
cut.h.cluster4 <- cutree(h.cluster4, k=5)                        # 分成5群
power_TC[81] <- paste("cluster",cut.h.cluster4)                   # 分群結果
filter(power_TC,power_TC$V81 == "cluster 1") %>% nrow()   # 125/449 (27.8%)
filter(power_TC,power_TC$V81 == "cluster 2") %>% nrow()   # 36/449 (8.0%)
filter(power_TC,power_TC$V81 == "cluster 3") %>% nrow()   # 175/449 (39.0%)
filter(power_TC,power_TC$V81 == "cluster 4") %>% nrow()   # 76/449 (16.9%)
filter(power_TC,power_TC$V81 == "cluster 5") %>% nrow()   # 37/449 (8.3%)

# KH
E.dist5 <- power_KH[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster5 <- hclust(E.dist5, method="ward.D2")                  # 華德法
plot(h.cluster5)
cut.h.cluster5 <- cutree(h.cluster5, k=6)                        # 分成6群
power_KH[81] <- paste("cluster",cut.h.cluster5)                  # 分群結果
filter(power_KH,power_KH$V81 == "cluster 1") %>% nrow()   # 51/419 (12.2%)
filter(power_KH,power_KH$V81 == "cluster 2") %>% nrow()   # 144/419 (34.4%)
filter(power_KH,power_KH$V81 == "cluster 3") %>% nrow()   # 46/419 (10.9%)
filter(power_KH,power_KH$V81 == "cluster 4") %>% nrow()   # 121/419 (28.9%)
filter(power_KH,power_KH$V81 == "cluster 5") %>% nrow()   # 20/419 (4.8%)
filter(power_KH,power_KH$V81 == "cluster 6") %>% nrow()   # 37/419 (8.8%)


# 3.decision tree 
# TPE : 5 groups / TY : 6 groups / HC : 4 groups / TC : 5 groups / KH : 6 groups 
library(rpart)

# TPE
set.seed(1000)
train.index1 <- sample(x=1:nrow(power_TPE), size=ceiling(0.78*nrow(power_TPE) ))
train1 <- power_TPE[train.index1,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test1 <- power_TPE[-train.index1,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model1<- rpart(V81 ~ . , data=train1) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model1,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
    )

# TY
set.seed(121)
train.index2 <- sample(x=1:nrow(power_TY), size=ceiling(0.78*nrow(power_TY) ))
train2 <- power_TY[train.index2,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test2 <- power_TY[-train.index2,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model2<- rpart(V81 ~ . , data=train2) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model2,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# HC
set.seed(13553)
train.index3 <- sample(x=1:nrow(power_HC), size=ceiling(0.78*nrow(power_HC) ))
train3 <- power_HC[train.index3,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test3 <- power_HC[-train.index3,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model3<- rpart(V81 ~ . , data=train3) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model3,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# TC
set.seed(1390)
train.index4 <- sample(x=1:nrow(power_TC), size=ceiling(0.78*nrow(power_TC) ))
train4 <- power_TC[train.index4,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test4 <- power_TC[-train.index4,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model4<- rpart(V81 ~ . , data=train4) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model4,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# KH
set.seed(2010)
train.index5 <- sample(x=1:nrow(power_KH), size=ceiling(0.78*nrow(power_KH) ))
train5 <- power_KH[train.index5,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test5 <- power_KH[-train.index5,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model5<- rpart(V81 ~ . , data=train5) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model5,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)


# 4.PCA
# TPE : 5 groups / TY : 6 groups / HC : 4 groups / TC : 5 groups / KH : 6 groups 
library(factoextra)

# TPE
sample1 <- sample_n(power_TPE, size = 1400) 
res.pca <- prcomp(sample1[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTPE_offpeak_commute" 
)
res.pca <- prcomp(sample1[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(sample1[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(sample1[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTPE_offpeak_businesstrip" 
)


# TY
sample2 <- sample_n(power_TY, size = 90) 
res.pca <- prcomp(sample2[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTY_offpeak_commute" 
)
res.pca <- prcomp(sample2[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTY_offpeak_weather" 
)
res.pca <- prcomp(sample2[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(sample2[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTY_offpeak_businesstrip" 
)


# HC
sample3 <- sample_n(power_HC, size = 50) 
res.pca <- prcomp(sample3[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterHC_offpeak_commute" 
)
res.pca <- prcomp(sample3[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterHC_offpeak_weather" 
)
res.pca <- prcomp(sample3[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(sample3[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterHC_offpeak_businesstrip" 
)


# TC
sample4 <- sample_n(power_TC, size = 210) 
res.pca <- prcomp(sample4[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTC_offpeak_commute" 
)
res.pca <- prcomp(sample4[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTC_offpeak_weather" 
)
res.pca <- prcomp(sample4[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(sample4[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "PowerClusterTC_offpeak_businesstrip" 
)


# KH
sample5 <- sample_n(power_KH, size = 210) 
res.pca <- prcomp(sample5[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "PowerClusterKH_offpeak_commute" 
)
res.pca <- prcomp(sample5[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "PowerClusterKH_offpeak_weather" 
)
res.pca <- prcomp(sample5[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "PowerClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(sample5[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "PowerClusterKH_offpeak_businesstrip" 
)






##### EIGHTH PART : #####

# NonPower Clustering

# 1.group by city
npower_TPE <- nonpower %>% filter(nonpower$Home=="tpe") # size 10903
npower_TY <- nonpower %>% filter(nonpower$Home=="ty")   # size 885
npower_HC <- nonpower %>% filter(nonpower$Home=="hc")   # size 413
npower_TC <- nonpower %>% filter(nonpower$Home=="tc")   # size 1489
npower_KH <- nonpower %>% filter(nonpower$Home=="kh")   # size 1385


# 2.cluster tree 
# TPE
E.dist6 <- npower_TPE[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster6 <- hclust(E.dist6, method="ward.D2")                  # 華德法
plot(h.cluster6)
cut.h.cluster6 <- cutree(h.cluster6, k=5)                        # 分成5群
npower_TPE[81] <- paste("cluster",cut.h.cluster6)                 # 分群結果
filter(npower_TPE,npower_TPE$V81 == "cluster 1") %>% nrow()  # 1258/10903 (11.5%)
filter(npower_TPE,npower_TPE$V81 == "cluster 2") %>% nrow()  # 3777/10903 (34.6%)
filter(npower_TPE,npower_TPE$V81 == "cluster 3") %>% nrow()  # 1537/10903 (14.1%)
filter(npower_TPE,npower_TPE$V81 == "cluster 4") %>% nrow()  # 2628/10903 (24.1%)
filter(npower_TPE,npower_TPE$V81 == "cluster 5") %>% nrow()  # 1703/10903 (15.7%)

# TY
E.dist7 <- npower_TY[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster7 <- hclust(E.dist7, method="ward.D2")                  # 華德法
plot(h.cluster7)
cut.h.cluster7 <- cutree(h.cluster7, k=6)                        # 分成6群
npower_TY[81] <- paste("cluster",cut.h.cluster7)                  # 分群結果
filter(npower_TY,npower_TY$V81 == "cluster 1") %>% nrow()  # 128/885 (14.5%)
filter(npower_TY,npower_TY$V81 == "cluster 2") %>% nrow()  # 178/885 (20.1%)
filter(npower_TY,npower_TY$V81 == "cluster 3") %>% nrow()  # 32/885 (3.6%)
filter(npower_TY,npower_TY$V81 == "cluster 4") %>% nrow()  # 329/885 (37.2%)
filter(npower_TY,npower_TY$V81 == "cluster 5") %>% nrow()  # 202/885 (22.8%)
filter(npower_TY,npower_TY$V81 == "cluster 6") %>% nrow()  # 16/885 (1.8%)

# HC
E.dist8 <- npower_HC[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster8 <- hclust(E.dist8, method="ward.D2")                  # 華德法
plot(h.cluster8)
cut.h.cluster8 <- cutree(h.cluster8, k=4)                        # 分成4群
npower_HC[81] <- paste("cluster",cut.h.cluster8)                   # 分群結果
filter(npower_HC,npower_HC$V81 == "cluster 1") %>% nrow()   # 178/413 (43.1%)
filter(npower_HC,npower_HC$V81 == "cluster 2") %>% nrow()   # 26/413 (6.3%)
filter(npower_HC,npower_HC$V81 == "cluster 3") %>% nrow()   # 136/413 (32.9%)
filter(npower_HC,npower_HC$V81 == "cluster 4") %>% nrow()   # 73/413 (17.7%)

# TC
E.dist9 <- npower_TC[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster9 <- hclust(E.dist9, method="ward.D2")                  # 華德法
plot(h.cluster9)
cut.h.cluster9 <- cutree(h.cluster9, k=5)                        # 分成5群
npower_TC[81] <- paste("cluster",cut.h.cluster9)                   # 分群結果
filter(npower_TC,npower_TC$V81 == "cluster 1") %>% nrow()   # 119/1489 (8.0%)
filter(npower_TC,npower_TC$V81 == "cluster 2") %>% nrow()   # 649/1489 (43.6%)
filter(npower_TC,npower_TC$V81 == "cluster 3") %>% nrow()   # 172/1489 (11.6%)
filter(npower_TC,npower_TC$V81 == "cluster 4") %>% nrow()   # 226/1489 (15.2%)
filter(npower_TC,npower_TC$V81 == "cluster 5") %>% nrow()   # 323/1489 (21.6%)

# KH
E.dist10 <- npower_KH[-c(1:4)] %>% dist(method="euclidean")        # 歐式距離
h.cluster10 <- hclust(E.dist10, method="ward.D2")                  # 華德法
plot(h.cluster10)
cut.h.cluster10 <- cutree(h.cluster10, k=6)                        # 分成6群
npower_KH[81] <- paste("cluster",cut.h.cluster10)                  # 分群結果
filter(npower_KH,npower_KH$V81 == "cluster 1") %>% nrow()   # 180/1385 (13.0%)
filter(npower_KH,npower_KH$V81 == "cluster 2") %>% nrow()   # 452/1385 (32.6%)
filter(npower_KH,npower_KH$V81 == "cluster 3") %>% nrow()   # 88/1385 (6.4%)
filter(npower_KH,npower_KH$V81 == "cluster 4") %>% nrow()   # 410/1385 (29.6%)
filter(npower_KH,npower_KH$V81 == "cluster 5") %>% nrow()   # 222/1385 (16.0%)
filter(npower_KH,npower_KH$V81 == "cluster 6") %>% nrow()   # 33/1385 (2.4%)


# 3.decision tree 
# TPE : 5 groups / TY : 6 groups / HC : 4 groups / TC : 5 groups / KH : 6 groups 
library(rpart)

# TPE
set.seed(1000)
train.index6 <- sample(x=1:nrow(npower_TPE), size=ceiling(0.78*nrow(npower_TPE) ))
train6 <- npower_TPE[train.index6,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test6 <- npower_TPE[-train.index6,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model6<- rpart(V81 ~ . , data=train6) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model6,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# TY
set.seed(1221)
train.index7 <- sample(x=1:nrow(npower_TY), size=ceiling(0.78*nrow(npower_TY) ))
train7 <- npower_TY[train.index7,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test7 <- npower_TY[-train.index7,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model7<- rpart(V81 ~ . , data=train7) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model7,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# HC
set.seed(150)
train.index8 <- sample(x=1:nrow(npower_HC), size=ceiling(0.78*nrow(npower_HC) ))
train8 <- npower_HC[train.index8,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test8 <- npower_HC[-train.index8,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model8<- rpart(V81 ~ . , data=train8) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model8,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# TC
set.seed(1390)
train.index9 <- sample(x=1:nrow(npower_TC), size=ceiling(0.78*nrow(npower_TC) ))
train9 <- npower_TC[train.index9,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test9 <- npower_TC[-train.index9,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model9<- rpart(V81 ~ . , data=train9) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model9,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# KH
set.seed(10010)
train.index10 <- sample(x=1:nrow(npower_KH), size=ceiling(0.78*nrow(npower_KH) ))
train10 <- npower_KH[train.index10,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test10 <- npower_KH[-train.index10,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model10<- rpart(V81 ~ . , data=train10) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model10,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)


# 4.PCA
# TPE : 5 groups / TY : 6 groups / HC : 4 groups / TC : 5 groups / KH : 6 groups 
library(factoextra)

# TPE
sample6 <- sample_n(npower_TPE, size = 2700) 
res.pca <- prcomp(sample6[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample6$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTPE_offpeak_commute" 
)
res.pca <- prcomp(sample6[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample6$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(sample6[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample6$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(sample6[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample6$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTPE_offpeak_businesstrip" 
)


# TY
sample7 <- sample_n(npower_TY, size = 400) 
res.pca <- prcomp(sample7[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample7$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterTY_offpeak_commute" 
)
res.pca <- prcomp(sample7[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample7$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterTY_offpeak_weather" 
)
res.pca <- prcomp(sample7[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample7$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(sample7[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample7$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterTY_offpeak_businesstrip" 
)


# HC
sample8 <- sample_n(npower_HC, size = 250) 
res.pca <- prcomp(sample8[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample8$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterHC_offpeak_commute" 
)
res.pca <- prcomp(sample8[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample8$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterHC_offpeak_weather" 
)
res.pca <- prcomp(sample8[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample8$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(sample8[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample8$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterHC_offpeak_businesstrip" 
)


# TC
sample9 <- sample_n(npower_TC, size = 430) 
res.pca <- prcomp(sample9[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample9$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTC_offpeak_commute" 
)
res.pca <- prcomp(sample9[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample9$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTC_offpeak_weather" 
)
res.pca <- prcomp(sample9[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample9$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(sample9[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample9$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "NPowerClusterTC_offpeak_businesstrip" 
)


# KH
sample10 <- sample_n(npower_KH, size = 387) 
res.pca <- prcomp(sample10[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample10$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterKH_offpeak_commute" 
)
res.pca <- prcomp(sample10[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample10$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterKH_offpeak_weather" 
)
res.pca <- prcomp(sample10[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample10$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(sample10[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = sample10$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "NPowerClusterKH_offpeak_businesstrip" 
)




##### NINTH PART : #####

# ALL Clustering

# 1.combine 2 types of rider, and group by city
New_nonpower <- nonpower
New_nonpower[81] <- "nonpower"
New_power <- power
New_power[81] <- "power"
allrider <- rbind(New_nonpower,New_power)    # size 20725
all_TPE <- allrider %>% filter(allrider$Home=="tpe") # size 14793
all_TY <- allrider %>% filter(allrider$Home=="ty")   # size 1166
all_HC <- allrider %>% filter(allrider$Home=="hc")   # size 526
all_TC <- allrider %>% filter(allrider$Home=="tc")   # size 1938
all_KH <- allrider %>% filter(allrider$Home=="kh")   # size 1804


# 2.PCA
# TPE : 2 groups / TY : 2 groups / HC : 2 groups / TC : 2 groups / KH : 2 groups 
library(factoextra)

# TPE
asample1 <- sample_n(all_TPE, size = 2700) 
res.pca <- prcomp(asample1[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTPE_offpeak_commute" 
)
res.pca <- prcomp(asample1[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(asample1[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(asample1[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTPE_offpeak_businesstrip" 
)


# TY
asample2 <- sample_n(all_TY, size = 100) 
res.pca <- prcomp(asample2[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterTY_offpeak_commute" 
)
res.pca <- prcomp(asample2[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterTY_offpeak_weather" 
)
res.pca <- prcomp(asample2[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(asample2[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterTY_offpeak_businesstrip" 
)


# HC
asample3 <- sample_n(all_HC, size = 80) 
res.pca <- prcomp(asample3[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterHC_offpeak_commute" 
)
res.pca <- prcomp(asample3[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterHC_offpeak_weather" 
)
res.pca <- prcomp(asample3[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(asample3[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterHC_offpeak_businesstrip" 
)


# TC
asample4 <- sample_n(all_TC, size = 350) 
res.pca <- prcomp(asample4[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTC_offpeak_commute" 
)
res.pca <- prcomp(asample4[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTC_offpeak_weather" 
)
res.pca <- prcomp(asample4[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(asample4[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.18,
                title = "AClusterTC_offpeak_businesstrip" 
)


# KH
asample5 <- sample_n(all_KH, size = 287) 
res.pca <- prcomp(asample5[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterKH_offpeak_commute" 
)
res.pca <- prcomp(asample5[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterKH_offpeak_weather" 
)
res.pca <- prcomp(asample5[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(asample5[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1.5, 
                repel = TRUE, 
                palette = "Blues",
                col.circle = c("#05112a","#07193f","#0a2254","#0d2a6a","#0f337f"),
                addEllipses = TRUE,
                ellipse.level = 0.1,
                title = "AClusterKH_offpeak_businesstrip" 
)





##### TENTH PART : #####

# ALL Clustering with advanced classification : age 

# 1.combine 2 types of rider, and group by city
New_nonpower1 <- nonpower
New_power1 <- power
write.csv(x = New_nonpower1,file = "New_nonpower1.csv")
write.csv(x = New_power1,file = "New_power1.csv")
New_nonpower1 <- read.csv(file = "fNew_nonpower1.csv",encoding = "UTF-8")
New_power1 <- read.csv(file = "fNew_power1.csv",encoding = "UTF-8")

allrider <- rbind(New_nonpower1,New_power1)    # size 20725
all_TPE <- allrider %>% filter(allrider$Home=="tpe") # size 14793
all_TY <- allrider %>% filter(allrider$Home=="ty")   # size 1166
all_HC <- allrider %>% filter(allrider$Home=="hc")   # size 526
all_TC <- allrider %>% filter(allrider$Home=="tc")   # size 1938
all_KH <- allrider %>% filter(allrider$Home=="kh")   # size 1804

# 2.PCA
# TPE : 2 groups / TY : 2 groups / HC : 2 groups / TC : 2 groups / KH : 2 groups 
library(factoextra)
my_col <- c("#ffffff","#d1ddf6","#a3bbed","#7599e5","#4776dc","#174cbe","#123b94","#0d2a6a","#07193f","#020815")

# TPE
asample1 <- sample_n(all_TPE, size = 3500) 
res.pca <- prcomp(asample1[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_commute"
)
res.pca <- prcomp(asample1[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(asample1[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(asample1[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_businesstrip" 
)


# TY
asample2 <- sample_n(all_TY, size = 1100) 
res.pca <- prcomp(asample2[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_commute" 
)
res.pca <- prcomp(asample2[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_weather" 
)
res.pca <- prcomp(asample2[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(asample2[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_businesstrip" 
)


# HC
asample3 <- sample_n(all_HC, size = 450) 
res.pca <- prcomp(asample3[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_commute" 
)
res.pca <- prcomp(asample3[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_weather" 
)
res.pca <- prcomp(asample3[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(asample3[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_businesstrip" 
)


# TC
asample4 <- sample_n(all_TC, size = 1430) 
res.pca <- prcomp(asample4[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_commute" 
)
res.pca <- prcomp(asample4[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_weather" 
)
res.pca <- prcomp(asample4[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(asample4[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_businesstrip" 
)


# KH
asample5 <- sample_n(all_KH, size = 2187) 
res.pca <- prcomp(asample5[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_commute" 
)
res.pca <- prcomp(asample5[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_weather" 
)
res.pca <- prcomp(asample5[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(asample5[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$Cohort, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_businesstrip" 
)




##### ELEVENTH PART : #####

# ALL Clustering with advanced classification : cluster tree 

# 1.create new sheet and separate by cities
New_nonpower3 <- nonpower
New_power3 <- power

nn_TPE <- New_nonpower3 %>% filter(New_nonpower3$Home=="tpe") 
nn_TY <- New_nonpower3 %>% filter(New_nonpower3$Home=="ty")   
nn_HC <- New_nonpower3 %>% filter(New_nonpower3$Home=="hc")   
nn_TC <- New_nonpower3 %>% filter(New_nonpower3$Home=="tc")   
nn_KH <- New_nonpower3 %>% filter(New_nonpower3$Home=="kh") 

np_TPE <- New_power3 %>% filter(New_power3$Home=="tpe") 
np_TY <- New_power3 %>% filter(New_power3$Home=="ty")   
np_HC <- New_power3 %>% filter(New_power3$Home=="hc")   
np_TC <- New_power3 %>% filter(New_power3$Home=="tc")   
np_KH <- New_power3 %>% filter(New_power3$Home=="kh")

# 2. cluster tree

# TPE 
#(power)
E.dist11 <- np_TPE[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster11 <- hclust(E.dist11, method="ward.D2")                  # 華德法
plot(h.cluster11)
cut.h.cluster11 <- cutree(h.cluster11, k=3)                        # 分成5群
np_TPE[81] <- paste("cluster",cut.h.cluster11)                 # 分群結果
filter(np_TPE,np_TPE$V81 == "cluster 1") %>% nrow()  # 2580 (%)
filter(np_TPE,np_TPE$V81 == "cluster 2") %>% nrow()  # 824 (%)
filter(np_TPE,np_TPE$V81 == "cluster 3") %>% nrow()  # 486 (%)
#(nonpower)
E.dist16 <- nn_TPE[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster16 <- hclust(E.dist16, method="ward.D2")                  # 華德法
plot(h.cluster16)
cut.h.cluster16 <- cutree(h.cluster16, k=3)                        # 分成5群
nn_TPE[81] <- paste("cluster",cut.h.cluster16+3)                 # 分群結果
filter(nn_TPE,nn_TPE$V81 == "cluster 4") %>% nrow()  # 5035 (%)
filter(nn_TPE,nn_TPE$V81 == "cluster 5") %>% nrow()  # 3240 (%)
filter(nn_TPE,nn_TPE$V81 == "cluster 6") %>% nrow()  # 2628 (%)


# TY 
#(power)
E.dist12 <- np_TY[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster12 <- hclust(E.dist12, method="ward.D2")                  # 華德法
plot(h.cluster12)
cut.h.cluster12 <- cutree(h.cluster12, k=3)                        # 分成5群
np_TY[81] <- paste("cluster",cut.h.cluster12)                 # 分群結果
filter(np_TY,np_TY$V81 == "cluster 1") %>% nrow()  # 69 (%)
filter(np_TY,np_TY$V81 == "cluster 2") %>% nrow()  # 71 (%)
filter(np_TY,np_TY$V81 == "cluster 3") %>% nrow()  # 141 (%)
#(nonpower)
E.dist17 <- nn_TY[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster17 <- hclust(E.dist17, method="ward.D2")                  # 華德法
plot(h.cluster17)
cut.h.cluster17 <- cutree(h.cluster17, k=3)                        # 分成5群
nn_TY[81] <- paste("cluster",cut.h.cluster17+3)                 # 分群結果
filter(nn_TY,nn_TY$V81 == "cluster 4") %>% nrow()  # 176 (%)
filter(nn_TY,nn_TY$V81 == "cluster 5") %>% nrow()  # 507 (%)
filter(nn_TY,nn_TY$V81 == "cluster 6") %>% nrow()  # 202 (%)


# HC 
#(power)
E.dist13 <- np_HC[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster13 <- hclust(E.dist13, method="ward.D2")                  # 華德法
plot(h.cluster13)
cut.h.cluster13 <- cutree(h.cluster13, k=3)                        # 分成5群
np_HC[81] <- paste("cluster",cut.h.cluster13)                 # 分群結果
filter(np_HC,np_HC$V81 == "cluster 1") %>% nrow()  # 27 (%)
filter(np_HC,np_HC$V81 == "cluster 2") %>% nrow()  # 73 (%)
filter(np_HC,np_HC$V81 == "cluster 3") %>% nrow()  # 13 (%)
#(nonpower)
E.dist18 <- nn_HC[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster18 <- hclust(E.dist18, method="ward.D2")                  # 華德法
plot(h.cluster18)
cut.h.cluster18 <- cutree(h.cluster18, k=3)                        # 分成5群
nn_HC[81] <- paste("cluster",cut.h.cluster18+3)                 # 分群結果
filter(nn_HC,nn_HC$V81 == "cluster 4") %>% nrow()  # 178 (%)
filter(nn_HC,nn_HC$V81 == "cluster 5") %>% nrow()  # 99 (%)
filter(nn_HC,nn_HC$V81 == "cluster 6") %>% nrow()  # 136 (%)


# TC 
#(power)
E.dist14 <- np_TC[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster14 <- hclust(E.dist14, method="ward.D2")                  # 華德法
plot(h.cluster14)
cut.h.cluster14 <- cutree(h.cluster14, k=3)                        # 分成5群
np_TC[81] <- paste("cluster",cut.h.cluster14)                 # 分群結果
filter(np_TC,np_TC$V81 == "cluster 1") %>% nrow()  # 121 (%)
filter(np_TC,np_TC$V81 == "cluster 2") %>% nrow()  # 233 (%)
filter(np_TC,np_TC$V81 == "cluster 3") %>% nrow()  # 95 (%)
#(nonpower)
E.dist19 <- nn_TC[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster19 <- hclust(E.dist19, method="ward.D2")                  # 華德法
plot(h.cluster19)
cut.h.cluster19 <- cutree(h.cluster19, k=3)                        # 分成5群
nn_TC[81] <- paste("cluster",cut.h.cluster19+3)                 # 分群結果
filter(nn_TC,nn_TC$V81 == "cluster 4") %>% nrow()  # 442 (%)
filter(nn_TC,nn_TC$V81 == "cluster 5") %>% nrow()  # 821 (%)
filter(nn_TC,nn_TC$V81 == "cluster 6") %>% nrow()  # 226 (%)


# KH 
#(power)
E.dist15 <- np_KH[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster15 <- hclust(E.dist15, method="ward.D2")                  # 華德法
plot(h.cluster15)
cut.h.cluster15 <- cutree(h.cluster15, k=3)                        # 分成5群
np_KH[81] <- paste("cluster",cut.h.cluster15)                 # 分群結果
filter(np_KH,np_KH$V81 == "cluster 1") %>% nrow()  # 90 (%)
filter(np_KH,np_KH$V81 == "cluster 2") %>% nrow()  # 262 (%)
filter(np_KH,np_KH$V81 == "cluster 3") %>% nrow()  # 67 (%)
#(nonpower)
E.dist20 <- nn_KH[-c(1:4)] %>% dist(method="euclidean")       # 歐式距離
h.cluster20 <- hclust(E.dist20, method="ward.D2")                  # 華德法
plot(h.cluster20)
cut.h.cluster20 <- cutree(h.cluster20, k=3)                        # 分成5群
nn_KH[81] <- paste("cluster",cut.h.cluster20+3)                 # 分群結果
filter(nn_KH,nn_KH$V81 == "cluster 4") %>% nrow()  # 213 (%)
filter(nn_KH,nn_KH$V81 == "cluster 5") %>% nrow()  # 1084 (%)
filter(nn_KH,nn_KH$V81 == "cluster 6") %>% nrow()  # 88 (%)


# merge tables 
all_TPE <- rbind(nn_TPE,np_TPE)   
all_TY <- rbind(nn_TY,np_TY)  
all_HC <- rbind(nn_HC,np_HC)  
all_TC <- rbind(nn_TC,np_TC)  
all_KH <- rbind(nn_KH,np_KH)  


# 4.PCA part1 
# TPE : 6 groups / TY : 6 groups / HC : 6 groups / TC : 6 groups / KH : 6 groups 
library(factoextra)
my_col <- c("#ffdc73","#ffbf00","#a67c00","#9c2052","#571c3c","#101624")

# TPE
asample1 <- sample_n(all_TPE, size = 3500) 
res.pca <- prcomp(asample1[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_commute"
)
res.pca <- prcomp(asample1[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(asample1[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(asample1[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_businesstrip" 
)


# TY
asample2 <- sample_n(all_TY, size = 1100) 
res.pca <- prcomp(asample2[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_commute" 
)
res.pca <- prcomp(asample2[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_weather" 
)
res.pca <- prcomp(asample2[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(asample2[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_businesstrip" 
)


# HC
asample3 <- sample_n(all_HC, size = 450) 
res.pca <- prcomp(asample3[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_commute" 
)
res.pca <- prcomp(asample3[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_weather" 
)
res.pca <- prcomp(asample3[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(asample3[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_businesstrip" 
)


# TC
asample4 <- sample_n(all_TC, size = 1430) 
res.pca <- prcomp(asample4[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_commute" 
)
res.pca <- prcomp(asample4[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_weather" 
)
res.pca <- prcomp(asample4[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(asample4[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_businesstrip" 
)


# KH
asample5 <- sample_n(all_KH, size = 1100) 
res.pca <- prcomp(asample5[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_commute" 
)
res.pca <- prcomp(asample5[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_weather" 
)
res.pca <- prcomp(asample5[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(asample5[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_businesstrip" 
)

# 5.PCA part.2 (canceled)
# TPE : 6 groups / TY : 6 groups / HC : 6 groups / TC : 6 groups / KH : 6 groups 
library(factoextra)
my_col <- c("#ffdc73","#ffbf00","#a67c00","#9c2052","#571c3c","#101624")

# TPE
asample1 <- sample_n(all_TPE, size = 3500) 
res.pca <- prcomp(asample1[c(74:80)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_commute"
)
res.pca <- prcomp(asample1[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_weather" 
)
res.pca <- prcomp(asample1[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_shopping" 
)
res.pca <- prcomp(asample1[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample1$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTPE_offpeak_businesstrip" 
)


# TY
asample2 <- sample_n(all_TY, size = 1100) 
res.pca <- prcomp(asample2[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_commute" 
)
res.pca <- prcomp(asample2[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_weather" 
)
res.pca <- prcomp(asample2[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_shopping" 
)
res.pca <- prcomp(asample2[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample2$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTY_offpeak_businesstrip" 
)


# HC
asample3 <- sample_n(all_HC, size = 450) 
res.pca <- prcomp(asample3[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_commute" 
)
res.pca <- prcomp(asample3[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_weather" 
)
res.pca <- prcomp(asample3[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_shopping" 
)
res.pca <- prcomp(asample3[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample3$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterHC_offpeak_businesstrip" 
)


# TC
asample4 <- sample_n(all_TC, size = 1430) 
res.pca <- prcomp(asample4[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_commute" 
)
res.pca <- prcomp(asample4[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_weather" 
)
res.pca <- prcomp(asample4[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_shopping" 
)
res.pca <- prcomp(asample4[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample4$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterTC_offpeak_businesstrip" 
)


# KH
asample5 <- sample_n(all_KH, size = 1100) 
res.pca <- prcomp(asample5[c(30,35,40,45,50)],  scale = TRUE)   # offpeak commute
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_commute" 
)
res.pca <- prcomp(asample5[c(31,36,41,46,51)],  scale = TRUE)   # offpeak weather
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_weather" 
)
res.pca <- prcomp(asample5[c(32,37,42,47,52)],  scale = TRUE)   # offpeak shopping
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_shopping" 
)
res.pca <- prcomp(asample5[c(34,39,44,49,54)],  scale = TRUE)   # offpeak business trip
fviz_pca_biplot(res.pca,                 
                geom.ind = "point",
                fill.ind = asample5$V81, 
                col.ind = "white",
                col.var = "#424242",
                pointshape = 21,
                pointsize = 1, 
                repel = TRUE, 
                palette = my_col,
                addEllipses = TRUE,
                ellipse.level = 0.025,
                title = "AClusterKH_offpeak_businesstrip" 
)




##### TWELFTH PART : #####

# % of persona/hobby/social media usage 
# based on cluster

View(all_TPE)   
View(all_TY) 
View(all_HC)  
View(all_TC)  
View(all_KH)   

# filter by cluster
all_TPE1 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 1") 
all_TPE2 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 2") 
all_TPE3 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 3") 
all_TPE4 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 4") 
all_TPE5 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 5") 
all_TPE6 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 6") 

all_TY1 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 1") 
all_TY2 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 2") 
all_TY3 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 3") 
all_TY4 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 4") 
all_TY5 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 5") 
all_TY6 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 6") 

all_HC1 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 1") 
all_HC2 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 2") 
all_HC3 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 3") 
all_HC4 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 4") 
all_HC5 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 5") 
all_HC6 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 6") 

all_TC1 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 1") 
all_TC2 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 2") 
all_TC3 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 3") 
all_TC4 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 4") 
all_TC5 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 5") 
all_TC6 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 6") 

all_KH1 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 1") 
all_KH2 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 2") 
all_KH3 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 3") 
all_KH4 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 4") 
all_KH5 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 5") 
all_KH6 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 6") 


# chart 
a11 <- list()
a12 <- list()
a13 <- list()
a14 <- list()
a15 <- list()
a16 <- list()

a21 <- list()
a22 <- list()
a23 <- list()
a24 <- list()
a25 <- list()
a26 <- list()

a31 <- list()
a32 <- list()
a33 <- list()
a34 <- list()
a35 <- list()
a36 <- list()

a41 <- list()
a42 <- list()
a43 <- list()
a44 <- list()
a45 <- list()
a46 <- list()

a51 <- list()
a52 <- list()
a53 <- list()
a54 <- list()
a55 <- list()
a56 <- list()

# TPE
for(i in 74:80){
  a11[[i-73]] <- all_TPE1 %>% dplyr::filter(all_TPE1[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a12[[i-73]] <- all_TPE2 %>% dplyr::filter(all_TPE2[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a13[[i-73]] <- all_TPE3 %>% dplyr::filter(all_TPE3[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a14[[i-73]] <- all_TPE4 %>% dplyr::filter(all_TPE4[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a15[[i-73]] <- all_TPE5 %>% dplyr::filter(all_TPE5[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a16[[i-73]] <- all_TPE6 %>% dplyr::filter(all_TPE6[i] > 3) %>% nrow()  
}

persona_TPE <- rbind(
data.frame(a = a11[[1]],b = a11[[2]],c = a11[[3]],d = a11[[4]],e = a11[[5]],f = a11[[6]],g = a11[[7]]), 
data.frame(a = a12[[1]],b = a12[[2]],c = a12[[3]],d = a12[[4]],e = a12[[5]],f = a12[[6]],g = a12[[7]]),
data.frame(a = a13[[1]],b = a13[[2]],c = a13[[3]],d = a13[[4]],e = a13[[5]],f = a13[[6]],g = a13[[7]]),
data.frame(a = a14[[1]],b = a14[[2]],c = a14[[3]],d = a14[[4]],e = a14[[5]],f = a14[[6]],g = a14[[7]]),
data.frame(a = a15[[1]],b = a15[[2]],c = a15[[3]],d = a15[[4]],e = a15[[5]],f = a15[[6]],g = a15[[7]]),
data.frame(a = a16[[1]],b = a16[[2]],c = a16[[3]],d = a16[[4]],e = a16[[5]],f = a16[[6]],g = a16[[7]])
)


# TY
for(i in 74:80){
  a21[[i-73]] <- all_TY1 %>% dplyr::filter(all_TY1[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a22[[i-73]] <- all_TY2 %>% dplyr::filter(all_TY2[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a23[[i-73]] <- all_TY3 %>% dplyr::filter(all_TY3[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a24[[i-73]] <- all_TY4 %>% dplyr::filter(all_TY4[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a25[[i-73]] <- all_TY5 %>% dplyr::filter(all_TY5[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a26[[i-73]] <- all_TY6 %>% dplyr::filter(all_TY6[i] > 3) %>% nrow()  
}

persona_TY <- rbind(
  data.frame(a = a21[[1]],b = a21[[2]],c = a21[[3]],d = a21[[4]],e = a21[[5]],f = a21[[6]],g = a21[[7]]), 
  data.frame(a = a22[[1]],b = a22[[2]],c = a22[[3]],d = a22[[4]],e = a22[[5]],f = a22[[6]],g = a22[[7]]),
  data.frame(a = a23[[1]],b = a23[[2]],c = a23[[3]],d = a23[[4]],e = a23[[5]],f = a23[[6]],g = a23[[7]]),
  data.frame(a = a24[[1]],b = a24[[2]],c = a24[[3]],d = a24[[4]],e = a24[[5]],f = a24[[6]],g = a24[[7]]),
  data.frame(a = a25[[1]],b = a25[[2]],c = a25[[3]],d = a25[[4]],e = a25[[5]],f = a25[[6]],g = a25[[7]]),
  data.frame(a = a26[[1]],b = a26[[2]],c = a26[[3]],d = a26[[4]],e = a26[[5]],f = a26[[6]],g = a26[[7]])
)


# HC
for(i in 74:80){
  a31[[i-73]] <- all_HC1 %>% dplyr::filter(all_HC1[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a32[[i-73]] <- all_HC2 %>% dplyr::filter(all_HC2[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a33[[i-73]] <- all_HC3 %>% dplyr::filter(all_HC3[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a34[[i-73]] <- all_HC4 %>% dplyr::filter(all_HC4[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a35[[i-73]] <- all_HC5 %>% dplyr::filter(all_HC5[i] > 3) %>% nrow()  
} 
for(i in 74:80){
  a36[[i-73]] <- all_HC6 %>% dplyr::filter(all_HC6[i] > 3) %>% nrow()  
}

persona_HC <- rbind(
  data.frame(a = a31[[1]],b = a31[[2]],c = a31[[3]],d = a31[[4]],e = a31[[5]],f = a31[[6]],g = a31[[7]]), 
  data.frame(a = a32[[1]],b = a32[[2]],c = a32[[3]],d = a32[[4]],e = a32[[5]],f = a32[[6]],g = a32[[7]]),
  data.frame(a = a33[[1]],b = a33[[2]],c = a33[[3]],d = a33[[4]],e = a33[[5]],f = a33[[6]],g = a33[[7]]),
  data.frame(a = a34[[1]],b = a34[[2]],c = a34[[3]],d = a34[[4]],e = a34[[5]],f = a34[[6]],g = a34[[7]]),
  data.frame(a = a35[[1]],b = a35[[2]],c = a35[[3]],d = a35[[4]],e = a35[[5]],f = a35[[6]],g = a35[[7]]),
  data.frame(a = a36[[1]],b = a36[[2]],c = a36[[3]],d = a36[[4]],e = a36[[5]],f = a36[[6]],g = a36[[7]])
)


# TC
for(i in 74:80){
  a41[[i-73]] <- all_TC1 %>% dplyr::filter(all_TC1[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a42[[i-73]] <- all_TC2 %>% dplyr::filter(all_TC2[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a43[[i-73]] <- all_TC3 %>% dplyr::filter(all_TC3[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a44[[i-73]] <- all_TC4 %>% dplyr::filter(all_TC4[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a45[[i-73]] <- all_TC5 %>% dplyr::filter(all_TC5[i] > 3) %>% nrow()  
} 
for(i in 74:80){
  a46[[i-73]] <- all_TC6 %>% dplyr::filter(all_TC6[i] > 3) %>% nrow()  
}

persona_TC <- rbind(
  data.frame(a = a41[[1]],b = a41[[2]],c = a41[[3]],d = a41[[4]],e = a41[[5]],f = a41[[6]],g = a41[[7]]), 
  data.frame(a = a42[[1]],b = a42[[2]],c = a42[[3]],d = a42[[4]],e = a42[[5]],f = a42[[6]],g = a42[[7]]),
  data.frame(a = a43[[1]],b = a43[[2]],c = a43[[3]],d = a43[[4]],e = a43[[5]],f = a43[[6]],g = a43[[7]]),
  data.frame(a = a44[[1]],b = a44[[2]],c = a44[[3]],d = a44[[4]],e = a44[[5]],f = a44[[6]],g = a44[[7]]),
  data.frame(a = a45[[1]],b = a45[[2]],c = a45[[3]],d = a45[[4]],e = a45[[5]],f = a45[[6]],g = a45[[7]]),
  data.frame(a = a46[[1]],b = a46[[2]],c = a46[[3]],d = a46[[4]],e = a46[[5]],f = a46[[6]],g = a46[[7]])
)


# KH
for(i in 74:80){
  a51[[i-73]] <- all_KH1 %>% dplyr::filter(all_KH1[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a52[[i-73]] <- all_KH2 %>% dplyr::filter(all_KH2[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a53[[i-73]] <- all_KH3 %>% dplyr::filter(all_KH3[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a54[[i-73]] <- all_KH4 %>% dplyr::filter(all_KH4[i] > 3) %>% nrow()  
}
for(i in 74:80){
  a55[[i-73]] <- all_KH5 %>% dplyr::filter(all_KH5[i] > 3) %>% nrow()  
} 
for(i in 74:80){
  a56[[i-73]] <- all_KH6 %>% dplyr::filter(all_KH6[i] > 3) %>% nrow()  
}

persona_KH <- rbind(
  data.frame(a = a51[[1]],b = a51[[2]],c = a51[[3]],d = a51[[4]],e = a51[[5]],f = a51[[6]],g = a51[[7]]), 
  data.frame(a = a52[[1]],b = a52[[2]],c = a52[[3]],d = a52[[4]],e = a52[[5]],f = a52[[6]],g = a52[[7]]),
  data.frame(a = a53[[1]],b = a53[[2]],c = a53[[3]],d = a53[[4]],e = a53[[5]],f = a53[[6]],g = a53[[7]]),
  data.frame(a = a54[[1]],b = a54[[2]],c = a54[[3]],d = a54[[4]],e = a54[[5]],f = a54[[6]],g = a54[[7]]),
  data.frame(a = a55[[1]],b = a55[[2]],c = a55[[3]],d = a55[[4]],e = a55[[5]],f = a55[[6]],g = a55[[7]]),
  data.frame(a = a56[[1]],b = a56[[2]],c = a56[[3]],d = a56[[4]],e = a56[[5]],f = a56[[6]],g = a56[[7]])
)

# change to percentage 
persona_TPE[1,] <- persona_TPE[1,]/(all_TPE1 %>% nrow())
persona_TPE[2,] <- persona_TPE[2,]/(all_TPE2 %>% nrow())
persona_TPE[3,] <- persona_TPE[3,]/(all_TPE3 %>% nrow())
persona_TPE[4,] <- persona_TPE[4,]/(all_TPE4 %>% nrow())
persona_TPE[5,] <- persona_TPE[5,]/(all_TPE5 %>% nrow())
persona_TPE[6,] <- persona_TPE[6,]/(all_TPE6 %>% nrow())

persona_TY[1,] <- persona_TY[1,]/(all_TY1 %>% nrow())
persona_TY[2,] <- persona_TY[2,]/(all_TY2 %>% nrow())
persona_TY[3,] <- persona_TY[3,]/(all_TY3 %>% nrow())
persona_TY[4,] <- persona_TY[4,]/(all_TY4 %>% nrow())
persona_TY[5,] <- persona_TY[5,]/(all_TY5 %>% nrow())
persona_TY[6,] <- persona_TY[6,]/(all_TY6 %>% nrow())

persona_HC[1,] <- persona_HC[1,]/(all_HC1 %>% nrow())
persona_HC[2,] <- persona_HC[2,]/(all_HC2 %>% nrow())
persona_HC[3,] <- persona_HC[3,]/(all_HC3 %>% nrow())
persona_HC[4,] <- persona_HC[4,]/(all_HC4 %>% nrow())
persona_HC[5,] <- persona_HC[5,]/(all_HC5 %>% nrow())
persona_HC[6,] <- persona_HC[6,]/(all_HC6 %>% nrow())

persona_TC[1,] <- persona_TC[1,]/(all_TC1 %>% nrow())
persona_TC[2,] <- persona_TC[2,]/(all_TC2 %>% nrow())
persona_TC[3,] <- persona_TC[3,]/(all_TC3 %>% nrow())
persona_TC[4,] <- persona_TC[4,]/(all_TC4 %>% nrow())
persona_TC[5,] <- persona_TC[5,]/(all_TC5 %>% nrow())
persona_TC[6,] <- persona_TC[6,]/(all_TC6 %>% nrow())

persona_KH[1,] <- persona_KH[1,]/(all_KH1 %>% nrow())
persona_KH[2,] <- persona_KH[2,]/(all_KH2 %>% nrow())
persona_KH[3,] <- persona_KH[3,]/(all_KH3 %>% nrow())
persona_KH[4,] <- persona_KH[4,]/(all_KH4 %>% nrow())
persona_KH[5,] <- persona_KH[5,]/(all_KH5 %>% nrow())
persona_KH[6,] <- persona_KH[6,]/(all_KH6 %>% nrow())


# change colnames
colnames(persona_TPE) <- c("life_balance","life_cognition","work_feelings","family_relationship",
                           "work_free","money_usage","Indoor_Outdoor")
colnames(persona_TY) <- c("life_balance","life_cognition","work_feelings","family_relationship",
                           "work_free","money_usage","Indoor_Outdoor")
colnames(persona_HC) <- c("life_balance","life_cognition","work_feelings","family_relationship",
                           "work_free","money_usage","Indoor_Outdoor")
colnames(persona_TC) <- c("life_balance","life_cognition","work_feelings","family_relationship",
                           "work_free","money_usage","Indoor_Outdoor")
colnames(persona_KH) <- c("life_balance","life_cognition","work_feelings","family_relationship",
                           "work_free","money_usage","Indoor_Outdoor")

persona_TPE <- round(persona_TPE * 100,1)
persona_TY <- round(persona_TY * 100,1)
persona_HC <- round(persona_HC * 100,1)
persona_TC <- round(persona_TC * 100,1)
persona_KH <- round(persona_KH * 100,1)


# radar plot 
library(fmsb)
radarchart(persona_TPE[1:6,],
           axistype=3 , 
           #custom polygon
           #pcol=rgb(0.2,0.5,0.5,0.9),  #線顏色
           #pfcol=rgb(0.2,0.5,0.5,0.5),  #面顏色
           plwd=2, 
           #custom the grid
           cglcol="grey",
           cglty=1,
           axislabcol="grey",
           caxislabels=seq(0,20,5),
           cglwd=0.8,
           centerzero = TRUE,
           maxmin = FALSE,
           #custom labels
           vlcex=0.8 
)





##### THIRTEENTH PART : #####

# % of persona/hobby/social media usage 
# based on cluster

View(all_TPE)   
View(all_TY) 
View(all_HC)  
View(all_TC)  
View(all_KH)   

# filter by cluster
all_TPE1 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 1") 
all_TPE2 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 2") 
all_TPE3 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 3") 
all_TPE4 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 4") 
all_TPE5 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 5") 
all_TPE6 <- all_TPE %>% dplyr::filter(all_TPE$V81 == "cluster 6") 

all_TY1 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 1") 
all_TY2 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 2") 
all_TY3 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 3") 
all_TY4 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 4") 
all_TY5 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 5") 
all_TY6 <- all_TY %>% dplyr::filter(all_TY$V81 == "cluster 6") 

all_HC1 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 1") 
all_HC2 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 2") 
all_HC3 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 3") 
all_HC4 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 4") 
all_HC5 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 5") 
all_HC6 <- all_HC %>% dplyr::filter(all_HC$V81 == "cluster 6") 

all_TC1 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 1") 
all_TC2 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 2") 
all_TC3 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 3") 
all_TC4 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 4") 
all_TC5 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 5") 
all_TC6 <- all_TC %>% dplyr::filter(all_TC$V81 == "cluster 6") 

all_KH1 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 1") 
all_KH2 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 2") 
all_KH3 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 3") 
all_KH4 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 4") 
all_KH5 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 5") 
all_KH6 <- all_KH %>% dplyr::filter(all_KH$V81 == "cluster 6") 


# chart 
a11 <- list()
a12 <- list()
a13 <- list()
a14 <- list()
a15 <- list()
a16 <- list()

a21 <- list()
a22 <- list()
a23 <- list()
a24 <- list()
a25 <- list()
a26 <- list()

a31 <- list()
a32 <- list()
a33 <- list()
a34 <- list()
a35 <- list()
a36 <- list()

a41 <- list()
a42 <- list()
a43 <- list()
a44 <- list()
a45 <- list()
a46 <- list()

a51 <- list()
a52 <- list()
a53 <- list()
a54 <- list()
a55 <- list()
a56 <- list()

# TPE
for(i in 55:64){
  a11[[i-54]] <- all_TPE1 %>% dplyr::filter(all_TPE1[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a12[[i-54]] <- all_TPE2 %>% dplyr::filter(all_TPE2[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a13[[i-54]] <- all_TPE3 %>% dplyr::filter(all_TPE3[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a14[[i-54]] <- all_TPE4 %>% dplyr::filter(all_TPE4[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a15[[i-54]] <- all_TPE5 %>% dplyr::filter(all_TPE5[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a16[[i-54]] <- all_TPE6 %>% dplyr::filter(all_TPE6[i] > 3) %>% nrow()  
}

interest_TPE <- rbind(
  data.frame(a = a11[[1]],b = a11[[2]],c = a11[[3]],d = a11[[4]],e = a11[[5]],f = a11[[6]],g = a11[[7]],h = a11[[8]],i = a11[[9]],j = a11[[10]]), 
  data.frame(a = a12[[1]],b = a12[[2]],c = a12[[3]],d = a12[[4]],e = a12[[5]],f = a12[[6]],g = a12[[7]],h = a12[[8]],i = a12[[9]],j = a12[[10]]),
  data.frame(a = a13[[1]],b = a13[[2]],c = a13[[3]],d = a13[[4]],e = a13[[5]],f = a13[[6]],g = a13[[7]],h = a13[[8]],i = a13[[9]],j = a13[[10]]),
  data.frame(a = a14[[1]],b = a14[[2]],c = a14[[3]],d = a14[[4]],e = a14[[5]],f = a14[[6]],g = a14[[7]],h = a14[[8]],i = a14[[9]],j = a14[[10]]),
  data.frame(a = a15[[1]],b = a15[[2]],c = a15[[3]],d = a15[[4]],e = a15[[5]],f = a15[[6]],g = a15[[7]],h = a15[[8]],i = a15[[9]],j = a15[[10]]),
  data.frame(a = a16[[1]],b = a16[[2]],c = a16[[3]],d = a16[[4]],e = a16[[5]],f = a16[[6]],g = a16[[7]],h = a16[[8]],i = a16[[9]],j = a16[[10]])
)


# TY
for(i in 55:64){
  a21[[i-54]] <- all_TY1 %>% dplyr::filter(all_TY1[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a22[[i-54]] <- all_TY2 %>% dplyr::filter(all_TY2[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a23[[i-54]] <- all_TY3 %>% dplyr::filter(all_TY3[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a24[[i-54]] <- all_TY4 %>% dplyr::filter(all_TY4[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a25[[i-54]] <- all_TY5 %>% dplyr::filter(all_TY5[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a26[[i-54]] <- all_TY6 %>% dplyr::filter(all_TY6[i] > 3) %>% nrow()  
}

interest_TY <- rbind(
  data.frame(a = a21[[1]],b = a21[[2]],c = a21[[3]],d = a21[[4]],e = a21[[5]],f = a21[[6]],g = a21[[7]],h = a21[[8]],i = a21[[9]],j = a21[[10]]), 
  data.frame(a = a22[[1]],b = a22[[2]],c = a22[[3]],d = a22[[4]],e = a22[[5]],f = a22[[6]],g = a22[[7]],h = a22[[8]],i = a22[[9]],j = a22[[10]]),
  data.frame(a = a23[[1]],b = a23[[2]],c = a23[[3]],d = a23[[4]],e = a23[[5]],f = a23[[6]],g = a23[[7]],h = a23[[8]],i = a23[[9]],j = a23[[10]]),
  data.frame(a = a24[[1]],b = a24[[2]],c = a24[[3]],d = a24[[4]],e = a24[[5]],f = a24[[6]],g = a24[[7]],h = a24[[8]],i = a24[[9]],j = a24[[10]]),
  data.frame(a = a25[[1]],b = a25[[2]],c = a25[[3]],d = a25[[4]],e = a25[[5]],f = a25[[6]],g = a25[[7]],h = a25[[8]],i = a25[[9]],j = a25[[10]]),
  data.frame(a = a26[[1]],b = a26[[2]],c = a26[[3]],d = a26[[4]],e = a26[[5]],f = a26[[6]],g = a26[[7]],h = a26[[8]],i = a26[[9]],j = a26[[10]])
)


# HC
for(i in 55:64){
  a31[[i-54]] <- all_HC1 %>% dplyr::filter(all_HC1[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a32[[i-54]] <- all_HC2 %>% dplyr::filter(all_HC2[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a33[[i-54]] <- all_HC3 %>% dplyr::filter(all_HC3[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a34[[i-54]] <- all_HC4 %>% dplyr::filter(all_HC4[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a35[[i-54]] <- all_HC5 %>% dplyr::filter(all_HC5[i] > 3) %>% nrow()  
} 
for(i in 55:64){
  a36[[i-54]] <- all_HC6 %>% dplyr::filter(all_HC6[i] > 3) %>% nrow()  
}

interest_HC <- rbind(
  data.frame(a = a31[[1]],b = a31[[2]],c = a31[[3]],d = a31[[4]],e = a31[[5]],f = a31[[6]],g = a31[[7]],h = a31[[8]],i = a31[[9]],j = a31[[10]]), 
  data.frame(a = a32[[1]],b = a32[[2]],c = a32[[3]],d = a32[[4]],e = a32[[5]],f = a32[[6]],g = a32[[7]],h = a32[[8]],i = a32[[9]],j = a32[[10]]),
  data.frame(a = a33[[1]],b = a33[[2]],c = a33[[3]],d = a33[[4]],e = a33[[5]],f = a33[[6]],g = a33[[7]],h = a33[[8]],i = a33[[9]],j = a33[[10]]),
  data.frame(a = a34[[1]],b = a34[[2]],c = a34[[3]],d = a34[[4]],e = a34[[5]],f = a34[[6]],g = a34[[7]],h = a34[[8]],i = a34[[9]],j = a34[[10]]),
  data.frame(a = a35[[1]],b = a35[[2]],c = a35[[3]],d = a35[[4]],e = a35[[5]],f = a35[[6]],g = a35[[7]],h = a35[[8]],i = a35[[9]],j = a35[[10]]),
  data.frame(a = a36[[1]],b = a36[[2]],c = a36[[3]],d = a36[[4]],e = a36[[5]],f = a36[[6]],g = a36[[7]],h = a36[[8]],i = a36[[9]],j = a36[[10]])
)


# TC
for(i in 55:64){
  a41[[i-54]] <- all_TC1 %>% dplyr::filter(all_TC1[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a42[[i-54]] <- all_TC2 %>% dplyr::filter(all_TC2[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a43[[i-54]] <- all_TC3 %>% dplyr::filter(all_TC3[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a44[[i-54]] <- all_TC4 %>% dplyr::filter(all_TC4[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a45[[i-54]] <- all_TC5 %>% dplyr::filter(all_TC5[i] > 3) %>% nrow()  
} 
for(i in 55:64){
  a46[[i-54]] <- all_TC6 %>% dplyr::filter(all_TC6[i] > 3) %>% nrow()  
}

interest_TC <- rbind(
  data.frame(a = a41[[1]],b = a41[[2]],c = a41[[3]],d = a41[[4]],e = a41[[5]],f = a41[[6]],g = a41[[7]],h = a41[[8]],i = a41[[9]],j = a41[[10]]), 
  data.frame(a = a42[[1]],b = a42[[2]],c = a42[[3]],d = a42[[4]],e = a42[[5]],f = a42[[6]],g = a42[[7]],h = a42[[8]],i = a42[[9]],j = a42[[10]]),
  data.frame(a = a43[[1]],b = a43[[2]],c = a43[[3]],d = a43[[4]],e = a43[[5]],f = a43[[6]],g = a43[[7]],h = a43[[8]],i = a43[[9]],j = a43[[10]]),
  data.frame(a = a44[[1]],b = a44[[2]],c = a44[[3]],d = a44[[4]],e = a44[[5]],f = a44[[6]],g = a44[[7]],h = a44[[8]],i = a44[[9]],j = a44[[10]]),
  data.frame(a = a45[[1]],b = a45[[2]],c = a45[[3]],d = a45[[4]],e = a45[[5]],f = a45[[6]],g = a45[[7]],h = a45[[8]],i = a45[[9]],j = a45[[10]]),
  data.frame(a = a46[[1]],b = a46[[2]],c = a46[[3]],d = a46[[4]],e = a46[[5]],f = a46[[6]],g = a46[[7]],h = a46[[8]],i = a46[[9]],j = a46[[10]])
)


# KH
for(i in 55:64){
  a51[[i-54]] <- all_KH1 %>% dplyr::filter(all_KH1[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a52[[i-54]] <- all_KH2 %>% dplyr::filter(all_KH2[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a53[[i-54]] <- all_KH3 %>% dplyr::filter(all_KH3[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a54[[i-54]] <- all_KH4 %>% dplyr::filter(all_KH4[i] > 3) %>% nrow()  
}
for(i in 55:64){
  a55[[i-54]] <- all_KH5 %>% dplyr::filter(all_KH5[i] > 3) %>% nrow()  
} 
for(i in 55:64){
  a56[[i-54]] <- all_KH6 %>% dplyr::filter(all_KH6[i] > 3) %>% nrow()  
}

interest_KH <- rbind(
  data.frame(a = a51[[1]],b = a51[[2]],c = a51[[3]],d = a51[[4]],e = a51[[5]],f = a51[[6]],g = a51[[7]],h = a51[[8]],i = a51[[9]],j = a51[[10]]), 
  data.frame(a = a52[[1]],b = a52[[2]],c = a52[[3]],d = a52[[4]],e = a52[[5]],f = a52[[6]],g = a52[[7]],h = a52[[8]],i = a52[[9]],j = a52[[10]]),
  data.frame(a = a53[[1]],b = a53[[2]],c = a53[[3]],d = a53[[4]],e = a53[[5]],f = a53[[6]],g = a53[[7]],h = a53[[8]],i = a53[[9]],j = a53[[10]]),
  data.frame(a = a54[[1]],b = a54[[2]],c = a54[[3]],d = a54[[4]],e = a54[[5]],f = a54[[6]],g = a54[[7]],h = a54[[8]],i = a54[[9]],j = a54[[10]]),
  data.frame(a = a55[[1]],b = a55[[2]],c = a55[[3]],d = a55[[4]],e = a55[[5]],f = a55[[6]],g = a55[[7]],h = a55[[8]],i = a55[[9]],j = a55[[10]]),
  data.frame(a = a56[[1]],b = a56[[2]],c = a56[[3]],d = a56[[4]],e = a56[[5]],f = a56[[6]],g = a56[[7]],h = a56[[8]],i = a56[[9]],j = a56[[10]])
)

# change to percentage 
interest_TPE[1,] <- interest_TPE[1,]/(all_TPE1 %>% nrow())
interest_TPE[2,] <- interest_TPE[2,]/(all_TPE2 %>% nrow())
interest_TPE[3,] <- interest_TPE[3,]/(all_TPE3 %>% nrow())
interest_TPE[4,] <- interest_TPE[4,]/(all_TPE4 %>% nrow())
interest_TPE[5,] <- interest_TPE[5,]/(all_TPE5 %>% nrow())
interest_TPE[6,] <- interest_TPE[6,]/(all_TPE6 %>% nrow())

interest_TY[1,] <- interest_TY[1,]/(all_TY1 %>% nrow())
interest_TY[2,] <- interest_TY[2,]/(all_TY2 %>% nrow())
interest_TY[3,] <- interest_TY[3,]/(all_TY3 %>% nrow())
interest_TY[4,] <- interest_TY[4,]/(all_TY4 %>% nrow())
interest_TY[5,] <- interest_TY[5,]/(all_TY5 %>% nrow())
interest_TY[6,] <- interest_TY[6,]/(all_TY6 %>% nrow())

interest_HC[1,] <- interest_HC[1,]/(all_HC1 %>% nrow())
interest_HC[2,] <- interest_HC[2,]/(all_HC2 %>% nrow())
interest_HC[3,] <- interest_HC[3,]/(all_HC3 %>% nrow())
interest_HC[4,] <- interest_HC[4,]/(all_HC4 %>% nrow())
interest_HC[5,] <- interest_HC[5,]/(all_HC5 %>% nrow())
interest_HC[6,] <- interest_HC[6,]/(all_HC6 %>% nrow())

interest_TC[1,] <- interest_TC[1,]/(all_TC1 %>% nrow())
interest_TC[2,] <- interest_TC[2,]/(all_TC2 %>% nrow())
interest_TC[3,] <- interest_TC[3,]/(all_TC3 %>% nrow())
interest_TC[4,] <- interest_TC[4,]/(all_TC4 %>% nrow())
interest_TC[5,] <- interest_TC[5,]/(all_TC5 %>% nrow())
interest_TC[6,] <- interest_TC[6,]/(all_TC6 %>% nrow())

interest_KH[1,] <- interest_KH[1,]/(all_KH1 %>% nrow())
interest_KH[2,] <- interest_KH[2,]/(all_KH2 %>% nrow())
interest_KH[3,] <- interest_KH[3,]/(all_KH3 %>% nrow())
interest_KH[4,] <- interest_KH[4,]/(all_KH4 %>% nrow())
interest_KH[5,] <- interest_KH[5,]/(all_KH5 %>% nrow())
interest_KH[6,] <- interest_KH[6,]/(all_KH6 %>% nrow())


# change colnames
colnames(interest_TPE) <- c("HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article",
                            "HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer")
colnames(interest_TY) <- c("HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article",
                           "HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer")
colnames(interest_HC) <- c("HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article",
                           "HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer")
colnames(interest_TC) <- c("HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article",
                           "HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer")
colnames(interest_KH) <- c("HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article",
                           "HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer")


interest_TPE <- round(interest_TPE * 100,1)
interest_TY <- round(interest_TY * 100,1)
interest_HC <- round(interest_HC * 100,1)
interest_TC <- round(interest_TC * 100,1)
interest_KH <- round(interest_KH * 100,1)


# radar plot 
library(fmsb)
radarchart(interest_TPE[1:3,],
           axistype=3 , 
           #custom polygon
           #pcol=rgb(0.2,0.5,0.5,0.9),  #線顏色
           #pfcol=rgb(0.2,0.5,0.5,0.5),  #面顏色
           plwd=2, 
           #custom the grid
           cglcol="grey",
           cglty=1,
           axislabcol="grey",
           caxislabels=seq(0,20,5),
           cglwd=0.8,
           centerzero = TRUE,
           maxmin = FALSE,
           #custom labels
           vlcex=0.8 
)


# decision tree 
# TPE
set.seed(1010024)
train.index11 <- sample(x=1:nrow(all_TPE), size=ceiling(0.78*nrow(all_TPE) ))
train11 <- all_TPE[train.index11,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test11 <- all_TPE[-train.index11,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model11<- rpart(V81 ~ . , data=train11) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model11,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# TY
set.seed(1221)
train.index7 <- sample(x=1:nrow(npower_TY), size=ceiling(0.78*nrow(npower_TY) ))
train7 <- npower_TY[train.index7,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test7 <- npower_TY[-train.index7,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model7<- rpart(V81 ~ . , data=train7) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model7,           # 模型
    yesno = 2,
    gap = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# HC
set.seed(150)
train.index8 <- sample(x=1:nrow(npower_HC), size=ceiling(0.78*nrow(npower_HC) ))
train8 <- npower_HC[train.index8,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test8 <- npower_HC[-train.index8,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model8<- rpart(V81 ~ . , data=train8) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model8,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# TC
set.seed(1390)
train.index9 <- sample(x=1:nrow(npower_TC), size=ceiling(0.78*nrow(npower_TC) ))
train9 <- npower_TC[train.index9,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test9 <- npower_TC[-train.index9,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model9<- rpart(V81 ~ . , data=train9) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model9,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)

# KH
set.seed(10010)
train.index10 <- sample(x=1:nrow(npower_KH), size=ceiling(0.78*nrow(npower_KH) ))
train10 <- npower_KH[train.index10,c(30:32,34:37,39:42,44:47,49:52,54,81)]        # offpeak使用習慣建模
test10 <- npower_KH[-train.index10,c(30:32,34:37,39:42,44:47,49:52,54,81)]

cart.model10<- rpart(V81 ~ . , data=train10) # CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
require(rpart.plot) 
prp(cart.model10,           # 模型
    yesno = 2,
    gap = 0,
    space = 0,
    extra = 100,
    tweak = .7,
    compress = FALSE,
    ycompress = FALSE,
    faclen = 0 ,          # 呈現的變數不要縮寫
    fallen.leaves=TRUE    # 讓樹枝以垂直方式呈現
)


















