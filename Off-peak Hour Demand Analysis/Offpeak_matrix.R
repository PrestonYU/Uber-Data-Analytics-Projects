raw <- read.csv(file = "rawdata.csv",fileEncoding = "UTF-8")
library(dplyr)
library(magrittr)

#####迴圈 

OWN <- list()
for(i in 1:5){
   OWN[[i]] <- dplyr::filter(raw, raw[i+29] > 3)
 }

PUBLIC <- list()
for(i in 1:5){
  PUBLIC[[i]] <- dplyr::filter(raw, raw[i+34] > 3)
}

SHAREBIKE <- list()
for(i in 1:5){
  SHAREBIKE[[i]] <- dplyr::filter(raw, raw[i+39] > 3)
}

APPCAR <- list()
for(i in 1:5){
  APPCAR[[i]] <- dplyr::filter(raw, raw[i+44] > 3)
}

TAXI <- list()
for(i in 1:5){
  TAXI[[i]] <- dplyr::filter(raw, raw[i+49] > 3)
}


#####矩陣化

OWN_data <- list()
for(j in 1:5){
a1 <- list()
a2 <- list()
a3 <- list()
a4 <- list()
a5 <- list()
a6 <- list()
a7 <- list()
a8 <- list()
a9 <- list()
a10 <- list()
a11 <- list()
a12 <- list()
a13 <- list()
a14 <- list()
  a1[[j]] <- filter(OWN[[j]], OWN[[j]]$Gender == "male") %>% nrow()
  a2[[j]] <- filter(OWN[[j]], OWN[[j]]$Gender == "female") %>% nrow()
  a3[[j]] <- filter(OWN[[j]], OWN[[j]]$Age == "19-_years_old") %>% nrow()
  a4[[j]] <- filter(OWN[[j]], OWN[[j]]$Age == "20_29_years_old") %>% nrow()
  a5[[j]] <- filter(OWN[[j]], OWN[[j]]$Age == "30_39_years_old") %>% nrow()
  a6[[j]] <- filter(OWN[[j]], OWN[[j]]$Age == "40_49_years_old") %>% nrow()
  a7[[j]] <- filter(OWN[[j]], OWN[[j]]$Age == "50+_years_old") %>% nrow()
  a8[[j]] <- OWN[[j]]$life_balance %>% median() 
  a9[[j]] <- OWN[[j]]$life_cognition %>% median() 
  a10[[j]] <- OWN[[j]]$work_feelings %>% median() 
  a11[[j]] <- OWN[[j]]$family_relationship %>% median() 
  a12[[j]] <- OWN[[j]]$work_free %>% median() 
  a13[[j]] <- OWN[[j]]$money_usage %>% median() 
  a14[[j]] <- OWN[[j]]$Indoor_Outdoor %>% median() 
  OWN_data[[j]] <- cbind(a1[[j]],a2[[j]],a3[[j]],a4[[j]],a5[[j]],a6[[j]],a7[[j]],a8[[j]],a9[[j]],a10[[j]],a11[[j]],a12[[j]],a13[[j]],a14[[j]]) 
}



PUBLIC_data <- list()
for(j in 1:5){
  b1 <- list()
  b2 <- list()
  b3 <- list()
  b4 <- list()
  b5 <- list()
  b6 <- list()
  b7 <- list()
  b8 <- list()
  b9 <- list()
  b10 <- list()
  b11 <- list()
  b12 <- list()
  b13 <- list()
  b14 <- list()
  b1[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Gender == "male") %>% nrow()
  b2[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Gender == "female") %>% nrow()
  b3[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Age == "19-_years_old") %>% nrow()
  b4[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Age == "20_29_years_old") %>% nrow()
  b5[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Age == "30_39_years_old") %>% nrow()
  b6[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Age == "40_49_years_old") %>% nrow()
  b7[[j]] <- filter(PUBLIC[[j]], PUBLIC[[j]]$Age == "50+_years_old") %>% nrow()
  b8[[j]] <- PUBLIC[[j]]$life_balance %>% median() 
  b9[[j]] <- PUBLIC[[j]]$life_cognition %>% median() 
  b10[[j]] <- PUBLIC[[j]]$work_feelings %>% median() 
  b11[[j]] <- PUBLIC[[j]]$family_relationship %>% median() 
  b12[[j]] <- PUBLIC[[j]]$work_free %>% median() 
  b13[[j]] <- PUBLIC[[j]]$money_usage %>% median() 
  b14[[j]] <- PUBLIC[[j]]$Indoor_Outdoor %>% median() 
  PUBLIC_data[[j]] <- cbind(b1[[j]],b2[[j]],b3[[j]],b4[[j]],b5[[j]],b6[[j]],b7[[j]],b8[[j]],b9[[j]],b10[[j]],b11[[j]],b12[[j]],b13[[j]],b14[[j]]) 
}



SHAREBIKE_data <- list()
for(j in 1:5){
  c1 <- list()
  c2 <- list()
  c3 <- list()
  c4 <- list()
  c5 <- list()
  c6 <- list()
  c7 <- list()
  c8 <- list()
  c9 <- list()
  c10 <- list()
  c11 <- list()
  c12 <- list()
  c13 <- list()
  c14 <- list()
  c1[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Gender == "male") %>% nrow()
  c2[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Gender == "female") %>% nrow()
  c3[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Age == "19-_years_old") %>% nrow()
  c4[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Age == "20_29_years_old") %>% nrow()
  c5[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Age == "30_39_years_old") %>% nrow()
  c6[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Age == "40_49_years_old") %>% nrow()
  c7[[j]] <- filter(SHAREBIKE[[j]], SHAREBIKE[[j]]$Age == "50+_years_old") %>% nrow()
  c8[[j]] <- SHAREBIKE[[j]]$life_balance %>% median() 
  c9[[j]] <- SHAREBIKE[[j]]$life_cognition %>% median() 
  c10[[j]] <- SHAREBIKE[[j]]$work_feelings %>% median() 
  c11[[j]] <- SHAREBIKE[[j]]$family_relationship %>% median() 
  c12[[j]] <- SHAREBIKE[[j]]$work_free %>% median() 
  c13[[j]] <- SHAREBIKE[[j]]$money_usage %>% median() 
  c14[[j]] <- SHAREBIKE[[j]]$Indoor_Outdoor %>% median() 
  SHAREBIKE_data[[j]] <- cbind(c1[[j]],c2[[j]],c3[[j]],c4[[j]],c5[[j]],c6[[j]],c7[[j]],c8[[j]],c9[[j]],c10[[j]],c11[[j]],c12[[j]],c13[[j]],c14[[j]]) 
}


APPCAR_data <- list()
for(j in 1:5){
  d1 <- list()
  d2 <- list()
  d3 <- list()
  d4 <- list()
  d5 <- list()
  d6 <- list()
  d7 <- list()
  d8 <- list()
  d9 <- list()
  d10 <- list()
  d11 <- list()
  d12 <- list()
  d13 <- list()
  d14 <- list()
  d1[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Gender == "male") %>% nrow()
  d2[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Gender == "female") %>% nrow()
  d3[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Age == "19-_years_old") %>% nrow()
  d4[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Age == "20_29_years_old") %>% nrow()
  d5[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Age == "30_39_years_old") %>% nrow()
  d6[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Age == "40_49_years_old") %>% nrow()
  d7[[j]] <- filter(APPCAR[[j]], APPCAR[[j]]$Age == "50+_years_old") %>% nrow()
  d8[[j]] <- APPCAR[[j]]$life_balance %>% median() 
  d9[[j]] <- APPCAR[[j]]$life_cognition %>% median() 
  d10[[j]] <- APPCAR[[j]]$work_feelings %>% median() 
  d11[[j]] <- APPCAR[[j]]$family_relationship %>% median() 
  d12[[j]] <- APPCAR[[j]]$work_free %>% median() 
  d13[[j]] <- APPCAR[[j]]$money_usage %>% median() 
  d14[[j]] <- APPCAR[[j]]$Indoor_Outdoor %>% median() 
  APPCAR_data[[j]] <- cbind(d1[[j]],d2[[j]],d3[[j]],d4[[j]],d5[[j]],d6[[j]],d7[[j]],d8[[j]],d9[[j]],d10[[j]],d11[[j]],d12[[j]],d13[[j]],d14[[j]]) 
}


TAXI_data <- list()
for(j in 1:5){
  e1 <- list()
  e2 <- list()
  e3 <- list()
  e4 <- list()
  e5 <- list()
  e6 <- list()
  e7 <- list()
  e8 <- list()
  e9 <- list()
  e10 <- list()
  e11 <- list()
  e12 <- list()
  e13 <- list()
  e14 <- list()
  e1[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Gender == "male") %>% nrow()
  e2[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Gender == "female") %>% nrow()
  e3[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Age == "19-_years_old") %>% nrow()
  e4[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Age == "20_29_years_old") %>% nrow()
  e5[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Age == "30_39_years_old") %>% nrow()
  e6[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Age == "40_49_years_old") %>% nrow()
  e7[[j]] <- filter(TAXI[[j]], TAXI[[j]]$Age == "50+_years_old") %>% nrow()
  e8[[j]] <- TAXI[[j]]$life_balance %>% median() 
  e9[[j]] <- TAXI[[j]]$life_cognition %>% median() 
  e10[[j]] <- TAXI[[j]]$work_feelings %>% median() 
  e11[[j]] <- TAXI[[j]]$family_relationship %>% median() 
  e12[[j]] <- TAXI[[j]]$work_free %>% median() 
  e13[[j]] <- TAXI[[j]]$money_usage %>% median() 
  e14[[j]] <- TAXI[[j]]$Indoor_Outdoor %>% median() 
  TAXI_data[[j]] <- cbind(e1[[j]],e2[[j]],e3[[j]],e4[[j]],e5[[j]],e6[[j]],e7[[j]],e8[[j]],e9[[j]],e10[[j]],e11[[j]],e12[[j]],e13[[j]],e14[[j]]) 
}


#OWN_data
#PUBLIC_data
#SHAREBIKE_data
#APPCAR_data
#TAXI_data

###表格化

OWN_chart <- rbind(OWN_data[[1]],OWN_data[[2]],OWN_data[[3]],OWN_data[[4]],OWN_data[[5]])
PUBLIC_chart <- rbind(PUBLIC_data[[1]],PUBLIC_data[[2]],PUBLIC_data[[3]],PUBLIC_data[[4]],PUBLIC_data[[5]])
SHAREBIKE_chart <- rbind(SHAREBIKE_data[[1]],SHAREBIKE_data[[2]],SHAREBIKE_data[[3]],SHAREBIKE_data[[4]],SHAREBIKE_data[[5]])
APPCAR_chart <- rbind(APPCAR_data[[1]],APPCAR_data[[2]],APPCAR_data[[3]],APPCAR_data[[4]],APPCAR_data[[5]])
TAXI_chart <- rbind(TAXI_data[[1]],TAXI_data[[2]],TAXI_data[[3]],TAXI_data[[4]],TAXI_data[[5]])

pwchart1 <- OWN_chart %>% as.data.frame()
pwchart2 <- PUBLIC_chart %>% as.data.frame()
pwchart3 <- SHAREBIKE_chart %>% as.data.frame()
pwchart4 <- APPCAR_chart %>% as.data.frame()
pwchart5 <- TAXI_chart %>% as.data.frame()

pwchart1 <- pwchart1[-4,]
pwchart2 <- pwchart2[-4,]
pwchart3 <- pwchart3[-4,]
pwchart4 <- pwchart4[-4,]
pwchart5 <- pwchart5[-4,]

colnames(pwchart1) <- c("male","female","19-_years_old",
                        "20_29_years_old","30_39_years_old","40_49_years_old","50+_years_old",
                        "life_balance","life_cognition","work_feelings","family_relationship",
                        "work_free","money_usage","Indoor_Outdoor")
colnames(pwchart2) <- c("male","female","19-_years_old",
                        "20_29_years_old","30_39_years_old","40_49_years_old","50+_years_old",
                        "life_balance","life_cognition","work_feelings","family_relationship",
                        "work_free","money_usage","Indoor_Outdoor")
colnames(pwchart3) <- c("male","female","19-_years_old",
                        "20_29_years_old","30_39_years_old","40_49_years_old","50+_years_old",
                        "life_balance","life_cognition","work_feelings","family_relationship",
                        "work_free","money_usage","Indoor_Outdoor")
colnames(pwchart4) <- c("male","female","19-_years_old",
                        "20_29_years_old","30_39_years_old","40_49_years_old","50+_years_old",
                        "life_balance","life_cognition","work_feelings","family_relationship",
                        "work_free","money_usage","Indoor_Outdoor")
colnames(pwchart5) <- c("male","female","19-_years_old",
                        "20_29_years_old","30_39_years_old","40_49_years_old","50+_years_old",
                        "life_balance","life_cognition","work_feelings","family_relationship",
                        "work_free","money_usage","Indoor_Outdoor")






#######################




#####迴圈 

OWN <- list()
for(i in 1:5){
  OWN[[i]] <- dplyr::filter(raw, raw[i+29] > 3)
}

PUBLIC <- list()
for(i in 1:5){
  PUBLIC[[i]] <- dplyr::filter(raw, raw[i+34] > 3)
}

SHAREBIKE <- list()
for(i in 1:5){
  SHAREBIKE[[i]] <- dplyr::filter(raw, raw[i+39] > 3)
}

APPCAR <- list()
for(i in 1:5){
  APPCAR[[i]] <- dplyr::filter(raw, raw[i+44] > 3)
}

TAXI <- list()
for(i in 1:5){
  TAXI[[i]] <- dplyr::filter(raw, raw[i+49] > 3)
}


#####矩陣化

OWN_data <- list()
for(j in 1:5){
  a1 <- list()
  a2 <- list()
  a3 <- list()
  a4 <- list()
  a5 <- list()
  a6 <- list()
  a7 <- list()
  a8 <- list()
  a9 <- list()
  a10 <- list()
  a11 <- list()
  a12 <- list()
  a13 <- list()
  a14 <- list()
  a15 <- list()
  a16 <- list()
  a17 <- list()
  a18 <- list()
  a19 <- list()
  a1[[j]] <- OWN[[j]]$HOBBY.travel %>% mean() 
  a2[[j]] <- OWN[[j]]$HOBBY.photography %>% mean()
  a3[[j]] <- OWN[[j]]$HOBBY.cooking %>% mean()
  a4[[j]] <- OWN[[j]]$HOBBY.tech %>% mean()
  a5[[j]] <- OWN[[j]]$HOBBY.article %>% mean()
  a6[[j]] <- OWN[[j]]$HOBBY.sports %>% mean()
  a7[[j]] <- OWN[[j]]$HOBBY.shop %>% mean()
  a8[[j]] <- OWN[[j]]$HOBBY.movie %>% mean()
  a9[[j]] <- OWN[[j]]$HOBBY.finance %>% mean() 
  a10[[j]] <- OWN[[j]]$HOBBY.volunteer %>% mean() 
  a11[[j]] <- OWN[[j]]$SOCIAL.FB %>% mean()
  a12[[j]] <- OWN[[j]]$SOCIAL.Line %>% mean() 
  a13[[j]] <- OWN[[j]]$SOCIAL.Youtube %>% mean() 
  a14[[j]] <- OWN[[j]]$SOCIAL.PTT %>% mean()
  a15[[j]] <- OWN[[j]]$SOCIAL.TikTok %>% mean()
  a16[[j]] <- OWN[[j]]$SOCIAL.IG %>% mean()
  a17[[j]] <- OWN[[j]]$SOCIAL.WeChat %>% mean()
  a18[[j]] <- OWN[[j]]$SOCIAL.Twitter %>% mean()
  a19[[j]] <- OWN[[j]]$SOCIAL.Dcard %>% mean()
  OWN_data[[j]] <- cbind(a1[[j]],a2[[j]],a3[[j]],a4[[j]],a5[[j]],a6[[j]],a7[[j]],a8[[j]],a9[[j]],a10[[j]],a11[[j]],a12[[j]],a13[[j]],a14[[j]],a15[[j]],a16[[j]],a17[[j]],a18[[j]],a19[[j]]) 
}



PUBLIC_data <- list()
for(j in 1:5){
  b1 <- list()
  b2 <- list()
  b3 <- list()
  b4 <- list()
  b5 <- list()
  b6 <- list()
  b7 <- list()
  b8 <- list()
  b9 <- list()
  b10 <- list()
  b11 <- list()
  b12 <- list()
  b13 <- list()
  b14 <- list()
  b15 <- list()
  b16 <- list()
  b17 <- list()
  b18 <- list()
  b19 <- list()
  b1[[j]] <- PUBLIC[[j]]$HOBBY.travel %>% mean() 
  b2[[j]] <- PUBLIC[[j]]$HOBBY.photography %>% mean()
  b3[[j]] <- PUBLIC[[j]]$HOBBY.cooking %>% mean()
  b4[[j]] <- PUBLIC[[j]]$HOBBY.tech %>% mean()
  b5[[j]] <- PUBLIC[[j]]$HOBBY.article %>% mean()
  b6[[j]] <- PUBLIC[[j]]$HOBBY.sports %>% mean()
  b7[[j]] <- PUBLIC[[j]]$HOBBY.shop %>% mean()
  b8[[j]] <- PUBLIC[[j]]$HOBBY.movie %>% mean()
  b9[[j]] <- PUBLIC[[j]]$HOBBY.finance %>% mean() 
  b10[[j]] <- PUBLIC[[j]]$HOBBY.volunteer %>% mean() 
  b11[[j]] <- PUBLIC[[j]]$SOCIAL.FB %>% mean()
  b12[[j]] <- PUBLIC[[j]]$SOCIAL.Line %>% mean() 
  b13[[j]] <- PUBLIC[[j]]$SOCIAL.Youtube %>% mean() 
  b14[[j]] <- PUBLIC[[j]]$SOCIAL.PTT %>% mean()
  b15[[j]] <- PUBLIC[[j]]$SOCIAL.TikTok %>% mean()
  b16[[j]] <- PUBLIC[[j]]$SOCIAL.IG %>% mean()
  b17[[j]] <- PUBLIC[[j]]$SOCIAL.WeChat %>% mean()
  b18[[j]] <- PUBLIC[[j]]$SOCIAL.Twitter %>% mean()
  b19[[j]] <- PUBLIC[[j]]$SOCIAL.Dcard %>% mean() 
  PUBLIC_data[[j]] <- cbind(b1[[j]],b2[[j]],b3[[j]],b4[[j]],b5[[j]],b6[[j]],b7[[j]],b8[[j]],b9[[j]],b10[[j]],b11[[j]],b12[[j]],b13[[j]],b14[[j]],b15[[j]],b16[[j]],b17[[j]],b18[[j]],b19[[j]]) 
}



SHAREBIKE_data <- list()
for(j in 1:5){
  c1 <- list()
  c2 <- list()
  c3 <- list()
  c4 <- list()
  c5 <- list()
  c6 <- list()
  c7 <- list()
  c8 <- list()
  c9 <- list()
  c10 <- list()
  c11 <- list()
  c12 <- list()
  c13 <- list()
  c14 <- list()
  c15 <- list()
  c16 <- list()
  c17 <- list()
  c18 <- list()
  c19 <- list()
  c1[[j]] <- SHAREBIKE[[j]]$HOBBY.travel %>% mean() 
  c2[[j]] <- SHAREBIKE[[j]]$HOBBY.photography %>% mean()
  c3[[j]] <- SHAREBIKE[[j]]$HOBBY.cooking %>% mean()
  c4[[j]] <- SHAREBIKE[[j]]$HOBBY.tech %>% mean()
  c5[[j]] <- SHAREBIKE[[j]]$HOBBY.article %>% mean()
  c6[[j]] <- SHAREBIKE[[j]]$HOBBY.sports %>% mean()
  c7[[j]] <- SHAREBIKE[[j]]$HOBBY.shop %>% mean()
  c8[[j]] <- SHAREBIKE[[j]]$HOBBY.movie %>% mean()
  c9[[j]] <- SHAREBIKE[[j]]$HOBBY.finance %>% mean() 
  c10[[j]] <- SHAREBIKE[[j]]$HOBBY.volunteer %>% mean() 
  c11[[j]] <- SHAREBIKE[[j]]$SOCIAL.FB %>% mean()
  c12[[j]] <- SHAREBIKE[[j]]$SOCIAL.Line %>% mean() 
  c13[[j]] <- SHAREBIKE[[j]]$SOCIAL.Youtube %>% mean() 
  c14[[j]] <- SHAREBIKE[[j]]$SOCIAL.PTT %>% mean()
  c15[[j]] <- SHAREBIKE[[j]]$SOCIAL.TikTok %>% mean()
  c16[[j]] <- SHAREBIKE[[j]]$SOCIAL.IG %>% mean()
  c17[[j]] <- SHAREBIKE[[j]]$SOCIAL.WeChat %>% mean()
  c18[[j]] <- SHAREBIKE[[j]]$SOCIAL.Twitter %>% mean()
  c19[[j]] <- SHAREBIKE[[j]]$SOCIAL.Dcard %>% mean()  
  SHAREBIKE_data[[j]] <- cbind(c1[[j]],c2[[j]],c3[[j]],c4[[j]],c5[[j]],c6[[j]],c7[[j]],c8[[j]],c9[[j]],c10[[j]],c11[[j]],c12[[j]],c13[[j]],c14[[j]],c15[[j]],c16[[j]],c17[[j]],c18[[j]],c19[[j]]) 
}


APPCAR_data <- list()
for(j in 1:5){
  d1 <- list()
  d2 <- list()
  d3 <- list()
  d4 <- list()
  d5 <- list()
  d6 <- list()
  d7 <- list()
  d8 <- list()
  d9 <- list()
  d10 <- list()
  d11 <- list()
  d12 <- list()
  d13 <- list()
  d14 <- list()
  d15 <- list()
  d16 <- list()
  d17 <- list()
  d18 <- list()
  d19 <- list()
  d1[[j]] <- APPCAR[[j]]$HOBBY.travel %>% mean() 
  d2[[j]] <- APPCAR[[j]]$HOBBY.photography %>% mean()
  d3[[j]] <- APPCAR[[j]]$HOBBY.cooking %>% mean()
  d4[[j]] <- APPCAR[[j]]$HOBBY.tech %>% mean()
  d5[[j]] <- APPCAR[[j]]$HOBBY.article %>% mean()
  d6[[j]] <- APPCAR[[j]]$HOBBY.sports %>% mean()
  d7[[j]] <- APPCAR[[j]]$HOBBY.shop %>% mean()
  d8[[j]] <- APPCAR[[j]]$HOBBY.movie %>% mean()
  d9[[j]] <- APPCAR[[j]]$HOBBY.finance %>% mean() 
  d10[[j]] <- APPCAR[[j]]$HOBBY.volunteer %>% mean() 
  d11[[j]] <- APPCAR[[j]]$SOCIAL.FB %>% mean()
  d12[[j]] <- APPCAR[[j]]$SOCIAL.Line %>% mean() 
  d13[[j]] <- APPCAR[[j]]$SOCIAL.Youtube %>% mean() 
  d14[[j]] <- APPCAR[[j]]$SOCIAL.PTT %>% mean()
  d15[[j]] <- APPCAR[[j]]$SOCIAL.TikTok %>% mean()
  d16[[j]] <- APPCAR[[j]]$SOCIAL.IG %>% mean()
  d17[[j]] <- APPCAR[[j]]$SOCIAL.WeChat %>% mean()
  d18[[j]] <- APPCAR[[j]]$SOCIAL.Twitter %>% mean()
  d19[[j]] <- APPCAR[[j]]$SOCIAL.Dcard %>% mean()
  APPCAR_data[[j]] <- cbind(d1[[j]],d2[[j]],d3[[j]],d4[[j]],d5[[j]],d6[[j]],d7[[j]],d8[[j]],d9[[j]],d10[[j]],d11[[j]],d12[[j]],d13[[j]],d14[[j]],d15[[j]],d16[[j]],d17[[j]],d18[[j]],d19[[j]]) 
}


TAXI_data <- list()
for(j in 1:5){
  e1 <- list()
  e2 <- list()
  e3 <- list()
  e4 <- list()
  e5 <- list()
  e6 <- list()
  e7 <- list()
  e8 <- list()
  e9 <- list()
  e10 <- list()
  e11 <- list()
  e12 <- list()
  e13 <- list()
  e14 <- list()
  e15 <- list()
  e16 <- list()
  e17 <- list()
  e18 <- list()
  e19 <- list()
  e1[[j]] <- TAXI[[j]]$HOBBY.travel %>% mean() 
  e2[[j]] <- TAXI[[j]]$HOBBY.photography %>% mean()
  e3[[j]] <- TAXI[[j]]$HOBBY.cooking %>% mean()
  e4[[j]] <- TAXI[[j]]$HOBBY.tech %>% mean()
  e5[[j]] <- TAXI[[j]]$HOBBY.article %>% mean()
  e6[[j]] <- TAXI[[j]]$HOBBY.sports %>% mean()
  e7[[j]] <- TAXI[[j]]$HOBBY.shop %>% mean()
  e8[[j]] <- TAXI[[j]]$HOBBY.movie %>% mean()
  e9[[j]] <- TAXI[[j]]$HOBBY.finance %>% mean() 
  e10[[j]] <- TAXI[[j]]$HOBBY.volunteer %>% mean() 
  e11[[j]] <- TAXI[[j]]$SOCIAL.FB %>% mean()
  e12[[j]] <- TAXI[[j]]$SOCIAL.Line %>% mean() 
  e13[[j]] <- TAXI[[j]]$SOCIAL.Youtube %>% mean() 
  e14[[j]] <- TAXI[[j]]$SOCIAL.PTT %>% mean()
  e15[[j]] <- TAXI[[j]]$SOCIAL.TikTok %>% mean()
  e16[[j]] <- TAXI[[j]]$SOCIAL.IG %>% mean()
  e17[[j]] <- TAXI[[j]]$SOCIAL.WeChat %>% mean()
  e18[[j]] <- TAXI[[j]]$SOCIAL.Twitter %>% mean()
  e19[[j]] <- TAXI[[j]]$SOCIAL.Dcard %>% mean() 
  TAXI_data[[j]] <- cbind(e1[[j]],e2[[j]],e3[[j]],e4[[j]],e5[[j]],e6[[j]],e7[[j]],e8[[j]],e9[[j]],e10[[j]],e11[[j]],e12[[j]],e13[[j]],e14[[j]],e15[[j]],e16[[j]],e17[[j]],e18[[j]],e19[[j]]) 
}


#OWN_data
#PUBLIC_data
#SHAREBIKE_data
#APPCAR_data
#TAXI_data

###表格化

OWN_chart <- rbind(OWN_data[[1]],OWN_data[[2]],OWN_data[[3]],OWN_data[[4]],OWN_data[[5]])
PUBLIC_chart <- rbind(PUBLIC_data[[1]],PUBLIC_data[[2]],PUBLIC_data[[3]],PUBLIC_data[[4]],PUBLIC_data[[5]])
SHAREBIKE_chart <- rbind(SHAREBIKE_data[[1]],SHAREBIKE_data[[2]],SHAREBIKE_data[[3]],SHAREBIKE_data[[4]],SHAREBIKE_data[[5]])
APPCAR_chart <- rbind(APPCAR_data[[1]],APPCAR_data[[2]],APPCAR_data[[3]],APPCAR_data[[4]],APPCAR_data[[5]])
TAXI_chart <- rbind(TAXI_data[[1]],TAXI_data[[2]],TAXI_data[[3]],TAXI_data[[4]],TAXI_data[[5]])

pwchart11 <- OWN_chart %>% as.data.frame()
pwchart22 <- PUBLIC_chart %>% as.data.frame()
pwchart33 <- SHAREBIKE_chart %>% as.data.frame()
pwchart44 <- APPCAR_chart %>% as.data.frame()
pwchart55 <- TAXI_chart %>% as.data.frame()

pwchart11 <- pwchart11[-4,]
pwchart22 <- pwchart22[-4,]
pwchart33 <- pwchart33[-4,]
pwchart44 <- pwchart44[-4,]
pwchart55 <- pwchart55[-4,]

colnames(pwchart11) <- c("HOBBY_travel","HOBBY_photography","HOBBY_cooking",
                        "HOBBY_tech","HOBBY_article","HOBBY_sports","HOBBY_shopping",
                        "HOBBY_movie","HOBBY_finance","HOBBY_volunteer","SOCIAL_FB",
                        "SOCIAL_Line","SOCIAL_Youtube","SOCIAL_PTT","SOCIAL_TikTok","SOCIAL_IG"
                        ,"SOCIAL_WeChat","SOCIAL_Twitter","SOCIAL_Dcard")
colnames(pwchart22) <- c("HOBBY_travel","HOBBY_photography","HOBBY_cooking",
                         "HOBBY_tech","HOBBY_article","HOBBY_sports","HOBBY_shopping",
                         "HOBBY_movie","HOBBY_finance","HOBBY_volunteer","SOCIAL_FB",
                         "SOCIAL_Line","SOCIAL_Youtube","SOCIAL_PTT","SOCIAL_TikTok","SOCIAL_IG"
                         ,"SOCIAL_WeChat","SOCIAL_Twitter","SOCIAL_Dcard")
colnames(pwchart33) <- c("HOBBY_travel","HOBBY_photography","HOBBY_cooking",
                         "HOBBY_tech","HOBBY_article","HOBBY_sports","HOBBY_shopping",
                         "HOBBY_movie","HOBBY_finance","HOBBY_volunteer","SOCIAL_FB",
                         "SOCIAL_Line","SOCIAL_Youtube","SOCIAL_PTT","SOCIAL_TikTok","SOCIAL_IG"
                         ,"SOCIAL_WeChat","SOCIAL_Twitter","SOCIAL_Dcard")
colnames(pwchart44) <- c("HOBBY_travel","HOBBY_photography","HOBBY_cooking",
                         "HOBBY_tech","HOBBY_article","HOBBY_sports","HOBBY_shopping",
                         "HOBBY_movie","HOBBY_finance","HOBBY_volunteer","SOCIAL_FB",
                         "SOCIAL_Line","SOCIAL_Youtube","SOCIAL_PTT","SOCIAL_TikTok","SOCIAL_IG"
                         ,"SOCIAL_WeChat","SOCIAL_Twitter","SOCIAL_Dcard")
colnames(pwchart55) <- c("HOBBY_travel","HOBBY_photography","HOBBY_cooking",
                         "HOBBY_tech","HOBBY_article","HOBBY_sports","HOBBY_shopping",
                         "HOBBY_movie","HOBBY_finance","HOBBY_volunteer","SOCIAL_FB",
                         "SOCIAL_Line","SOCIAL_Youtube","SOCIAL_PTT","SOCIAL_TikTok","SOCIAL_IG"
                         ,"SOCIAL_WeChat","SOCIAL_Twitter","SOCIAL_Dcard")




