# 讀取轉化量表之後的csv檔
data <- read.csv(file = "raw3.csv",encoding = "UTF-8")

library(dplyr)
# sample
tpe <- filter(.data = data, Home == "tpe")
tyhc <- filter(.data = data, Home != "tpe",Home != "tc",Home != "kh" )
tckh <- filter(.data = data, Home != "tpe",Home != "ty",Home != "hc")


##########################

# cluster
library(magrittr)
E.dist <- tpe[-c(1:4)] %>% dist(method="euclidean")      # 歐式距離
h.cluster <- hclust(E.dist, method="ward.D2") # 華德法
plot(h.cluster, xlab="歐式距離")
abline(h=57, col="red")
cut.h.cluster <- cutree(h.cluster, k=4)  # 分成三群
cut.h.cluster                            # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)       # 分群結果和實際結果比較
tpe[81] <- paste("cluster",cut.h.cluster)


set.seed(110200)
train.index <- sample(x=1:nrow(tpe), size=ceiling(0.5*nrow(tpe) ))
train <- tpe[train.index,c(74:80,81)]
test <- tpe[-train.index,c(74:80,81)]

# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(V81 ~ . , data=train)
require(rpart.plot) 
prp(cart.model,   # 模型
    #faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=100)

#write.csv(x = tpe,file = "tpe_cluster.csv")


######################


# cluster
library(magrittr)
E.dist <- tyhc[-c(1:4)] %>% dist(method="euclidean")      # 歐式距離
h.cluster <- hclust(E.dist, method="ward.D2") # 華德法
plot(h.cluster, xlab="歐式距離")
abline(h=49, col="red")
cut.h.cluster <- cutree(h.cluster, k=4)  # 分成三群
cut.h.cluster                            # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)       # 分群結果和實際結果比較
tyhc[81] <- paste("cluster",cut.h.cluster)


set.seed(600)
train.index <- sample(x=1:nrow(tyhc), size=ceiling(0.8*nrow(tyhc) ))
train <- tyhc[train.index,c(55:73,81)]
test <- tyhc[-train.index,c(55:73,81)]

# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(V81 ~ . , data=train)
require(rpart.plot) 
prp(cart.model,   # 模型
    #faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=100)


######################



# cluster
library(magrittr)
E.dist <- tckh[-c(1:4)] %>% dist(method="euclidean")      # 歐式距離
h.cluster <- hclust(E.dist, method="ward.D2") # 華德法
plot(h.cluster, xlab="歐式距離")
abline(h=49, col="red")
cut.h.cluster <- cutree(h.cluster, k=4)  # 分成三群
cut.h.cluster                            # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)       # 分群結果和實際結果比較
tckh[81] <- paste("cluster",cut.h.cluster)


set.seed(10460)
train.index <- sample(x=1:nrow(tckh), size=ceiling(0.8*nrow(tckh) ))
train <- tckh[train.index,c(9:29,81)]
test <- tckh[-train.index,c(9:29,81)]

# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(V81 ~ . , data=train)
require(rpart.plot) 
prp(cart.model,   # 模型
    #faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=100)











