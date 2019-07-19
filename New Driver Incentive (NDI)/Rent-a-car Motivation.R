######### data cleaning ########

Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
library(dplyr)
a <- read.csv(file = "racms.csv",encoding = "UTF-8")
a <- a[-1]
a <- a[-12]
#a <- a[-28]
colnames(a) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","S1","S2","S3","S4","S5","S6","S7","S8","S9","S10",
                 "JobStat","WorkHR","IncomeStat","Gender","Age","Edu","RCP","Reg","Retention")

####### grouping1 ########
tpe_raw <- dplyr::filter(.data = a, Reg == "TPE")
hc_raw <- dplyr::filter(.data = a, Reg == "HC")
tc_raw <- dplyr::filter(.data = a, Reg == "TC")
kh_raw <- dplyr::filter(.data = a, Reg == "KH")


####### grouping2 ########

library(magrittr)
E.dist1 <- tpe_raw[-c(22:30)] %>% dist(method="euclidean")      # 歐式距離
h.cluster1 <- hclust(E.dist1, method="ward.D2")                 # 華德法
plot(h.cluster1, xlab="歐式距離")
abline(h=57, col="red")
cut.h.cluster1 <- cutree(h.cluster1, k=4)                       # 分成三群
cut.h.cluster1                                                  # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)               # 分群結果和實際結果比較
tpe_raw[31] <- paste("cluster",cut.h.cluster1)


library(magrittr)
E.dist2 <- hc_raw[-c(22:30)] %>% dist(method="euclidean")      # 歐式距離
h.cluster2 <- hclust(E.dist2, method="ward.D2")                 # 華德法
plot(h.cluster2, xlab="歐式距離")
abline(h=57, col="red")
cut.h.cluster2 <- cutree(h.cluster2, k=4)                       # 分成三群
cut.h.cluster2                                                  # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)               # 分群結果和實際結果比較
hc_raw[31] <- paste("cluster",cut.h.cluster2)


library(magrittr)
E.dist3 <- tc_raw[-c(22:30)] %>% dist(method="euclidean")      # 歐式距離
h.cluster3 <- hclust(E.dist3, method="ward.D2")                 # 華德法
plot(h.cluster3, xlab="歐式距離")
abline(h=57, col="red")
cut.h.cluster3 <- cutree(h.cluster3, k=4)                       # 分成三群
cut.h.cluster3                                                  # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)               # 分群結果和實際結果比較
tc_raw[31] <- paste("cluster",cut.h.cluster3)


library(magrittr)
E.dist4 <- kh_raw[-c(22:30)] %>% dist(method="euclidean")      # 歐式距離
h.cluster4 <- hclust(E.dist4, method="ward.D2")                 # 華德法
plot(h.cluster4, xlab="歐式距離")
abline(h=57, col="red")
cut.h.cluster4 <- cutree(h.cluster4, k=4)                       # 分成三群
cut.h.cluster4                                                  # 分群結果
#table(cut.h.cluster, tpe$offpeak_APPCAR_commute)               # 分群結果和實際結果比較
kh_raw[31] <- paste("cluster",cut.h.cluster4)



####### PCA #######

library(factoextra)
tpe_sample <- sample_n(tbl = tpe_raw, size = 272)
hc_sample <- sample_n(tbl = hc_raw, size = 29)
tc_sample <- sample_n(tbl = tc_raw, size = 108)
kh_sample <- sample_n(tbl = kh_raw, size = 102)

res.pca <- prcomp(tpe_raw[1:11],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = tpe_raw$V31, 
                col.ind = "white",
                pointshape = 21,
                pointsize = 2, 
                #repel = TRUE, 
                palette = c("#0084ff", "#44bec7", "#ffc300","#fa3c4c","#d696bb","#edc951","#fffeb3","#cc2a36","#4f372d","#00a0b0","#0084ff","#44bec7","#ffc300","#fa3c4c","#d696bb"),
                addEllipses = TRUE,
                ellipse.level = 0.5,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                title = "Tpe_VALUE"#,
                #gradient.cols = "RdYlBu"
)

write.csv(x = tpe_raw, file = "tpe_raw.csv")


res.pca <- prcomp(hc_raw[1:11],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = hc_raw$V31, 
                col.ind = "white",
                pointshape = 21,
                pointsize = 2, 
                #repel = TRUE, 
                palette = c("#0084ff", "#44bec7", "#ffc300","#fa3c4c","#d696bb","#edc951","#fffeb3","#cc2a36","#4f372d","#00a0b0","#0084ff","#44bec7","#ffc300","#fa3c4c","#d696bb"),
                addEllipses = TRUE,
                ellipse.level = 0.5,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                title = "Tpe_VALUE"#,
                #gradient.cols = "RdYlBu"
)

write.csv(x = hc_raw, file = "hc_raw.csv")


res.pca <- prcomp(tc_raw[1:11],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = tc_raw$V31, 
                col.ind = "white",
                pointshape = 21,
                pointsize = 2, 
                #repel = TRUE, 
                palette = c("#0084ff", "#44bec7", "#ffc300","#fa3c4c","#d696bb","#edc951","#fffeb3","#cc2a36","#4f372d","#00a0b0","#0084ff","#44bec7","#ffc300","#fa3c4c","#d696bb"),
                addEllipses = TRUE,
                ellipse.level = 0.5,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                title = "Tpe_VALUE"#,
                #gradient.cols = "RdYlBu"
)

write.csv(x = tc_raw, file = "tc_raw.csv")


res.pca <- prcomp(kh_raw[1:11],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = kh_raw$V31, 
                col.ind = "white",
                pointshape = 21,
                pointsize = 2, 
                #repel = TRUE, 
                palette = c("#0084ff", "#44bec7", "#ffc300","#fa3c4c","#d696bb","#edc951","#fffeb3","#cc2a36","#4f372d","#00a0b0","#0084ff","#44bec7","#ffc300","#fa3c4c","#d696bb"),
                addEllipses = TRUE,
                ellipse.level = 0.5,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                title = "Tpe_VALUE"#,
                #gradient.cols = "RdYlBu"
)

write.csv(x = kh_raw, file = "kh_raw.csv")

