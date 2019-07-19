# 刪除空白欄
raw <- read.csv(file = "raw.csv",encoding = "UTF-8")
raw <- raw[-c(1,2,3,5,6,7,8,9)]
raw <- raw[-c(2,3,4,5,6)]
raw <- raw[-c(7,8,9,10,11)]
raw <- raw[-c(12:20)]
raw <- raw[-c(17:25)]
raw <- raw[-c(22:30)]
raw <- raw[-c(52:59)]
raw <- raw[-c(78:87)]

# 刪除未完成的列
library(dplyr)
for(i in 1:81){
  raw <- filter(.data = raw, raw[i]!="") 
}

# 設定colname
colnames(raw) <- c("timestamp",
                   "FREQ_own","FREQ_public","FREQ_sharebike","FREQ_appcar","FREQ_taxi",
                   "RUSH_own","RUSH_public","RUSH_sharebike","RUSH_appcar","RUSH_taxi",
                   "WEATHER_own","WEATHER_public","WEATHER_sharebike","WEATHER_appcar","WEATHER_taxi",
                   "SHOP_own","SHOP_public","SHOP_sharebike","SHOP_appcar","SHOP_taxi",
                   "BUSINESS_own","BUSINESS_public","BUSINESS_sharebike","BUSINESS_appcar","BUSINESS_taxi",
                   "offpeak_OWN_commute","offpeak_OWN_weather","offpeak_OWN_shop","offpeak_OWN_dining","offpeak_OWN_business",
                   "offpeak_PUBLIC_commute","offpeak_PUBLIC_weather","offpeak_PUBLIC_shop","offpeak_PUBLIC_dining","offpeak_PUBLIC_business",
                   "offpeak_SHAREBIKE_commute","offpeak_SHAREBIKE_weather","offpeak_SHAREBIKE_shop","offpeak_SHAREBIKE_dining","offpeak_SHAREBIKE_business",
                   "offpeak_APPCAR_commute","offpeak_APPCAR_weather","offpeak_APPCAR_shop","offpeak_APPCAR_dining","offpeak_APPCAR_business",
                   "offpeak_TAXI_commute","offpeak_TAXI_weather","offpeak_TAXI_shop","offpeak_TAXI_dining","offpeak_TAXI_business",
                   "HOBBY-travel","HOBBY-photography","HOBBY-cooking","HOBBY-tech","HOBBY-article","HOBBY-sports","HOBBY-shop","HOBBY-movie","HOBBY-finance","HOBBY-volunteer",
                   "SOCIAL-FB","SOCIAL-Line","SOCIAL-Youtube","SOCIAL-PTT","SOCIAL-TikTok","SOCIAL-IG","SOCIAL-WeChat","SOCIAL-Twitter","SOCIAL-Dcard",
                   "life_balance","life_cognition","work_feelings","family_relationship","work_free","money_usage","Indoor_Outdoor",
                   "Gender","Age","Job","Home")

write.csv(x = raw,file = "raw2.csv")

#######################################################################################

# 讀取轉化量表之後的csv檔
data <- read.csv(file = "raw3.csv",encoding = "UTF-8")

library(dplyr)
# PCA
tpe <- filter(.data = data, Home == "tpe")
tyhc <- filter(.data = data, Home != "tpe",Home != "tc",Home != "kh" )
tckh <- filter(.data = data, Home != "tpe",Home != "ty",Home != "hc")

library(factoextra)
tpe_sample <- sample_n(tbl = tpe, size = 800)
tyhc_sample <- sample_n(tbl = tyhc, size = 400)
tckh_sample <- sample_n(tbl = tckh, size = 400)

res.pca <- prcomp(tpe[c(74:80)],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = tpe$V81, 
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

fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = tpe_male$Job, # color by groups
             palette = c("#666547", "#fb2e01", "#6fcb9f","#ffe28a","#fffeb3","#edc951","#eb6841","#cc2a36","#4f372d","#00a0b0","#0084ff","#44bec7","#ffc300","#fa3c4c","#d696bb"),
             addEllipses = TRUE # Concentration ellipses
)




#################################

pca <- prcomp(formula = ~ 
                +tpe$RUSH_own
              +tpe$RUSH_public
              +tpe$RUSH_sharebike
              +tpe$RUSH_appcar
              +tpe$RUSH_taxi,  
              data = tpe[c(10:14)]
              ,scale = TRUE
) 
pca
# 使用plot()函式
plot(pca,         # 放pca
     type="line", # 用直線連結每個點
     main="Q4 NDI") # 主標題
# 用藍線標示出特徵值=1的地方
abline(h=1, col="blue") # Kaiser eigenvalue-greater-than-one rule

vars <- (pca$sdev)^2  # 從pca中取出標準差(pca$sdev)後再平方，計算variance(特徵值)
vars

# 計算每個主成分的解釋比例 = 各個主成分的特徵值/總特徵值
props <- vars / sum(vars)    
props

cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props

#當我們取前三個主成份，可以解釋 70.64% 的變異
cumulative.props[2]
plot(cumulative.props)

# pca$rotation 
top2_pca.data <- pca$x[, 1:2]
top2_pca.data 

# 特徵向量(原變數的線性組合)
pca$rotation
top2.pca.eigenvector <- pca$rotation[, 1:2]
top2.pca.eigenvector

first.pca <- top2.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top2.pca.eigenvector[, 2]  #  第二主成份

# 第一主成份：由小到大排序原變數的係數
first.pca[order(first.pca, decreasing=FALSE)]  # Avg_TPH和Avg_NRH有關係
# 使用dotchart，繪製主成份負荷圖
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")   


# 第二主成份：由小到大排序原變數的係數
second.pca[order(second.pca, decreasing=FALSE)] #Earning/Trip/SH有關係
# 使用dotchart，繪製主成份負荷圖
dotchart(second.pca[order(second.pca, decreasing=FALSE)] ,  # 排序後的係數
         main="Loading Plot for PC2",                       # 主標題
         xlab="Variable Loadings",                          # x軸的標題
         col="blue") 

# 選取 PC1 和 PC2 繪製主成份負荷圖
biplot(pca, choices=1:2, col = my_cols[tpe$Age] ,pch = 19, cex = 0.5) 



















