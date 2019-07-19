H1 <- read.csv(file = "H1.csv",encoding = "UTF-8")
H1$subsidy_type <- gsub(pattern = "Plate-TY",replacement = "Plate", x = H1$subsidy_type)
test <- dplyr::filter(.data = H1)#, H1$Status!="", H1$subsidy_type=="Car",H1$convert2plate!="Converted")
test$Status <- gsub(pattern = "Active",replacement = 1, x = test$Status)
test$Status <- gsub(pattern = "Churn",replacement = 0, x = test$Status)
test$Status <- as.numeric(test$Status)
pca <- prcomp(formula = ~ 
              +Total_SH
              +Avg_TPH
              +Avg_NRH.w..NDI.
              +Total_Earning
              +Total_TRIP
              +NDI_received...
              +NDI_complete.,  
              data = test[c(10,11,12,13,14,15,16,27)]
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
biplot(pca, choices=1:2, col = my_cols[] ,pch = 19, cex = 0.5) 



my_cols <- c("#4f6f81","#ffd356","#4f6f81") 
pairs(test[c(12,13,14,15,16)], pch = 19, cex = 0.5 ,panel = panel.smooth, main = "All_H1Plate",col= my_cols[test$Status],lower.panel=NULL)






res.pca <- prcomp(test[,c(12,13,14,15,16)],  scale = TRUE)
fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = test$subsidy_type, col.ind = "white",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdBu"
)
+
  labs(fill = "Status", color = "Contrib", alpha = "Contrib") # Change legend title
#+
  #labs(fill = "Species") # Change legend title








