library(readxl)
# 读取所有子表格
all_sheets <- lapply(excel_sheets("DPSIR.xlsx"), function(sheet) {
  read_excel("DPSIR.xlsx", sheet = sheet)
})

# 给每个子表格赋予名称
names(all_sheets) <- excel_sheets("DPSIR.xlsx")

D = all_sheets$`D-驱动力`
a = D[D$产业=="工业",]
b = D[D$产业=="原材料",]
c = D[D$产业=="公用事业",]
d = D[D$产业=="能源",]
which.max(d$`企业市值（元）`)#d 333 ;a 5500; b 403; c 442

#####################################
DPSIR = cbind(all_sheets$`D-驱动力`,all_sheets$`P-压力`[,5:10],
              all_sheets$`S-状态`[,5:9],all_sheets$`I-影响`[,5:6],
              all_sheets$`R-响应`[,5:7])
m = 1825 #样本
n = 19 #指标
p = 5 #年数
df = DPSIR[,5:23]
#极差法标准化
df_normalized = as.data.frame(lapply(df, function(x) (x - min(x)) / (max(x) - min(x))))
#信息熵
entropy_values = lapply(df_normalized, function(x) {
  p_ij <- x / sum(x)
  p_ij <- p_ij[p_ij > 0] # 移除概率为0的元素
  -sum(p_ij * log2(p_ij))
})
#权重
weights <- lapply(entropy_values, function(x) {
  (1 - x) / sum(1 - unlist(entropy_values))
})
# 将列表转换为数值向量
weights <- as.numeric(unlist(weights))
# 计算综合评分
esg_scores <- as.matrix(df_normalized) %*% weights
# 将综合评分添加到数据框中
DPSIR_final <- cbind(DPSIR, ESG_Score = esg_scores)


#可视化
time_points <- c("2017","2018","2019","2020", "2021") # 时间点
esg_scores1 <- esg_scores[8576:8580,] #中国石油
esg_scores2 <- esg_scores[8531:8535,] # 中国建筑
esg_scores3 <- esg_scores[1361:1365,] # "福莱新材"
esg_scores4 <- esg_scores[6471:6475,] #"通源环境"

# 将评分和时间点组合成数据框
df_scores1 <- data.frame(Time = time_points, ESG_Score = esg_scores1, Dataset = "能源")
df_scores2 <- data.frame(Time = time_points, ESG_Score = esg_scores2, Dataset = "工业")
df_scores3 <- data.frame(Time = time_points, ESG_Score = esg_scores3, Dataset = "原材料")
df_scores4 <- data.frame(Time = time_points, ESG_Score = esg_scores4, Dataset = "公用事业")

# 将数据框合并为一个大的数据框
df_scores <- rbind(df_scores1, df_scores2, df_scores3,df_scores4)

library(ggplot2)
# 绘制折线图
ggplot(df_scores, aes(x = Time, y = ESG_Score, color = Dataset)) +
  geom_line(aes(group = Dataset)) + # 按数据集分组绘制线条
  geom_point() +
  labs(x = "时间", y = "ESG评分", title = "不同产业的ESG评分随时间的变化趋势") + # 设置坐标轴标签和标题
  theme_minimal()  # 使用简洁的主题
ggsave("ESG_score.png", width = 7, height = 4, units = "in",bg="white")

######################################
library(readxl)
control = read_xlsx("Control.xlsx")
pbESG = read_xls("彭博ESG.xls")
# 能源
power = control[control$Industry=="B",c(3,5,6,7,9,10,13,14,17,37)]#多证券代码 27,28,37
power = na.omit(power)
names(power)[names(power) == "证券代码"] <- "code"
power_esg = pbESG[grepl("B",pbESG$"行业代码"),]
names(power_esg)[names(power_esg) == "year"] <- "Year"
power_esg = power_esg[,1:3]
power_esg$Year = as.numeric(power_esg$Year)
#合并control和pbesg
library(dplyr)
# 使用inner_join函数找出两个dataframe中完全相同的行
common_rows <- power %>% 
  inner_join(power_esg, by = c("code", "Year"))
#合并control和esg
names(DPSIR_final)[names(DPSIR_final) == "证券代码"] <- "code"
names(DPSIR_final)[names(DPSIR_final) == "年份"] <- "Year"
common_rows2 = power %>% inner_join(DPSIR_final[,c(1,2,24)],by = c("code","Year"))

#变量
library(corrplot)
corrplot(cor(common_rows[,-1]))

#回归
fit1 = lm(ROA~.,data = common_rows[,c(-1)])
par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(fit1)

library(car)
vif_values = vif(fit1)

#固定效应
library(plm)
fit2 = plm(ROA~.,data = common_rows,index=c("code","Year"))
fit3 = plm(sqrt(ROA)~.,data = power_boxcox,index=c("code","Year"))

rows2020 = common_rows2[common_rows2$Year==2020,]
fit3 = lm(ROA~.,rows2020[,c(-1,-12,-10)])

#boxcox
library(MASS)
power_boxcox = common_rows[common_rows$ROA>0,]
power_boxcox2 = common_rows2[common_rows2$ROA>0,]
fit4 = boxcox(ROA~.,data=power_boxcox[,c(-1)])#找lambda
fit5 = lm(sqrt(ROA)~.,data = power_boxcox[,c(-1)])
fit6 = lm(sqrt(ROA)~.,data = power_boxcox[c(-107,-60,-61,-9),c(-1)])
fit7 = lm(ROA^(1/2)~.,data = power_boxcox2[,c(-1)])

fit = lm(ROA~.,data = common_rows[,c(-1,-2,-9)])

#存入xlsx
library(xlsx)
write.xlsx(vif_values,"output.xlsx")
write.xlsx(summary(fit7)$coefficients,"output.xlsx")

# 选择的最佳模型
library(leaps)
leapSet = leaps(x=common_rows[,c(-1,-4)],y=common_rows$ROA,nbest = 3,method = "adjr2")
leapSet$which[which.max(leapSet$adjr2),]
leapSet2 = leaps(x=common_rows[,c(-1,-4)],y=common_rows$ROA,nbest = 3,method = "Cp")
leapSet2$which[which.min(leapSet2$Cp),]

leapSet3 = regsubsets(x=common_rows[,c(-1,-4)],y=common_rows$ROA,nbest = 3)

# 公用
public = control[control$Industry=="N",c(3,5,6,7,9,10,13,14,17,37)]#多证券代码 27,28,37
public = na.omit(public)
names(public)[names(public) == "证券代码"] <- "code"
common_rows3 = public %>% inner_join(DPSIR_final[,c(1,2,24)],by = c("code","Year"))
fitp = lm(ROA~.,data = common_rows3[,c(-1)])

#原材料
public = control[control$Industry=="C",c(3,5,6,7,9,10,13,14,17,37)]#多证券代码 27,28,37
public = na.omit(public)
names(public)[names(public) == "证券代码"] <- "code"
common_rows3 = public %>% inner_join(DPSIR_final[,c(1,2,24)],by = c("code","Year"))
fitp = lm(ROA~.,data = common_rows3[,c(-1)])



#可视化
time_points <- c("2017","2018","2019","2020", "2021") # 时间点
esg_scores1 <- esg_scores[951:955,]
esg_scores2 <- esg_scores[971:975,] # 中国建筑
esg_scores3 <- esg_scores[1116:1120,] # "福莱新材"
esg_scores4 <- esg_scores[8786:8790,] 
esg_scores5 <- esg_scores[8656:8660,] 
esg_scores6 <- esg_scores[8571:8575,] 

# 将评分和时间点组合成数据框
df_scores1 <- data.frame(Time = time_points, ESG_Score = esg_scores1, Dataset = "电光科技")
df_scores2 <- data.frame(Time = time_points, ESG_Score = esg_scores2, Dataset = "电投能源")
df_scores3 <- data.frame(Time = time_points, ESG_Score = esg_scores3, Dataset = "东华能源")
df_scores4 <- data.frame(Time = time_points, ESG_Score = esg_scores4, Dataset = "中曼石油")
df_scores5 <- data.frame(Time = time_points, ESG_Score = esg_scores5, Dataset = "中海油服")
df_scores6 <- data.frame(Time = time_points, ESG_Score = esg_scores6, Dataset = "中国石化")

# 将数据框合并为一个大的数据框
df_scores <- rbind(df_scores1, df_scores2, df_scores3,df_scores4,df_scores5,df_scores6)

library(ggplot2)
# 绘制折线图
ggplot(df_scores, aes(x = Time, y = ESG_Score, color = Dataset)) +
  geom_line(aes(group = Dataset)) + # 按数据集分组绘制线条
  geom_point() +
  labs(x = "时间", y = "ESG评分", title = "不同能源类企业的ESG评分随时间的变化趋势") + # 设置坐标轴标签和标题
  theme_minimal()  # 使用简洁的主题
ggsave("nengyuan.png", width = 7, height = 4, units = "in",bg="white")

#######################################
predict_power = DPSIR_final[DPSIR_final$产业=="能源",c("Year","名称","ESG_Score")]
# 加载tidyr包
library(tidyr)

# 使用pivot_wider函数转换数据框
df_wide <- predict_power %>%
  pivot_wider(
    names_from = Year, # 使用time列作为新列的名字
    values_from = ESG_Score # 使用score列作为新列的值
  )
time = 2017:2021
df_wide$"2022" = NA
for(i in 1:72){
  model = lm(as.numeric(df_wide[i,c(-1,-7)])~time)
  new_data <- data.frame(time = c(2022))
  predicted_scores <- predict(model, newdata = new_data)
  df_wide$"2022"[i] = predicted_scores
}


# 使用pivot_longer函数逆转转换
df_original <- df_wide %>%
  pivot_longer(
    cols = -"名称",          # 除了id列以外的所有列
    names_to = "Year",   # 新的时间列名
    values_to = "ESG_Score"  # 新的分数列名
  )
colnames(df_original) = c("name","Year","ESG_Score")
ggplot(df_original[1:60,], aes(x = Year, y = ESG_Score, color = name)) +
  geom_line(aes(group = name)) + # 按数据集分组绘制线条
  geom_point() +
  labs(x = "时间", y = "ESG评分", title = "不同能源类企业的ESG评分2022年预测") + # 设置坐标轴标签和标题
  theme_minimal()  # 使用简洁的主题
ggsave("predict.png", width = 7, height = 4, units = "in",bg="white")

