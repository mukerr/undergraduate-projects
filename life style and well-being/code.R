data = read.csv("Wellbeing_and_lifestyle_data_Kaggle.csv",header = T)
data = data[,-1]
data$DAILY_STRESS = as.integer(data$DAILY_STRESS)
data = data[-10006,]#含有NA
age_dict <- list('Less than 20' = 1, '21 to 35' = 2, '36 to 50' = 3, '51 or more' = 4)
data$AGE <- as.integer(factor(data$AGE, levels = names(age_dict), labels = age_dict))
gender_dict <- list('Female' = 1, 'Male' = 0)
data$GENDER <- as.integer(factor(data$GENDER, levels = names(gender_dict), labels = gender_dict))

data$class2 = ifelse(data$WORK_LIFE_BALANCE_SCORE<667,0,1)
data$class2 = as.factor(data$class2)
data$score = as.factor(data$WORK_LIFE_BALANCE_SCORE)

###EDA
library(ggplot2)
library(reshape2)
melted_data = melt(data[,-23])
ggplot(melted_data,aes(x =variable, y = value, color = variable)) +
  geom_boxplot() +
  labs(x = "Variables", y = "Values") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("boxplot.png", plot = last_plot(), width = 8, height = 4, units = "in", dpi = 300)

library(ggcorrplot)
ggcorrplot(cor(data)) + 
  theme_minimal() +  
  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1),
        axis.text.y = element_text(size = 6)) +
  labs(x = "",y = "")
ggsave("corrplot.png", plot = last_plot(), width = 6, height = 6, units = "in", dpi = 300, bg = "white")

#不同年龄score
data$AGE = as.factor(data$AGE)
ggplot(data, aes(x = AGE, fill = class2)) +
  geom_bar(position = "dodge") +
  labs(x = "Age",
       y = "Count",
       fill = "Category") +
  theme_minimal()
ggsave("age-category.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

ggplot(data, aes(x = WORK_LIFE_BALANCE_SCORE, col = AGE,fill=AGE)) +
  geom_density(alpha=0.3) +
  theme_minimal() +
  labs(
    x = "work_life_balance_score",
    y = "density"
  )
ggsave("age-score.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

#不同性别score分布
data$GENDER = as.factor(data$GENDER)
ggplot(data, aes(x = WORK_LIFE_BALANCE_SCORE, col = GENDER,fill=GENDER)) +
  geom_density(alpha=0.3) +
  theme_minimal() +
  labs(
    x = "work_life_balance_score",
    y = "density"
  )
ggsave("gender-score.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

###PCA
lifedata = data[,c(1,3,4,5,10,12,14,16,20)]
lifedata = scale(lifedata)
welldata = data[,c(2,7,19)]
welldata = scale(welldata)

library(stats)
lifepca = princomp(lifedata,scores = T,cor = T)

evals<-data.frame(lifepca$sdev^2)
names(evals)<-"eigen.vals"
evals$component.num<-as.integer(seq(nrow(evals)))
ggplot(evals,aes(x=component.num,y=eigen.vals))+geom_point()

pca_scores = lifepca$scores[, 1:5]

wellpca = princomp(welldata)
well_pca_scores = wellpca$scores[,1:2]

###FA
fit1 <- factanal(lifedata,factors=5,rotation="varimax")


###Clustering
dat = as.data.frame(pca_scores)
dat$comp1 = well_pca_scores[,1]
dat$comp2 = well_pca_scores[,2]
dat$class = as.factor(data$class2)

##k-means
library(factoextra)
fviz_nbclust(welldata, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) + theme(plot.title = element_text(hjust = 0.5))
res4<-kmeans(welldata,4)
dat$cluster4 = as.factor(res4$cluster)
ggplot(dat, aes(x = comp1, y = comp2, color = as.factor(cluster4))) +
  geom_point() +
  labs(title = "4 Clusters based on First 2 Principal Components",
       x = "PC1",
       y = "PC2",
       color = "Cluster4") +
  theme_minimal()
ggsave("cluster4.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

#3 Clusters
res3 = kmeans(welldata,3)
dat$cluster3 = as.factor(res3$cluster)

ggplot(dat, aes(x = comp1, y = comp2, color = as.factor(cluster3))) +
  geom_point() +
  labs(title = "3 Clusters based on First 2 Principal Components",
       x = "PC1",
       y = "PC2",
       color = "Cluster3") +
  theme_minimal()
ggsave("cluster3.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

#2 clusters
res2 = kmeans(welldata,2)
dat$cluster2 = as.factor(res2$cluster)

ggplot(dat, aes(x = comp1, y = comp2, color = as.factor(cluster2))) +
  geom_point() +
  labs(title = "2 Clusters based on First 2 Principal Components",
       x = "PC1",
       y = "PC2",
       color = "Cluster2") +
  theme_minimal()
ggsave("cluster2.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

#和score对比
ggplot(dat, aes(x = comp1, y = comp2, color = class)) +
  geom_point() +
  labs(title = "Score Class based on First 2 Principal Components",
       x = "PC1",
       y = "PC2",
       color = "Class") +
  theme_minimal()
ggsave("score.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 300, bg = "white")

mds=cmdscale(dist(welldata[1:600,],method="euclidean"))
par(mfrow=c(1,2))
plot(mds,col=dat$cluster2,main='kmeans k=2',pch=18)
plot(mds,col=dat$class,main='Original clusters',pch=18)


###DA
#lda
library(MASS)
lifedata = as.data.frame(lifedata)
lifedata$cluster2 = as.factor(dat$cluster2)
lifedata$cluster3 = as.factor(dat$cluster3)
L = lda(cluster3~.,data=lifedata)
yhat = predict(L, lifedata)$class
tab = table(pred=yhat, true=lifedata$cluster3)
dat$lda = as.factor(yhat)
ggplot(dat[1:500,],aes(x=Comp.1,y=Comp.2,col=lda,shape=cluster3)) + geom_point()
ggsave("lda3.png", plot = last_plot(), width = 7, height = 4, units = "in", dpi = 300, bg = "white")

L2 = lda(cluster2~.,data=lifedata)
yhat2 = predict(L2, lifedata)$class
tab2 = table(pred=yhat2, true=lifedata$cluster2)
dat$lda2 = as.factor(yhat2)
ggplot(dat[1:500,],aes(x=Comp.1,y=Comp.2,col=lda2,shape=cluster2)) + geom_point()
ggsave("lda2.png", plot = last_plot(), width = 7, height = 4, units = "in", dpi = 300, bg = "white")

#CV LDA
train = lifedata[1:11180,]
test = lifedata[11181:15971,]
L3 = lda(cluster2~.,data=train)
yhat3 = predict(L3, test)$class
yhat4 = predict(L3,train)$class
tab4 = table(pred = yhat4,true = train$cluster2)
tab3 = table(pred=yhat3, true=test$cluster2)

#qda
lifedata$cluster3 = dat$cluster3
Q = qda(cluster3~.,data=lifedata)
yhatq = predict(Q, lifedata)$class
tabq = table(pred=yhatq, true=lifedata$cluster3)
dat$qda = as.factor(yhatq)
ggplot(dat[1:500,],aes(x=Comp.1,y=Comp.2,col=qda,shape=cluster3)) + geom_point()
ggsave("qda3.png", plot = last_plot(), width = 7, height = 4, units = "in", dpi = 300, bg = "white")

