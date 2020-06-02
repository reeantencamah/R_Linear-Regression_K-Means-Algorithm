library(sparklyr)
library(dplyr)
library(ggplot2)

sc<-spark_connect(master="local")

#load dataset
lungCap<-read.csv("C:/Users/galaxy/Desktop/LungCapData.csv")
lungCap$Smoke<- ifelse(lungCap$Smoke == "yes", 1, 0)
lungCap$Caesarean<- ifelse(lungCap$Caesarean == "yes", 1, 0)
lungCap_tbl<-copy_to(sc,lungCap)
head(lungCap_tbl)
summary(lungCap_tbl)

#linear Reggression
#model1
lm_model1<-lungCap_tbl%>%select(LungCap,Age,Height,Smoke,Gender,Caesarean)%>% ml_linear_regression(LungCap~Age+Height+Smoke+Gender+Caesarean)
summary(lm_model1)
#prediction model1
partitions<-lungCap_tbl%>%sdf_partition(training=0.5,test=0.5)
preds<-sdf_predict(partitions$test,lm_model1)
preds<-data.frame(preds)

#plot Actual vs prediction
preds<-preds[order(preds$Height),]
v<-preds$LungCap
v1<-preds$Height
v2<-data.frame(v,v1)
w<-preds$prediction
w1<-data.frame(w,v1)

g=ggplot()+
  geom_line(data=v2,aes(x=v1,y=v),color="red")+
  geom_line(data=w1,aes(x=v1,y=w),color="blue")+xlab('Height')+ylab('LungCap')+ggtitle("Actual LungCap(Red) vs Predicted(Blue)")
g

 

#model2
lm_model2<-lungCap_tbl%>%select(LungCap,Age,Smoke)%>% ml_linear_regression(LungCap~Age+Smoke)
summary(lm_model2)

#plot graph LungCap vs Age,Smoke
plot(lungCap$Age[lungCap$Smoke=="0"],lungCap$LungCap[lungCap$Smoke=="0"],col="blue",xlim =c(1,20)
     ,ylim =c(0,15),xlab ="Age",ylab = "LungCap",main="LungCap vs Age,Smoke")
points(lungCap$Age[lungCap$Smoke=="1"],lungCap$LungCap[lungCap$Smoke=="1"],col="red",pch=16)
legend(1,16,legend = c("Non Smoker","Smoker"), col=c("blue","red"),pch = c(1,16),bty = "n")
abline(a=1.08,b=0.555,col="blue",lwd=2)
abline(a=0.431,b=0.555,col="red",lwd=2)

#prediction model2
preds1<-sdf_predict(partitions$test,lm_model2)
preds1<-data.frame(preds1)

# #plot Actual vs prediction
# preds1<-preds1[order(preds1$Age),]
# v<-preds1$LungCap
# v1<-preds1$Age
# v2<-data.frame(v,v1)
# w<-preds1$prediction
# w1<-data.frame(w,v1)
# 
# g=ggplot()+
#   geom_line(data=v2,aes(x=v1,y=v),color="red")+
#   geom_line(data=w1,aes(x=v1,y=w),color="blue")+xlab('Age')+ylab('LungCap')
# g  


# #logistic
# glm_model1<-lungCap_tbl%>% mutate(binary_reponse=(as.numeric(Caesarean=="no")))%>%ml_logistic_regression(binary_reponse~ LungCap+Smoke)
# summary(glm_model1)




#K means algorithm 
#model1
lungCap_tbl2<-copy_to(sc,lungCap)

kmeans_model<-lungCap_tbl2%>% ml_kmeans(formula= ~ LungCap+Age,k=2)
kmeans_model

predicted<-ml_predict(kmeans_model,lungCap_tbl2)%>% collect
            table(predicted$Gender,predicted$prediction)
            
 #plot actual
plot(lungCap[c("Age","LungCap")],col=lungCap$Gender)
            
# plot cluster graph
ml_predict(kmeans_model) %>%
collect() %>%
ggplot(aes(Age, LungCap)) +
geom_point(aes(Age, LungCap, col = factor(prediction + 1)),size = 2, alpha = 0.5) + 
              geom_point(data = kmeans_model$centers, aes(Age, LungCap),
                         col = scales::muted(c("red", "green")),pch = 'x', size = 12) +
              scale_color_discrete(name = "Predicted Cluster",labels = paste("Cluster", 1:2)) +
              labs(x = "Age",y = "LungCap",title = "K-Means Clustering") 

# 
# #model2
# kmeans_model2<-lungCap_tbl2%>% ml_kmeans(formula= ~ LungCap+Height,k=2)
# kmeans_model2
# 
# predicted<-ml_predict(kmeans_model2,lungCap_tbl2)%>% collect
# table(predicted$Gender,predicted$prediction)
# 
# 
# # plot cluster graph
# ml_predict(kmeans_model2) %>%
#   collect() %>%
#   ggplot(aes(Height, LungCap)) +
#   geom_point(aes(Height, LungCap, col = factor(prediction + 1)),size = 2, alpha = 0.5) + 
#   geom_point(data = kmeans_model2$centers, aes(Height, LungCap),
#              col = scales::muted(c("red", "green")),pch = 'x', size = 12) +
#   scale_color_discrete(name = "Predicted Cluster",labels = paste("Cluster", 1:2)) +
#   labs(x = "Height",y = "LungCap",title = "K-Means Clustering") 



# ab<-ml_predict(kmeans_model2)
# table(lungCap$Gender,ab$prediction)


#model 2
#create categories of Height
CatHeight<-cut(lungCap$Height,breaks =c(0,50,60,70,100),labels=c("A","B","C","D") )
lungCap$Height[1:10]
CatHeight[1:10]

#copy of dataset
lungData<-lungCap
lungData$Height<-CatHeight
lungData

#k-means
lungData2<-copy_to(sc,lungData)
kmeans_model3<-lungData2%>% ml_kmeans(formula= ~Age+Smoke+LungCap,k=4)
kmeans_model3

predicted<-ml_predict(kmeans_model3,lungData2)%>% collect
table(predicted$Height,predicted$prediction)


# plot cluster graph
ml_predict(kmeans_model3) %>%
  collect() %>%
  ggplot(aes(Age, LungCap)) +
  geom_point(aes(Age, LungCap, col = factor(prediction + 1)),size = 2, alpha = 0.5) +
  scale_color_discrete(name = "Predicted Cluster",labels = paste("Cluster", 1:4)) +
  labs(x = "Age",y = "LungCap",title = "K-Means Clustering Model 2") 
