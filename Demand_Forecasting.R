
library(tidyr)
library(dplyr)
library(caret)
library(e1071)
library(stringr)
library(lubridate)
library(tidyverse)
library(glmnet)
library(randomForest)
library(rpart)
library(gbm)
library(rattle)
library(rpart.plot)
library(h2o)
library(mice)
library(VIM)


##Read Csv File

W<-read.csv("D:/Temp/Data/SF_Bike_Sharing_Hourly.csv",stringsAsFactors = T)

names(W)
W$hour<-as.factor(W$hour)
W$Month<-as.factor(W$Month)

k<-W %>% dplyr::group_by_(names(W)[2]) %>% dplyr::summarise_(Min=interp(~min(var, na.rm = TRUE), var = as.name(names(W)[3])),
                                                                                  Max=interp(~max(var, na.rm = TRUE), var = as.name(names(W)[3])),
                                                                                  Mean=interp(~mean(var, na.rm = TRUE), var = as.name(names(W)[3])),
                                                                                  Median=interp(~median(var, na.rm = TRUE), var = as.name(names(W)[3])),
                                                                                  Sum=interp(~sum(var, na.rm = TRUE), var = as.name(names(W)[3])))
attach(W)
E<-crosstab(interp(~var, var = as.name(names(W)[2])),interp(~var, var = as.name(names(W)[13])),prop.r = T,prop.c = T,chisq = T,prop.t = T)


crosstab(names(W)[2],names(W)[13])

E$tab
E$prop.row
E$prop.tbl
E$CST

E$tab
E$prop.row
names(k)[2]<-paste("Min_",names(W)[3],sep="")

apply(W,2,function(x)sum(is.na(x)))



x<-as.data.frame(apply(W,2,function(x)(sum(is.na(x))/nrow(W))))
colnames(x)[1]<-"Missing_Value_percent"
x$variables<-rownames(x)
x<-x %>% filter(Missing_Value_percent>0)
x

View(x)







#Missing Value Visualization
p<-ggplot(x,aes(reorder(x=variables,Missing_Value_percent),y=Missing_Value_percent))+geom_col(fill="red") +coord_flip()+
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(vjust = 0),plot.title = element_text(hjust = 0.5))+
  labs(title = "Variable vs Missing Value",x="Variables")+scale_y_continuous(labels=scales::percent)

print(p)


mice_plot <- aggr(W, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(W), cex.axis=.7,cex.numbers=.7,
                  gap=3, ylab=c("Missing data","Pattern"))



W$Start.Date<-as.Date(W$Start.Date,format = "%m/%d/%Y")


W$day<-as.factor(weekdays(W$Start.Date))

W$workingday<-as.factor(ifelse(weekdays(W$Start.Date) %in% c("Sunday","Saturday"),"0","1"))



W$hour<-as.factor(W$hour)
W$Year<-as.factor(W$Year)
W$Month<-as.factor(W$Month)


W$Peakhour<-as.factor(ifelse(W$hour %in% c( 8,9,17,18) & W$workingday==1,"1","0"))


W$Temp.<-W$Temp. %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Pressure<-W$Pressure %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Dew.Point<-W$Dew.Point %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Visibility<-W$Visibility %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Wind.Speed<-W$Wind.Speed %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Humidity<-W$Humidity %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Wind.Dir<-W$Wind.Dir %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))
W$Conditions<-W$Conditions %>% str_replace_all(.,"[#NAÂN/A%-]","") %>% trimws(.,which = ("both"))


W$Temp.<-as.numeric(as.character(W$Temp.))
W$Pressure<-as.numeric(as.character(W$Pressure))
W$Dew.Point<-as.numeric(as.character(W$Dew.Point))
W$Visibility <-as.numeric(as.character(W$Visibility))
W$Humidity<-as.numeric(as.character(W$Humidity))
W$Wind.Dir<-as.factor(W$Wind.Dir)
W$Conditions<-as.factor(W$Conditions)



W<-na.omit(W)





W<-W %>% arrange(Start.Date)

W<-W[,-c(which(colnames(W) %in% c("Wind.Speed","Start.Date")))]




##BoxPlot


 # W$Demand<-W$counter

p<- ggplot(W,aes(hour,Demand))+geom_boxplot() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.background = element_rect(fill = "antiquewhite")) + theme(plot.title = element_text(hjust = 0.5)) +labs(title = "Demand vs Hour")
+labs(x = "Hour", y = "Demand")


print(p)
ggplotly(p)


str(W)


W$Mth<-factor(month.abb[W$Month],level=c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

W %>% group_by(Mth,Year) %>% summarise(Sum_demand=sum(Demand)) %>% ggplot(.,aes(Mth,Sum_demand,group=Year,color=Year))+geom_point()+geom_line() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.background = element_rect(fill = "antiquewhite"))+ theme(plot.title = element_text(hjust = 0.5)) +labs(title = "Demand vs Month")

W %>% group_by(workingday) %>% summarise(Demand=sum(counter)) %>% mutate(Proportion_demand=(Demand/sum(Demand))) %>% ggplot(.,aes(workingday,Proportion_demand))+geom_col(fill="#4271AE",color="#1F3552")+
  scale_y_continuous(labels = scales::percent)+labs(x="WorkingDay",title="Distribution of demand on days")+theme(plot.title=element_text(hjust=.5))

library(corrplot)


W_numeric


W<-W[,-c(which(colnames(W) %in% c("Wind.Speed","Start.Date")))]

W_numeric<-W[,sapply(W, is.numeric)]

x <- "Demand"
W_numeric<-W_numeric[c(x, setdiff(names(W_numeric), x))]


corrplot(cor(W_numeric),method="number",type = c("upper"))


##Linear Regression

set.seed(123)

smp_size <- floor(0.75 * nrow(W))
# train_ind <- sample(seq_len(nrow(W)), size = smp_size,replace = F)



train <- W[1:11844, ]
test <- W[!row.names(W) %in% row.names(train), ]



##Linear Regression

Weather.lm<-lm(counter~.,data=train)

summary(Weather.lm)

k<-predict(Weather.lm,test)

RMSE(test$counter,k)

MAE(test$counter,k)







##Penalized Linear Regression

dummy_train<-dummyVars(" ~ .",data=train)
dummy_train<-data.frame(predict(dummy_train,newdata=train))

dummy_test<-dummyVars(" ~ .",data=test)
dummy_test<-data.frame(predict(dummy_test,newdata=test))

colnames(dummy_train)
colnames(dummy_test)

cv.lasso <- cv.glmnet(as.matrix(dummy_train[,-c(25)]), dummy_train$counter, family='poisson', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mse')
plot(cv.lasso)

lasso_final<-glmnet(as.matrix(dummy_train[,-c(25)]), dummy_train$counter, alpha=1,lambda = cv.lasso$lambda.min , standardize=T)


summary(lasso_final)


results <- predict(cv.lasso, as.matrix(dummy_test[,-c(25)]), type="response")

RMSE(test$counter,results)

MAE(test$counter,results)




A<-as.data.frame(as.matrix(lasso_final$beta))
colnames(A)<-"Beta"
A$Variables<-rownames(A)

A<-A %>% filter(Beta!=0) %>% arrange(desc(abs(Beta))) %>% slice(1:10)

ggplot(A,mapping=aes(reorder(x=Variables,Beta),y=Beta))+geom_col(fill="blue")+coord_flip() + theme(plot.subtitle = element_text(hjust = .5), 
    plot.caption = element_text(hjust = .5)) +labs(title = "Variable Importance Lasso",x="Variables") + 
    theme(panel.background = element_rect(fill = "cornsilk")) 







##Decision trees

fit <- rpart(counter ~.,
             method="anova", data=train)


s<-predict(fit,test)


RMSE(test$counter,s)

MAE(test$counter,s)


dummy_train

##RandomForest-Regression

rf1<-randomForest(counter~.,data=train,proximity=TRUE,importance=T)


Rf_results<-predict(rf1,test,type="response")

RMSE(Rf_results,test$counter)

MAE(test$counter,Rf_results)


##Random FOrest Dummy Train

rf12<-randomForest(counter~.,data=dummy_train,proximity=TRUE,importance=T)


Rf_results<-predict(rf12,dummy_test,type="response")

RMSE(Rf_results,test$counter)

MAE(test$counter,Rf_results)



##Predict


a<-as.data.frame(rf12$importance)

a$Variables<-rownames(a)


a<-a %>% arrange(desc(IncNodePurity)) %>% group_by(Variables) %>% head(n=10)

a$Variables<-factor(a$Variables,levels=a$Variables[order(a$IncNodePurity)])


ggplot(a,aes(Variables,IncNodePurity))+geom_col(fill="red")+coord_flip()+
  ggtitle("Variable Importance Random forest")+ theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = scales::comma)+theme(panel.background = element_rect(fill = "cornsilk"))






##Support Vector Regressor

a<-svm(counter~.,data=train,type="nu-regression",kernel="radial")
RMSE(predict(a,test),test$counter)

MAE(test$counter,predict(a,test))


a<-svm(counter~.,data=train,type="nu-regression",kernel="linear")
RMSE(predict(a,test),test$counter)

MAE(test$counter,predict(a,test))



##Gradient Boosting Regressor

booster<-gbm(counter ~ . ,data = train,distribution = "gaussian",n.trees = 500,
                 shrinkage = 0.01, interaction.depth = 4)
q<-predict(booster,test,n.trees = 500)
RMSE(q,test$counter)
MAE(q,test$counter)





str(W)

#Tuning

ggplot(W,aes(Temp.,counter))+geom_point()+geom_smooth(method = "lm",aes(color="Linear"),label="lm")+
  geom_smooth(method = "loess",aes(color="GAM"),label="loess") +
  scale_colour_manual(name="Legend", values=c("blue", "red"))+
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1),
    panel.background = element_rect(fill = "bisque"))+labs(title="Temperature vs Demand",y="Demand") + theme(plot.title = element_text(hjust = 0.5))
 + theme(panel.background = element_rect(fill = "antiquewhite1"), 
    plot.background = element_rect(fill = "gray40"))

##Segmentation

install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(rattle)

q<-rpart(counter~Temp.,data=W)
q
prp(q)
fancyRpartPlot(q)

W$Temp_bucket<-ifelse(W$Temp.>61,"Bucket_High_Temp",ifelse(W$Temp.<61 & W$Temp.>54,"Bucket_Medium_Temp","Bucket_Low_Temp"))
t<-W[,-c(which(colnames(W) %in% ("Temp.")))]


set.seed(123)

smp_size <- floor(0.75 * nrow(t))
# train_ind <- sample(seq_len(nrow(t)), size = smp_size,replace = F)


train <- t[train_ind, ]
test <- t[-train_ind, ]


##Linear Regression

Weather.lm<-lm(counter~.,data=train)

summary(Weather.lm)

k<-predict(Weather.lm,test)

RMSE(test$counter,k)




##Lasso Regression







#Tuning Cart





##Tuning Randomforest

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(counter~., data=train, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)




##h2O Tuning



# grid_space <- list()
# 
# grid_space$mtries <- c(100,200,500)
# 
# grid_space$max_depth <- c(1:8)
# 
# grid_space$ntrees <- c(2:8)

h2o.init(nthreads=-1, max_mem_size = "10G")
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
colnames(train.h2o)

y.dep<-2

x.indep<-c(1,3:13)


h2o.random <- h2o.grid("randomForest", x = x.indep, y = y.dep, training_frame = train.h2o,
                       hyper_params = list(ntrees = c(1,2,5,10,50,100,250,500,750,1000), seed = 1122))



# 
# h2o.random <- h2o.grid("randomForest", x = x.indep, y = y.dep, training_frame = train.h2o,
#                        hyper_params = list(ntrees = c(500),mtries = c(4,5,6,7),  seed = 1122))



# 
# 
# 
# h2o.random <- h2o.grid("randomForest", x = x.indep, y = y.dep, training_frame = train.h2o,
#                        hyper_params = list(ntrees = c(500),mtries = c(4,5,6,7),max_depth = c(10:20)),  seed = 1122)
# 

a<-as.data.frame(h2o.random@summary_table)


a$ntrees<-factor(a$ntrees,levels=c("1","2","5","10","50","100","250","500","750","1000"))
a$residual_deviance<-as.numeric(a$residual_deviance)

ggplot(a,aes(ntrees,residual_deviance,group=1))+geom_point()+geom_line()+theme(panel.background = element_rect(fill = "antiquewhite1"))+
  labs(title="Residual_Deviance vs ntrees")+theme(plot.title = element_text(hjust = 0.5))


write.csv(a,"D:/Temp/Output/trees.csv")



View(h2o.random)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 500, mtries = 4,max_depth=17,seed = 1122)
predict.regression <- as.data.frame(h2o.predict(rforest.model, test.h2o))
RMSE(predict.regression,test$counter)


h2o.shutdown(prompt = TRUE)




h2o.init(nthreads=-1, max_mem_size = "10G")
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)


y.dep<-2

x.indep<-c(1,3:13)









## Tuning GBM



