
library(lubridate)
library(Hmisc)
library(randomForest)
library(caret)
library(e1071)


a<-data.frame(seq(
  from=as.POSIXct("2015-09-01 0:00", tz="UTC"),
  to=as.POSIXct("2016-08-31 23:00", tz="UTC"),
  by="hour"
))

b<-data.frame(replicate(nrow(a),"2nd at Folsom"))



colnames(a)[1]<-"Date"

colnames(b)[1]<-"Station_Name"

c<-cbind(a,b)


t<-c("2nd at South Park",
     "2nd at Townsend",
     "5th at Howard",
     "5th St at Folsom St",
     "Beale at Market",
     "Broadway St at Battery St",
     "Civic Center BART (7th at Market)",
     "Clay at Battery",
     "Commercial at Montgomery",
     "Cyril Magnin St at Ellis St",
     "Davis at Jackson",
     "Embarcadero at Bryant",
     "Embarcadero at Folsom",
     "Embarcadero at Sansome",
     "Embarcadero at Vallejo",
     "Golden Gate at Polk",
     "Grant Avenue at Columbus Avenue",
     "Harry Bridges Plaza (Ferry Building)",
     "Howard at 2nd",
     "Market at 10th",
     "Market at 4th",
     "Market at Sansome",
     "Mechanics Plaza (Market at Battery)",
     "Powell at Post (Union Square)",
     "Powell Street BART",
     "San Francisco Caltrain (Townsend at 4th)",
     "San Francisco Caltrain 2 (330 Townsend)",
     "San Francisco City Hall",
     "South Van Ness at Market",
     "Spear at Folsom",
     "Steuart at Market",
     "Temporary Transbay Terminal (Howard at Beale)",
     "Townsend at 7th",
     "Yerba Buena Center of the Arts (3rd @ Howard)"
)

for (i in 1:length(t)){
  
  b<-data.frame(replicate(nrow(a),t[i]))
  
  a<-data.frame(seq(
    from=as.POSIXct("2015-09-01 0:00", tz="UTC"),
    to=as.POSIXct("2016-08-31 23:00", tz="UTC"),
    by="hour"
  ))
  
  colnames(a)[1]<-"Date"
  
  colnames(b)[1]<-"Station_Name"
  
  d<-cbind(a,b)
  
  c<-rbind(c,d)
  
}



c$hour<-hour(c$Date)
c$Date<-as.Date(c$Date,format = "%m/%d/%Y")

#write.csv(c,"D:/Temp/timerseries.csv")





simulate<-read.csv("D:/Temp/timerseries.csv")


c<-as.character(unique(simulate$Station_Name))

for (i in 1:length(c)){
  
t<-simulate %>% filter(Station_Name==c[i]) 



t$Date<-strptime(t$Date,format = "%m/%d/%Y")


str(t)



t$day<-factor(weekdays(t$Date,T),levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
t$workingday<-as.factor(ifelse(weekdays(t$Date) %in% c("Sunday","Saturday"),"0","1"))
t$peakhour<-as.factor(ifelse(((t$hour) %in% c(8,9,17,18) & t$day %in% c("Mon","Tue","Wed","Thu","Fri")),"1","0"))

t$Date<-as.POSIXct(t$Date,format = "%m/%d/%Y")


t$yt1 <- Lag(t$Count,168)
t$yt2 <- Lag(t$Count,336)
t$yt3 <- Lag(t$Count,504)
t$yt4 <- Lag(t$Count,672)



#write.csv(t,"D:/Temp/station_data.csv")


train<-t %>% filter(Date>="2015-09-01" & Date<"2016-04-01")
test<-t %>% filter(Date>="2016-04-01")



train<-na.omit(train)
train<-train[,-c(which(colnames(train) %in% c("Date","Station_Name","Season")))]
train$day<-as.factor(train$day)
train$hour<-as.factor(train$hour)


test<-na.omit(test)
test<-test[,-c(which(colnames(test) %in% c("Date","Station_Name","Season")))]
test$day<-as.factor(test$day)
test$hour<-as.factor(test$hour)





rf1<-randomForest(Count~.,data=train,proximity=TRUE,importance=T)
s<-predict(rf1,test,type="response")
s<-round(s)
Error<-RMSE(s,test$Count)

if (i==1){
  
  j<-c[i]
  final_error<-cbind(Error,j)
}


else{
  j<-c[i]
  z<-cbind(Error,j)
  final_error<-rbind(final_error,z)
}
}



##Time Series



