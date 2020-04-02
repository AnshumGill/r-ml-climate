library(lubridate)
library(ggplot2)

data<-read.csv(file="GlobalLandTemperaturesByCity.csv",header = TRUE)
data<-data[!(is.na(data$AverageTemperature) | data$AverageTemperature==""),]
data$dt<-as.Date(data$dt)
data$Year<-year(data$dt)
data$Month<-month(data$dt)

dummy<-data
dummy$Latitude<-as.character(dummy$Latitude)
dummy$Latitude<-substr(dummy$Latitude,1,nchar(dummy$Latitude)-1)
dummy$Longitude<-as.character(dummy$Longitude)
dummy$Longitude<-substr(dummy$Longitude,1,nchar(dummy$Longitude)-1)

dummy$dt<-NULL
dummy$AverageTemperatureUncertainty<-NULL
dummy$City<-NULL

dummy$Latitude<-as.double(dummy$Latitude)
dummy$Longitude<-as.double(dummy$Longitude)

dummy<-dummy[(dummy$Year >= 1950),]
dummy<-dummy[(dummy$Latitude <= 15.0 | dummy$Latitude >=60.0),]

#ggplot(dummy,aes(x=AverageTemperature,y=Country))+geom_point()

set.seed(123)
split<-sample(seq_len(nrow(dummy)),size=floor(0.75*nrow(dummy)))
trainData<-dummy[split,]
testData<-dummy[-split,]

predictionModel<-lm(AverageTemperature ~.,data=trainData)

summary(predictionModel)

df<-data.frame("AverageTemperature"="","Country"="Nigeria","Latitude"=5.63,"Longitude"=8.07,"Year"=2013,"Month"=8)

indiaData_old<-dummy %>%
                  filter(Country=="India")

indiaData<-dummy %>%
              filter(Country=="India") %>%
              mutate(Year=Year+1000)
indiaData<-indiaData %>%
              mutate(AverageTemperature=indiaPredict)

indiaPredict<-predict(predictionModel,newdata=indiaData)
head(indiaPredict)
prediction<-predict(predictionModel,newdata = testData)
# testing<-data.frame("AverageTemperature"="","Country"="India","Latitude"=13.66,"Longitude"=78.44,"Year"=3000,"Month"=6)
# pred<-predict(predictionModel,newdata=testing)
head(prediction)

indiaData$AverageTemperature_old<-indiaData_old$AverageTemperature


#ggplot(dummy,aes(x=Year,y=AverageTemperature,color=Country))+geom_point()+facet_wrap(~Country)

dummy %>%
  group_by(Year) %>%
  summarise(AverageTemperature=mean(AverageTemperature)) %>%
  ggplot(aes(x=Year,y=AverageTemperature,color=AverageTemperature))+ geom_point()+scale_color_gradient(low="blue",high="red")

dummy %>%
  filter(Latitude>=60.0)%>%
  group_by(Year) %>%
  summarise(AverageTemperature=mean(AverageTemperature)) %>%
  ggplot(aes(x=Year,y=AverageTemperature,color=AverageTemperature))+ geom_point()+scale_color_gradient(low="blue",high="red")

indiaData %>%
  group_by(Year) %>%
  summarise(AverageTemperature=mean(AverageTemperature))%>%
  ggplot(aes(Year,AverageTemperature,color=AverageTemperature))+geom_line(size=4)+geom_smooth(method="lm",color="yellow")+scale_color_gradient(low="blue4",high="red2")

indiaData_old %>%
  group_by(Year) %>%
  summarise(AverageTemperature=mean(AverageTemperature)) %>%
  ggplot(aes(Year,AverageTemperature,color=AverageTemperature))+geom_point()+geom_smooth()+scale_color_gradient(low="blue4",high="red2")