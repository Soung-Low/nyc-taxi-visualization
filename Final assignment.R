setwd("C:/Users/lenovo/Dropbox/R language/Final Assignment")

taxi <- read.table("sample.csv", header =TRUE, sep=",")
humid <- read.table("humidity.txt", header=FALSE, sep=",")
prec <- read.table("precipitation.txt", header=FALSE, sep=",")
temp<- read.table("temperature.txt",header =FALSE, sep=",")

library(dplyr)
humid <- as.data.frame(t(humid))
prec <- as.data.frame(t(prec))
temp <- as.data.frame(t(temp))

date <- seq(from =as.Date("2015/06/01"), to =as.Date("2015/06/30"), by =1)
humid<- mutate(humid, date)
prec <- mutate(prec, date)
temp <- mutate(temp,date)

colnames(humid) <- c("humidity","date")
colnames(temp) <- c("temp","date")
colnames(prec) <- c("prec","date")
taxi <- mutate(taxi, date= as.Date(substring(taxi$tpep_pickup_datetime,1,10)))

table1 <- inner_join(taxi,humid,by="date")
table2 <- inner_join(table1,prec,by="date")
table3 <- inner_join(table2,temp,by="date")  

#q1:pick_up_map
library(ggmap)
nycmap10 = readRDS("nycmap10.rds")
map1 <- ggmap(nycmap10) + geom_point(data=table3,aes(x=pickup_longitude,y=pickup_latitude),alpha=0.5,color='red')

#q1:pick_up_date & total_passenger
tp1 <-group_by(table3,date) %>%
          summarize(totalp= sum(passenger_count)) %>%
          mutate(day=weekdays(as.POSIXct(date)))
g1 <- ggplot(tp1,aes(x=date,y=totalp,fill=day))
chart1 <- g1 + geom_bar(stat = "identity") + coord_cartesian(ylim=c(5500,8500)) + ggtitle("Total passengers in June 2015")

#q1:pick_up_time & passenger_count
table3 <- mutate(table3,pickuptime= substring(table3$tpep_pickup_datetime,12,13))
tp2 <- group_by(table3,pickuptime) %>% summarize(totalp=sum(passenger_count))
g2 <- ggplot(tp2,aes(x=pickuptime))
chart2 <- g2 + geom_bar(aes(y=tp2$totalp),stat="identity") + ggtitle("Total passengers in each hour") 
+ ylab("Total passenger count") + xlab("Pickup Time") + coord_cartesian(ylim=c(2000,13000))

#q2:humidity & passenger_count
humid1 <- mutate(humid,tp=tp1$totalp,day=weekdays(as.POSIXct(date)))
g3 <- ggplot(humid1,aes(x=humidity,y=tp))
chart3 <- g3+ geom_point(aes(color=day))

#q2: precipitation & passenger_count
prec1 <- mutate(prec,tp= tp1$totalp,day=weekdays(as.POSIXct(date)))
g4 <- ggplot(prec1,aes(x=prec,y=tp))
chart4 <- g4+ geom_point(aes(color=day))

#q2:temperature & passenger_count
temp1 <- mutate(temp, tp=tp1$totalp,day=weekdays(as.POSIXct(date)))
g5 <- ggplot(temp1,aes(x=temp,y=tp))
chart5 <- g5+ geom_point(aes(color=day))

#q3:ratecode & tips
tip1 <- select(table3,RateCodeID,tip_amount)
tip2 <- group_by(tip1,RateCodeID) %>% summarize(averagetip= sum(tip_amount)/sum(RateCodeID==RateCodeID)) %>% filter(RateCodeID!=99)
g6 <- ggplot(tip2, aes(x=RateCodeID,y=averagetip))
chart6 <- g6 + geom_bar(stat="identity",fill=tip2$RateCodeID)

#q3: days & tips
table3 <- mutate(table3,day=weekdays(as.POSIXct(date)))
tip3 <- group_by(table3,day) %>% summarize(averagetip= sum(tip_amount)/sum(day==day)) 
g7 <-ggplot(tip3,aes(x=day,y=averagetip)) 
chart7 <- g7 + geom_bar(stat="identity",aes(fill=tip3$day)) + coord_cartesian(ylim=c(1.25,2.0))

#q3:pickuplocation & tips
nycmap10 = readRDS("nycmap10.rds")
nycmap12 = readRDS("nycmap12.rds")
tip4 <- filter(table3, tip_amount >= 10)
map2 <- ggmap(nycmap12) + geom_point(data=tip4,aes(x=pickup_longitude,y=pickup_latitude,color=tip_amount),alpha=0.5)

#q3:pickuptime & tips
tip5 <- group_by(table3,pickuptime) %>% summarize(averagetip= sum(tip_amount)/sum(day==day)) 
g8 <-ggplot(tip5,aes(x=pickuptime,y=averagetip)) 
chart8 <- g8 + geom_bar(stat="identity",fill=rainbow) + coord_cartesian(ylim=c(1.5,2.0))

#q4: passenger_count & tips
tip6 <- group_by(table3,passenger_count) %>% summarize(averagetip= sum(tip_amount)/sum(passenger_count==passenger_count)) %>% filter(passenger_count!=0)
g9<- ggplot(tip6,aes(x=passenger_count,y=averagetip))
chart9 <- g9 + geom_bar(stat="identity",aes(fill=tip6$passenger_count)) + coord_cartesian(ylim=c(1.0,1.8)) + ggtitle("Average tips for different passenger count")

tip8 <- filter(table3, passenger_count!=0) %>% select (passenger_count,tip_amount)
g14 <- ggplot(tip8,aes(x=factor(passenger_count),y=tip_amount))
g14 + geom_boxplot() + coord_cartesian(ylim=c(0,10))

#q4: trip_distance & tips
g10 <- ggplot(table3,aes(x=trip_distance,y=tip_amount))
chart10 <- g10+ geom_point(alpha=0.5) + coord_cartesian(xlim=c(0,5),ylim=c(0,5)) + geom_smooth(method="lm")

#q1: weekdays & weekend & passengers
weekend <- filter(table3,day=="Sunday"|day=="Saturday")
tp3 <- group_by(weekend,pickuptime) %>% summarize(totalp=sum(passenger_count))
g11 <- ggplot(tp3,aes(x=pickuptime,y=totalp))
chart11 <- g11 + geom_bar(stat="identity") + ggtitle("Total passenger on weekends")

weekday <- filter(table3,day!="Sunday" & day!="Saturday")
tp4 <- group_by(weekday,pickuptime) %>% summarize(totalp=sum(passenger_count))
g12 <- ggplot(tp4,aes(x=pickuptime,y=totalp))
chart12 <- g12 + geom_bar(stat="identity") + ggtitle("Total passenger on weekdays")

#q1: drop-off point & map
table5 <- cut(as.numeric(table3$pickuptime),breaks=quantile(as.numeric(table3$pickuptime), probs=seq(0,1, by=1/6), na.rm=TRUE), include.lowest=TRUE)
table5 <- mutate(table3,period=table5)
map2 <- ggmap(nycmap10) + geom_point(data=table5,aes(x=dropoff_longitude,y=dropoff_latitude,color=period),alpha=0.5)

#q3:date & tips
tip7 <- group_by(table3,date) %>% summarize(averagetip= sum(tip_amount)/sum(date==date)) %>% mutate(day=weekdays(as.POSIXct(date)))
g13 <- ggplot(tip7,aes(x=date,y=averagetip))
g13 + geom_bar(stat = "identity",aes(fill=tip7$day)) + coord_cartesian(ylim=c(1.0,2.0))
