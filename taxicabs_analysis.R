# Use NYC taxicab data and answer some questions

library(sp)
library(maptools)
df1=read.csv("D:\\texts\\datascience\\trip_data_3.csv")
#help(read.csv)
#head(d1)
df2=read.csv("D:\\texts\\datascience\\trip_fare_3.csv")
df3=merge(df1,df2)
#What fraction of payments under $5 use a credit card
less5=subset(df3,total_amount<5)
sum(less5["payment_type"]=="CRD")/nrow(less5)
#What fraction of payments over $50 use a credit card
over50=subset(df3,total_amount>50)
sum(over50["payment_type"]=="CRD")/nrow(over50)
dr_time=as.numeric(strptime(df3$dropoff_datetime, "%Y-%m-%d %H:%M:%OS")-strptime(df3$pickup_datetime, "%Y-%m-%d %H:%M:%OS"))/60
dr_time=sapply(as.list(1:length(dr_time)), function(x) ifelse(isTRUE(all.equal(dr_time[x],0, tolerance = 1e-8)),NA,dr_time[x]))
mean(df3$fare_amount/dr_time, na.rm=TRUE)
median(df3$fare_amount/df3$trip_distance, na.rm=TRUE)
quantile(df3$trip_distance/(dr_time),.95, na.rm=TRUE)
pickup=data.matrix(subset(df3,select=c(pickup_longitude, pickup_latitude)))
dropoff=data.matrix(subset(df3,select=c(dropoff_longitude, dropoff_latitude)))
actual_distance=sapply(1:nrow(pickup),function(row) ifelse(is.na(dropoff[row,1])||is.na(dropoff[row,2]),NA,spDists(t(as.vector(pickup[row,])),t(as.vector(dropoff[row,])),longlat=TRUE)/1.60934))
actual_distance=sapply(as.list(1:length(actual_distance)), function(x) ifelse(isTRUE(all.equal(actual_distance[x],0, tolerance = 1e-8))||(actual_distance[x]>100),NA,actual_distance[x]))
df3td=df3$trip_distance
trip_distance=sapply(as.list(1:length(df3td)), function(x) ifelse(isTRUE(all.equal(df3td[x],0, tolerance = 1e-8)),NA,df3td[x]))
mean(actual_distance/trip_distance, na.rm=TRUE)
from_JFK=subset(df3,sapply(1:nrow(pickup),function(row) as.numeric(spDists(t(as.vector(pickup[row,])),t(c(-73.7789,40.6397)),longlat=TRUE))<4))
mean(from_JFK$tip_amount)
revenue=aggregate(total_amount ~ medallion, df3, sum)
mean(revenue$total_amount)
median(revenue$total_amount)
max(revenue$total_amount)
hist(revenue$total_amount)
