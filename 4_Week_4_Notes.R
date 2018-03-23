#Clustering Case Study---------------------------------------------
setwd("C:/Coursera/Repos/datasciencecoursera/4_Week_4_Ritems/ProgrammingAssignment4")

#Get data

# download.file("http://github.com/DataScienceSpecialization/courses/tree/master/04_ExploratoryAnalysis/clusteringExample/data/samsungData.rda",
#               destfile ="samsungData.rda", mode="wb",method="internal")
#Not working


load("data/samsungData.rda")


names(samsungData)[1:12]

table(samsungData$activity)

#Plotting avg acceleration for subject 1
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))#transform converts to factor variable
sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity),
       pch = 1)


#Clustering based just on average acceleration

source("myplclust.R")
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))


#Plotting max acceleration for the first subject
par(mfrow = c(1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])


#Clustering based on maximum acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))



#Singular Value Decomposition
svd1 = svd(scale(sub1[, -c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)


#Find maximum contributor
plot(svd1$v[, 2], pch = 19)


#New clustering with maximum contributer
maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))


#New clustering with maximum contributer
names(samsungData)[maxContrib]

#K-means clustering (nstart=1, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)

#K-means clustering (nstart=1, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1)
table(kClust$cluster, sub1$activity)

#K-means clustering (nstart=100, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

#K-means clustering (nstart=100, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

#Cluster 1 Variable Centers (Laying)
plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

#Cluster 2 Variable Centers (Walking)
plot(kClust$center[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")


#Air Pollution Case Study--------------------------------------------------

#No PDF

#EPA Air pollution site http://goo.gl/soQZHM
#https://aqs.epa.gov/aqsweb/documents/data_mart_welcome.html
#https://aqs.epa.gov/aqsweb/airdata/download_files.html

air99<-read.csv("data/daily_88101_1999.csv",stringsAsFactors = F)
air12<-read.csv("data/daily_88101_2012.csv",stringsAsFactors = F)

dim(air99)
head(air99)
names(air99)

summary(air99$Arithmetic.Mean)
mean(is.na(air99))
mean(is.na(air99$Arithmetic.Mean))

dim(air12)
head(air12)
names(air12)

summary(air12$Arithmetic.Mean)
mean(is.na(air12))
mean(is.na(air12$Arithmetic.Mean))

boxplot(air99$Arithmetic.Mean,air12$Arithmetic.Mean)
boxplot(log10(air99$Arithmetic.Mean),log10(air12$Arithmetic.Mean))

#explore the negative values
negative<-air12$Arithmetic.Mean<0
str(negative)
sum(negative, na.rm=TRUE)
dates<-air12$Date.Local
dates<-as.Date(dates,"%Y-%m-%d")
str(dates)
hist(dates,"month")
hist(dates[negative],"month")

#Exploring change at one monitor
ny99<-unique(subset(air99,State.Code==36,c(County.Code,Site.Num)))
ny12<-unique(subset(air12,State.Code==36,c(County.Code,Site.Num)))

ny99<-paste(ny99[,1],ny99[,2],sep=".")
ny12<-paste(ny12[,1],ny12[,2],sep=".")
both<-intersect(ny99,ny12)

air99$county.site<-with(air99,paste(County.Code,Site.Num,sep="."))
air12$county.site<-with(air12,paste(County.Code,Site.Num,sep="."))

cnt99<-subset(air99,State.Code==36 & county.site %in% both)
cnt12<-subset(air12,State.Code==36 & county.site %in% both)

split(cnt99,cnt99$county.site)

sapply(split(cnt99,cnt99$county.site),nrow)
sapply(split(cnt12,cnt12$county.site),nrow)

air12sub<-subset(air12,State.Code == 36 & County.Code == 63 & Site.Num == 2008)
air99sub<-subset(air99,State.Code == 36 & County.Code == 63 & Site.Num == 2008)

dates12<-air12sub$Date.Local
dates12<-as.Date(dates12,"%Y-%m-%d")
dates99<-air99sub$Date.Local
dates99<-as.Date(dates99,"%Y-%m-%d")

x12<-air12sub$Arithmetic.Mean
x99<-air99sub$Arithmetic.Mean

plot(x12~dates12)
plot(x99~dates99)

#go same panel use par
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(x99~dates99,col="red")
abline(h=median(x99,na.rm=T))
plot(x12~dates12,col="blue")
abline(h=median(x12,na.rm=T))

#fix scales
rng<-range(x99,x12,na.rm=T)
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(x99~dates99,col="red",ylim=rng)
abline(h=median(x99,na.rm=T))
plot(x12~dates12,col="blue",ylim=rng)
abline(h=median(x12,na.rm=T))


#By state
mn99<-with(air99,tapply(Arithmetic.Mean,State.Code,mean,na.rm=T))
mn12<-with(air12,tapply(Arithmetic.Mean,State.Code,mean,na.rm=T))
summary(mn99)
summary(mn12)

d99<-data.frame(state=names(mn99),mean=(mn99))
d12<-data.frame(state=names(mn12),mean=(mn12))

mrg<-merge(d99,d12,by="state")
mrg




par(mfrow = c(1,1))
with(mrg,plot(rep(1999,51),mrg[,2],xlim=c(1998,2013)))
with(mrg,points(rep(2012,51),mrg[,3],xlim=c(1998,2013)))
segments(rep(1999,51),mrg[,2],rep(2012,51),mrg[,3])



































