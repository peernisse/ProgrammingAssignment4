library(tidyverse)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")

#Summarize
annualSums<-NEI %>% 
        group_by(year) %>% 
        summarize(`Total PM 2.5`=sum(Emissions))
x<-annualSums$year
y<-annualSums$`Total PM 2.5`

#Plot
png(filename = "plot1.png",width = 5,height=3, units="in", res=300)
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(y~x,main="Total PM 2.5 by Year (tons)\nAll States, All Sources",cex=1.5)
lines(y~x)

dev.off()







