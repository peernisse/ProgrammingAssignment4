library(tidyverse)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")

#Summarize
pData<-NEI %>% 
        filter(fips=="24510") %>% 
        group_by(year) %>% 
        summarize(totPM25=sum(Emissions),SD=sd(Emissions))


#Plot

png(filename = "plot2.png",width = 5,height=3, units="in", res=300)
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(pData$totPM25~pData$year,
     ylim=range(c(pData$totPM25-(2.6*(pData$SD)), pData$totPM25+(2.6*(pData$SD)))),
                  main="Total PM 2.5 Emissions in Baltimore City (tons)\nAll Sources",
     xlab='',ylab='')
#arrows(pData$year, pData$totPM25-(2.6*(pData$SD)), pData$year, pData$totPM25+(2.6*(pData$SD)), length=0.05, angle=90, code=3)
lines(pData$totPM25~pData$year)

dev.off()














