library(tidyverse)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC<- readRDS("data/Source_Classification_Code.rds")

#Isolate motor vehicle source codes
unique(SCC$EI.Sector)
mobile<-filter(SCC, grepl("Mobile - On-Road",EI.Sector))
unique(mobile$EI.Sector)
sccIDs<-unique(mobile$SCC) %>% as.character()

#Filter data source
vehData<-filter(NEI,SCC %in% sccIDs,fips=='24510')


#Plot
annualSums<-vehData %>% 
        group_by(year) %>% 
        summarize(`Total Emissions (tons)`=sum(Emissions))


png(filename = "plot5.png",width = 5,height=3, units="in", res=300)

par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(annualSums$`Total Emissions (tons)`~annualSums$year,
     main="Total Vehicle Emissions by Year (tons)\nBaltimore City",
     ylab = '',xlab='',cex=1.5)
lines(annualSums$`Total Emissions (tons)`~annualSums$year)

dev.off()
