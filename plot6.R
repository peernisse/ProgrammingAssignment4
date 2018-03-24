library(tidyverse)
options(scipen=6)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC<- readRDS("data/Source_Classification_Code.rds")

#Isolate motor vehicle source codes
unique(SCC$EI.Sector)
mobile<-filter(SCC, grepl("Mobile - On-Road",EI.Sector))
unique(mobile$EI.Sector)
sccIDs<-unique(mobile$SCC) %>% as.character()

#Filter data source
vehData<-filter(NEI,SCC %in% sccIDs,fips %in% c('24510','06037'))
vehData$City<-ifelse(vehData$fips=='24510',"Baltimore City","Los Angeles County")
unique(vehData$City)

g<-ggplot(vehData,aes(x=year,y=Emissions,fill=City))+
        geom_boxplot()+
        facet_grid(.~City)
g

g<-ggplot(vehData,aes(x=as.character(year),y=sum(Emissions),fill=as.character(year)))+
        geom_col(alpha=0.5)+
        facet_grid(City~.)+
        theme(legend.position = "bottom",legend.title = element_blank())+
        theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
        labs(title="Total Vehicle Emissions by Year (tons)\nBaltimore City and LA County")
g


ggsave("plot6.png",g,dpi=300)


