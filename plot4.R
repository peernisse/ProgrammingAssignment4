library(tidyverse)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC<- readRDS("data/Source_Classification_Code.rds")
names(NEI)

#Isolate coal combustion sources
srcs<-SCC[,c(1,4)]
unique(srcs$EI.Sector)
srcs<-filter(srcs,EI.Sector %in% c("Fuel Comb - Electric Generation - Coal",
                                   "Fuel Comb - Industrial Boilers, ICEs - Coal",
                                   "Fuel Comb - Comm/Institutional - Coal"))

coalSCC<-unique(srcs$SCC,stringsAsFactors=FALSE) %>% 
        as.character()

coalData<-filter(NEI,SCC %in% coalSCC)
length(unique(coalData$SCC))
#80 out of 99 coal source codes remain. Must not all be in the NEI dataset

#Make plot
g<-ggplot(coalData,aes(x=as.character(year),y=Emissions))+
        geom_jitter(alpha=0.25)+
        facet_grid(.~year)+
        theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
        labs(title="U.S. Coal Combustion Sourced Air Pollution - 1999 to 2008 (tons)")+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank())
g

ggsave("plot4.png",g,dpi=300)



