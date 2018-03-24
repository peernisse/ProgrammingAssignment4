library(tidyverse)

#Read in data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC<- readRDS("data/Source_Classification_Code.rds")
names(NEI)

typeData<-NEI %>% 
        filter(fips=="24510") %>% 
        select(5,4,6) %>% 
        group_by(type,year) %>% 
        summarize(totalPM25=sum(Emissions))

typeData$year<-as.character(typeData$year)

g<-ggplot(typeData,aes(x=year,y=totalPM25,fill=year))+
        geom_col()+
        facet_wrap(~type,scales="free")+
        theme(legend.position = "bottom")+
        theme(strip.background = element_rect(fill="steelblue"),
              strip.text = element_text(color="white",face="bold"))+
        labs(y="PM 2.5 (tons)",title="Total PM 2.5 (tons) in Baltimore City by Year and Source Type")
g

ggsave("plot3.png",g,dpi=300)


