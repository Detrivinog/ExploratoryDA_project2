library(dplyr)
library(RColorBrewer)
library(ggplot2)

nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California 
#(\color{red}{\verb|fips == "06037"|}fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

#Sumarise data with the filter, and total emissions
scc_select <- droplevels(scc$SCC[grep("[Mm]obile", scc$SCC.Level.One)])

#Filter data with select scc codes 
motor <- nei %>% filter(fips=="24510"| fips=="06037", SCC %in% scc_select) %>% 
    mutate(year = as.factor(year), city = sub("24510", "Baltimore", fips)) %>%
    mutate(city = sub("06037", "LA", city)) %>% mutate(city = as.factor(city))
#Summarise data: total emissions
motor1 <- nei %>% filter(fips=="24510"| fips=="06037", SCC %in% scc_select) %>% 
    mutate(year = as.factor(year), city = sub("24510", "Baltimore", fips)) %>%
    mutate(city = sub("06037", "LA", city)) %>% mutate(city = as.factor(city)) %>%
    group_by(year, city) %>% summarise(total = sum(Emissions), sd = sd(Emissions))


g<- ggplot(data=motor1, aes(x=year, y=total, fill=city)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_brewer(palette="Paired") + theme_minimal() +
    ylab("Emissions") + ggtitle("Baltimore-LA motor vehicle emmission PM2.5")

g
# Make the plot
png(filename = "plot6.png", width = 480, height = 480, units = "px"); g; dev.off()
