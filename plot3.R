library(dplyr)
library(RColorBrewer)
library(ggplot2)

nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Of the four types of sources indicated by the \color{red}{\verb|type|}type 
# (point, nonpoint, onroad, nonroad) variable, which of these four sources have 
# seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

#Sumarise data with the filter, and total emissions
total <- nei %>%
    group_by(year, type) %>%
    filter(fips=="24510") %>%
    summarise(total=sum(Emissions), media=mean(Emissions), std = sd(Emissions)) %>%
    mutate(type=as.factor(type), year=as.factor(year))


g<- ggplot(data=total, aes(x=year, y=total, fill=type)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(aes(ymin=total-std, ymax=total+std), width=.2, position=position_dodge(.9)) +
    scale_fill_brewer(palette="Paired") + theme_minimal() +
    ylab("Emissions") + ggtitle("Baltimore emissions PM2.5 by type")

# Make the plot
png(filename = "plot3.png", width = 480, height = 480, units = "px"); g; dev.off()

