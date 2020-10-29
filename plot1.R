library(dplyr)
library(RColorBrewer)
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Sumarise data
total <- nei %>%
    group_by(year) %>%
    summarise(total=sum(Emissions), media=mean(Emissions), std = sd(Emissions)) %>%
    mutate(year=as.factor(year))

#Select colors
cols <- brewer.pal(4, "Pastel2")

png(filename = "plot1.png", width = 500, height = 500, units = "px")
par(mfrow=c(1,2))
boxplot(Emissions~year, nei, ylim=c(0,0.7), outline=F, col=cols, main="Average")
barplot(total~year, total, ylab = "Emissions", col=cols, main="Total")
title("USA Emissions PM2.5", outer = T, line = -1)
dev.off()

