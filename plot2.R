library(dplyr)
library(RColorBrewer)
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

#Summarise data
total <- nei %>%
    group_by(year) %>%
    filter(fips=="24510") %>%
    summarise(total=sum(Emissions), media=mean(Emissions), std = sd(Emissions)) %>%
    mutate(year=as.factor(year))


#Select colors
cols <- brewer.pal(4, "Pastel2")

png(filename = "plot2.png", width = 480, height = 480, units = "px")
par(mfrow=c(1,2))
boxplot(Emissions~year, nei, ylim=c(0,5), outline=F, col=cols, main="Average",
        subset= fips=="24510")
barplot(total~year, total, ylab = "Emissions", col=cols, main="Total")
title("Baltimore City Emisions PM2.5", line = -1, outer = TRUE)
dev.off()
