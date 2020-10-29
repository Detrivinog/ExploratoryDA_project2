library(dplyr)
library(RColorBrewer)
library(ggplot2)

nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Select list of scc codes related in enrgy sector with coal
scc_select <- droplevels(scc$SCC[grep("[Mm]obile", scc$SCC.Level.One)])

#Filter data with select scc codes 
motor <- nei %>% filter(fips=="24510", SCC %in% scc_select) %>% mutate(year = as.factor(year))
#Summarise data: total emissions
motor1 <- nei %>% filter(fips=="24510", SCC %in% scc_select) %>% mutate(year = as.factor(year)) %>%
    group_by(year) %>% summarise(total = sum(Emissions))


#Figure
cols <- brewer.pal(4, "Pastel2")
png(filename = "plot5.png", width = 500, height = 500, units = "px")
par(mfrow=c(1,2))
boxplot(Emissions~year, motor, ylim=c(0,3.7), outline=F, col=cols, main="Average")
barplot(total~year, motor1, ylab = "Emissions", col=cols, main="Total")
title("Baltimore vehicle-motor emissions PM2.5", outer = T, line = -1)
dev.off()
