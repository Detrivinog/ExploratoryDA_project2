library(dplyr)
library(RColorBrewer)
library(ggplot2)

nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

# Select list of scc codes related in enrgy sector with coal
scc_select <- droplevels(scc$SCC[grep("[Cc]oal", scc$EI.Sector)])

#Filter data with select scc codes 
coal <- nei %>% filter(SCC %in% scc_select) %>% mutate(year = as.factor(year))
#Summarise data: total emissions
coal1 <- nei %>% filter(SCC %in% scc_select) %>% mutate(year = as.factor(year)) %>%
    group_by(year) %>% summarise(total = sum(Emissions), sd = sd(Emissions))


#Figure
cols <- brewer.pal(4, "Pastel2")
png(filename = "plot4.png", width = 500, height = 500, units = "px")
par(mfrow=c(1,2))
boxplot(Emissions~year, coal, ylim=c(0,25), outline=F, col=cols, main="Average")
barplot(total~year, coal1, ylab = "Emissions", col=cols, main="Total")
title("USA Coal combustion-related emissions PM2.5", outer = T, line = -1)
dev.off()

