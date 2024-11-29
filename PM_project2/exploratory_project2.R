#1
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Emissions <- tapply(NEI$Emissions, NEI$year, sum)
png("plot1.png")
barplot(Emissions/1000000, xlab="Year", ylab="PM2.5 Emissions (millions of tons)", main="Total PM2.5 Emissions in US by Year", ylim=c(0,8))
dev.off()

#2
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
baltimore <- subset(NEI, fips=="24510")
balt_emissions <- tapply(baltimore$Emissions, baltimore$year, sum)
png("plot2.png")
barplot(balt_emissions, xlab="Year", ylab="PM2.5 Emissions (tons)", main="Total PM2.5 Emissions in Baltimore by Year", ylim=c(0, 3500))
dev.off()

#3
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
baltimore <- subset(NEI, fips=="24510")
balt_emissions_type <- aggregate(Emissions ~ year + type, baltimore, sum)
png("plot3.png")
ggplot(balt_emissions_type, aes(x=year, y=Emissions, color=type))+ 
  geom_line() + xlab("Year") + ylab("Total PM2.5 Emissions (tons)") + ggtitle("Total PM2.5 Emissions in Baltimore by Year and Type")
dev.off()

#4
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
coal <- grepl("coal", SCC$EI.Sector, ignore.case=TRUE)
SCC_coal <- SCC[coal,]
coal_NEI <- merge(NEI, SCC_coal, by="SCC")
coal_sum <- tapply(coal_NEI$Emissions, coal_NEI$year, sum)
coal_sum <- as.data.frame(coal_sum)
names(coal_sum)[1] <- "Emissions"
rownames(coal_sum) <- c(1:4)
coal_sum$Year <- c(1999, 2002, 2005, 2008)
png("plot4.png")
ggplot(coal_sum, aes(x=Year, y=Emissions)) +
  geom_line() + geom_point() + xlab("Year") + ylab("Total PM.25 Emissions (tons)") + ggtitle("Total PM2.5 Emissions from Coal Combustion-Related Sources by Year")
dev.off()

#5
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
mv <- grepl("vehicle", SCC$EI.Sector, ignore.case=TRUE)
SCC_mv <- SCC[mv,]
mv_NEI <- merge(NEI, SCC_mv, by="SCC")
baltimore <- subset(mv_NEI, fips=="24510")
balt_mv <- tapply(baltimore$Emissions, baltimore$year, sum)
balt_mv <- as.data.frame(balt_mv)
names(balt_mv)[1] <- "Emissions"
rownames(balt_mv) <- c(1:4)
balt_mv$Year <- c(1999, 2002, 2005, 2008)
png("plot5.png")
ggplot(balt_mv, aes(x=Year, y=Emissions)) +
  geom_line() + geom_point() + xlab("Year") + ylab("Total PM.25 Emissions (tons)") + ggtitle("Total PM2.5 Emissions from Motor Vehicle Sources in Baltimore by Year")
dev.off()

#6
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
mv <- grepl("vehicle", SCC$EI.Sector, ignore.case=TRUE)
SCC_mv <- SCC[mv,]
mv_NEI <- merge(NEI, SCC_mv, by="SCC")
baltimore_la <- subset(mv_NEI, fips=="24510" | fips=="06037")
baltimore_la$city <- ifelse(baltimore_la$fips=="24510", "Baltimore", "Los Angeles")
Emissions <- aggregate(Emissions ~ year + city, baltimore_la, sum)
png("plot6.png", width=500, height=480)
ggplot(Emissions, aes(x=year, y=Emissions, color=city)) +
  geom_line() + geom_point() + xlab("Year") + ylab("Total PM.25 Emissions (tons)") + ggtitle("Total Motor Vehicle Sources PM2.5 Emissions in Baltimore vs. Los Angeles")
dev.off()

