## Loading and preprocessing the data

##set up work directory 
setwd('"~/Desktop/course_fei/EDA/project2/EDA_project2"')
##unzip file
unzip("exdata-data-NEI_data.zip")
##read data needed 
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


##check data structure 

str(NEI)
head(NEI,3)
tail(NEI,3)
sum(is.na(NEI))

sum(is.na(SCC))
str(SCC)
head(SCC,3)
tail(SCC,3)

##20330

##############################################################################################
#Question1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#############################################################################################
###Calculate the total PM2.5 emission by year
totalEM <- aggregate(Emissions ~ year,NEI, sum)
str(TotalEM)
##plot totalPM2.5 emission from all sources for each year 
png("plot1.png")

plot(totalEM, 
     type="o", 
     main='Total PM2.5 Emissions from All US Sources by year', 
     xlab='Year', 
     ylab=expression('PM'[2.5]),
     pch=19,col='purple',lty=6)

dev.off()

png("plot1B.png")
barplot(totalEM$Emissions/1000, 
        names.arg = totalEM$year,
        main='Total PM2.5 Emissions from All US Sources by year', 
        xlab='Year', 
        ylab="PM2.5 Emissions in kilotons"
        
        )
     
dev.off()

##############################################################################################
#Question2 Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? 
#############################################################################################
# Calculate the total PM2.5 emission by year for Baltimore
totalEMB<- aggregate(Emissions ~ year , data =NEI, 
                          subset=NEI$fips=="24510", FUN = sum)

##plot totalPM2.5 emission from all sources for each year for Baltimore
png("plot2.png")
plot(totalEMB, type="o",
     main="MD Baltimore Total PM2.5 Emissions by Year",
     xlab = "Year", 
     ylab=expression('PM'[2.5]) ,
     pch=19,col='purple',lty=6)

dev.off()

png("plot2B.png")
barplot(totalEMB$Emissions, 
        names.arg = totalEM$year,
        main='MD Baltimore Total PM2.5 Emissions by Year', 
        xlab='Year', 
        ylab="PM2.5 Emissions (Tons)"
        
)

dev.off()


##############################################################################################
#Question3 
#Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008?
#############################################################################################
str(NEI)
library(ggplot2)

# Calculate the total PM2.5 emission by year for Baltimore
# Subset NEI data by Baltimore's fip.
baltimoreNEI <- subset(NEI,fips=='24510')

totalEMBType<- aggregate(Emissions ~ type+year, data =NEI, 
                     subset=NEI$fips=="24510", FUN = sum)
?aggregate()
png("plot3B.png")
ggplot(totalEMBType, aes(x=year, y=Emissions/1000,fill=type)) + 
        geom_line(color="blue") + 
        facet_wrap(~ type, nrow=1, ncol=4) +
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
        ggtitle(expression(paste('Total Emission of PM'[2.5], ' in Baltmore MD by Source Type')))

dev.off()

png("plot3.png")
qplot(year,Emissions/1000, data=totalEMBType,color=type,geom= "line") +
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
        ggtitle(expression(paste('Total Emission of PM'[2.5], ' in Baltmore MD by Source Type')))

dev.off()


##############################################################################################
#Question4
#Across the United States, 
#how have emissions from coal combustion-related sources changed from 1999–2008?
#############################################################################################
str(NEI)
str(SCC$Short.Name)

subSCC <- SCC[grepl("Coal" , SCC$Short.Name), ]
subNEI<- NEI[NEI$SCC %in% subSCC$SCC, ]
subNEI2<-aggregate(Emissions ~ year, data =subNEI, FUN = sum)



png("plot4.png")
ggplot(subNEI2, aes(x=year, y=Emissions/1000)) + 
        geom_line(aes(group=1, col=Emissions)) + geom_point(aes(size=2, col=Emissions)) + 
        ggtitle(expression('Total Coal-Related of PM'[2.5])) + 
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
        geom_text(aes(label=round(Emissions/1000,digits=2), size=1.5, hjust=1, vjust=1)) + 
        theme(legend.position='none') + scale_colour_gradient(low='black', high='blue')
dev.off()

png("plot4B.png")
ggplot(subNEI, aes(x = factor(year), y = Emissions/1000, fill =type)) + 
        geom_bar(stat="identity", width = .4) + 
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
        ggtitle(expression('Total Coal-Related of PM'[2.5])) 

dev.off()

##############################################################################################
#Question5
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#############################################################################################


subNEImo<-aggregate(Emissions ~ year, data =NEI, subset=NEI$fips=="24510"&type=='ON-ROAD', FUN = sum)

png("plot5.png")
ggplot(subNEImo, aes(x=year, y=Emissions)) + 
        geom_line(aes(group=1, col=Emissions)) + 
        ggtitle(expression('Baltimore MD, total motor vehicle sources of PM'[2.5])) + 
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5])) )

dev.off()


##############################################################################################
# Question5
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle emissions?
#############################################################################################

NEIcomp<-subset(NEI,fips %in% c("06037","24510")&type=="ON-ROAD")
subNEIcomp<-aggregate(Emissions ~ year+fips, data=NEI, subset=fips %in% c("06037","24510")&type=="ON-ROAD", FUN = sum)

png("plot6.png")
qplot(year,Emissions/1000, data=subNEIcomp,color=fips,geom= "line") +
        xlab("year") +
        ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
        ggtitle('Motor vehicle sources PM2.5 \n in Baltimore,MD(24510) vs. Los Angeles,CA(06037)')

dev.off()