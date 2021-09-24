#creating a dataframe
#Example: RPI Weather dataframe

days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')# days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)# Temp in F'
snowed <- c('T','T','F','F','T','T','F')#snowed on that day: T= True, F= False
help("data.frame")
#creating the dataframe using the data.frame() function
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
#head of the data shows only 6 rows of the data frame.
head(RPI_Weather_Week)
# we can look at the structure of the dataframe using data.frame() function
str(RPI_Weather_Week)
#summary of the dataframe
summary(RPI_Weather_Week)

#showing the 1st row of all coumns
RPI_Weather_Week[1,]
#showing the 1st column of all rows
RPI_Weather_Week[,1]

RPI_Weather_Week[,"snowed"]
RPI_Weather_Week[,"days"]
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset = snowed=='T')

sorted.snowed <-order(RPI_Weather_Week["snowed"])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

#RPI_Weather_Week descending snowed
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

#Creating an empty data frame
empty.DataFrame <-data.frame()
v1 <-1:10
v1
letters
v2 <-letters[1:10]
df <-data.frame(col.name.1=v1,col.name.2=v2)
df
#importing data and exporting data
#writing to a csv file
write.csv(df,file= 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')



#Lab 1 EPI dataset
data_EPI <-read.csv(file.choose(),header=T)
data_EPI 
View(data_EPI)
#Remove the NA values from the dataset and saved in data_EPI1
data_EPI1 <- na.omit(data_EPI)
View(data_EPI1)
stem(data_EPI1$Population07)
stem(data_EPI1$PopulationDensity)
hist(data_EPI1$Population07)
hist(data_EPI1$WATER_E, seq(57,67, 80),probability = TRUE)
lines(density(data_EPI1$WATER_E,na.rm = TRUE, bw = 1))
#plot AIR_E and add a rug to the plot.
plot(density(data_EPI1$AIR_E,bw=1 ))
rug(data_EPI1$AIR_E)
help(rug)
#cumulative density function
plot(ecdf(data_EPI1$WATER_H),do.points= FALSE,verticals= TRUE)

#Quantile- quantile
par(pty="s")
qqnorm(data_EPI1$GDPCAP07);qqline(data_EPI1$AIR_E)
#Make a qq plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),data_EPI1$Population07,xlab = "Q-Q plot for t dsn")
qqline(data_EPI1$Population07)

#Exploration for DALY 
qqnorm(data_EPI1$DALY);qqline(data_EPI1$DALY)
#Make a qq plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),data_EPI1$DALY,xlab = "Q-Q plot for t dsn")
qqline(data_EPI1$DALY)

#Exploration for WATER_H
qqnorm(data_EPI1$WATER_H);qqline(data_EPI1$WATER_H)
#Make a qq plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),data_EPI1$WATER_H,xlab = "Q-Q plot for t dsn")
qqline(data_EPI1$WATER_H)

boxplot(data_EPI1$DALY,data_EPI1$WATER_H)
qqplot(data_EPI1$ENVHEALTH,data_EPI1$ECOSYSTEM)
qqplot(data_EPI1$BIODIVERSITY,data_EPI1$WATER_E)



