############################lab 2 Part 1a ###########################
#Measures of Central Tendency/Histograms/Data Manipulation:
data_EPI <-read.csv(file.choose(),header=T)
View(data_EPI)

#Generate central tendency value for EPI variable
data_EPI1 <- na.omit(data_EPI$EPI)
View(data_EPI1)
mean(data_EPI$EPI, trim = 0, na.rm = TRUE)
median(data_EPI$EPI, trim=0, na.rm = TRUE)
#Function for get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
result_mode <- getmode(data_EPI1)
result_mode

#Generate central tendency value for DALY variable
data_EPI2 <- na.omit(data_EPI$DALY)
View(data_EPI2)
mean(data_EPI$DALY, trim = 0, na.rm = TRUE)
median(data_EPI$DALY, trim=0, na.rm = TRUE)
#Function for get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
result_mode1 <- getmode(data_EPI2)
result_mode1

#Generate histograms for EPI and DALY variable
data_2010EPI <-read.csv(file.choose(),header=T)
View(data_2010EPI)
data_2010EPI
head(data_2010EPI)
names(data_2010EPI) <-as.matrix(data_2010EPI[1,])
data_2010EPI <-data_2010EPI[-1,]
data_2010EPI[] <-lapply(data_2010EPI, function(x) type.convert(as.character(x)))
data_2010EPI
View(data_2010EPI)
help("lapply")
hist(data_2010EPI$EPI)
hist(data_2010EPI$DALY)

#Boxplot
boxplot(data_2010EPI$ENVHEALTH, data_2010EPI$ECOSYSTEM)

#qqplot
qqplot(data_2010EPI$ENVHEALTH, data_2010EPI$ECOSYSTEM)

#########################lab 2 Part 1b ###### on EPI dataset Regression Excercise
data_EPI <-read.csv(file.choose(),header=T)
View(data_EPI)

install.packages("corrplot")

data_EPI_Int <-na.omit(data_EPI[,6:160])
View(data_EPI_Int)

data_EPI_Matrix <-cor(data_EPI_Int, method = c("pearson","kendall","spearman"))
View(data_EPI_Matrix)
data_EPI_Mat <-na.omit(data_EPI_Matrix[,"EPI"])
data_sort <- sort(data_EPI_Mat, decreasing = FALSE)
View(data_sort)
data_sort
# Conclusion- ENVHEALTH has the highest value and hence the most important factor. 

#Linear and least squares
EPI_data <-data_EPI
attach(EPI_data)
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH <-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
#Predict
DAILYNew <- c(seq(5,95,5))
AIR_HNew <- c(seq(5,95,5))
WATER_HNew <- c(seq(5,95,5))
New_df <-data.frame(DAILYNew,AIR_HNew, WATER_HNew)
New_df
pENV <- predict(lmENVH,New_df,interval = "prediction")
cENV <-predict(lmENVH,New_df,interval = "confidence")

# For AIR_E
boxplot(AIR_E,DALY,AIR_H,WATER_H)
lmAIR_E <-lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIR_E
pAIR_E <-predict(lmAIR_E,New_df,interval = "prediction")
cAIR_E <-predict(lmAIR_E,New_df,interval = "confidence")

#For CLIMATE
boxplot(CLIMATE,DALY,AIR_H,WATER_H)
lmCLIMATE <-lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE
pCLIMATE <-predict(lmCLIMATE,New_df,interval = "prediction")
cCLIMATE <-predict(lmCLIMATE,New_df,interval = "confidence")
View(cCLIMATE)
