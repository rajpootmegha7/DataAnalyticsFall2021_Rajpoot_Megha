set.seed(12345)
help("par")

par(mar=rep(0.2,4))
data_Matrix <-matrix(rnorm(400),nrow = 40)
image(1:10,1:40, t(data_Matrix)[,nrow(data_Matrix):1]) 
help("heatmap")
par(mar=rep(0.2,4))
heatmap(data_Matrix)

help("rbinom")
set.seed(678910)
for (i in 1:40) {
  #flipping the coin and getting the data.
  coin_Flip <-rbinom(1,size=1,prob = 0.5)
  #if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i,]<- data_Matrix[i,]+rep(c(0,3),each=5)
  }
  }
#if any one is true than just added a pattern to my data 5 with mean 0 and others with mean 3.
#plot the data
par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
#now we will return the heatmap() function on the data, we can see that the
#two sets of columns are easily separated. 
heatmap(data_Matrix)
?heatmap
#Look at the different column and rows pattern.
#10 column mean and 40 different row mean.
hh <-hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow =c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,xlab= "The Row Mean",ylab = "Row",pch=19)
plot(colMeans(data_Matrix_Ordered),xlab = "Column",ylab="Column Mean",pch=19)


#Kmeans1.R
data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1]) 


