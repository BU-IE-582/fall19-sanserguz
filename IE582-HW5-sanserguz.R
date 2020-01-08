setwd("~/Downloads/UniStuff/IE 582/HW 5")


library("stats")
library("cluster")
library("aricode")
library("factoextra")
library("scatterplot3d")

Xtrain <- read.table(file="uWaveGestureLibrary_X_TRAIN",header=FALSE)
Ytrain <- read.table(file="uWaveGestureLibrary_Y_TRAIN",header=FALSE)
Ztrain <- read.table(file="uWaveGestureLibrary_Z_TRAIN",header=FALSE)
Xtest <- read.table(file="uWaveGestureLibrary_X_TEST",header=FALSE)
Ytest <- read.table(file="uWaveGestureLibrary_Y_TEST",header=FALSE)
Ztest <- read.table(file="uWaveGestureLibrary_Z_TEST",header=FALSE)

# XtrainOrdered = Xtrain[order(Xtrain$V1),]
# YtrainOrdered = Ytrain[order(Ytrain$V1),]
# ZtrainOrdered = Ztrain[order(Ztrain$V1),]

# Part a

Gesture1 = which(Ytrain$V1 == 1)
Gesture2 = which(Ytrain$V1 == 2)
Gesture3 = which(Ytrain$V1 == 3)
Gesture4 = which(Ytrain$V1 == 4)
Gesture5 = which(Ytrain$V1 == 5)
Gesture6 = which(Ytrain$V1 == 6)
Gesture7 = which(Ytrain$V1 == 7)
Gesture8 = which(Ytrain$V1 == 8)

# Plotting an instance (first instance) of each gesture

ins = vector()
ins[1] = Gesture1[1]
ins[2] = Gesture2[1]
ins[3] = Gesture3[1]
ins[4] = Gesture4[1]
ins[5] = Gesture5[1]
ins[6] = Gesture6[1]
ins[7] = Gesture7[1]
ins[8] = Gesture8[1]


x=as.matrix(Xtrain[ins, 2:316])
y=as.matrix(Ytrain[ins, 2:316])
z=as.matrix(Ztrain[ins, 2:316])

# Transforming acceleration data to distance by taking cumulative sum twice for each dimension.

for (i in 1:8){
  
x[i,]=t(cumsum(t(cumsum(t(x[i,])))))
y[i,]=t(cumsum(t(cumsum(t(y[i,])))))
z[i,]=t(cumsum(t(cumsum(t(z[i,])))))

}

#Plotting an instance of Gesture 1

G = 1
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 1",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 2

G = 2
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 2",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 3

G = 3
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 3",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 4

G = 4
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 4",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 5

G = 5
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 5",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 6

G = 6
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 6",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 7

G = 7
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 7",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

#An instance of Gesture 8

G = 8
par(mfrow=c(1,3))
plot(2:316,x[G,], xlab="",main="X Dimension")
plot(2:316,y[G,], xlab="",main="Y Dimension")
plot(2:316,z[G,], xlab="",main="Z Dimension")
par(mfrow=c(1,1))
plot3D::scatter3D(x[G,],y[G,],z[G,], main = "An instance of Gesture 8",scale=TRUE,center=TRUE,pch = 19, bty="b2",theta = 45, phi = 30)

# Part b and c

# Being the most commonly used ones, I have decided to use Euclidean and Manhattan as distance measures.
# Need to scale the dataset to avoid getting misleading results

trainset<-cbind(Xtrain,Ytrain[,2:316],Ztrain[,2:316])
trainset[,2:946] = scale(trainset[,2:946])
testset<-cbind(Xtest,Ytest[,2:316],Ztest[,2:316])
testset[,2:946] = scale(testset[,2:946])

mandist=dist(trainset[,2:946], method = "manhattan")
eucdist=dist(trainset[,2:946], method = "euclidean")

# -----Hierarchial Clustering------

# Hierarchial Clustering
#
# There are several methods for hierarchial clustering. 
# I will try average, single link, complete link and ward methods.

hfit = hclust(mandist, method = "complete", members = NULL)
plot(hfit, cex = 0.6, hang = -1)

method <- c( "average", "single", "complete", "ward")
ac = vector(length=4)
names(ac) <- c( "average", "single", "complete", "ward")

# Agglomerative clustering function agnes() yields a coefficient called agglomerative coefficient which is used for determining a method's performance of structural data analysis.

i=1
for(m in method){
  hfit = agnes(mandist, method = m)
  ac[i]=hfit$ac
  i = i+1
}
ac

i=1
for(m in method){
  hfit = agnes(eucdist, method = m)
  ac[i]=hfit$ac
  i = i+1
}
ac

# For both distance measures, Ward’s method gives the greatest agglomerative coefficient out of four methods, meaning it works better than the others identifying the clustering structure of our dataset.
#
# Now we need to decide on the number of clusters.
# I have decided to use silhouette tracking and within sum of squared to see the discrepancy


fviz_nbclust(trainset[,2:946], FUN = hcut, method = "silhouette")
fviz_nbclust(trainset[,2:946], FUN = hcut, method = "wss")

# Optimal cluster size found out to be 9 from silhouette tracking method plot as above
# Elbow method using within cluster sum of squared error yields a similar size as well.




# -----K medoids approach-----

# K medoids approach
#  
# Main difference of K means and K medoids approaches are their objective functions.
# K means tries to minimize total squared error while k medoids attempts to minimize total dissimilarity.
# K medoids approach is more flexible and robust to outliers than K means, but it is expensive and computationally more complex.
# They would converge to be equivelant in a perfectly normalized data set (i.e. a sphere) since mean and median would be same.
# PAM (Partition around medoids) function is used for this case.


build.euc = vector()
for (i in 2:20){
  kmedoidfit = pam(trainset[,2:946], i, metric = "euclidean", stand = FALSE)
  build.euc[i]=kmedoidfit$objective[1]
}
plot(build.euc)

build.man = vector()
for (i in 2:20){
  kmedoidfit = pam(trainset[,2:946], i, metric = "manhattan", stand = FALSE)
  build.man[i]=kmedoidfit$objective[1]
}
plot(build.man)

# Using elbow's method on objective values for each fit, and also considering correspondence with the previous methods optimality, optimal cluster number is selected to be 9 for this part as well.

# Part d

# Using Hierarchial clustering approach
# Optimal classifier was found to use ward's method for linkage
# Optimal cluster number was found to be 9

final_hfit = hclust(mandist, method = "ward.D2", members = NULL)
cluster = cutree(final_hfit, k = 9)

head(cbind(cluster,class=trainset[,1]),50)


# Use this model on testset to get cluster labels for each instance
# Note that class labels and assigned clusters are not scaled the same
# (class 5 corresponds to cluster 2, 6 to 1 etc.)

# We need a performance measure for testing
# As the performance metric, I have decided to use Normalized Mutual Information between acquired cluster labels and gesture classes from the ground data.
# This metric is independent of the absolute values of the labels. A rearrangement of  class or cluster labels won’t change the score value in any way.
#
# NMI close to 0:  Dissimilar information sets
# NMI close to 1:  Equivalence of information sets


mandisttest=dist(testset[,2:946], method = "manhattan")
eucdisttest=dist(testset[,2:946], method = "euclidean")

# By Manhattan Distance Measure

finalhfit_man = hclust(mandisttest, method = "ward.D2", members = NULL)
predictions = cutree(finalhfit_man, k = 9)
hclust_man_res=cbind(predictions,class=testset[,1])
head(hclust_man_res,50)
NMI(hclust_man_res[,1],hclust_man_res[,2])

# NMI : 0.7661979

# By Euclidean Distance Measure

finalhfit_euc = hclust(eucdisttest, method = "ward.D2", members = NULL)
predictions = cutree(finalhfit_euc, k = 9)
hclust_euc_res=cbind(predictions,class=testset[,1])
head(hclust_euc_res,50)
NMI(hclust_euc_res[,1],hclust_euc_res[,2])

# NMI : 0.7078667

# k-medoids (PAM) approach

# By Manhattan Distance Measure
kmedoidfitman = pam(testset[,2:946], 9, metric = "manhattan", stand = FALSE)
predictions = kmedoidfitman$clustering
kmedoid_man_res = cbind(predictions,class=testset[,1])
head(kmedoid_man_res,50)
NMI(kmedoid_man_res[,1],kmedoid_man_res[,2])

# NMI : 0.6278999

# By Euclidean Distance Measure

kmedoidfiteuc = pam(testset[,2:946], 9, metric = "euclidean", stand = FALSE)
predictions = kmedoidfiteuc$clustering
kmedoid_euc_res = cbind(predictions,class=testset[,1])
head(kmedoid_euc_res,50)
NMI(kmedoid_euc_res[,1],kmedoid_euc_res[,2])

# NMI : 0.6328526

# K-medoids approach yields significantly lower NMI satistics for both distance measures
# Hierarchial Clustering was found to be superior than K-medoids, managing to analyze the structure of the data and make accurate clustering assignments that fits the class of the ground data.

# Part e

# Best performing model is Hierarchial Clustering with 9 clusters using ward's method
# Fit the model using scaled testing set and construct 9 clusters

finalhfitman = hclust(mandisttest, method = "ward.D2", members = NULL)
cluster = cutree(finalhfitman, k = 9)

# Use the unscaled raw data for time series

testset2=cbind(Xtest,Ytest[,2:316],Ztest[,2:316])
series = cbind(cluster,testset2)

Cl1=series[cluster == 1,]
Cl2=series[cluster == 2,]
Cl3=series[cluster == 3,]
Cl4=series[cluster == 4,]
Cl5=series[cluster == 5,]
Cl6=series[cluster == 6,]
Cl7=series[cluster == 7,]
Cl8=series[cluster == 8,]
Cl9=series[cluster == 9,]

# For each cluster, time series plots of each instance were plotted in three dimensions separately since 3D scatterplots fails to show this amount of information with clarity.
# At each plot, cluster prototype of the respective dimension is also plotted.
# 
# Cluster 1

C1x = Cl1[,3:317]
C1y = Cl1[,318:632]
C1z = Cl1[,633:947]

C1prototype_x = colMeans(C1x)
C1prototype_y = colMeans(C1y)
C1prototype_z = colMeans(C1z)

ts.plot(t(C1x), main ="Cluster 1 - X dimension",col="blue")
lines(C1prototype_x,col="red",lwd=3)
ts.plot(t(C1y), main ="Cluster 1 - Y dimension",col="red")
lines(C1prototype_y,col="blue",lwd=3)
ts.plot(t(C1z), main ="Cluster 1 - Z dimension")
lines(C1prototype_z,col="green",lwd=3)


# Cluster 2

C2x = Cl2[,3:317]
C2y = Cl2[,318:632]
C2z = Cl2[,633:947]

C2prototype_x = colMeans(C2x)
C2prototype_y = colMeans(C2y)
C2prototype_z = colMeans(C2z)

ts.plot(t(C2x), main ="Cluster 2 - X dimension",col="blue")
lines(C2prototype_x,col="red",lwd=3)
ts.plot(t(C2y), main ="Cluster 2 - Y dimension",col="red")
lines(C2prototype_y,col="blue",lwd=3)
ts.plot(t(C2z), main ="Cluster 2 - Z dimension")
lines(C2prototype_y,col="green",lwd=3)

# Cluster 3

C3x = Cl3[,3:317]
C3y = Cl3[,318:632]
C3z = Cl3[,633:947]

C3prototype_x = colMeans(C3x)
C3prototype_y = colMeans(C3y)
C3prototype_z = colMeans(C3z)

ts.plot(t(C3x), main ="Cluster 3 - X dimension",col="blue")
lines(C3prototype_x,col="red",lwd=3)
ts.plot(t(C3y), main ="Cluster 3 - Y dimension",col="red")
lines(C3prototype_y,col="blue",lwd=3)
ts.plot(t(C3z), main ="Cluster 3 - Z dimension")
lines(C3prototype_z,col="green",lwd=3)

# Cluster 4

C4x = Cl4[,3:317]
C4y = Cl4[,318:632]
C4z = Cl4[,633:947]


C4prototype_x = colMeans(C4x)
C4prototype_y = colMeans(C4y)
C4prototype_z = colMeans(C4z)

ts.plot(t(C4x), main ="Cluster 4 - X dimension",col="blue")
lines(C4prototype_x,col="red",lwd=3)
ts.plot(t(C4y), main ="Cluster 4 - Y dimension",col="red")
lines(C4prototype_y,col="blue",lwd=3)
ts.plot(t(C4z), main ="Cluster 4 - Z dimension")
lines(C4prototype_z,col="green",lwd=3)

# Cluster 5

C5x = Cl5[,3:317]
C5y = Cl5[,318:632]
C5z = Cl5[,633:947]


C5prototype_x = colMeans(C5x)
C5prototype_y = colMeans(C5y)
C5prototype_z = colMeans(C5z)

ts.plot(t(C5x), main ="Cluster 5 - X dimension",col="blue")
lines(C5prototype_x,col="red",lwd=3)
ts.plot(t(C5y), main ="Cluster 5 - Y dimension",col="red")
lines(C5prototype_y,col="blue",lwd=3)
ts.plot(t(C5z), main ="Cluster 5 - Z dimension")
lines(C5prototype_z,col="green",lwd=3)

# Cluster 6

C6x = Cl6[,3:317]
C6y = Cl6[,318:632]
C6z = Cl6[,633:947]


C6prototype_x = colMeans(C6x)
C6prototype_y = colMeans(C6y)
C6prototype_z = colMeans(C6z)

ts.plot(t(C6x), main ="Cluster 6 - X dimension",col="blue")
lines(C6prototype_x,col="red",lwd=3)
ts.plot(t(C6y), main ="Cluster 6 - Y dimension",col="red")
lines(C6prototype_y,col="blue",lwd=3)
ts.plot(t(C6z), main ="Cluster 6 - Z dimension")
lines(C6prototype_z,col="green",lwd=3)

# Cluster 7

C7x = Cl7[,3:317]
C7y = Cl7[,318:632]
C7z = Cl7[,633:947]


C7prototype_x = colMeans(C7x)
C7prototype_y = colMeans(C7y)
C7prototype_z = colMeans(C7z)

ts.plot(t(C7x), main ="Cluster 7 - X dimension",col="blue")
lines(C7prototype_x,col="red",lwd=3)
ts.plot(t(C7y), main ="Cluster 7 - Y dimension",col="red")
lines(C7prototype_y,col="blue",lwd=3)
ts.plot(t(C7z), main ="Cluster 7 - Z dimension")
lines(C7prototype_z,col="green",lwd=3)

# Cluster 8

C8x = Cl8[,3:317]
C8y = Cl8[,318:632]
C8z = Cl8[,633:947]


C8prototype_x = colMeans(C8x)
C8prototype_y = colMeans(C8y)
C8prototype_z = colMeans(C8z)

ts.plot(t(C8x), main ="Cluster 8 - X dimension",col="blue")
lines(C8prototype_x,col="red",lwd=3)
ts.plot(t(C8y), main ="Cluster 8 - Y dimension",col="red")
lines(C8prototype_y,col="blue",lwd=3)
ts.plot(t(C8z), main ="Cluster 8 - Z dimension")
lines(C8prototype_z,col="green",lwd=3)

# Cluster 9

C9x = Cl9[,3:317]
C9y = Cl9[,318:632]
C9z = Cl9[,633:947]


C9prototype_x = colMeans(C9x)
C9prototype_y = colMeans(C9y)
C9prototype_z = colMeans(C9z)

ts.plot(t(C9x), main ="Cluster 9 - X dimension",col="blue")
lines(C9prototype_x,col="red",lwd=3)
ts.plot(t(C9y), main ="Cluster 9 - Y dimension",col="red")
lines(C9prototype_y,col="blue",lwd=3)
ts.plot(t(C9z), main ="Cluster 9 - Z dimension")
lines(C9prototype_z,col="green",lwd=3)