### Homework 2
library(devtools)
library(ggbiplot)

### Task 1

# 1.a
setwd("~/Downloads/UniStuff/IE 582/HW2")
muskdata <- read.csv(file="MUSK1.csv",header=TRUE)

pca <- princomp(muskdata[3:168], center = TRUE, scale = TRUE)
str(pca)
plot(pca$scores[,1],pca$scores[,2],col=muskdata$X1+1,pch=".", cex=7, xlab="First Component", ylab="Second Component", main="PCA")

# If we plot scores of first and second component (with their bag labels color coded), we observe that there are three main clusters. Two of those have both musk and non-musk labels mixed, meaning first two components -even though they can explain 54% of the variance- fail to explain label distribution for these two clusters. The other main cluster consists solely of confirmed musk molecules. The rest of the data which consist of smaller clusters and some random looking elements seems to be seperated with respect to their labels correctly. (No color mix)

ggbiplot(pca)


par(mfrow=c(1,1))
screeplot(pca, type = "l", npcs = 25, main = "Screeplot of the first 25 PCs")
abline(h = 1, col="red", lty=5)

# In this screeplot, red line equals 1, indicating that the data points falling below red line have eigenvalues less than 1 - which occurs after 90% cumulative variance threshold. That split occurs between components 16 and 21. 

par(mfrow=c(1,2))
comp.var <- (pca$sdev)^2
comp.var[1:10]
p.comp.var <- comp.var/(sum(comp.var))
p.comp.var[1:20]
sum(p.comp.var[1:18])
plot(p.comp.var)
plot(cumsum(p.comp.var))

# Here we observe that reducing our data two first two components would explain the variance by 54% but in order to account for more variance -lets say 90%- we need to use at least 18 components.

par(mfrow=c(1,1))
cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[1:25], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 18, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)

# Another look at our PCA components


#### Metric MDS ####

mds <- cmdscale(dist(muskdata[3:168]),eig=TRUE, k=2) # k is the number of dim
x <- mds$points[,1]
y <- mds$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS", type="n")
text(x, y, labels = row.names(muskdata), cex=.7,col=muskdata$X1+1)

# We obtain the same plot we had when we plotted our data with respect to first two components in PCA part.

## 1.b

setwd("~/Downloads/UniStuff/IE 582/HW2")
muskdata <- read.csv(file="MUSK1.csv",header=TRUE)

means=NULL
for(i in 1:92){means = rbind(means,colMeans(subset(muskdata, X1.1 == i)))}
meanframe = data.frame(means)
avg_pca <- prcomp(means)

par(mfrow=c(1,2))
summary(avg_pca)
ggbiplot(avg_pca)
avg_mds <- cmdscale(dist(meanframe[3:168]),eig=TRUE, k=2) # k is the number of dim
x <- avg_mds$points[,1]
y <- avg_mds$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS, Averaged frame", type="p",col=meanframe$X1+1
     )
text(x, y, labels = row.names(muskdata), cex=.7,col=meanframe$X1+1)
par(mfrow=c(1,1))

# Here, we observed a different distribution. Three main clusters are still present with two of them having mixed bags with respect to their labeling just like the previous part. The other main cluster which was consisted solely of positive labeled data in the previous part, happened to enter into a region of heterogeinity.
# The cause behind this may be the loss of information resulted by aggregating individual instances to have a group level inference.
# Thus we can comment that taking the average among instances might not be the best idea as it cause information loss.

### Task 2

# 2.1

library("imager")
img <- load.image("newyear.jpg")
plot(img)

# 2.2

minred = min(img[1:256,1:256,1])
mingreen = min(img[1:256,1:256,2])
minblue = min(img[1:256,1:256,3])
maxred = max(img[1:256,1:256,1])
maxgreen = max(img[1:256,1:256,2])
maxblue = max(img[1:256,1:256,3])

noise1<-matrix(runif(256*256,minred,maxred*0.1),nrow=256)
noise2<-matrix(runif(256*256,mingreen,maxgreen*0.1),nrow=256)
noise3<-matrix(runif(256*256,minblue,maxblue*0.1),nrow=256)

par(mfrow=c(1,3))

img[,,1]=img[,,1]+noise1
img[,,2]=img[,,2]+noise2
img[,,3]=img[,,3]+noise3

par(mfrow=c(1,1))
plot(img)

# Noisy image was created by taking minimum and maximum pixel values for each channel into account.
# Each noise value in a channel is distributed uniformly between minimum  pixel value of that channel and 0.1 times maximum pixel value.
# Three noise matrices, one for each channel, was created as instructed and merged into the original image.

par(mfrow=c(1,3))
image(img[,,1])
image(img[,,2])
image(img[,,3])

par(mfrow=c(1,3))
image(img[,,1], col = grey(seq(0, 1, length = 200)))
image(img[,,2], col = grey(seq(0, 1, length = 200)))
image(img[,,3], col = grey(seq(0, 1, length = 200)))

# Origin in a loaded image refers to the top left corner. However, while displaying a matrix with image() function, origin is displayed at the bottom left. Thus, we have a rotated image. I will leave these ones for show but will rotate the rest of the images from now on via a newly defined rotate() function or via sorting matrix byrow=TRUE.
# Note that images above displays each channel seperately taking only the values into account.
# If color is also desired to be displayed, one can present this via equalling values in other channels (apart from channel to be displayed) into zero and display the image.

# 2.3

# 2.3.a

gray.img=grayscale(img)
par(mfrow=c(1,1))
plot(gray.img)

patchdim = 25 
patches <- matrix(, nrow = (256-(patchdim-1))*(256-(patchdim-1)), ncol = patchdim^2)

k=1
for(i in (1+(patchdim-1)/2):(256-(patchdim-1)/2))
  for(j in (1+(patchdim-1)/2):(256-(patchdim-1)/2))
  {patches[k,]=gray.img[(i-(patchdim-1)/2):(i+(patchdim-1)/2),(j-(patchdim-1)/2):(j+(patchdim-1)/2),,]
  k=k+1}

# A for loop was constructed in order to extract patches from the image.
# A variable called patchdim was defined in order to try different patch sizes.

pca <- princomp(patches, center = TRUE, scale = TRUE)
par(mfrow=c(1,3))
plot(pca$scores[,1],pca$scores[,2])
comp.var <- (pca$sdev)^2
p.comp.var <- comp.var/(sum(comp.var))
p.comp.var[1:10]
cumsum(p.comp.var)[1:10]
plot(p.comp.var)
plot(cumsum(p.comp.var))

# We see that first component alone can explain more than 3/4 of the variance and first three components are enough to explain nearly 9/10 of the variance of the data.


# 2.3.b

rotate <- function(x) t(apply(x, 2, rev))
par(mfrow=c(1,3))
firstscore=pca$scores[,1]
firstcomp.m=matrix(firstscore,nrow=(256-(patchdim-1)))
image(rotate(firstcomp.m), col = grey(seq(0, 1, length = 200)))

secondscore=pca$scores[,2]
seccomp.m=matrix(secondscore,nrow=(256-(patchdim-1)))
image(rotate(seccomp.m), col = grey(seq(0, 1, length = 200)))

thirdscore=pca$scores[,3]
thirdcomp.m=matrix(thirdscore,nrow=(256-(patchdim-1)))
image(rotate(thirdcomp.m), col = grey(seq(0, 1, length = 200)))

# Scores of the first component provides a good silhouette, as if we see the image with a myopic POV.
# Second and third components, expectedly, provides a less clear image compared to the precision of the first component.
# Also, I tried different patch sizes, at 5x5, scores of the first component provides more than a silhouette. Smaller patch sizes would expectedly provide a clearer dislay as it would converge to the original image.

# 2.3.c

par(mfrow=c(1,3))
image(matrix(pca$loadings[,1],nrow=patchdim,byrow=TRUE),col = grey(seq(0, 1, length = 200)))
image(matrix(pca$loadings[,2],nrow=patchdim,byrow=TRUE),col = grey(seq(0, 1, length = 200)))
image(matrix(pca$loadings[,3],nrow=patchdim,byrow=TRUE),col = grey(seq(0, 1, length = 200)))

# Eigenvectors are extracted from PCA via $loadings.
# In these plots, bolder regions refer to a higher value of an eigenvector than clear regions.
# Each component displays the part of the patch corresponding to their bolder regions more accurately than other parts.
# For example, first component mostly explains the information on the edges of an image, a patch.
