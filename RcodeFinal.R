install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library("dplyr")
install.packages("corrplot")
library(corrplot)
install.packages("sqldf")
library("sqldf")
install.packages("mice")
library("mice")

dataset <- read.csv(file.choose(), header = TRUE)
#remove gender because all the records are female
dataset <- dataset[,-9]
#remove insulin due to large amount of missing data (260/599)
dataset <- dataset[-5]
#Change column names for comfortability
colnames(dataset)<- c("numpreg", "plasma", "diastolic", "triceps", "bmi", "dpf", "age", "Diabetes")

#remove records where age is under 12 and they got pregnant (43)
dataset <- sqldf(c("delete from dataset where age < 12 and numpreg > 0",  "select * from dataset"))

#remove records where diastolic is NA (3)
dataset <- sqldf(c("delete from dataset where diastolic is null", "select * from dataset"))

#Filling plasma
dataset$plasma[is.na(dataset$plasma)] <- mean(dataset$plasma[!is.na(dataset$plasma)])

#Discrete bmi into 7 categories (0 is not possible)
dataset$bmi[dataset$bmi < 8] <- 0
dataset$bmi[dataset$bmi > 8 & dataset$bmi <=18.5] <- 1
dataset$bmi[dataset$bmi > 18.5 & dataset$bmi <=25] <- 2
dataset$bmi[dataset$bmi > 25 & dataset$bmi <=29.9] <- 3
dataset$bmi[dataset$bmi > 29.9 & dataset$bmi <=34.9] <- 4
dataset$bmi[dataset$bmi > 34.9 & dataset$bmi <=39.9] <- 5
dataset$bmi[dataset$bmi > 39.9] <- 6

#Discrete age into 3 categories
dataset$age[dataset$age <= 44] <- 1
dataset$age[dataset$age > 44 & dataset$age <=64] <- 2
dataset$age[dataset$age > 64] <- 3

#Discrete diastolic into 7 categories (0 is not possible)
dataset$diastolic[dataset$diastolic < 30] <- 0
dataset$diastolic[dataset$diastolic >= 30 & dataset$diastolic < 60] <- 1
dataset$diastolic[dataset$diastolic >= 60 & dataset$diastolic < 84] <- 2
dataset$diastolic[dataset$diastolic >= 84 & dataset$diastolic < 89] <- 3
dataset$diastolic[dataset$diastolic >= 89 & dataset$diastolic < 99] <- 4
dataset$diastolic[dataset$diastolic >= 99 & dataset$diastolic < 109] <- 5
dataset$diastolic[dataset$diastolic >= 109] <- 6

#Discrete plasma into 4 categories (0 is not possible)
dataset$plasma[dataset$plasma < 45] <- 0
dataset$plasma[dataset$plasma >= 45 & dataset$plasma < 140] <- 1
dataset$plasma[dataset$plasma >= 140 & dataset$plasma < 199] <- 2
dataset$plasma[dataset$plasma >= 199] <- 3

#Converting from mmol/L to mg/dL
#bla$plasma[bla$plasma < 15] <- bla$plasma[bla$plasma < 15] * 18.01801801801802

#Filling Triceps using PMI technique
tempData <- mice(data = dataset, m = 5, method = "pmm", maxit = 50, seed = 500)
dataset <- complete(tempData, 1)

#Correlation matrix
cor(dataset)


#Histograms of variables
hist(dataset$Number.of.times.pregnant)
hist(dataset$Plasma.glucose.concentration.a.2.hours.in.an.oral.glucose.tolerance.test)
hist(dataset$Diastolic.blood.pressure..mm.Hg.)
hist(dataset$Triceps.skin.fold.thickness..mm.)
hist(dataset$X2.Hour.serum.insulin..mu.U.ml.)
hist(dataset$bmi.)
hist(dataset$Diabetes.pedigree.function)
hist(dataset$Age..years.)






#Density functions and aprior probability
prop.table(table(dataset$Diabetes))
prop.table(table(dataset$bmi))
prop.table(table(dataset$age))
prop.table(table(dataset$plasma))
prop.table(table(dataset$diastolic))

ggplot(dataset) + geom_density(aes(dataset$numpreg)) + labs(x = "number of pregnants") + theme(text = element_text(size=15))
ggplot(dataset) + geom_density(aes(dataset$triceps)) + labs(x = "Triceps skin fold thickness mm") + theme(text = element_text(size=15))
ggplot(dataset) + geom_density(aes(dataset$dpf)) + labs(x = "Diabetes pedigree function") + theme(text = element_text(size=15))


##########################Machine Learning Models##################################
dataset$Diabetes <- as.factor(dataset$Diabetes)

#######Neural Networks########
install.packages("sqldf")
library("sqldf")
install.packages("nnet")
library("nnet")
#Convert the categorical columns into dummy:
datasetDummy <-dataset
datasetDummy$numpreg <- as.factor(datasetDummy$numpreg)
datasetDummy$plasma <- as.factor(datasetDummy$plasma)
datasetDummy$diastolic <- as.factor(datasetDummy$diastolic)
datasetDummy$triceps <- as.factor(datasetDummy$triceps)
datasetDummy$bmi <- as.factor(datasetDummy$bmi)
datasetDummy$age <- as.factor(datasetDummy$age)

#Create the training vector:
#----------------------------------------------
datasetDummyLength<-nrow(datasetDummy)
SampleTrain<-sample(datasetDummyLength, datasetDummyLength*0.6, replace = FALSE, prob = NULL) #Take 60% of the dataset
train<-datasetDummy[SampleTrain,]
#----------------------------------------------
#Create the test and the validation vector:
#----------------------------------------------
#First, delete the records from the training set on the original dataset:
datasetDummyTemp<-datasetDummy[-SampleTrain,] 
datasetDummyLength<-nrow(datasetDummyTemp)
#Create the validation set:
SampleValidation<-sample(datasetDummyLength, datasetDummyLength*0.5, replace = FALSE, prob = NULL)
validation<-datasetDummyTemp[SampleValidation,]
#Create the test set:
test<-datasetDummyTemp[-SampleValidation,]

#----------------------------------------------

#Run on default settings:
#softmax:
nndefault <- nnet(x=train[,1:7], y=class.ind(train[,8]), size = 8, softmax=T)
preds.nndefault.train <- factor(predict(nndefault, newdata=train[,1:7], type='class'))
preds.nndefault.val   <- factor(predict(nndefault, newdata=validation[,1:7], type='class'))
preds.nndefault.test  <- factor(predict(nndefault, newdata=test[,1:7], type='class'))

nnDefAccTrain <- (sum(preds.nndefault.train==train[,8]))/nrow(train)
nnDefAccTest <- (sum(preds.nndefault.test==test[,8]))/nrow(test)
nnDefAccValidation <- (sum(preds.nndefault.val==validation[,8]))/nrow(validation)

#entropy:
nndefaultEntropy <- nnet(x=train[,1:7], y=class.ind(train[,8]), size = 8, softmax=T)
preds.nndefaultEntropy.train <- factor(predict(nndefaultEntropy, newdata=train[,1:7], type='class'))
preds.nndefaultEntropy.val   <- factor(predict(nndefaultEntropy, newdata=validation[,1:7], type='class'))
preds.nndefaultEntropy.test  <- factor(predict(nndefaultEntropy, newdata=test[,1:7], type='class'))

nnDefEntAccTrain <- (sum(preds.nndefaultEntropy.train==train[,8]))/nrow(train)
nnDefEntAccTest <- (sum(preds.nndefaultEntropy.test==test[,8]))/nrow(test)
nnDefEntAccValidation <- (sum(preds.nndefaultEntropy.val==validation[,8]))/nrow(validation)

#Checking the number of neurons needed:
nnum <- seq(1,101,1)
nn.acc.train <- matrix(0,length(nnum),2)
nn.acc.val   <- matrix(0,length(nnum),2)
i    <- 1 
runs <- 1
for(neurons in nnum){
  run<-1
  sumAccRateTrainnum <- 0
  sumAccRateValnum <- 0
  for (run in runs){
    nn             <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=neurons, linout=FALSE, softmax=T, MaxNWts = 30000)
    preds.nn.train <- factor(predict(nn, newdata=train[,1:7], type='class'))
    preds.nn.val   <- factor(predict(nn, newdata=validation[,1:7], type='class'))
    accRateTrainnum<- (sum(preds.nn.train==train[,8]))/nrow(train)
    accRateValnum  <- (sum(preds.nn.val==validation[,8]))/nrow(validation)
    sumAccRateTrainnum <- sumAccRateTrainnum + accRateTrainnum
    sumAccRateValnum <- sumAccRateValnum + accRateValnum
  }
  nn.acc.train[i,2] <- sumAccRateTrainnum/length(runs)
  nn.acc.train[i,1] <- i
  nn.acc.val[i,2]   <- sumAccRateValnum/length(runs)
  nn.acc.val[i,1]   <- i
  i                 <- i + 1
}

optNumOfNeurons = which.is.max(nn.acc.val[,2])

nnBest             <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=neurons, linout=FALSE, softmax=T, MaxNWts = 30000)
preds.nn.test  <- factor(predict(nnBest, newdata=test[,1:7], type='class'))
accRateTest <- (sum(preds.nn.test==test[,8]))/nrow(test)

nn.acc.val <- data.frame(numOfNeurons = nn.acc.val[,1], Accuracy = nn.acc.val[,2])
nn.acc.train <- data.frame(numOfNeurons = nn.acc.train[,1], Accuracy = nn.acc.train[,2])

ggplot(data.frame(x=nn.acc.train$numOfNeurons, y=nn.acc.train$Accuracy)) + geom_line(aes(x,y), color='blue') + 
  xlab("Number of Neurons") + ylab("Training Accuracy")
ggplot(data.frame(x=nn.acc.val$numOfNeurons, y=nn.acc.val$Accuracy)) + geom_line(aes(x,y), color='blue') + 
  xlab("Number of Neurons") + ylab("Validation Accuracy")


#Checking which number of iterations is best with the optimal number of neurons, found earlier:

maxiterations <- seq(100,200,10)
runs <- seq(1,100,1)

nnmax.acc.train <- matrix(0,length(maxiterations),2)
nnmax.acc.val   <- matrix(0,length(maxiterations),2)


i<-1
maxi<-100
for(maxi in maxiterations){
  run<-1
  sumAccRateTrain <- 0
  sumAccRateVal <- 0
  for (run in runs){
    nnmax            <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=optNumOfNeurons, linout=FALSE, softmax=T, MaxNWts = 30000, maxit = maxi)
    preds.nnmax.train <- factor(predict(nnmax, newdata=train[,1:7], type='class'))
    preds.nnmax.val   <- factor(predict(nnmax, newdata=validation[,1:7], type='class'))
    accRateTrain <- (sum(preds.nnmax.train==train[,8]))/nrow(train)
    accRateVal <- (sum(preds.nnmax.val==validation[,8]))/nrow(validation)
    sumAccRateTrain <- sumAccRateTrain + accRateTrain
    sumAccRateVal <- sumAccRateVal + accRateVal
  }
  nnmax.acc.train[i,2]  <- sumAccRateTrain/length(runs)
  nnmax.acc.train[i,1]  <- 100+ (i-1)*10
  nnmax.acc.val[i,2]    <- sumAccRateVal/length(runs)
  nnmax.acc.val[i,1]    <- 100+ (i-1)*10
  i              <- i + 1
}


optNumOfIterations = 100 + 10*(which.is.max(nnmax.acc.val[,2])-1)
nnmaxBest            <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=optNumOfNeurons, linout=FALSE, softmax=T, MaxNWts = 30000, maxit = optNumOfIterations)
preds.nnmax.test  <- factor(predict(nnmaxBest, newdata=test[,1:7], type='class'))
accRateTest <- (sum(preds.nnmax.test==test[,8]))/nrow(test)

nnmax.acc.val <- data.frame(numOfIterations = nnmax.acc.val[,1], Accuracy = nnmax.acc.val[,2])
nnmax.acc.train <- data.frame(numOfIterations = nnmax.acc.train[,1], Accuracy = nnmax.acc.train[,2])

ggplot(data.frame(x=nnmax.acc.train$numOfIterations, y=nnmax.acc.train$Accuracy)) + geom_line(aes(x,y), color='blue') + 
  xlab("Number of iterations") + ylab("Training Accuracy")
ggplot(data.frame(x=nnmax.acc.val$numOfIterations, y=nnmax.acc.val$Accuracy)) + geom_line(aes(x,y), color='blue') + 
  xlab("Number of iterations") + ylab("Validation Accuracy")


#Check which is better: entropy or softmax model:

nnEntropy.acc.train <- matrix(0,2,2)
nnEntropy.acc.val   <- matrix(0,2,2)

#Checking Entropy:
sumAccEntropyRateTrain <- 0
sumAccEntropyRateVal <- 0
run<-1
for (run in runs){
  nnEntropy  <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=optNumOfNeurons, linout=FALSE, entropy = T, MaxNWts = 30000, maxit = optNumOfIterations)
  preds.nnEntropy.train <- factor(predict(nnEntropy, newdata=train[,1:7], type='class'))
  preds.nnEntropy.val   <- factor(predict(nnEntropy, newdata=validation[,1:7], type='class'))
  accEntropyRateTrain <- (sum(preds.nnEntropy.train==train[,8]))/nrow(train)
  accEntropyRateVal <- (sum(preds.nnEntropy.val==validation[,8]))/nrow(validation)
  accEntropyRateTest <- (sum(preds.nnEntropy.test==test[,8]))/nrow(test)
  sumAccEntropyRateTrain <- sumAccEntropyRateTrain + accEntropyRateTrain
  sumAccEntropyRateVal <- sumAccEntropyRateVal + accEntropyRateVal
  sumAccEntropyRateTest <- sumAccEntropyRateTest + accEntropyRateTest
}
nnEntropy.acc.train[1,2]  <- sumAccEntropyRateTrain/length(runs)
nnEntropy.acc.train[1,1]  <- "Entropy"
nnEntropy.acc.val[1,2]    <- sumAccEntropyRateVal/length(runs)
nnEntropy.acc.val[1,1]    <- "Entropy"
nnEntropy.acc.test[1,2]  <- sumAccEntropyRateTest/length(runs)
nnEntropy.acc.test[1,1]  <- "Entropy"



#Checking Softmax:
sumAccSoftmaxRateTrain <- 0
sumAccSoftmaxRateVal <- 0
sumAccSoftmaxRateTest <- 0
run<-1
for (run in runs){
  nnSoftmax  <- nnet(x=train[,1:7], y=class.ind(train[,8]), size=optNumOfNeurons, linout=FALSE, softmax = T, MaxNWts = 30000, maxit = optNumOfIterations)
  preds.nnSoftmax.train <- factor(predict(nnSoftmax, newdata=train[,1:7], type='class'))
  preds.nnSoftmax.val   <- factor(predict(nnSoftmax, newdata=validation[,1:7], type='class'))
  preds.nnSoftmax.test  <- factor(predict(nnSoftmax, newdata=test[,1:7], type='class'))
  accSoftmaxRateTrain <- (sum(preds.nnSoftmax.train==train[,8]))/nrow(train)
  accSoftmaxRateVal <- (sum(preds.nnSoftmax.val==validation[,8]))/nrow(validation)
  accSoftmaxRateTest <- (sum(preds.nnSoftmax.test==test[,8]))/nrow(test)
  sumAccSoftmaxRateTrain <- sumAccSoftmaxRateTrain + accSoftmaxRateTrain
  sumAccSoftmaxRateVal <- sumAccSoftmaxRateVal + accSoftmaxRateVal
  sumAccSoftmaxRateTest <- sumAccSoftmaxRateTest + accSoftmaxRateTest
}
nnEntropy.acc.train[2,2]  <- sumAccSoftmaxRateTrain/length(runs)
nnEntropy.acc.train[2,1]  <- "Softmax"
nnEntropy.acc.val[2,2]    <- sumAccSoftmaxRateVal/length(runs)
nnEntropy.acc.val[2,1]    <- "Softmax"
nnEntropy.acc.test[2,2]  <- sumAccSoftmaxRateTest/length(runs)
nnEntropy.acc.test[2,1]  <- "Softmax"


#######Decision Tree#######
install.packages("rpart")
library("rpart")
install.packages("rpart.plot")
library("rpart.plot")

#Split dataset into train(70%) and test(30%)
split <- sample(1:nrow(dataset), 0.7*nrow(dataset))
train_set<- dataset[split,]
test_set <- dataset[-split,]

#Create Decision Tree on train set
tree <- rpart(train_set$Diabetes ~ . ,data = train_set, method="class")

#plotting cp table:
printcp(tree)
#Looking for the cp with the lowest validation error
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]

#Prune the tree according to the cp parameter with the lowest xerror
pruned_model <- prune(tree, cp)
#Plot the pruned tree
rpart.plot(pruned_model)

#Predict on train set
pred.train <- predict(pruned_model, train_set[1:7])
#set prediction to 0,1 (like the dependent variable)
pred.train <- apply(pred.train, 1, which.max)
pred.train <- pred.train - 1
#Calculate accuracy of the tree on test set
sum(pred.train == train_set$Diabetes)/nrow(train_set)

#Predict on test set
pred.test <- predict(pruned_model, test_set[1:7])
pred.test <- apply(pred.test, 1, which.max)
pred.test <- pred.test - 1
sum(pred.test == test_set$Diabetes)/nrow(test_set)
table(predict = pred.test, acutaly = test_set$Diabetes)

#Prune the tree according to the cp parameter with the highest xerror
cp <- 0.01
pruned_model2 <- prune(tree, cp)
rpart.plot(pruned_model2)

#Predict on train set
pred.train <- predict(pruned_model2, train_set[1:7])
#set prediction to 0,1 (like the dependent variable)
pred.train <- apply(pred.train, 1, which.max)
pred.train <- pred.train - 1
#Calculate accuracy of the tree on test set
sum(pred.train == train_set$Diabetes)/nrow(train_set)

#Predict on test set
pred.test <- predict(pruned_model2, test_set[1:7])
pred.test <- apply(pred.test, 1, which.max)
pred.test <- pred.test - 1
sum(pred.test == test_set$Diabetes)/nrow(test_set)
table(predict = pred.test, acutaly = test_set$Diabetes)

#Prune the tree according to the cp parameter equals 0
cp <- 0
pruned_model3 <- prune(tree, cp)
rpart.plot(pruned_model3)

#Predict on train set
pred.train <- predict(pruned_model3, train_set[1:7])
#set prediction to 0,1 (like the dependent variable)
pred.train <- apply(pred.train, 1, which.max)
pred.train <- pred.train - 1
#Calculate accuracy of the tree on test set
sum(pred.train == train_set$Diabetes)/nrow(train_set)

#Predict on test set
pred.test <- predict(pruned_model3, test_set[1:7])
pred.test <- apply(pred.test, 1, which.max)
pred.test <- pred.test - 1
sum(pred.test == test_set$Diabetes)/nrow(test_set)
table(predict = pred.test, acutaly = test_set$Diabetes)

#Prune the tree according to the cp parameter equals 0
cp <- 0.15
pruned_model4 <- prune(tree, cp)
rpart.plot(pruned_model4)

#Predict on train set
pred.train <- predict(pruned_model4, train_set[1:7])
#set prediction to 0,1 (like the dependent variable)
pred.train <- apply(pred.train, 1, which.max)
pred.train <- pred.train - 1
#Calculate accuracy of the tree on test set
sum(pred.train == train_set$Diabetes)/nrow(train_set)

#Predict on test set
pred.test <- predict(pruned_model4, test_set[1:7])
pred.test <- apply(pred.test, 1, which.max)
pred.test <- pred.test - 1
sum(pred.test == test_set$Diabetes)/nrow(test_set)
table(predict = pred.test, acutaly = test_set$Diabetes)

######K-Modes######
install.packages("klaR")
library("klaR")
install.packages("clusterSim")
library("clusterSim")

#Running K-Modes with k=2 and default values
cluster_data <- kmodes(scale(dataset[,1:7]), modes = 2)

#Exploring the cluster
install.packages("gridExtra")
library(gridExtra)
dataset$cluster <- cluster_data$cluster
p1 <- ggplot(dataset, aes(x=dataset$numpreg, y=dataset$age,  color=dataset$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(dataset, aes(x=dataset$numpreg, y=dataset$dpf,  color=dataset$cluster, size=10)) + geom_point() + guides(color=F, size=F)
p3 <- ggplot(dataset, aes(x=dataset$bmi, y=dataset$age, color=dataset$cluster, size=10)) + geom_point() + guides(color=F, size=F)
p4 <- ggplot(dataset, aes(x=dataset$bmi,  y=dataset$age, color=dataset$cluster, size=10)) + geom_point() + guides(color=F, size=F)
p5 <- ggplot(dataset, aes(x=dataset$bmi,  y=dataset$triceps,  color=dataset$cluster, size=10)) + geom_point() + guides(color=F, size=F)
grid.arrange(p1, p2, p3, p4, p5, ncol=3)
#Finding the best K

#Silhouette function
silhouette <- function(dataset, clusters_vec) {
  n         <- length(clusters_vec)
  k         <- length(unique(clusters_vec))
  SC_sum    <- 0
  disim_mat <- as.matrix(daisy(dataset, metric="gower"))
  for(i in 1:n) {
    dist_vec <- rep(0, k)
    S        <- 0
    for(j in 1:k) {
      dist_vec[j] <- mean(disim_mat[which(clusters_vec==j),i])
    }
    a      <- dist_vec[clusters_vec[i]]
    b      <- min(dist_vec[-clusters_vec[i]])
    S      <- (b-a)/max(b,a)
    SC_sum <- SC_sum + S
  }
  SC <- SC_sum/n
  return(SC)
}
#Initiate


set.seed(1234)
MinK    <- 1
MaxK    <- 10
models  <- list()
centers <- list()
D_B     <- list()
Sill     <- list()

#Looking for the best K for k-modes
for (K in MinK:MaxK) {
  clust_data <- kmodes(scale(dataset[,1:7]), modes = K, iter.max = 25)
  models[[K-MinK+1]]   <- clust_data
  centers[[K-MinK+1]]  <- clust_data$centers
  D_B[[K-MinK+1]]      <- index.DB(dataset[,1:7], clust_data$cluster, d = NULL, centrotypes = "centroids", p = 2, q = 2)[1]
  Sill[[K-MinK+1]]      <- silhouette(dataset[,1:7], clust_data$cluster)
}


#Plotting the results
plot(unlist(D_B), xlab='K', ylab='Value', col="red", main="DB")
lines(unlist(D_B), col="red")

plot(unlist(Sill), xlab='K', ylab='Value', col="blue", main="Silhouette")
lines(unlist(Sill), col="blue")

###Hierarchial Clustering
install.packages("cluster")
library("cluster")
distMatrix <- daisy(scale(dataset[,1:7]), metric="gower")
hclust_data <- hclust(distMatrix, method="complete")
plot(hclust_data)
rect.hclust(hclust_data, k=3)

#Calculating Purity
dataset$clusters <- cluster_data$cluster

purity <- function(data, k) {
  purity_vector <- c()
  for (i in 1:k) {
    count0 <- length(data$Diabetes[data$Diabetes == 0 & data$clusters == i])
    count1 <- length(data$Diabetes[data$Diabetes == 1 & data$clusters == i])
    purity_vector[i] <- max(count0, count1)/length(data$Diabetes[data$clusters == i])
  }
  purity_index <- sum(purity_vector)
  return(purity_index)
}

cluster_acc <- purity(dataset, 2)/2


#Final predict on given test set
pred.final <- predict(pruned_model, dataset)
pred.final <- apply(pred.final, 1, which.max)
pred.final <- pred.final - 1
write.csv(pred.final, file = "C:/ML/TestY.csv")
