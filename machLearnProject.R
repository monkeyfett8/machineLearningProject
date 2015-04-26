setwd("/Users/monkeyfett8/Desktop/coursera/project")
library(caret)
set.seed(356)

# load training and test sets remove user,date/time cols
training <- read.csv("pml-training.csv")
training <- training[,-c(1:7)]
testing <- read.csv("pml-testing.csv")
testing <- testing[,-c(1:7)]

# determine columns of mostly NAs
naCols <- !sapply(training,function(x) sum(is.na(x))/length(x)>.1)
#remove columns missing from future tests
testCols <- !is.na(colSums(testing))
cbind(naCols,testCols,naCols&testCols)

# remove mostly NA columns
train2 <- training[,testCol&testingCols]
test2 <- testing[,testCol&testingCols]

# create partitions for validating within training set
index <- createDataPartition(training$classe,p=0.7,list=FALSE)
valSet <- train2[-index,]
trainSet <- train2[index,]

#print size of each sub-set
print(paste("training set:", nrow(trainSet)))
print(paste("testing set:", nrow(valSet)))

answers = c("B","A","B","A","A","E","D","B","A","A",
			"B","C","B","A","E","E","A","B","B","B")

# model <- train(classe ~ . , data=training, method="rpart2")#, trControl=fitControl)
modelTree <- train(classe ~ . , data=trainSet, method="rpart")
modelLDA  <- train(classe ~ . , data=trainSet, method="lda")
modelQDA  <- train(classe ~ . , data=trainSet, method="qda")


# check each model against validation set and get accuracy
valPredTree <- as.character(predict(modelTree, newdata=valSet))
correctTree <- sum(valPredTree ==valSet$classe)/length(valPredTree)

valPredLDA <- as.character(predict(modelLDA, newdata=valSet))
correctLDA <- sum(valPredLDA ==valSet$classe)/length(valPredLDA)

valPredQDA <- as.character(predict(modelQDA, newdata=valSet))
correctQDA <- sum(valPredQDA ==valSet$classe)/length(valPredQDA)


# compare methods
methods <- c("Tree","LDA","QDA")
correct <- c(correctTree,correctLDA,correctQDA)
compare <- cbind(methods,correct)

# predict data for testing set
predTree <- as.character(predict(modelTree, newdata=test2))
predLDA <- as.character(predict(modelLDA, newdata=test2))
predQDA <- as.character(predict(modelQDA, newdata=test2))



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#pml_write_files(predQDA)