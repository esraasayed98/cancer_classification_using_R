data1 = read.csv("Task_data.csv")
data=data1[,-c(7,25,4372,4810,4811,4373,4377,4379,4830,4816,4817,4818,4819,4821,4825,
               4833,5290,7663,7664,7665,7666,7667,8123,9306,9308,9316,9318,9322,9325,
               9454,10123,11960,12647,13993,14160,14161,14163,15140,15142,15143,15448,12720,13862)]



#Encoding categorical data
data$Class = factor(data$Class,
                    levels = c('PRAD' , 'LUAD' ,'BRCA','KIRC','COAD'),
                    labels = c(1,2,3,4,5))

pca= preProcess(x=data[-1] , method='pca',pcaComp =30)

new_data=predict(pca,data)

library(caTools)
set.seed(123)
split = sample.split(new_data$Class , SplitRatio=0.8)
training_set = subset(new_data , split == TRUE)
test_set = subset(new_data , split == FALSE)



library(class)
y_pred = knn(train = training_set[, -1],
             test = test_set[, -1],
             cl =training_set$Class,
             k = 5,
             prob = TRUE)




# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
accuracy = ((cm[1,1] + cm[2,2] + cm[3,3]) + cm[4,4] + cm[5,5]) / (sum(cm)) 
accuracy
# Applying k-Fold Cross Validation
# install.packages('caret')
library(lattice)
library(caret)
folds=createFolds(new_data$Class,k=10)
cv = lapply(folds, function(x) {
  training_fold = new_data[-x, ]
  test_fold = new_data[x, ]
  #print(sum(is.na(training_fold)))
  #print(sum(is.na(test_fold)))
  
  y_pred = knn(train = training_fold[, -1],
               test = test_fold[, -1],
               cl = training_fold[[1]],
               k = 5,
               prob = TRUE)
  cm = table(test_fold[[ 1]], y_pred)
  accuracy = (cm[1,1] + cm[2,2] + cm[3,3] + cm[4,4] + cm[5,5]) / 
    (sum(cm))
  return(accuracy)
})
accuracy_tot = mean(as.numeric(cv))



