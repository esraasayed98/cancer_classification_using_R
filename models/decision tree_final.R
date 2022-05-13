#importing the dataset

data1=read.csv('../data/Task_data.csv')




data=data1[,-c(7,25,4372,4810,4811,4373,4377,4379,4830,4816,4817,4818,4819,4821,4825,
               4833,5290,7663,7664,7665,7666,7667,8123,9306,9308,9316,9318,9322,9325,
               9454,10123,11960,12647,13993,14160,14161,14163,15140,15142,15143,15448,
               12720,13862)]



#Encoding categorical data
data$Class = factor(data$Class,
                    levels = c('PRAD' , 'LUAD' ,'BRCA','KIRC','COAD'),
                    labels = c(1,2,3,4,5))


#applying pca

pca= preProcess(x=data[-1] , method='pca',pcaComp =50)

new_data=predict(pca,data)


library(caTools)
set.seed(123)
split = sample.split(new_data$Class , SplitRatio=0.8)

training_set = subset(new_data , split == TRUE)
test_set = subset(new_data , split == FALSE)



# Fitting classifier to the Training set
library(rpart)
classifier=rpart(formula = Class ~ . , data=training_set)

# Predicting the Test set results
#type=class bet3aml m3 y_pred 3la anha factor ze class ali 
#f test_set al aslya 3shan a3raf akarnhom b b3d w ageb confussion matrix
y_pred = predict(classifier, newdata = test_set[,-1],type='class')

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred) #bkarn test set al 72e2ya w ali tk3ta mn modle
accuracy= (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5])/ sum(cm)



#apply cross validation
library(lattice)
library(ggplot2)
library(caret)
folds=createFolds(new_data$Class,k=10)
cv=lapply(folds,function(x_fold){
  training_fold=new_data[-x_fold,]
  test_fold=new_data[x_fold,]
  classifier=rpart(formula=Class~. , data=training_fold)
  y_pred = predict(classifier, newdata = test_fold[-1],type='class')
  cm = table(test_fold[,1], y_pred)
  accuracy=((cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5])/ sum(cm))
  return(accuracy)
})
tot_acc=mean(as.numeric(cv))




