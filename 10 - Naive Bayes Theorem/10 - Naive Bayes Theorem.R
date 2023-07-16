# Naive Bayes Theorem

install.packages('e1071')

# Installing Package into ‘C:/Users/NGT/Documents/R/win-library/3.6’ (as ‘lib’ is unspecified)
library(e1071)

data=read.csv('E:/College Files/MSC.IT PART 1 SEM 2/PRACTICAL CODES/Big Data Analytics/10 - Naive Bayer Theorem/Data Sets/diabetes.csv')
View(data)
head(data)

nbsmodel=naiveBayes(Outcome ~.,data=data)
nbsmodel

nbspred=predict(nbsmodel,data)
nbspred

tt=table(nbspred,data$Outcome)
tt

acc_test=sum(diag(tt)/sum(tt))
acc_test


