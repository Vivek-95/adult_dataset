library(ggplot2)
library(reshape2)
data<-matrix(c(86.27,86.35,86.35,85.93,86.23,86.25,81.2,81.15,81.1,80.42,81,81,85.3,85.23,85.2,84.7,85.07,85.2,86.9,86.9,86.9,86.42,86.78,87.07,83.22,83.2,83.2,82.65,83.25,83.26),nrow=6,ncol=5)
df<-as.data.frame(data)
colum<-c("RandomForest","NaiveBayes","SVM","C5.0","knn")
colnames(df)<-colum
df <- cbind(Imputation_techniques=c("Mice","knn","MissForest","Omission","Mode","AmeliaForest"), df)
df <- melt(df, id.vars = 'Imputation_techniques', variable.name = 'ML_Algorithms')
colnames(df) <- c(colnames(df)[1:2], "Accuracy")
ggplot(df, aes(ML_Algorithms, Accuracy, group=Imputation_techniques)) + geom_line(aes(color=Imputation_techniques))
