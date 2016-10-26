#Set Working Directory to correct working directory
#Building a User Interface for deploying the model into prediciton using shiny
#The model used is C5.0() and the Imputation Method used is Amelia Forest
#User Interface is created only for model trained by the C5.0() Algorithm. The dataset is imputed using the Amelia Forest method.
#Eg for >50K classification: Workclass:Federal-Gov;Education:Assoc-adm;MaritalStatus:Married;Occupation:Exec-managerial;Relationship:Husband;Race:White;Sex:Male;CapitalGain:0;CapitalLoss:0;HoursPerWeek:80;NativeCountry:UnitedStates

library(shiny)
library(C50)
library(Amelia)
library(caret)
library(car)


#Reading csv files
adult.train <- read.csv("adult.data", header=FALSE, na.strings="?", stringsAsFactors=FALSE)#Load Training Dataset 
adult.train$V14<-as.character(adult.train$V14)
adult.test <- read.csv("adult.test", header=FALSE,stringsAsFactors = F)#Load Testing Dataset
columns<-c('age','workclass','fnlwgt','education','EducationNo','MaritalStatus','occupation','relationship','race','sex','CapitalGain','CapitalLoss','HoursPerWeek','NativeCountry','salary')
colnames(adult.test)<-columns
adult.test<-adult.test[2:16282,]
is.na(adult.test)<-adult.test==' ?'#Replacing the question marks in the testing dataset with NA values
is.na(adult.train)<-adult.train==' ?'#Replacing the question marks in the training dataset with NA values
colnames(adult.train)<-columns
summary(adult.train) #Whenever workclass is missing ocuupation is missing as well
colnames(adult.train)[apply(is.na(adult.train),2,any)]#To find out column names with missing values

#Data cleaning
#Train Dataset
#Deleting education no and fnlwgt
adult.train<-subset(adult.train,select =-c(fnlwgt,EducationNo))#Removing 'fnlwgt' and 'EducationNo' variables
train<-adult.train
conversion<-c(2:8,12,13)
train[,conversion]<-lapply(train[,conversion],factor)
#Recoding factor levels in categorical variables
#workclass
train$workclass<-recode(train$workclass,'c(" Never-worked"," Without-pay")="Unemployed"')

#education
train$education<-recode(train$education,'c(" Preschool"," 1st-4th"," 7th-8th", " 9th", " 10th"," 5th-6th"," 11th"," 12th")="SchoolLevel"')
train$education<-recode(train$education,'c(" Doctorate"," Masters")="HigherEdu"')

#Marital Status
train$MaritalStatus<-recode(train$MaritalStatus,'c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent")="Married"')
train$MaritalStatus<-recode(train$MaritalStatus,'c(" Divorced"," Separated")="NotTogether"')

#Ocuupation
train$occupation<-recode(train$occupation,'c(" Armed-Forces"," Other-service")="Other-Services"')
train$occupation<-recode(train$occupation,'c(" Priv-house-serv"," Handlers-cleaners")=" Handlers-cleaners"')
train$occupation<-recode(train$occupation,'c(" Farming-fishing"," Handlers-cleaners"," Transport-moving"," Craft-repair"," Machine-op-inspct")="BlueCollar"')


#Race
train$race<-recode(train$race,"c(' Other',' Asian-Pac-Islander',' Amer-Indian-Eskimo')='Others'")


#Native Country
train$NativeCountry<-recode(train$NativeCountry,'c(" Scotland"," Ireland"," England")="Great-Britain"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Cambodia"," Laos"," Vietnam"," Hong"," Philippines"," Taiwan"," Thailand")="South-East-Asia"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Trinadad&Tobago"," Puerto-Rico"," Jamaica"," Dominican-Republic"," Haiti"," Cuba")="Caribbean-Islands"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Honduras"," El-Salvador"," Guatemala"," Nicaragua")="Central-America"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Columbia"," Ecuador"," Peru"," South")="South-America"')
train$NativeCountry<-recode(train$NativeCountry,'c(" China"," Japan"," India")="Asia"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Outlying-US(Guam-USVI-etc)"," United-States" )="United-States"')
train$NativeCountry<-recode(train$NativeCountry,'c(" France"," Germany"," Holand-Netherlands"," Hungary"," Italy"," Poland"," Portugal"," Yugoslavia"," Greece")="Eurasia"')


#Continuous variables
train<-subset(train,train$age<75)#First only include the data of people who's age isn;t above 74 in the dataset because that's the maximum age in the testing dataset and 99% of people's age in the training dataset is less than or equal to 74
train$age<-NULL #Then remove age variable as 35% of the data present in the test dataset is less than that of the minimum value present in the train dataset

#Test dataset

colnames(adult.test)[apply(is.na(adult.test),2,any)]#To find out column names with missing values
#Data cleaning
#Deleting education no and fnlwgt
adult.test<-subset(adult.test,select =-c(fnlwgt,EducationNo))
test<-adult.test
conversion<-c(2:8,12,13)
test[,conversion]<-lapply(test[,conversion],factor)

#Recoding categorical variables
#workclass
test$workclass<-recode(test$workclass,'c(" Never-worked"," Without-pay")="Unemployed"')

#Education
test$education<-recode(test$education,'c(" Preschool"," 1st-4th"," 7th-8th", " 9th", " 10th"," 5th-6th"," 11th"," 12th")="SchoolLevel"')
test$education<-recode(test$education,'c(" Doctorate"," Masters")="HigherEdu"')

#Marital Status
test$MaritalStatus<-recode(test$MaritalStatus,'c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent")="Married"')
test$MaritalStatus<-recode(test$MaritalStatus,'c(" Divorced"," Separated")="NotTogether"')

#Occupation
test$occupation<-recode(test$occupation,'c(" Armed-Forces"," Other-service")="Other-Services"')
test$occupation<-recode(test$occupation,'c(" Priv-house-serv"," Handlers-cleaners")=" Handlers-cleaners"')
test$occupation<-recode(test$occupation,'c(" Farming-fishing"," Handlers-cleaners"," Transport-moving"," Craft-repair"," Machine-op-inspct" )="BlueCollar"')

#Race
test$race<-recode(test$race,"c(' Other',' Asian-Pac-Islander',' Amer-Indian-Eskimo')='Others'")

#Native Country
test$NativeCountry<-recode(test$NativeCountry,'c(" Scotland"," Ireland"," England")="Great-Britain"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Cambodia"," Laos"," Vietnam"," Hong"," Philippines"," Taiwan"," Thailand")="South-East-Asia"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Trinadad&Tobago"," Puerto-Rico"," Jamaica"," Dominican-Republic"," Haiti"," Cuba")="Caribbean-Islands"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Honduras"," El-Salvador"," Guatemala"," Nicaragua")="Central-America"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Columbia"," Ecuador"," Peru"," South")="South-America"')
test$NativeCountry<-recode(test$NativeCountry,'c(" China"," Japan"," India")="Asia"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Outlying-US(Guam-USVI-etc)"," United-States" )="United-States"')
test$NativeCountry<-recode(test$NativeCountry,'c(" France"," Germany"," Holand-Netherlands"," Hungary"," Italy"," Poland"," Portugal"," Yugoslavia"," Greece")="Eurasia"')

#Continuous variables
test$age<-NULL

#Imputing Train dataset
temp_train_amelia<-train
amelia_train<-amelia(temp_train_amelia,m=5,noms=c(1,2,3,4,5,6,7,11),ords = c(8,9,10),idvars =c(12))

#Imputing test dataset
temp_test_amelia<-test
amelia_test<-amelia(temp_test_amelia,m=5,noms=c(1,2,3,4,5,6,7,11),ords = c(8,9,10),idvars=c(12))

#c5.0 model
amelia_c5<-C5.0(salary~.,amelia_train$imputations$imp3,trials=98)
amelia_c5_pred<-predict(amelia_c5,amelia_test$imputations$imp3)
confusionMatrix(as.numeric(amelia_c5_pred),as.numeric(test$salary))#Acc=87.07%


shinyServer(function(input, output,session) {
  
  output$results <- renderTable({
    req(input$CapitalLoss>=0 & input$CapitalGain>=0 & input$HoursPerWeek>=0)
    new_df<-data.frame(workclass=input$workclass,education=input$education,MaritalStatus=input$MaritalStatus,occupation=input$occupation,relationship=input$relationship,race=input$race,sex=input$sex,CapitalGain=input$CapitalGain,CapitalLoss=input$CapitalLoss,HoursPerWeek=input$HoursPerWeek,NativeCountry=input$NativeCountry)
    prediction<-predict(amelia_c5,newdata=new_df)
    new_df$salary<-prediction
    new_df
    
  })
})