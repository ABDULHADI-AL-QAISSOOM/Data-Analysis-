
# Draft3-8-DEC

# first installing all needed packages:

install.packages("readr")      # Reading data
install.packages("psych")      # Describing data
install.packages("broom")      # View regression results in a different way
install.packages("stargazer")  # Exporting regression results
install.packages("car")        # Post-estimation plotting
install.packages("tidyverse")  # Data Wrangling
install.packages("magrittr")   # To use Pipe operator 
install.packages("caret")      # K-fold cross validation
install.packages("rio")        # Data import/export
install.packages("ggplot2")    # Data visualization
install.packages("reshape2")    # Reshaping data
install.packages("fastDummies") # Creating dummy variables
install.packages("carData")    # Datasets for the car package
install.packages("dplyr")      # Data manipulation
install.packages("class")      # Functions for Classification
install.packages("factoextra")  # Clustering package
install.packages("gridExtra")   # "Grid" Graphics
install.packages("ggdendro")    # Dendrogram visualization

library(readr)     # Reading data
library(psych)     # Describing data
library(broom)     # View regression results in a different way
library(stargazer) # Exporting regression results
library(car)       # Post-estmiation plotting
library(tidyverse) # Data Wrangling
library(magrittr)  # To use Pipe operator 
library(caret)     # K-fold cross validation
library(rio)
library(ggplot2)
library(reshape2)
library(fastDummies)
library(car)
library(carData)
library(dplyr)
library(class)     # Functions for Classification
library(factoextra) # Clustering package
library(gridExtra)  # "Grid" Graphics
library(ggdendro)   # Create Dendrograms





#reading the data file:
AbsEmployees_Data <- read_csv("AbsEmployees.csv")


                                  #################DATA WRANGLING################
# describing & understanding the data structure
head(AbsEmployees_Data)
glimpse(AbsEmployees_Data)
describe(AbsEmployees_Data, na.rm = T)
summary(AbsEmployees_Data)    #we have obs with age less than 18, and more than 65
count(AbsEmployees_Data, Gender)  #we have unbiased gender selection, can be used as a dummy variable
table(AbsEmployees_Data$Division)
table(AbsEmployees_Data$BusinessUnit) #can be use as a dummy varible
table(AbsEmployees_Data$DepartmentName)
table(AbsEmployees_Data$StoreLocation)
table(AbsEmployees_Data$JobTitle)  #why there is so many chashiers
table(AbsEmployees_Data$Surname) #is there any thing wierd?

########DATA CLEANING

#having Employees with Ages only (18-65)
AbsEmployees_Data <- subset(AbsEmployees_Data, Age>=18 & Age <= 65)
view(AbsEmployees_Data)
summary(AbsEmployees_Data)
#Removing Employees that have lengthService more than their age
AbsEmployees_Data %>%
  mutate(theo_age= Age - LengthService) %>%
  filter(theo_age<18)

theo_age= AbsEmployees_Data$Age - AbsEmployees_Data$LengthService
AbsEmployees_Data <- subset(AbsEmployees_Data, theo_age>=18)
#checking for null values
sum(is.na(AbsEmployees_Data))
#collapsing similar StoreLocation
AbsEmployees_Data %>%
  count(StoreLocation) %>%
  print(n=45)

Westminster_Categories <- c("New Westminister")
Vancouver_Categories <- c("North Vancouver", "West Vancouver")
StoreLocation_Collapsed <- fct_collapse(AbsEmployees_Data$StoreLocation,
                                        Vancouver=Vancouver_Categories,
                                        "New Westminster"=Westminster_Categories)

AbsEmployees_Data <- AbsEmployees_Data %>%
  mutate(StoreLocation_Collapsed = as.character(StoreLocation_Collapsed))  
#Converting Gender, DepartmentName, Division and  Business Unit to factors
# Gender
summary(AbsEmployees_Data$Gender)

AbsEmployees_Data <- AbsEmployees_Data %>%
  mutate(Gender = as.factor(as.character(Gender)))
is.factor(AbsEmployees_Data$Gender)
summary(AbsEmployees_Data$Gender)
# DepartmentName
AbsEmployees_Data <- AbsEmployees_Data %>%
  mutate(DepartmentName = as.factor(as.character(DepartmentName)))
is.factor(AbsEmployees_Data$DepartmentName)
summary(AbsEmployees_Data$DepartmentName)
# Division
AbsEmployees_Data <- AbsEmployees_Data %>%
  mutate(Division = as.factor(as.character(Division)))
is.factor(AbsEmployees_Data$Division)
summary(AbsEmployees_Data$Division)
# Business Unit
AbsEmployees_Data <- AbsEmployees_Data %>%
  mutate(BusinessUnit = as.factor(as.character(BusinessUnit)))
is.factor(AbsEmployees_Data$BusinessUnit)
summary(AbsEmployees_Data$BusinessUnit)
#Checking for full & partial duplicates
sum(duplicated(AbsEmployees_Data))

AbsEmployees_Data %>%
  count(Surname, GivenName) %>%
  filter(n>1)


######Descriptive Analytics

glimpse(AbsEmployees_Data)
#Arranging Employees with most Absent Hours
AbsEmployees_Data %>%
  arrange(desc(AbsentHours))

AbsEmployees_Data %>%
  filter(DepartmentName == 'Dairy') %>%
  arrange(desc(AbsentHours))
#Total number of employees in each department
TotalEmpDepartment<- AbsEmployees_Data %>%
  group_by(DepartmentName) %>%
  summarise(totalEmp= n_distinct(EmployeeNumber))
ggplot(TotalEmpDepartment, aes(x=totalEmp, y=DepartmentName))+geom_col()
print(TotalEmpDepartment, n=21)
#Sum Absent Hours in each all  Department
SumAbsDepartments<- AbsEmployees_Data %>% 
  group_by(DepartmentName)%>%
  summarize(SumAbsentHours= sum(AbsentHours)) %>%
  arrange(desc(SumAbsentHours))
ggplot(SumAbsDepartments, aes(x=SumAbsentHours , y= DepartmentName)) + geom_col()
# Mean Absent Hours in each all Departments
MeanDepartments<- AbsEmployees_Data %>% 
  group_by(DepartmentName)%>%
  summarize(MeanAbsentHours= mean(AbsentHours)) %>%
  arrange(desc(MeanAbsentHours))
ggplot(MeanDepartments, aes(x=MeanAbsentHours , y= DepartmentName)) + geom_col()
# mean AbsentHours in each StoreLocation (collapsed)
MeanAbsStores<- AbsEmployees_Data %>% 
  group_by(StoreLocation_Collapsed)%>%
  summarize(MeanAbsentHours= mean(AbsentHours)) %>%
  arrange(desc(MeanAbsentHours))   
ggplot(MeanAbsStores, aes(x=MeanAbsentHours , y= StoreLocation_Collapsed)) + geom_col()
# total AbsentHours in each StoreLocation (collapsed)
SumAbsStores<- AbsEmployees_Data %>% 
  group_by(StoreLocation_Collapsed)%>%
  summarize(SumAbsentHours= sum(AbsentHours)) %>%
  arrange(desc(SumAbsentHours))   
ggplot(SumAbsStores, aes(x=SumAbsentHours , y= StoreLocation_Collapsed)) + geom_col()
view(SumAbsStores)
# Visualizing the frequency of Stores in each Location
Location_Counts<-count(AbsEmployees_Data, StoreLocation_Collapsed)
ggplot(Location_Counts,aes(n,StoreLocation_Collapsed, las=1))+geom_bar(stat="identity")
describe(Location_Counts$n)
#Boxplot for Absent Hours in Head Office (Departments)
BusinessUnit_Head <- AbsEmployees_Data %>%
  filter(BusinessUnit == 'HeadOffice')
ggplot(BusinessUnit_Head, aes(x=DepartmentName, y=AbsentHours)) +
  geom_boxplot()+scale_y_log10()
#Boxplot for Absent Hours in Head Office (Length Service)
ggplot(BusinessUnit_Head, aes(x=LengthService, y=AbsentHours)) +
  geom_boxplot()+scale_y_log10()
#Boxplot for Absent Hours in Head Office (Age)
ggplot(BusinessUnit_Head, aes(x=Age, y=AbsentHours)) +
  geom_boxplot()+scale_y_log10()
#Boxplot for Absent Hours in Head Office (Gender)
ggplot(BusinessUnit_Head, aes(x=Gender, y=AbsentHours)) +
  geom_boxplot()+scale_y_log10()
#Boxplot for Absent Hours in Stores Departments
BusinessUnit_Stores <- AbsEmployees_Data %>%
  filter(BusinessUnit == 'Stores') %>%
  group_by(DepartmentName)
ggplot(BusinessUnit_Stores, aes(x= DepartmentName,y=AbsentHours ))+
  geom_boxplot()+scale_y_log10()
# showing the relationship between the age and Length service and absent hours
# in all data
ggplot(AbsEmployees_Data, aes(x =LengthService , y = AbsentHours, color=Age )) +
  geom_point()

# in all
ggplot(AbsEmployees_Data, aes(x = Age, y = AbsentHours, color=LengthService)) +
  geom_point()
# in Head Office only
ggplot(BusinessUnit_Head, aes(x = Age, y = AbsentHours, color=LengthService)) +
  geom_point()
# in Stores only
ggplot(BusinessUnit_Stores, aes(x = Age, y = AbsentHours, color=LengthService)) +
  geom_point()

ggplot(BusinessUnit_Stores, aes(x = Age, y = AbsentHours)) +
  geom_point()

# showing the relationship between the length service, absent hours, Division, Gender
ggplot(AbsEmployees_Data, aes(x = LengthService, y = AbsentHours, color=Division)) +
  geom_point() +facet_wrap(~Division) 

ggplot(AbsEmployees_Data, aes(x= LengthService, y=AbsentHours, color= Gender )) + 
  geom_point() +facet_wrap(~Division) 

#facet wrap by BusinessUnit
ggplot(AbsEmployees_Data, aes(x = LengthService, y = AbsentHours, color=Division)) +
  geom_point() +facet_wrap(~BusinessUnit) 
#facet wrap by job tilte
ggplot(AbsEmployees_Data, aes(x = LengthService, y = AbsentHours, color=JobTitle)) +
  geom_point() +facet_wrap(~BusinessUnit) 

#Correlation Matrix for all Numarical data:
AbsEmp_Num <- AbsEmployees_Data[c("Age","LengthService","AbsentHours")] # Create a subset with Numerical variables only
cor(AbsEmp_Num, use = "complete.obs")

CorMatrix <- cor(AbsEmp_Num, use = "complete.obs") 
meltCorMatrix <- melt(CorMatrix)                      
ggplot(data = meltCorMatrix,                          
       aes(x = Var1, y = Var2, fill = value)) + geom_tile() + scale_fill_gradient(low = "white", high = "blue")
#Correlation for Age and Absent hours
cov(AbsEmployees_Data$AbsentHours, AbsEmployees_Data$Age, use = "complete.obs")
plot(AbsEmployees_Data$Age,AbsEmployees_Data$AbsentHours, use = "complete.obs")
ggplot(AbsEmployees_Data, aes(x = Age, y = AbsentHours)) +
  geom_point()
#Correlation for Age and Absent hours in HeadOffice only
plot(BusinessUnit_Head$Age,BusinessUnit_Head$AbsentHours, use = "complete.obs")

#Correlation for LengthService and Absent hours in HeadOffice only
HeadOffice_Cor <- BusinessUnit_Head[c("LengthService","AbsentHours")]
cor(HeadOffice_Cor, use = "complete.obs")
plot(HeadOffice_Cor$LengthService, HeadOffice_Cor$AbsentHours, use = "complete.obs")

#Continengency table to test the autocorrelation between the 2 catigorical varibles
con1<- table(AbsEmployees_Data$BusinessUnit, AbsEmployees_Data$Gender)
mosaicplot(con1)
chisq.test(con1)


summary(BusinessUnit_Head)
summary(BusinessUnit_Stores)
count(BusinessUnit_Stores, Gender)

                                ##############RGRESSION MODEL BUILDING###############

#Regression Model1: Stores; Age only 
ggplot(BusinessUnit_Stores, aes(x=Age, y=AbsentHours))+ geom_point()+geom_smooth(method = "lm")
myLm1 <- lm(AbsentHours ~ Age, data = BusinessUnit_Stores)
summary(myLm1)
#Regression Model2: Stores; Age & Length Service 
myLm2 <- lm(AbsentHours ~ Age+LengthService, data = BusinessUnit_Stores)
summary(myLm2) #the length service is not significant
vif(myLm2) #VIF to detect autocorrelation between Age & Length Service
#Regression Model3: Stores;  Age, Gender
myLm3 <- lm(AbsentHours ~ Age + Gender , data = BusinessUnit_Stores)
summary(myLm3) # R-adj=0.7314
vif(myLm3) #VIF to detect autocorrelation between Age, Length Service, Gender
par(mfrow = c(2, 2))
plot(myLm3)
#Regression Model4:Stores; Age, Length Service, Gender & Store Location
myLm4 <- lm(AbsentHours ~ Age + Gender + StoreLocation_Collapsed, data = BusinessUnit_Stores)
summary(myLm4) #R-adj= 0.7317, store locations insginficant
vif(myLm4) #VIF to detect autocorrelation between Age, Length Service, Gender & Store Location
par(mfrow = c(2, 2))
plot(myLm4)


#Regression Model5: HeadOffice; Age
ggplot(BusinessUnit_Head, aes(x=Age, y=AbsentHours))+ geom_point()+geom_smooth(method = "lm")
myLm5 <- lm(AbsentHours ~ Age , data = BusinessUnit_Head)
summary(myLm5) #Age and Absent Hours doesn't have a linear relatioship
##Regression Model6: HeadOffice; Age and LengthService
myLm6 <- lm(AbsentHours ~ LengthService+Age , data = BusinessUnit_Head)
summary(myLm6)
vif(myLm6)
##Regression Model7: HeadOffice; Age, LengthService and Gender 
myLm7 <- lm(AbsentHours ~ LengthService+Age+Gender , data = BusinessUnit_Head)
summary(myLm7)
tidy(myLm7) #Intercept is insignificant
vif(myLm7)
##Regression Model8: HeadOffice;  Age, LengthService and Division 
myLm8 <- lm(AbsentHours ~ LengthService+Age+Division , data = BusinessUnit_Head)
summary(myLm8) 
tidy(myLm8)#Division is insignificant
vif(myLm8) 
##Regression Model9: HeadOffice;  Age, LengthService and JobTitle
myLm9 <- lm(AbsentHours ~ LengthService+Age+JobTitle, data = BusinessUnit_Head)
summary(myLm9) #JobTitle is insignificant
tidy(myLm9)
vif(myLm9)
##Regression Model10: HeadOffice;  Age, LengthService and DepartmentName
myLm10 <- lm(AbsentHours ~ LengthService+Age+DepartmentName, data = BusinessUnit_Head)
summary(myLm10)
tidy(myLm10)
vif(myLm10)
##Regression Model11: HeadOffice; Age, LengthService and Gender (without interceprt)
myLm11 <- lm(AbsentHours ~ 0+LengthService+Age+Gender , data = BusinessUnit_Head)
summary(myLm11) #R-adj=0.6841
tidy(myLm11)
vif(myLm11)
par(mfrow = c(2, 2))
plot(myLm11)
##Regression Model12: HeadOffice; Age, LengthService  (without interceprt)
myLm12 <- lm(AbsentHours ~ 0+LengthService+Age , data = BusinessUnit_Head)
summary(myLm12) #R-adj=0.6717
tidy(myLm12)
vif(myLm12)
par(mfrow = c(2, 2))
plot(myLm12)
residualPlots(myLm12)


#######Models Modifications
##identifying outliers for best model in Stores

myLm3.stdres=rstandard(myLm3) # Standardized residual
##Normal Quantile to Quantile plot
qqnorm(myLm3.stdres,   
       ylab="Standardized Residuals",
       xlab="Normal Scores",
       main=" Normal Probability Plot of Standardized Residuals"         )

# If absolute std residula >2
myLm3.stdres[abs(myLm3.stdres) > 2]
# If absolute std residula >3
myLm3.stdres[abs(myLm3.stdres) > 3]

# Leaverage 
hatvalues(myLm3)  # Calculate the leverage values
## Cutoff value = (2k+2)/n = 0.02
hatvalues(myLm3)[hatvalues(myLm3) > 0.02]

#CooksDi
cooksDi3 <- cooks.distance(myLm3)
influential3 <- cooksDi3[(cooksDi3 > (3 * mean(cooksDi3, na.rm = TRUE)))]
influential3

names_of_influential3 <- names(influential3)
outliers3 <- BusinessUnit_Stores[names_of_influential3,]
Stores_without_outliers <- BusinessUnit_Stores %>% anti_join(outliers3)

#Regression Model13: Age, Gender 
myLm13 <- lm(AbsentHours ~ Age + Gender , data = Stores_without_outliers)
summary(myLm13) #R-adj= 0.7809,


##identifying outliers for best model in Head office
myLm11.stdres=rstandard(myLm11) # Standardized residual
##Normal Quantile to Quantile plot
qqnorm(myLm11.stdres,   
       ylab="Standardized Residuals",
       xlab="Normal Scores",
       main=" Normal Probability Plot of Standardized Residuals"         )

# If absolute std residula >2
myLm11.stdres[abs(myLm11.stdres) > 2]
# If absolute std residula >3
myLm11.stdres[abs(myLm11.stdres) > 3]

# Leaverage 
hatvalues(myLm11)  # Calculate the leverage values
## Cutoff value = (2k+2)/n = 0.02
hatvalues(myLm11)[hatvalues(myLm11) > 0.02]

#CooksDi
cooksDi11 <- cooks.distance(myLm11)
influential11 <- cooksDi11[(cooksDi11 > (3 * mean(cooksDi11, na.rm = TRUE)))]
influential11

names_of_influential <- names(influential11)
outliers <- BusinessUnit_Head[names_of_influential,]
Head_without_outliers <- BusinessUnit_Head %>% anti_join(outliers)

##Modified Regression Model14: HeadOffice; Age, LengthService and Gender (without interceprt)
myLm14 <- lm(AbsentHours ~ 0+LengthService+Age+Gender , data = Head_without_outliers)
summary(myLm14) #R-adj=0.7444
tidy(myLm14)


####K-fold cross validation
#Business Unit: Stores - Model13
sample_set <- createDataPartition(y = Stores_without_outliers$AbsentHours, p = .75, list = FALSE)

# Creating training & testing datasets:
Stores_without_outliers_train <- Stores_without_outliers[sample_set, ]
Stores_without_outliers_test <- Stores_without_outliers[-sample_set, ]

set.seed(1234)
myLm13_Training1 <- train(
  AbsentHours ~ Age + Gender ,
  data = Stores_without_outliers,
  metric = "Rsquared",
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

print(myLm13_Training1)
summary(myLm13_Training1)


# Pass both the chosen model and the test data to the predict()
myLm13_Testing1 <- predict(myLm13_Training1, Stores_without_outliers_test)

# Compute the R-squared & RMSE of the testing dataset to assess performance of the model.
SSE = sum((Stores_without_outliers_test$AbsentHours - myLm13_Testing1)^2)
SST = sum((Stores_without_outliers_test$AbsentHours - mean(Stores_without_outliers_train$AbsentHours)) ^2)
R_square = 1 - SSE/SST
RMSE = sqrt(SSE/length(myLm13_Testing1))

message('R_squared on the test data:')
R_square
message("Root mean square error on the test data: ")
RMSE



#Business Unit: HeadOffice - Model14
sample_set <- createDataPartition(y = Head_without_outliers$AbsentHours, p = .75, list = FALSE)

# Creating training & testing datasets:
Head_without_outliers_train <- Head_without_outliers[sample_set, ]
Head_without_outliers_test <- Head_without_outliers[-sample_set, ]

set.seed(1234)
myLm14_Training1 <- train(
  AbsentHours ~ LengthService + Age + Gender ,
  data = Head_without_outliers,
  metric = "Rsquared",
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

print(myLm14_Training1)
summary(myLm14_Training1)

# Pass both the chosen model and the test data to the predict()
myLm14_Testing1 <- predict(myLm14_Training1, Head_without_outliers_test)

# Compute the R-squared & RMSE of the testing dataset to assess performance of the model.
SSE = sum((Head_without_outliers_test$AbsentHours - myLm14_Testing1)^2)
SST = sum((Head_without_outliers_test$AbsentHours - mean(Head_without_outliers_train$AbsentHours)) ^2)
R_square = 1 - SSE/SST
RMSE = sqrt(SSE/length(myLm14_Testing1))

message('R_squared on the test data:')
R_square
message("Root mean square error on the test data: ")
RMSE



                                #################K-NN METHOD##################    
#Model 1: try k=8
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

HeadOffice_Data_knn <- Head_without_outliers %>%
  mutate(Age = normalize(Age)) %>%
  mutate(LengthService = normalize(LengthService))

HeadOffice_Data_knn <- HeadOffice_Data_knn %>%
  select(Age, LengthService, AbsentHours)

HeadOffice_Data_knn <- data.frame(HeadOffice_Data_knn)

HeadOffice_Data_knn_labels <- HeadOffice_Data_knn %>% select(AbsentHours)
HeadOffice_Data_knn <- HeadOffice_Data_knn %>% select(-AbsentHours)

set.seed(1234)
sample_index <- sample(nrow(HeadOffice_Data_knn), round(nrow(HeadOffice_Data_knn) * .75), replace = FALSE)
HeadOffice_Data_knn_train <- HeadOffice_Data_knn[sample_index,]
HeadOffice_Data_knn_test <- HeadOffice_Data_knn[-sample_index,]

HeadOffice_Data_knn_labels_train <- as.factor(HeadOffice_Data_knn_labels[sample_index,])
HeadOffice_Data_knn_labels_test <- as.factor(HeadOffice_Data_knn_labels[-sample_index,])

knn_pred1 <-
  knn(
    train = HeadOffice_Data_knn_train,
    test = HeadOffice_Data_knn_test,
    cl = HeadOffice_Data_knn_labels_train,
    k = 8
  )


knn_pred1_table <- table(HeadOffice_Data_knn_labels_test, knn_pred1)
knn_pred1_table

sum(diag(knn_pred1_table)) / nrow(HeadOffice_Data_knn_test)


#Model 2: try k=10
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

HeadOffice_Data_knn <- Head_without_outliers %>%
  mutate(Age = normalize(Age)) %>%
  mutate(LengthService = normalize(LengthService))

HeadOffice_Data_knn <- HeadOffice_Data_knn %>%
  select(Age, LengthService, AbsentHours)

HeadOffice_Data_knn <- data.frame(HeadOffice_Data_knn)

HeadOffice_Data_knn_labels <- HeadOffice_Data_knn %>% select(AbsentHours)
HeadOffice_Data_knn <- HeadOffice_Data_knn %>% select(-AbsentHours)

set.seed(1234)
sample_index <- sample(nrow(HeadOffice_Data_knn), round(nrow(HeadOffice_Data_knn) * .75), replace = FALSE)
HeadOffice_Data_knn_train <- HeadOffice_Data_knn[sample_index,]
HeadOffice_Data_knn_test <- HeadOffice_Data_knn[-sample_index,]

HeadOffice_Data_knn_labels_train <- as.factor(HeadOffice_Data_knn_labels[sample_index,])
HeadOffice_Data_knn_labels_test <- as.factor(HeadOffice_Data_knn_labels[-sample_index,])

knn_pred1 <-
  knn(
    train = HeadOffice_Data_knn_train,
    test = HeadOffice_Data_knn_test,
    cl = HeadOffice_Data_knn_labels_train,
    k = 10
  )


knn_pred1_table <- table(HeadOffice_Data_knn_labels_test, knn_pred1)
knn_pred1_table

sum(diag(knn_pred1_table)) / nrow(HeadOffice_Data_knn_test)


#Model 3: try k=12
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

HeadOffice_Data_knn <- Head_without_outliers %>%
  mutate(Age = normalize(Age)) %>%
  mutate(LengthService = normalize(LengthService))

HeadOffice_Data_knn <- HeadOffice_Data_knn %>%
  select(Age, LengthService, AbsentHours)

HeadOffice_Data_knn <- data.frame(HeadOffice_Data_knn)

HeadOffice_Data_knn_labels <- HeadOffice_Data_knn %>% select(AbsentHours)
HeadOffice_Data_knn <- HeadOffice_Data_knn %>% select(-AbsentHours)

set.seed(1234)
sample_index <- sample(nrow(HeadOffice_Data_knn), round(nrow(HeadOffice_Data_knn) * .75), replace = FALSE)
HeadOffice_Data_knn_train <- HeadOffice_Data_knn[sample_index,]
HeadOffice_Data_knn_test <- HeadOffice_Data_knn[-sample_index,]

HeadOffice_Data_knn_labels_train <- as.factor(HeadOffice_Data_knn_labels[sample_index,])
HeadOffice_Data_knn_labels_test <- as.factor(HeadOffice_Data_knn_labels[-sample_index,])

knn_pred1 <-
  knn(
    train = HeadOffice_Data_knn_train,
    test = HeadOffice_Data_knn_test,
    cl = HeadOffice_Data_knn_labels_train,
    k = 12
  )


knn_pred1_table <- table(HeadOffice_Data_knn_labels_test, knn_pred1)
knn_pred1_table

sum(diag(knn_pred1_table)) / nrow(HeadOffice_Data_knn_test)


#Model 4: try k=15
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

HeadOffice_Data_knn <- Head_without_outliers %>%
  mutate(Age = normalize(Age)) %>%
  mutate(LengthService = normalize(LengthService))

HeadOffice_Data_knn <- HeadOffice_Data_knn %>%
  select(Age, LengthService, AbsentHours)

HeadOffice_Data_knn <- data.frame(HeadOffice_Data_knn)

HeadOffice_Data_knn_labels <- HeadOffice_Data_knn %>% select(AbsentHours)
HeadOffice_Data_knn <- HeadOffice_Data_knn %>% select(-AbsentHours)

set.seed(1234)
sample_index <- sample(nrow(HeadOffice_Data_knn), round(nrow(HeadOffice_Data_knn) * .75), replace = FALSE)
HeadOffice_Data_knn_train <- HeadOffice_Data_knn[sample_index,]
HeadOffice_Data_knn_test <- HeadOffice_Data_knn[-sample_index,]

HeadOffice_Data_knn_labels_train <- as.factor(HeadOffice_Data_knn_labels[sample_index,])
HeadOffice_Data_knn_labels_test <- as.factor(HeadOffice_Data_knn_labels[-sample_index,])

knn_pred1 <-
  knn(
    train = HeadOffice_Data_knn_train,
    test = HeadOffice_Data_knn_test,
    cl = HeadOffice_Data_knn_labels_train,
    k = 15
  )


knn_pred1_table <- table(HeadOffice_Data_knn_labels_test, knn_pred1)
knn_pred1_table

sum(diag(knn_pred1_table)) / nrow(HeadOffice_Data_knn_test)


#Model 5: try k=17
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

HeadOffice_Data_knn <- Head_without_outliers %>%
  mutate(Age = normalize(Age)) %>%
  mutate(LengthService = normalize(LengthService))

HeadOffice_Data_knn <- HeadOffice_Data_knn %>%
  select(Age, LengthService, AbsentHours)

HeadOffice_Data_knn <- data.frame(HeadOffice_Data_knn)

HeadOffice_Data_knn_labels <- HeadOffice_Data_knn %>% select(AbsentHours)
HeadOffice_Data_knn <- HeadOffice_Data_knn %>% select(-AbsentHours)

set.seed(1234)
sample_index <- sample(nrow(HeadOffice_Data_knn), round(nrow(HeadOffice_Data_knn) * .75), replace = FALSE)
HeadOffice_Data_knn_train <- HeadOffice_Data_knn[sample_index,]
HeadOffice_Data_knn_test <- HeadOffice_Data_knn[-sample_index,]

HeadOffice_Data_knn_labels_train <- as.factor(HeadOffice_Data_knn_labels[sample_index,])
HeadOffice_Data_knn_labels_test <- as.factor(HeadOffice_Data_knn_labels[-sample_index,])

knn_pred1 <-
  knn(
    train = HeadOffice_Data_knn_train,
    test = HeadOffice_Data_knn_test,
    cl = HeadOffice_Data_knn_labels_train,
    k = 17
  )


knn_pred1_table <- table(HeadOffice_Data_knn_labels_test, knn_pred1)
knn_pred1_table

sum(diag(knn_pred1_table)) / nrow(HeadOffice_Data_knn_test)








