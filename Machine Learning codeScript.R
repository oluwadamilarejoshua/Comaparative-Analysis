# install.packages('Boruta')


# Loading required libraries ----------------------------------------------

library(readxl)
library(Boruta)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(caTools)
library(e1071)


# Loading the data tables into the workspace ------------------------------

dress_attributes <- as.data.frame(read_excel(
  "Machine Learning/Dresses_Attribute_Sales/Dresses_Attribute_Sales/Attribute DataSet.xlsx"))
dress_sales <- as.data.frame(read_excel(
  "Machine Learning/Dresses_Attribute_Sales/Dresses_Attribute_Sales/Extra files/Dress Sales.xlsx"))
str(dress_attributes)
str(dress_sales)


# Cleaning the names of the dress sales table -----------------------------

salesDay <- vector(length = ncol(dress_sales) - 1)

for (i in 1:length(salesDay)) {
  salesDay[i] <- paste('Day', i, sep = "_")
}

colnames(dress_sales) <- c(colnames(dress_sales)[1], salesDay)

head(dress_attributes)
head(dress_sales)


# Joining of the data tables into a single table --------------------------

fullDressData <- cbind(dress_sales[, -1], dress_attributes[, -1])
head(fullDressData)


names(fullDressData) <- c(colnames(fullDressData)[1:34], "Pattern_Type", "Recommendation")
str(fullDressData)


# Cleaning the data types -------------------------------------------------

for (i in 1:23) {
  if(is.character(fullDressData[, i])){
    fullDressData[, i] <- as.numeric(fullDressData[, i])
  }
}

for (i in 1:ncol(fullDressData)) {
  if(is.character(fullDressData[, i])){
    fullDressData[, i] <- as.factor(fullDressData[, i])
  }
}

fullDressData$Recommendation <- as.factor(fullDressData$Recommendation)

dim(fullDressData)

fullDressData <- na.omit(fullDressData)

Price <- as.character(fullDressData$Price)
str(Price)

for (i in 1:length(Price)) {
  if(Price[i] == "high"){
    Price[i] <- "High"
  }else if(Price[i] == "low"){
    Price[i] <- "Low"
  }
}

Patter_Type <- as.character(fullDressData$Pattern_Type)
table(Patter_Type)
fullDressData$Patter_Type <- Patter_Type
fullDressData$Patter_Type <- as.factor(fullDressData$Patter_Type)
str(fullDressData$Patter_Type)

table(Price)

fullDressData$Price <- Price
fullDressData$Price <- as.factor(fullDressData$Price)
str(fullDressData$Price)

# Boruta for feature selection --------------------------------------------

boruta_select <- Boruta(Recommendation ~ ., data = fullDressData)
useful_attributes <- getSelectedAttributes(boruta_select, withTentative = T)
useful_attributes
plot(boruta_select, cex.axis = .7, las = 2, xlab = "", main = "Feature Importance")
str(useful_attributes)

data_for_modelling <- fullDressData %>% select(all_of(useful_attributes))
head(data_for_modelling)


data_for_modelling <- cbind(data_for_modelling,
                            fullDressData$Recommendation)
str(data_for_modelling)

theNames <- c(names(data_for_modelling)[-11],
                               'Recommendation')

names(data_for_modelling) <- theNames
str(data_for_modelling)

# Data Spliting for modelling and testing ---------------------------------

train_fraction <- floor(0.85 * nrow(data_for_modelling))
set.seed(1234)
selection_guide <- sample(x = seq_len(nrow(data_for_modelling)), 
                          size = train_fraction)

train_set <- data_for_modelling[selection_guide, ]
test_set <- data_for_modelling[-selection_guide, ]

str(train_set)
str(train_set$Recommendation)
table(train_set$Pattern_Type)

# First ML algorithm (Generalized Logistic Regression) --------------------

logit_model <- glm(Recommendation ~ ., data = train_set, family = 'binomial')
mod_sum <- summary(logit_model)

# write.csv(mod_sum$coefficients, file = "Machine Learning/logitCoeff.csv")

logit_prediction <- predict(logit_model,
                            test_set[, -11],
                            type = "response")

logit_prediction_class <- as.factor(ifelse(logit_prediction > 0.57, 1, 0))

str(logit_prediction_class)

logit_confusion_matrix <- confusionMatrix(data = logit_prediction_class,
                                          reference = test_set$Recommendation)
logit_confusion_matrix

# Second ML algorithm (Decision Trees) ------------------------------------

decision_trees_model <- rpart(Recommendation ~ ., data = train_set)
# summary(decision_trees_model)
rpart.plot(decision_trees_model)

decision_trees_prediction <- predict(decision_trees_model,
                            test_set[, -11],
                            type = "class")

decision_tress_confusion_matrix <- 
  confusionMatrix(decision_trees_prediction, 
                  reference = test_set$Recommendation)
decision_tress_confusion_matrix


# Third ML Algorithm (Support Vector Machine) -----------------------------

svm_model <- svm(Recommendation ~ .,
                 data = train_set,
                 type = 'C-classification',
                 kernel = 'linear')

svm_model

svm_prediction <- predict(svm_model,
                          test_set[, -11],
                          type = "class")

svm_confusion_matrix <- confusionMatrix(svm_prediction, 
                                        reference = test_set$Recommendation)
svm_confusion_matrix
