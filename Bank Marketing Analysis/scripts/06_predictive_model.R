library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(pROC)

bank <- read.csv("data/bank-full.csv", sep = ";")

glimpse(bank)

model_data <- bank %>%
  mutate(
    y = factor(y, levels = c("no", "yes")),
    job = factor(job),
    marital = factor(marital),
    education = factor(education),
    default = factor(default),
    housing = factor(housing),
    loan = factor(loan),
    contact = factor(contact),
    month = factor(month),
    poutcome = factor(poutcome)
  )

table(model_data$y)

prop.table(table(model_data$y))

colSums(is.na(model_data))

# Selecting Predictors

model_data <- model_data %>% 
  select(
    y, 
    age,
    job, 
    marital, 
    education,
    default, 
    balance, 
    housing, 
    loan, 
    contact, 
    day, 
    month, 
    campaign, 
    pdays,
    previous,
    poutcome
  )

glimpse(model_data)

nrow(model_data)
ncol(model_data)

set.seed(123)

train_idx <- createDataPartition(
  model_data$y,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

dim(train_data)
dim(test_data)

prop.table(table(train_data$y))
prop.table(table(test_data$y))

# Logistic Regression Model
logis_model  <- glm(
  y ~ .,
  train_data,
  family = binomial
)

summary(logis_model)

exp(coef(logis_model))

pred <- predict(logis_model, test_data, type = "response")
head(pred)

# Default cutoff as 0.5
pred_class <- ifelse(pred > 0.5, "yes", "no") 
pred_class <- factor(pred_class, levels = c("no", "yes"))
table(pred_class)
confusionMatrix(pred_class, test_data$y, positive = "yes")

# Default cutoff as 0.2
# Recall is important ro identify customers who are likely to subscribe for banks to target likely customers.
pred_class20 <- ifelse(pred > 0.20, "yes", "no")
pred_class20 <- factor(pred_class20, levels = c("no", "yes"))
confusionMatrix(pred_class20, test_data$y, positive = "yes")

# Decision Tree
tree_model <- rpart(
  y ~ .,
  data = train_data,
  method = "class"
)

tree_model

tree_pred <- predict(tree_model, test_data, type = "class")
confusionMatrix(tree_pred, test_data$y, positive = "yes")

# Increasing Depth of Decision Tree
tree_model2 <- rpart(
  y ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(maxdepth = 5)
)

tree_pred2 <- predict(tree_model2, test_data, type = "class")
confusionMatrix(tree_pred2, test_data$y, positive = "yes")

# Random Forest Model with 50% threshold
randFor_model <- randomForest(
  y ~ .,
  data = train_data, 
  ntree = 300
)

randFor_pred <- predict(randFor_model, test_data)
confusionMatrix(randFor_pred, test_data$y, positive = "yes")

# Random Forest with 20% threshold
# recall is 54.68 % i.e this many customers actually subscribe
randFor_prob <- predict(randFor_model, test_data, type = "prob")[, "yes"]

randFor_pred20 <- ifelse(randFor_prob > 0.20, "yes", "no")
randFor_pred20 <- factor(
  randFor_pred20,
  levels = c("no", "yes")
)
confusionMatrix(randFor_pred20, test_data$y, positive = "yes")

# Using ROC-AUC to compare Logistic Regression and Random Forest
ranfor_roc <- roc(test_data$y, randFor_prob)
auc(ranfor_roc)

logis_roc <- roc(test_data$y, pred)
auc(logis_roc)

models_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  Accuracy = c(0.8722, 0.8926, 0.8614),
  Recall = c(0.4314, 0.1741, 0.5468),
  Precision = c(0.4515, 0.6525, 0.4275),
  ROC_AUC = c(0.7673, NA, 0.7922)
)

models_comparison

# Ranking most important variables for prediction
importance(randFor_model)
varImpPlot(randFor_model)

importance_table <- data.frame(
  var = rownames(importance(randFor_model)),
  Importance = importance(randFor_model)[, "MeanDecreaseGini"]
)

importance_table <- importance_table[
  order(-importance_table$Importance)
]

importance_table

# Balance, month, age and day are among important predictors in the Random Forest feastures importance.