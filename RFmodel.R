
# Data Preparation
# Required Packages
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

# Data Cleaning
breastc_cleaned <- breastc %>% 
  mutate(breastc, 
         age = as.numeric(age),
         meno = as.factor(meno),
         size = as.numeric(size),
         grade = as.numeric(grade),
         nodes = as.numeric(nodes),
         pgr = as.numeric(pgr),
         er = as.numeric(er),
         hormon = as.numeric(hormon),
         rfstime = as.numeric(rfstime),
         status = as.factor(status)) %>% 
  select( age, meno, size, grade, nodes, pgr,
          er, hormon, rfstime, status)
levels(breastc_cleaned$status)[levels(breastc_cleaned$status)=="0"] <- "alive"
levels(breastc_cleaned$status)[levels(breastc_cleaned$status)=="1"] <- "expired"
View(breastc_cleaned)

# Training and Testing Data Sets
set.seed(123)
rftrainIndex <- createDataPartition(y = breastc_cleaned$status,
                                     p = 0.7, list  = FALSE)
rftrain <- breastc_cleaned[trainIndex, ]
rftest <- breastc_cleaned[-trainIndex, ]

# Model Fit
forest <- randomForest(status~., data = rftrain)
forest

# Assessment for Optimal Parameters
oob.error.data <- data.frame(
  trees = rep(1:nrow(forest$err.rate), times = 3),
  type = rep(c("OOB", "alive", "expired"), each = nrow(forest$err.rate)),
  error = c(forest$err.rate[,"OOB"],
            forest$err.rate[,"alive"],
            forest$err.rate[,"expired"]))

ggplot(data = oob.error.data, aes(x = trees, y = error)) +
  geom_line(aes(color = type))

forest_2 <- randomForest(status~., data = rftrain, ntree = 1500)

oob.error.data2 <- data.frame(
  trees2 = rep(1:nrow(forest_2$err.rate), times = 3),
  type2 = rep(c("OOB", "alive", "expired"), each = nrow(forest_2$err.rate)),
  error2 = c(forest_2$err.rate[,"OOB"],
            forest_2$err.rate[,"alive"],
            forest_2$err.rate[,"expired"]))
ggplot(data = oob.error.data2, aes(x = trees2, y = error2)) +
  geom_line(aes(color = type2))

# Optimal splits
oob.optimal <- vector(length = 15)
for(i in 1:15) {
  temp.model <- randomForest(status~., data = rftrain, 
                             mtry = i, ntree = 2000)
  oob.optimal[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.optimal

# Variable Importance
varImp(forest)

# Assumption Checks
# Checking for Missing Values
sum(is.na(breastc))

#Prediction
predict_rf <- predict(forest_2, rftest)

# Model Diagnostics
rfcm <- confusionMatrix(predict_rf, rftest$status)
rfcm
rfcm$byClass
rfcm$overall









