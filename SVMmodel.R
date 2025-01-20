
# Required Package
library(e1071)
library(caret)
library(dplyr)

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
levels(breastc_clean$status)[levels(breastc_cleaned$status)=="0"] <- "alive"
levels(breastc_clean$status)[levels(breastc_cleaned$status)=="1"] <- "expired"

# Training and Testing Data Sets
set.seed(123)
svmtrainIndex <- createDataPartition(y = breastc_cleaned$status,
                                  p = 0.7, list  = FALSE)
svmtrain <- breastc_cleaned[trainIndex, ]
svmtest <- breastc_cleaned[-trainIndex, ]

# Model fitting
svm_model <- svm(status~., data = svmtrain, type = "C-classification",
                 kernel = "radial")
svm_model

# Assumption Checks
# Removal of Outliers using IQR Method
support_vectors <- svm_model$SV
dual_coefs <- svm_model$coefs

leverage_scores <- apply(support_vectors, 1, function(sv){
  sum(dual_coefs * sv)
})

outliers_train <- svmtrain
leverage_data <- data.frame(outliers_train, Leverage = leverage_scores)

# Prediction
prediction <- predict(svm_model, newdata = svmtest)

# Model Diagnostics
conf_matrix <- confusionMatrix(prediction, svmtest$status)
conf_matrix
conf_matrix$byClass
conf_matrix$overall


