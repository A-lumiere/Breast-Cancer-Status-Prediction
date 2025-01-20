
# Required Packages and Data Preparation
library(class)
library(dplyr)

# Data cleaning
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

normalization <- function(i){
  return((i - mean(i))/(max(i) - min(i)))
}

View(n.breastc)
# Testing and Training Sets
set.seed(123)
knntrainIndex <- createDataPartition(y = breastc_cleaned$status,
                                  p = 0.7, list  = FALSE)
knntrain <- breastc_cleaned[trainIndex, ]
knntest <- breastc_cleaned[-trainIndex, ]

# Feature Scaling
cont_knntrain <- knntrain %>% 
  select(age, size, nodes, er, rfstime)
disc_knntrain <- knntrain %>% 
  select(meno, grade, pgr, hormon, status)

cont_knntest <- knntest %>% 
  select(age, size, nodes, er, rfstime)
disc_knntest <- knntest %>% 
  select(meno, grade, pgr, hormon, status)


n.train <- as.data.frame(lapply(cont_knntrain[,1:5], normalization))
n.test <- as.data.frame(lapply(cont_knntest[,1:5], normalization))

n.knntrain <- data.frame(n.train, disc_knntrain)
n.knntest <- data.frame(n.test, disc_knntest)
n.knntrain.labels <- n.knntrain[, 10]
n.knntest.labels <- n.knntest[, 10]

print(breastc_cleaned[1:481, 10])

# Optimal K Values
set.seed(123)
i = 1
k.optm = 1
for (i in 1:28){
  knn.mod <- knn(train = n.knntrain[, -10], 
                 test = n.knntest[, -10], 
                 cl = n.knntrain.labels, 
                 k = i)
  k.optm[i] <- 100 * sum(n.knntrain.labels == knn.mod) / NROW(n.knntest.labels)
  k = i
  cat(k, '=', k.optm[i], '\n')
}
k_optimal<- data.frame(1:28, k.optm)
k_optimal
ggplot(data = k_optimal, aes(x = 1:28, y = k.optm)) + 
  geom_line(color = "red") + geom_point(color = "red") + 
  labs(x = "Optimal k value", y = "accuracy")

# Model Prediction
knn_model <- knn(train = n.knntrain[, -10], 
                 test = n.knntest[, -10], 
                 cl = n.knntrain.labels, 
                 k = 16)

knncm <- confusionMatrix(knn_model, n.knntest.labels)
knncm
knncm$byClass
knncm$overall

