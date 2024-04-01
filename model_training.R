library(caret)
library(randomForest)
library(e1071) # For SVM
library(class)

# Load the dataset
data <- read.csv("wdbc.csv", stringsAsFactors = TRUE)

# Ensure diagnosis is a factor
data$diagnosis <- as.factor(ifelse(data$diagnosis == "M", 1, 0))

# Subset for the predictors discussed
predictors <- data[, c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", 
                       "smoothness_mean", "compactness_mean", "concavity_mean", 
                       "concave_points_mean", "symmetry_mean", "fractal_dimension_mean")]

# Split the dataset
set.seed(123)
index <- createDataPartition(data$diagnosis, p = .8, list = FALSE)
trainData <- data[index, ]
testData <- data[-index, ]

# Train the models
logisticModel <- glm(diagnosis ~ ., data = trainData, family = "binomial")
svmModel <- svm(diagnosis ~ ., data = trainData, type = 'C-classification', kernel = 'linear')
randomForestModel <- randomForest(diagnosis ~ ., data = trainData)
knnModel <- knn3(diagnosis ~ ., data = trainData, k = 5)

# Save the models
saveRDS(logisticModel, "logisticModel.rds")
saveRDS(svmModel, "svmModel.rds")
saveRDS(randomForestModel, "randomForestModel.rds")
saveRDS(knnModel, "knnModel.rds")
