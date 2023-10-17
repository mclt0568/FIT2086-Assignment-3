WORKING_DIR <- "~/Documents/project-c2001/FIT2086-Assignment3/code/"
setwd(WORKING_DIR)

#################################################
## Question 1                                   #
#################################################

rm(list = ls())
q1_df <- read.csv("housing.2023.csv")

# Question 1.1
q1_fit <- lm(medv ~ ., q1_df)
q1_summary <- summary(q1_fit)
q1_pvals <- coefficients(q1_summary)[,4]
print(q1_pvals)

# Question 1.2
q1_bp <- q1_pvals < 0.05 / 12
print(q1_bp)
print(sum(q1_bp))

# Question 1.3
print(q1_fit)

# Question 1.4
q1_4_fit <- lm(medv
               ~ chas
               + nox
               + rm
               + dis
               + ptratio
               + lstat, q1_df)
source("my.prediction.stats.R")
q1_bic <- step(q1_4_fit, k = log(nrow(q1_df)), direction = "both", trace = 0)
summary(q1_bic)

# Question 1.5
"Code N/A for this question."

# Question 1.6
q1_6_predictors <- data.frame(crim = 0.04741,
                              zn = 0,
                              indus = 11.93,
                              chas = 0,
                              nox = 0.573,
                              rm = 6.03,
                              age = 80.8,
                              dis = 2.505,
                              rad = 1,
                              tax = 273,
                              ptratio = 21,
                              lstat = 7.88)
prediction <- predict(q1_4_fit, newdata = q1_6_predictors,
          interval = "confidence")
print(prediction)

# Question 1.7
q1_7_interaction_fit <- lm(medv
                           ~ chas
                           + nox
                           + rm
                           + dis
                           + ptratio
                           + lstat
                           + rm * dis,
                           q1_df)

summary(q1_4_fit)
summary(q1_7_interaction_fit)


#################################################
## Question 2                                   #
#################################################

rm(list = ls())

library(glmnet)
library(rpart)
library(randomForest)
library(kknn)
source("wrappers.R")
source("my.prediction.stats.R")

q2_df_train <- read.csv("heart.train.2023.csv")
q2_df_test <- read.csv("heart.test.2023.csv")

# Question 2.1
summary(q2_df_train)

q2_tree <- rpart(HD ~ ., q2_df_train)
q2_cv <- learn.tree.cv(HD ~ .,
                       data = q2_df_train,
                       nfolds = 10,
                       m = 5000)

print(q2_cv)

# Question 2.2
dev.off(dev.list()["RStudioGD"])
# png("plot.png", pointsize=3, width=700, height=1000, res=100)
plot(q2_cv$best.tree, margin = 0.5)
text(q2_cv$best.tree, pretty = 12, digits = 3, use.n = TRUE)
dev.off()

# Question 2.3
"Code N/A for this question."
q2_df_train$HD <- factor(q2_df_train$HD)
q2_fit <- glm(HD ~ ., q2_df_train, family = binomial)
q2_bic_step <- step(q2_fit,
                    k = log(nrow(q2_df_train)),
                    direction = "both",
                    trace = 0)
print(summary(q2_bic_step))

# Question 2.4
"Code N/A for this question."

# Question 2.5
"Code N/A for this question."

# Question 2.6
q2_df_test$HD <- factor(q2_df_test$HD)
my.pred.stats(predict(q2_bic_step,
                      q2_df_test,
                      type = "response"),
              q2_df_test$HD)

my.pred.stats(predict(q2_tree,
                      q2_df_test)[, 2],
              q2_df_test$HD)


#################################################
## Question 3                                   #
#################################################

rm(list = ls())
library("boot")
q3_df_measured <- read.csv("ms.measured.2023.csv")
q3_df_truth <- read.csv("ms.truth.2023.csv")

library("kknn")

# Question 3.1
error <- c()
for (k in 1:25){
  knn <- kknn(intensity ~ .,
              q3_df_measured,
              q3_df_truth,
              k = k)
  fitted <- fitted(knn)

  msqer <- mean((fitted - q3_df_truth$intensity) ^ 2)
  error <- c(error, msqer)
}

plot(error)

# Question 3.2
k_vals <- c(2,5,10,25)
for (k in k_vals){
  knn <- kknn(intensity ~ .,
              q3_df_measured,
              q3_df_truth,
              k = k)
  fitted <- fitted(knn)

  plot(q3_df_measured$MZ, 
       q3_df_measured$intensity,
       type = "p",
       col = "red")
  lines(q3_df_truth$MZ,
       q3_df_truth$intensity,
       col = "blue")
  lines(q3_df_truth$MZ,
       fitted,
       type = "l",
       col = "green")
}

# Question 3.3
"Code N/A for this question."

# Question 3.4
"Code N/A for this question."

# Question 3.5
q3_cv <- train.kknn(intensity ~ .,
                    q3_df_measured,
                    kmax = 25,
                    kernel = "optimal")

q3_best_param <- q3_cv$best.parameters$k
print(q3_best_param)

# Question 3.6
"TODO"

# Question 3.7
q3_7_fitted <- fitted(kknn(intensity ~ .,
                        q3_df_measured,
                        q3_df_truth,
                        k = q3_best_param,
                        kernel = "optimal"))

q3_7_maximum <- which.max(q3_7_fitted)
q3_7_mz <- q3_df_truth$MZ[q3_7_maximum]
print(q3_7_mz)

# Question 3.8
"TODO"