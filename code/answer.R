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
q2_df <- read.csv("heart.train.2023.csv")

# Question 2.1
summary(q2_df)