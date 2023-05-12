set.seed(5)
library(dplyr)
library(MASS)
library(car)
library(fastDummies)

# Read data
math <- read.csv("data/student-mat.csv", sep=";")
math <- subset(math, select = -c(school, G1, G2))

por <- read.csv("data/student-por.csv", sep=";")
por <- subset(por, select = -c(school, G1, G2))

pordumm <- dummy_cols(por, select_columns = c("sex", "address", "famsize", "Pstatus", "Medu",
                                              "Fedu", "Mjob", "Fjob", "reason", "guardian", 
                                              "traveltime", "studytime", "schoolsup", "famsup", 
                                              "paid", "activities", "nursery", "higher", "internet", 
                                              "romantic", "famrel", "freetime", "goout", 
                                              "Dalc", "Walc", "health"), 
                      remove_first_dummy = TRUE, 
                      remove_selected_columns = TRUE)

# Model for Portuguese
model_high <- lm(G3 ~ higher, data = por)
summary(model_high)
cat("HCCM Error for coef:", round(sqrt(hccm (model_high , type = "hc2" )[2 , 2]), 3))

full_model <- lm(G3 ~., data = pordumm)
summary(full_model)

data_wout_h <- pordumm %>% dplyr::select(-higher_yes)
full_model_wout_h <- lm(G3 ~ ., data = data_wout_h)

beta <- full_model_wout_h$coefficients

library(glmnet)
library(gglasso)

data <- pordumm %>% dplyr::select(-c('G3', 'higher_yes'))
X <- as.matrix(data)
y <- as.matrix(pordumm$G3)

group <- c(1, 2, 3, 4, 5, 6, 7, 
           8, 8, 8, 8, 
           9, 9, 9, 9, 
           10, 10, 10, 10, 
           11, 11, 11, 11, 
           12, 12, 12, 13, 
           13, 14, 
           14, 14, 
           15, 15, 15, 
           16, 17, 18, 19, 20, 21, 22, 
           23, 23, 23, 23, 
           24, 24, 24, 24, 
           25, 25, 25, 25, 
           26, 26, 26, 26, 
           27, 27, 27, 27, 
           28, 28, 28, 28)

lasso <- glmnet(X, y, lambda = 0.1,
                family="gaussian", alpha=1,
                intercept = TRUE)

# Just to show this is irrelevant
lasso <- cv.glmnet(X, y, aplha=1,
                   family="gaussian")

gr_lasso<- cv.gglasso(X, y, 
                      group = group,
                      pred.loss="L2",
                      loss="ls")

gr_lasso_fitted <- gglasso(X, y,
                           group = group,
                           lambda = gr_lasso$lambda.1se,
                           loss="ls")

coefs_z <- data.frame(
  z_initial = beta,
  z_gr_lasso = coef(gr_lasso_fitted)
)
names(coefs_z) <- c("z_initial", "z_from_gr_lasso")

library(glmnet)
library(gglasso)

data <- pordumm %>% dplyr::select(-c('G3', 'higher_yes'))
X <- as.matrix(data)
y <- as.matrix(pordumm$higher_yes)
group <- c(1, 2, 3, 4, 5, 6, 7, 
           8, 8, 8, 8, 
           9, 9, 9, 9, 
           10, 10, 10, 10, 
           11, 11, 11, 11, 
           12, 12, 12, 13, 
           13, 14, 
           14, 14, 
           15, 15, 15, 
           16, 17, 18, 19, 20, 21, 22, 
           23, 23, 23, 23, 
           24, 24, 24, 24, 
           25, 25, 25, 25, 
           26, 26, 26, 26, 
           27, 27, 27, 27, 
           28, 28, 28, 28)

lasso <- glmnet(X, y, lambda = 0.1,
                family="gaussian", alpha=1,
                intercept = TRUE)

# Just to show this is irrelevant
lasso <- cv.glmnet(X, y, aplha=1,
                   family="gaussian")

gr_lasso<- cv.gglasso(X, y, 
                      group = group,
                      pred.loss="L2",
                      loss="ls")

gr_lasso_fitted <- gglasso(X, y,
                           group = group,
                           lambda = gr_lasso$lambda.1se,
                           loss="ls")

coefs_y <- data.frame(
  y_initial = beta,
  y_gr_lasso = coef(gr_lasso_fitted)
)
names(coefs_y) <- c("y_initial", "y_from_gr_lasso")

all_coefs <- cbind(coefs_y, coefs_z)
all_coefs$union <- all_coefs$y_from_gr_lasso != 0 | all_coefs$z_from_gr_lasso != 0
all_coefs %>% filter(union == TRUE)

final_data <- por %>% dplyr::select('age', 'failures', 'absences', 
                                    'sex', 'address', 'Medu', 'internet', 'higher', 'G3')

final_data <- dummy_cols(final_data, select_columns = c("sex", "address", "Medu", 
                                                        "internet", 'higher'),
                         remove_selected_columns = TRUE,
                         remove_first_dummy = TRUE)


print("Full model (Portuguese): ")
summary(full_model)$coefficients["higher_yes", ]
confint(full_model)["higher_yes", ]
hccm(full_model , type = "hc2" )["higher_yes", "higher_yes"]

HC2 <- hccm(full_model , type = "hc2" )["higher_yes", "higher_yes"]
estimator <- summary(full_model)$coefficients["higher_yes", ]['Estimate']
c(estimator - HC2 * qt(0.975, df.residual(full_model)),
  estimator - HC2 * qt(0.025, df.residual(full_model)))

print("Restricted model (Portuguese): ")

restricted_model <- lm(G3 ~ ., data=final_data)
summary(restricted_model)$coefficients["higher_yes", ]
confint(restricted_model)["higher_yes", ]
hccm(restricted_model , type = "hc2" )["higher_yes", "higher_yes"]

HC2 <- hccm(restricted_model , type = "hc2" )["higher_yes", "higher_yes"]
estimator <- summary(restricted_model)$coefficients["higher_yes", ]['Estimate']
c(estimator - HC2 * qt(0.975, df.residual(restricted_model)),
  estimator - HC2 * qt(0.025, df.residual(restricted_model)))

final_data$G3 <- ifelse(final_data$G3 >= 10, 1, 0)
summary(glm(G3 ~ ., data = final_data, family = "binomial"))

# Model for Math

mathdumm <- dummy_cols(math, select_columns = c("sex", "address", "famsize", "Pstatus", "Medu",
                                                "Fedu", "Mjob", "Fjob", "reason", "guardian", 
                                                "traveltime", "studytime", "schoolsup", "famsup", 
                                                "paid", "activities", "nursery", "higher", "internet", 
                                                "romantic", "famrel", "freetime", "goout", 
                                                "Dalc", "Walc", "health"), 
                       remove_first_dummy = TRUE, 
                       remove_selected_columns = TRUE)

model_high <- lm(G3 ~ higher, data = math)
summary(model_high)
cat("HCCM Error for coef:", round(sqrt(hccm (model_high , type = "hc2" )[2 , 2]), 3))

full_model <- lm(G3 ~., data = mathdumm)
summary(full_model)

data_wout_h <- mathdumm %>% dplyr::select(-higher_yes)
full_model_wout_h <- lm(G3 ~ ., data = data_wout_h)

beta <- full_model_wout_h$coefficients

library(glmnet)
library(gglasso)

data <- mathdumm %>% dplyr::select(-c('G3', 'higher_yes'))
X <- as.matrix(data)
y <- as.matrix(mathdumm$G3)

group <- c(1, 2, 3, 4, 5, 6, 7, 
           8, 8, 8, 8, 
           9, 9, 9, 9, 
           10, 10, 10, 10, 
           11, 11, 11, 11, 
           12, 12, 12, 13, 
           13, 14, 
           14, 14, 
           15, 15, 15, 
           16, 17, 18, 19, 20, 21, 22, 
           23, 23, 23, 23, 
           24, 24, 24, 24, 
           25, 25, 25, 25, 
           26, 26, 26, 26, 
           27, 27, 27, 27, 
           28, 28, 28, 28)

lasso <- glmnet(X, y, lambda = 0.1,
                family="gaussian", alpha=1,
                intercept = TRUE)

# Just to show this is irrelevant
lasso <- cv.glmnet(X, y, aplha=1,
                   family="gaussian")
plot(lasso)
lasso$lambda.min

gr_lasso<- cv.gglasso(X, y, 
                      group = group,
                      pred.loss="L2",
                      loss="ls",
                      nfolds = 10)

gr_lasso_fitted <- gglasso(X, y,
                           group = group,
                           lambda = gr_lasso$lambda.1se,
                           loss="ls")

coefs_z <- data.frame(
  z_initial = beta,
  z_gr_lasso = coef(gr_lasso_fitted)
)
names(coefs_z) <- c("z_initial", "z_from_gr_lasso")

data <- mathdumm %>% dplyr::select(-c('G3', 'higher_yes'))
X <- as.matrix(data)
y <- as.matrix(mathdumm$higher_yes)
group <- c(1, 2, 3, 4, 5, 6, 7, 
           8, 8, 8, 8, 
           9, 9, 9, 9, 
           10, 10, 10, 10, 
           11, 11, 11, 11, 
           12, 12, 12, 13, 
           13, 14, 
           14, 14, 
           15, 15, 15, 
           16, 17, 18, 19, 20, 21, 22, 
           23, 23, 23, 23, 
           24, 24, 24, 24, 
           25, 25, 25, 25, 
           26, 26, 26, 26, 
           27, 27, 27, 27, 
           28, 28, 28, 28)

lasso <- glmnet(X, y, lambda = 0.1,
                family="gaussian", alpha=1,
                intercept = TRUE)

# Just to show this is irrelevant
lasso <- cv.glmnet(X, y, aplha=1,
                   family="gaussian")
plot(lasso)
lasso$lambda.min

gr_lasso<- cv.gglasso(X, y, 
                      group = group,
                      pred.loss="L2",
                      loss="ls",
                      nfolds = 10)

gr_lasso_fitted <- gglasso(X, y,
                           group = group,
                           lambda = gr_lasso$lambda.1se,
                           loss="ls")

coefs_y <- data.frame(
  y_initial = beta,
  y_gr_lasso = coef(gr_lasso_fitted)
)
names(coefs_y) <- c("y_initial", "y_from_gr_lasso")

all_coefs <- cbind(coefs_y, coefs_z)
all_coefs$union <- all_coefs$y_from_gr_lasso != 0 | all_coefs$z_from_gr_lasso != 0
all_coefs %>% filter(union == TRUE)

final_data <- math %>% dplyr::select('failures', 'absences', 
                                     'higher', 'G3')

print("Full model (math): ")
summary(full_model)$coefficients["higher_yes", ]
hccm(full_model , type = "hc2" )["higher_yes", "higher_yes"]
confint(full_model)["higher_yes", ]

HC2 <- hccm(full_model , type = "hc2" )["higher_yes", "higher_yes"]
estimator <- summary(full_model)$coefficients["higher_yes", ]['Estimate']
c(estimator - HC2 * qt(0.975, df.residual(full_model)),
  estimator - HC2 * qt(0.025, df.residual(full_model)))

print("Restricted model (math): ")
restricted_model <- lm(G3 ~ ., data=final_data)
summary(restricted_model)$coefficients["higheryes", ]
hccm(restricted_model , type = "hc2" )["higheryes", "higheryes"]
confint(restricted_model)["higheryes", ]

HC2 <- hccm(restricted_model , type = "hc2" )["higheryes", "higheryes"]
estimator <- summary(restricted_model)$coefficients["higheryes", ]['Estimate']
c(estimator - HC2 * qt(0.975, df.residual(restricted_model)),
  estimator - HC2 * qt(0.025, df.residual(restricted_model)))