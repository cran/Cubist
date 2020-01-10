## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(Cubist)
library(dplyr)
library(rlang)
library(tidyrules)
theme_set(theme_bw())
options(digits = 3)

## ----bh1----------------------------------------------------------------------
library(Cubist)
library(mlbench)

data(BostonHousing)
BostonHousing$chas <- as.numeric(BostonHousing$chas) - 1

set.seed(1)
inTrain <- sample(1:nrow(BostonHousing), floor(.8*nrow(BostonHousing)))

train_pred <- BostonHousing[ inTrain, -14]
test_pred  <- BostonHousing[-inTrain, -14]

train_resp <- BostonHousing$medv[ inTrain]
test_resp  <- BostonHousing$medv[-inTrain]

model_tree <- cubist(x = train_pred, y = train_resp)
model_tree

## ----bh2----------------------------------------------------------------------
summary(model_tree)

## ----bh3----------------------------------------------------------------------
model_tree_pred <- predict(model_tree, test_pred)
## Test set RMSE
sqrt(mean((model_tree_pred - test_resp)^2))
## Test set R^2
cor(model_tree_pred, test_resp)^2

## ----bh4----------------------------------------------------------------------
set.seed(1)
com_model <- cubist(x = train_pred, y = train_resp, committees = 5)
summary(com_model)

## ----bh5----------------------------------------------------------------------
com_pred <- predict(com_model, test_pred)
## RMSE
sqrt(mean((com_pred - test_resp)^2))
## R^2
cor(com_pred, test_resp)^2

## ----bh6----------------------------------------------------------------------
inst_pred <- predict(com_model, test_pred, neighbors = 5)
## RMSE
sqrt(mean((inst_pred - test_resp)^2))
## R^2
cor(inst_pred, test_resp)^2

## ----tune---------------------------------------------------------------------
library(caret)

grid <- expand.grid(committees = c(1, 10, 50, 100),
                    neighbors = c(0, 1, 5, 9))
set.seed(1)
boston_tuned <- train(
  x = train_pred,
  y = train_resp,
  method = "cubist",
  tuneGrid = grid,
  trControl = trainControl(method = "cv")
  )
boston_tuned

## ----plot-tune, echo = FALSE, fig = TRUE, width = 6, height = 4.25------------
ggplot(boston_tuned) + 
  theme(legend.position = "top")

## ----lstat--------------------------------------------------------------------
lstat_df <- train_pred[, "lstat", drop = FALSE]
rules_only <- cubist(x = lstat_df, y = train_resp)
rules_and_com <- cubist(x = lstat_df, y = train_resp, committees = 100)

predictions <- lstat_df
predictions$medv <- train_resp
predictions$rules_neigh <- predict(rules_only, lstat_df, neighbors = 5)
predictions$committees <- predict(rules_and_com, lstat_df)

## ----lstatPlot, echo = FALSE, fig = TRUE, width = 8, height = 4.5-------------
ggplot(predictions, aes(x = lstat, y = medv)) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = rules_neigh), col = "red", alpha = .5, lwd = 1) + 
  geom_line(aes(y = committees), col = "blue", alpha = .5, lwd = 1)  

## ----vimp---------------------------------------------------------------------
summary(model_tree)
model_tree$usage
library(caret)
varImp(model_tree)

## ----summary-tree-------------------------------------------------------------
summary(model_tree)

## ----tidy_rules_example-------------------------------------------------------
library(dplyr)
library(modeldata)
data("attrition")
attrition <- as_tibble(attrition)

# lets predict monthly income
attrition_x <- attrition %>% dplyr::select(-MonthlyIncome, -Attrition)
attrition_y <- attrition %>% dplyr::select(MonthlyIncome) %>% unlist()

model_tree_attrition <- cubist(x = attrition_x, y = attrition_y)

library(tidyrules)

tr <- tidyRules(model_tree_attrition)
tr
tr[, c("LHS", "RHS")]

# lets look at 7th rule
tr$LHS[7]
tr$RHS[7]

## ----rule-7-------------------------------------------------------------------
library(dplyr)
library(rlang)

char_to_expr <- function(x, index = 1, model = TRUE) {
  x <- x %>% dplyr::slice(index) 
  if (model) {
    x <- x %>% dplyr::pull(RHS) %>% rlang::parse_expr()
  } else {
    x <- x %>% dplyr::pull(LHS) %>% rlang::parse_expr()
  }
  x
}

rule_expr  <- char_to_expr(tr, 7, model = FALSE)
model_expr <- char_to_expr(tr, 7, model = TRUE)

# filter the data corresponding to rule 7
attrition %>% 
  dplyr::filter(eval_tidy(rule_expr, attrition)) %>%
  # evaluate the estimated MonthlyIncome
  dplyr::mutate(MonthlyIncome_est = eval_tidy(model_expr, .)) %>%
  dplyr::select(MonthlyIncome, MonthlyIncome_est)

