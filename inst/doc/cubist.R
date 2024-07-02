## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(Cubist)
library(dplyr)
library(rlang)
library(rules)
options(digits = 3)

## ----bh1----------------------------------------------------------------------
library(Cubist)

data(ames, package = "modeldata")
# model the data on the log10 scale
ames$Sale_Price <- log10(ames$Sale_Price)

set.seed(11)
in_train_set <- sample(1:nrow(ames), floor(.8*nrow(ames)))

predictors <- 
  c("Lot_Area", "Alley", "Lot_Shape", "Neighborhood", "Bldg_Type", 
    "Year_Built", "Total_Bsmt_SF", "Central_Air", "Gr_Liv_Area", 
    "Bsmt_Full_Bath", "Bsmt_Half_Bath", "Full_Bath", "Half_Bath", 
    "TotRms_AbvGrd",  "Year_Sold", "Longitude", "Latitude")

train_pred <- ames[ in_train_set, predictors]
test_pred  <- ames[-in_train_set, predictors]

train_resp <- ames$Sale_Price[ in_train_set]
test_resp  <- ames$Sale_Price[-in_train_set]

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
com_model <- cubist(x = train_pred, y = train_resp, committees = 3)
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

## ----echo = FALSE, fig.align='center'-----------------------------------------
knitr::include_graphics("neighbors.gif")

## ----summary-tree-------------------------------------------------------------
summary(model_tree)

## -----------------------------------------------------------------------------
library(rules)

rule_df <- tidy(model_tree)

rule_df

rule_df$estimate[[1]]

rule_df$statistic[[1]]

## -----------------------------------------------------------------------------
# Text
rule_7 <- rule_df$rule[7]

# Convert to an expression
rule_7 <- rlang::parse_expr(rule_7)
rule_7

# Use in a dplyr filter: 
nrow(train_pred)

library(dplyr)

train_pred %>% filter(!!rule_7) %>% nrow()

## ----vimp, eval = FALSE-------------------------------------------------------
#  caret::varImp(model_tree)
#  
#  # or
#  
#  vip::vi(model_tree)

