#######################################
###Tasks
#######################################

##PART I: Probability prediction
#Predict probabilities
#Look at cross-validated performance and pick your favorite model

##PART II: Classification
#Think about the business problem, and define your loss function (like FP=X dollars, FN=Y dollars)
  #For each model, predict probabilities, look for the optimal classification threshold, 
  #calculate expected loss with your loss function. 
#Pick the model that has the smallest average (over 5 folds) expected loss

##PART III Discussion of results
#Show a confusion table (on a selected fold or holdout set)
#Discuss results, evaluate how useful your model may be


########################################

# CLEAR MEMORY
rm(list=ls())

# Import libraries 

library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)
library(funModeling)
library(dplyr)
library(lmtest)
library(fixest)
library(texreg)
library(estimatr)
library(stargazer)
library(kableExtra)

library(rattle)
library(caret)
library(pROC)
library(rpart)
library(partykit)
library(rpart.plot)

# Random forest package
if (!require(ranger)){
  install.packages('ranger')
  library(ranger)
}


###########################################
##set working directory and import raw data

getwd()
data_in <- "C:/Users/User/OneDrive - Central European University/Desktop/assignment_03/"
setwd(data_in)



##import raw data 

data <- read.csv(paste0(data_in,"cs_bisnode_panel.csv"),
               sep = ",", header = TRUE, stringsAsFactors = FALSE)

# drop variables with many NAs
# Keep panel data from 2010 to 2015
data <- data %>% 
  select(-c(COGS, finished_prod, D, net_dom_sales, net_exp_sales, wages)) %>%
    filter(year >= 2010 & year <= 2015)


###########################################################
# label engineering for fast growing firms
###########################################################

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
data <- data %>%
  complete(year, comp_id)

# Now, the focus is to define first growing firms
# Trying to define first growing firms based on the sales in higher quantile
summary(data$sales)
Hmisc::describe(data$sales)

# If sales of a firm is in at least 40th percentile
sales_quantile_1= quantile(data$sales, probs = 0.4, na.rm = TRUE)
sales_quantile_1


# If sales of a firm is in at least 60th percentile
sales_quantile_2= quantile(data$sales, probs = 0.6, na.rm=TRUE)
sales_quantile_2

# generate status_fastgrowing, if sales is larger than or equal to 40th percentile then firm is fast growing in year 1
data  <- data %>%
  mutate(status_fastgrowing1 = (sales >= sales_quantile_1 & !is.na(sales)) %>%
                                  as.numeric(.))

# generate status_fastgrowing, if sales is larger than or equal to from 30th to 40th percentile then firm is fast growing in two years later
data  <- data %>%
mutate(status_fastgrowing2 = (sales>=sales_quantile_2 & !is.na(sales))  %>%
         as.numeric(.))

# defaults in two years if there are 40th percentile sale in certain year and at least 70th percentile two years later
data <- data %>%
  group_by(comp_id) %>%
  mutate (default = ((status_fastgrowing1 == 1) & ((lead(status_fastgrowing1, 2) == (status_fastgrowing2== 1)))) %>%
           as.numeric(.)) %>%
  ungroup()


Hmisc::describe(data$default)



## Size and growth

summary(data$sales) 

# There is some missing values- NAs, those will be dropped in the next stage
#Also to create the log values we are converting the sales<0

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))


# Difference in sales
data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()


# replace (.),  0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .))
         

###########################################################
# sample design
###########################################################

# look at cross section
summary(data$sales_mil, data$year == 2012)
# sales_mil greater than 20th percentile but less than 70m 
data <- data %>%
  filter((year == 2012) & (status_fastgrowing1 == 1)) %>%
  filter(sales_mil > 0.001) %>% 
  filter(sales_mil < 100)

Hmisc::describe(data$default)


###########################################################
# Feature engineering
###########################################################


# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 98, .)
  )

table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))



###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)



pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
## creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))
########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(default_f = factor(default, levels = c(0,1)) %>%
           recode(., `0` = 'no_default', `1` = "default"))

########################################################################
# sales pattern # sales change
# lowess
########################################################################

data <- data %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)


sales0_graph <- ggplot(data = data, aes(x=sales_mil_log, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = F, size=1)+
  geom_smooth(method="loess", se=F, size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "default") 
 
sales0_graph


ols_s <- lm(default~sales_mil_log+sales_mil_log_sq,
            data = data)
summary(ols_s)


Hmisc::describe(data$d1_sales_mil_log) # no missing

d1sale_1<-ggplot(data = data, aes(x=d1_sales_mil_log, y=as.numeric(default))) +
  geom_point(size=0.1,  shape=20, stroke=2) +
  geom_smooth(method="loess", se=F, size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default") +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))
d1sale_1



## generate variables ---------------------------------------------------

data <- data %>%
  mutate(d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 2, 2, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

d1sale_2<-ggplot(data = data, aes(x=d1_sales_mil_log_mod, y=as.numeric(default))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill="blue") +
  geom_smooth(method="loess", se=F, size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default")

d1sale_2


d1sale_3<-ggplot(data = data, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill="blue")+
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))
d1sale_3


#summary
datasummary_skim(data, type='numeric', histogram = TRUE)


##############################################################
#Variable identification for different models
##############################################################

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")

qualityvars <- c("balsheet_length", "balsheet_notfullyear")

engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")

engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")

d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq")

hr <- c("female", "ceo_age","ceo_count","foreign_management")

firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")


## interactions for logit, LASSO

interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m")

interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")


X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs",
        "curr_liab_bs ",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar,  d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, d1, hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, d1, hr, qualityvars, interactions1, interactions2)



## for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, d1, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)



##############################################################
#Model formulation
##############################################################


# Check simplest model X1
ols_modelx1 <- lm(formula(paste0("default ~", paste0(X1, collapse = " + "))),
                  data = data)

summary(ols_modelx1)


glm_modelx1 <- glm(formula(paste0("default ~", paste0(X1, collapse = " + "))),
                   data = data, family = "binomial")

summary(glm_modelx1)


# Check model X2

glm_modelx2 <- glm(formula(paste0("default ~", paste0(X2, collapse = " + "))),
                   data = data, family = "binomial")

summary(glm_modelx2)


#calculate average marginal effects (dy/dx) for logit

mx2 <- margins(glm_modelx2)

sum_table <- summary(glm_modelx2) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx2)[,c("factor","AME")])


kable(x = sum_table, format = "latex", digits = 3,
      col.names = c("Variable", "Coefficient", "dx/dy"),
      caption = "Average Marginal Effects (dy/dx) for Logit Model") %>%
  cat(.,file= paste0(data_in,"AME_logit_X2.tex"))


# baseline model is X4 (all vars, but no interactions) -------------------------------------------------------

ols_model <- lm(formula(paste0("default ~", paste0(X4, collapse = " + "))),
                data = data)

summary(ols_model)


glm_model <- glm(formula(paste0("default ~", paste0(X4, collapse = " + "))),
                 data = data, family = "binomial")

summary(glm_model)

#calculate average marginal effects (dy/dx) for logit
# vce="none" makes it run much faster, here we do not need variances

m <- margins(glm_model, vce = "none")

sum_table2 <- summary(glm_model) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])

kable(x = sum_table2, format = "latex", digits = 3,
      col.names = c("Variable", "Coefficient", "SE", "dx/dy"),
      caption = "Average Marginal Effects (dy/dx) for Logit Model") %>%
  cat(.,file= paste0(data_in,"AME_logit_X4.tex"))


################################################
# separate datasets for cross validation

set.seed(13505)

train_indices <- as.integer(createDataPartition(!is.na(data$default), p = 0.8, list = FALSE))
data_train <- data[train_indices, stringsAsFactors=FALSE]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$default_f)
Hmisc::describe(data_train$default_f)
Hmisc::describe(data_holdout
                $default_f)




#######################################################x
# PREDICT PROBABILITIES
# Predict with logit models 
#######################################################x

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)


# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()


for (model_name in names(logit_model_vars)) {
  
  features <- logit_model_vars[[model_name]]
  
  set.seed(13505)

  glm_model <- train(
    formula(paste0("default_f~", paste0(features, collapse = " + "))),
              
    method="glm",
    data = data_train,
    family = binomial,
    trControl = train_control,
    na.action=na.exclude
  )
} 
 
 glm_model



 logit_models[[model_name]] <- glm_model
  

#######################################################x
# predict with Logit lasso models 
#######################################################x


lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("default_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})
logit_lasso_model

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
write.csv(lasso_coeffs, paste0(data_in, "lasso_logit_coeffs.csv"))



#############################################x
# No loss fn
#############################################x

# Draw ROC Curve and calculate AUC for each folds --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$default)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }}
 
  model 
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))

  CV_AUC_folds[[model_name]]
# For each model: average RMSE and average AUC for models ----------------------------------

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

# We have 6 models, (5 logit and the logit lasso). For each we have a 5-CV RMSE and AUC.
# We pick our preferred model based on that. -----------------------------------------------

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

kable(x = logit_summary1, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) %>%
  cat(.,file= paste0(data_in, "logit_summary1.tex"))




#############################################x
# With a loss function
#############################################x

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=1
FN=10
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$default)/length(data_train$default)

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$default)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$default)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}


logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))
logit_summary2

kable(x = logit_summary2, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(data_in, "logit_summary2.tex"))



#################################################
# PREDICTION WITH RANDOM FOREST
#################################################

# -----------------------------------------------
# RANDOM FOREST GRAPH EXAMPLE
# -----------------------------------------------

data_for_graph <- data_train
levels(data_for_graph$default_f) <- list("not_fastgrowing" = "no_default", "fast_growing" = "default")

set.seed(13505)
rf_for_graph <-
  rpart(
    formula = default_f ~ sales_mil + profit_loss_year+ foreign_management,
    data = data_for_graph,
    control = rpart.control(cp = 0.0028, minbucket = 100)
  )

rpart.plot(rf_for_graph, tweak=1, digits=2, extra=107, under = TRUE)


#################################################
# Probability forest
# Split by gini, ratio of 1's in each tree, average over trees
#################################################

# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# getModelInfo("ranger")
set.seed(13505)
rf_model_p <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control,
  na.action=na.exclude
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$default)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}


CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)

# Now use loss function and search for best thresholds and expected loss over folds -----
best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$default)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$default)
}

# average
best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))


rf_summary <- data.frame("CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])

kable(x = rf_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("CV RMSE", "CV AUC",
                                  "Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(data_in, "rf_summary.tex"))




#################################################
# Classification forest
# Split by Gini, majority vote in each tree, majority vote over trees
#################################################
# Show expected loss with classification RF and default majority voting to compare

train_control <- trainControl(
  method = "cv",
  n = 5
)
train_control$verboseIter <- TRUE

set.seed(13505)
rf_model_f <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control,
  na.action=na.exclude
)
rf_model_f



#####################################################################
# Summary results (confusion table)
#####################################################################

nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X1", "Logit X4",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X1", "X4", "LASSO", "rf_p"))


kable(x = summary_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors", "CV AUC",
                                  "CV threshold", "CV expected Loss")) %>%
  cat(.,file= paste0(data_in, "summary_results.tex"))

 ##################################################################################################
