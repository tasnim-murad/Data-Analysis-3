#######################################
###Tasks
#######################################
#Your task will be to help a company operating small and mid-size apartments hosting 2-6 guests. 
#The company is set to price their new apartments not on the market. 
#Build a price prediction model similarly to how we did in our case study for London. 
#Discuss your modeling decisions and compare your results to those of the case study. 
#Have at least 3 different models and compare performance
#Argue for your choice of models
#One model must be Random Forest or any boosting algorithm

########################################

# CLEAR MEMORY
rm(list=ls())

# Import libraries 
# Descriptive statistics and regressions
library(tidyverse)
library(skimr)
library(knitr)
library(glmnet)
library(huxtable)
library(kableExtra)
library(modelsummary)
library(funModeling)
library(lmtest)
library(fixest)
library(texreg)
library(stargazer)
library(Hmisc)
library(caret)
library(RColorBrewer)

if (!require(rattle)){
  install.packages('rattle')
  library(rattle)
}

# Random forest package
if (!require(ranger)){
  install.packages('ranger')
  library(ranger)
}

###########################################
##set working directory and import raw data

getwd()
data_in <- "C:/Users/User/OneDrive - Central European University/Desktop/assignment02/"
setwd(data_in)


##import raw data 

df <- read.csv(paste0(data_in,"listings_barcelona.csv"),
               sep = ",", header = TRUE, stringsAsFactors = FALSE)


#########################################
##Data cleaning and variable generation
#########################################

## Drop some unnecessary variables

drops <- c("name","description", "neighborhood_overview", "listing_url","last_scraped","bathrooms", "name","picture_url",
           "host_url","host_name","host_about","host_thumbnail_url","host_picture_url", "host_neighbourhood", "neighbourhood_cleansed")
df<-df[ , !(names(df) %in% drops)]


#drop broken lines - where id is not a character of numbers

df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]


#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)


#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

# drop if any missing values in price
df <- df %>%
  drop_na(price)

#remove dollar signs from price variables
for (pricevars in c("price")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}


#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic",
                 "host_identity_verified","instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}


#As per the question our focus is on the company operating small and mid-size 
#apartments hosting 2-6 guests. 

# rename accommodates to guest_hosting
df <- df %>%
  mutate(guest_hosting = accommodates)


# rename entire rental unit to apartment

df <- df %>%
  mutate(
    property_type = ifelse(df$property_type == "Entire rental unit", "Apartment", df$property_type))
  

# Filter apartments hosting 2-6 guests

df <- df %>% filter(property_type =='Apartment') %>% 
  filter(guest_hosting >= 2 & guest_hosting <= 6)


# Rename room type because it is too long
df$room_type <- ifelse(df$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(df$f_room_type== "Private room", "Private",
                                          ifelse(df$f_room_type== "Shared room", "Shared", ".")))

# Separate bathroom text

df <-separate(df, bathrooms_text, ' ', into =
                c('bathrooms', 'bath_type'))
df$bathrooms <- as.numeric(df$bathrooms)


# Description of bathrooms

df$bath_type <- ifelse(df$bath_type== "baths", "common", 'private')

f_bath_type <- factor(df$bath_type)

for (binary in c("bath_type")){
  df[[binary]][df[[binary]]=="private"] <- 1
  df[[binary]][df[[binary]]=="common"] <- 0
}

# Mutate the variable host location

df <- df %>%
  mutate(m_host_location= ifelse(host_location == "Barcelona, Spain", 
                                 "convenient",'non-convenient'))

for (binary in c("m_host_location")){
  df[[binary]][df[[binary]]=="convenient"] <- 1
  df[[binary]][df[[binary]]=="non-convenient"] <- 0
}

# Mutate the variable host response time

df <- df %>%
  mutate(m_host_response_time = ifelse(host_response_time == "within an hour"| 
                                         host_response_time == "within a few hours", 
                                       'fast response','slow response'))

for (binary in c("m_host_response_time")){
  df[[binary]][df[[binary]]=="fast response"] <- 1
  df[[binary]][df[[binary]]=="slow response"] <- 0
}


# Mutate the variable license
df <- df %>%
  mutate(has_license = ifelse(license == "."| license == "exempt", 
                                       'No license','has license'))

for (binary in c("has_license")){
  df[[binary]][df[[binary]]=="has license"] <- 1
  df[[binary]][df[[binary]]=="no license"] <- 0
}

# factor the variable neighbuorhood_group_cleansed

f_neighbourhood_group_cleansed = factor(df$neighbourhood_group_cleansed)


# if availability is >=50 in availability_90, rename highly available
df <- df %>%
  mutate(m_availability
         = ifelse(availability_90 >= 50, 
                              'highly available','moderately or rarely avalable'))

for (binary in c("m_availability")){
  df[[binary]][df[[binary]]=="highly available"] <- 1
  df[[binary]][df[[binary]]=="moderately or rarely avalable"] <- 0
}

## Create Numerical variables
df <- df %>%
  mutate(host_response_rate = as.numeric(host_response_rate))


# add new numeric columns from certain columns
numericals <- c("bathrooms","beds","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "host_response_rate", "host_acceptance_rate")
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


###########################
#Cleaned data
###########################

#write csv
write.csv(df,file=paste0(data_in,"airbnb_barcelona_cleaned.csv"))

# opening dataset
barcelona<-read.csv(paste0(data_in,"airbnb_barcelona_cleaned.csv"),
                    sep=",",header = TRUE, stringsAsFactors = FALSE) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))

barcelona <- replace(barcelona, is.na(barcelona), 0)

# basic description stat 
skimr::skim(barcelona)
summary(barcelona$price)
Hmisc::describe(barcelona$price)
table(barcelona$number_of_reviews)

# Boxplot for price & number of guests hosting in an apartment

graph_01 <- ggplot(df, aes(x = factor(guest_hosting), y = price,
                        fill = factor(host_is_superhost), color=factor(host_is_superhost))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "number of persons hosting in an apartment",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme(legend.position = c(0.3,0.8) )

graph_01

#####################
# OLS Regression 
#####################


# Squares and further values to create
barcelona <- barcelona %>%
  mutate(guest_hosting2=guest_hosting^2)

# Squares and further values to create
barcelona <- barcelona %>%
  mutate(bathrooms2=bathrooms^2)


ols_reg_model <- as.formula(price ~ guest_hosting + guest_hosting2 + host_response_time + host_is_superhost+
                              + beds_n+ bathrooms_n+ bath_type+ number_of_reviews+review_scores_rating_n+ review_scores_cleanliness+
                              review_scores_location+guest_hosting*beds_n + guest_hosting*bathrooms_n+ instant_bookable)

ols_reg <- feols(ols_reg_model, data=barcelona, vcov = 'hetero')


# Evaluation of the ols regression model

fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")
etable( ols_reg, fitstat = c('aic','bic','rmse','r2','n','k'), digits=2, float = F)

# evaluation of the model
models <- c("ols_reg")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

# Get for all models
for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$ols_reg_model$price)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

stargazer(regr[[1]], fitstat = c('aic','bic','rmse','r2','n','k'),type="text", title = "Regression Model Evaluation", 
          out=paste(data_in,"Table_01.txt",sep=""), align = T, digits=2, float = F, no.space = T)


# REGRESSION ANALYSIS with Graph

# lowess

Evaluation_graph01<- ggplot(data = barcelona, aes(x=guest_hosting, y=price)) +
  geom_point( color = "blue", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, colour="red", size=2, span=0.9) +
  labs(x = "number of guest in an apartment",y = "price per day (US dollars)") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,150), breaks = seq(0,150, 15)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,6), breaks = seq(0,60, 6))

Evaluation_graph01



#################################
#           LASSO               #
#################################


#Variable generation

# Basic Variables
basic_lev  <- c("guest_hosting", "beds_n", "bathrooms_n", "host_response_rate_n","host_acceptance_rate_n")

# Factorized variables
basic_add <- c( "bath_type")
reviews <- c("reviews_per_month_n","number_of_reviews_n","review_scores_rating", 
             "review_scores_cleanliness", "review_scores_location", "review_scores_communication",
             "review_scores_checkin")
# Higher orders
poly_lev <- c("guest_hosting2", "bathrooms2")

#not use p_host_response_rate due to missing observations

# Dummy variables: 
dummy_lev <- c("host_is_superhost", "instant_bookable", "m_host_response_time", "m_host_location", "m_availability")
               
##
#  create the interaction terms

X1  <- c('bath_type*beds_n',  'bath_type*guest_hosting', 'm_availability*bath_type', 'instant_bookable*bath_type')

# Additional interactions of factors and dummies

X2  <- c(paste0('(bath_type + beds_n) * (',
                paste(dummy_lev, collapse=' + '),')'))




## Manage different samples:


# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(barcelona))

# Set the random number generator:
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table

holdout_ids <- sample(seq_len(nrow(barcelona)), size = smp_size)
barcelona$holdout <- 0
barcelona$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- barcelona %>% filter(holdout == 1)

#Working data set
data_work <- barcelona %>% filter(holdout == 0)


## N= 5
n_folds <- 5
# Define seed value
seed_val <- 20210117

## Use LASSO

# Build variable model

vars_model_1 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,dummy_lev)
vars_model_2 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,dummy_lev)

# Set lasso tuning parameters

train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model without the interactions 
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_2, "price"), collapse = " + ")))


set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = 1)  # the column has a name "1", to be renamed

print(lasso_coeffs)

# Check the number of variables which actually has coefficients other than 0
lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Get the RMSE of the Lasso model 
#   Note you should compare this to the test RMSE
lasso_fitstats <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) 
lasso_fitstats
# Create an auxilary tibble
lasso_add <- tibble(Model='LASSO', Coefficients=nrow(lasso_coeffs_nz),
                    R_squared=lasso_fitstats$Rsquared, BIC = NA, 
                    Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE)
lasso_add
        
lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])



###########################################
#Random Forest
###########################################


# create train and holdout samples 
# train is where we do it all, incl CV

set.seed(2801)

# First pick a smaller than usual training set so that models run faster

# try <- createDataPartition(barcelona$price, p = 0.2, list = FALSE)
#data <- data[try, ]


train_indices <- as.integer(createDataPartition(barcelona$price, p = 0.7, list = FALSE))
data_train <- barcelona[train_indices, ]
data_holdout <- barcelona[-train_indices, ]

dim(data_train)
dim(data_holdout)



# Basic Variables inc neighnourhood

basic_vars  <- c("bath_type","guest_hosting", "beds_n", "bathrooms_n", 
                "host_response_rate_n","host_acceptance_rate_n", "neighbourhood_group_cleansed")

# reviews
reviews <- c("reviews_per_month_n","number_of_reviews_n","review_scores_rating", 
             "review_scores_cleanliness", "review_scores_location", "review_scores_communication",
             "review_scores_checkin")

# Dummy variables

dummy_var<- c("host_is_superhost", "instant_bookable", "m_host_response_time", "m_host_location", "m_availability")

#interactions 
X1  <- c('bath_type*beds_n',  'bath_type*guest_hosting', 'm_availability*bath_type', 'instant_bookable*bath_type')

# with boroughs
X2  <- c("bath_type*review_scores_rating", "instant_bookable*review_scores_rating",
         "guest_hosting*review_scores_rating", "beds_n*review_scores_rating")

basic_vars <- c("guest_hosting","beds_n")


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, dummy_var)
predictors_E <- c(basic_vars, reviews, dummy_var, X1,X2)



# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(1, 2, 3),
  .splitrule = "variance",
  .min.node.size = c(10, 20)
)



# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
    )
  })
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(2, 3, 4),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2

rf_model_2auto <-rf_model_2

# evaluate random forests

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2,
    model_2b = rf_model_2auto
    
  )
)
summary(results)

# Save outputs

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(data_in,"rf_tuning_modelB.tex"))



# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_2auto$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size,
  rf_model_2auto$finalModel$min.node.size
  
),
nrow=3, ncol=2,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(data_in,"rf_models_turning_choices.tex"))

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`),
                     mean(results$values$`model_2b~RMSE`)
),
nrow=3, ncol=1,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(data_in,"rf_models_rmse.tex"))

Decision: From the analysis, it is found that RMSE is lowwer in case of Random Forest and R squared value
is higher followed by LASSO and OLS regression model. So, Random Forest model can give more fit result. 
Since, I faced problems to generate more variables for random forest, more trees were not possible to run. 
Still I find, random forest gives the best prediction.
