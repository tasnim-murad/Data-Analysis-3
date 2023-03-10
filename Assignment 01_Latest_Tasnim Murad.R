##Tasks
#Build four predictive models using linear regression for earnings per hour. 
#1.	Models: the target variable is earnings per hour, all others would be predictors.
#2.	Model 1 shall be the simplest, model 4 the more complex. It shall be OLS. 
#   You shall explain your choice of predictors.
#3.	Compare model performance of these models (a) RMSE in the full sample, (2) cross-validated RMSE and 
#   (c) BIC in the full sample. 
#4.	Discuss the relationship between model complexity and performance. You may use visual aids. 

# CLEAR MEMORY
rm(list=ls())

# Import libraries 

library(tidyverse)
library(arm)
library(lmtest)
library(fixest)
library(texreg)
library(huxtable)
library(estimatr)
library(sandwich)
library(segmented)
library(lspline)
library(cowplot)
library(stargazer)
library(modelsummary)
library(funModeling)
library(haven)
library(caret)
library(grid)
if (!require(ggpubr)){
  if (!require(car)){
    install.packages('car')
  }
  install.packages('ggpubr')
  library(ggpubr)
}

##set working directory
getwd()
data_in <- "C:/Users/User/OneDrive - Central European University/Desktop/Assignment_01/"
setwd(data_in)


##import data (state must be as character: it's a mix of double and character in raw)
cps_earn <- read_csv(paste0(data_in,'morg-2014-emp.csv'),
                     col_types = cols(.default = "?", 
                                      state = "c"))
janitor::tabyl(cps_earn$state)

# check the data table
glimpse(cps_earn)

#SELECT OCCUPATION
# Pick a occupation types: Healthcare practitioner and technical occupations
cps_earn <- cps_earn %>% filter(occ2012>=3000 & occ2012<=3540)


#####################
#Feature Engineering
#######################

#gen female, wage(earnings/hour), lnwage, and age^2 variables
cps_earn <- cps_earn %>% mutate(female=as.numeric(sex==1)) %>%
  mutate(wage=cps_earn$earnwke/cps_earn$uhours) %>% 
  mutate(lnwage=log(wage)) %>%  
  mutate(agesq=age^2)   

#DISTRIBUTION OF EARNINGS
cps_earn %>% dplyr::select(earnwke,uhours,wage) %>% summary()

cps_earn %>% filter(wage>=1) %>% dplyr::select(earnwke,uhours,wage) %>% summary()

tabulate(cps_earn$sex)
table(cps_earn$occ2012,cps_earn$sex)


# condition by class( Work in Public or Private organization)
cps_earn <- cps_earn %>%
  mutate(class_govt = ifelse(class=="Government - Federal" | 
                               class=="Government - Local" | class=="Government-State", 1,0),
         class_private = ifelse(class=="Private, For Profit" | class=="Private, Nonprofit", 1,0))
         

# condition by ownchild (Whether has a child or not)
cps_earn <- cps_earn %>%
  mutate(haskid = ifelse(ownchild>=1, 1,0))

# check frequency by haskid
cps_earn %>% 
  group_by(haskid) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Generate Education Category
cps_earn <- cps_earn %>% mutate(ed_MA=as.numeric(grade92==44),
                      ed_Profess = as.numeric(grade92==45),
                      ed_PhD = as.numeric(grade92==46)
)

#Overview of the variables

kable(funModeling::df_status(cps_earn))

variables <- c('wage','age', 'female', 'haskid', 'class_govt', 'class_private', 'ed_MA', 'ed_Profess', 'ed_PhD')
info <- c('earning per hour in US dollar', 'age of the individual', 'if the individuaal is female', 
          'if the individual has kids', 'job in government sector', 'job in private sector', 
          'MA diploma holder', 'Professoinal Diploma holder', 'Doctoral degree holder')
type <- c('numeric', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric', 'numeric', 'numeric')

description <- data.frame(variables, info, type)

kable(description, caption = "Description of the main variables in the cleaned dataset")

# data summary
datasummary( age+ wage + female+ haskid+ class_govt+ class_private + ed_MA + ed_Profess + ed_PhD ~
               mean + Median + Min + Max + P25 + P75 + N , data = cps_earn )%>%
  kableExtra::kable_styling(latex_options = "hold_position")

###################################
# Linear regressions

###################################

model1 <- as.formula(wage ~ age + agesq)
model2 <- as.formula(wage ~ age + agesq + female)
model3 <- as.formula(wage ~ age + agesq + female + age*female + haskid+ 
                       class_govt +ed_MA + ed_Profess + ed_PhD)
model4 <- as.formula(wage ~ age + agesq + female + age*female + haskid+ female*haskid +
                       class_govt+ class_private+ ed_MA+ ed_Profess + ed_PhD)

# Running simple OLS
reg1 <- feols(model1, data=cps_earn, vcov = 'hetero')
reg2 <- feols(model2, data=cps_earn, vcov = 'hetero')
reg3 <- feols(model3, data=cps_earn, vcov = 'hetero')
reg4 <- feols(model4, data=cps_earn, vcov = 'hetero')


screenreg(list(reg1,reg2,reg3, reg4), caption= "Regressions Results_table_01", caption.above=TRUE)

huxreg(reg1, reg2, reg3,reg4, statistics = c(N = "nobs", R2 = "r.squared")) # Alternatively


##################################
## evaluation of the models
################################

fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")
etable( reg1 , reg2 , reg3 , reg4, fitstat = c('aic','bic','rmse','r2','n','k') )

##
# For writing out with stargazer: use lm instead
reg1 <- lm(model1, data=cps_earn)
reg2 <- lm(model2, data=cps_earn)
reg3 <- lm(model3, data=cps_earn)
reg4 <- lm(model4, data=cps_earn)

# evaluation of the models
models <- c("reg1", "reg2","reg3", "reg4")
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
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$wage)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

stargazer(regr[[1]],regr[[2]], regr[[3]], regr[[4]], type="text", title = "Regression Model Evaluation", 
          out=paste(data_in,"Table_02.txt",sep=""), digits=2, float = F, no.space = T)


eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)
stargazer(eval, summary = F, type="text", title = "All model evaluation", out=paste(data_in,"Table_03.txt",sep=""), 
          digits=2, dep.var.caption = "Dep. var: wage",float = F, no.space = T)

# The table in a nicer format
stargazer(reg1, reg2, reg3, reg4 , align = T,   digits=2, dep.var.caption = "Dep. var: wage", keep.stat = c("rsq","n"),
          type="text", title = "All model Evaluation", out=paste0(data_in,"table_04.txt",sep=""), no.space = T)


######################################
# Cross-validation(cv)
######################################
# set number of folds
k <- 4

set.seed(13505)
cv1 <- train(model1, cps_earn, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2 <- train(model2, cps_earn, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3 <- train(model3, cps_earn, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4 <- train(model4, cps_earn, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
cv_mat

stargazer(cv_mat, summary = F, digits=0, float=F, dep.var.caption = "Dep. var: wage",type="text", 
          title = "Cross Validation", out=paste(data_in,"table_05.txt",sep=""),no.space = T)


################################################
# Some graphs to evaluate the regression
###############################################

# lowess
Evaluation_graph01<- ggplot(data = cps_earn, aes(x=age, y=wage)) +
  geom_point( color = "blue", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, colour="red", size=1, span=0.9) +
  labs(x = "Age (years)",y = "Earning per hour (US dollars)") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,400), breaks = seq(0,150, 15)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,60), breaks = seq(0,60, 6))

Evaluation_graph01


# Lowess vs. quadratic (reg1) regression
Evaluation_graph2 <- ggplot(data = cps_earn, aes(x=age)) +
  geom_smooth(aes(y=wage, colour="blue"), method="loess", se=F, size=1) +
  geom_line(aes(y=predict(reg1), colour="red"), size=1,lty=2) +
  labs(x = "Age (years)",y = "Earning per hour (US dollars)") +
  scale_color_manual(name="", values=c("blue","red"),labels=c("Lowess in age","Quadratic in age")) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, 5)) +
  scale_y_continuous(limits = c(0,160), breaks = seq(0,160, 15)) +
  theme(legend.position = c(0.6,0.8),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "white"))

Evaluation_graph2

##############################
# Prediction of best model
##############################

cps_earn <- cps_earn %>% dplyr::select(age, agesq, female, haskid,
                                 class_govt, class_private, ed_MA, ed_Profess, ed_PhD, wage)

# Add new observation
new <- tibble(age=35, agesq=25^2,female=1,haskid=0, class_govt=1, 
              class_private=0,ed_MA=1,ed_Profess=0,ed_PhD=0, wage=NA)


# Predict price with only 2 predictors (Model3)
pred3 <- feols(model1, data=cps_earn, vcov = 'hetero')
# Standard errors of residuals
p3 <- predict(pred3, cps_earn)
resid_p3 <- p3-cps_earn$wage
summary(resid_p3)

# predict value for newly added obs
pred3_new <- predict(pred3, newdata = new ,se.fit = TRUE, interval = "prediction")
p3<- pred3_new$fit

# Predict price with all predictors (Model4)
pred4 <- feols(model4, data=cps_earn,vcov = 'hetero')

# Standard errors of residuals
p4 <- predict(pred4, cps_earn)
resid_p4 <- p4-cps_earn$wage
summary(resid_p4)


# predict value for newly added obs
pred4_new <- predict(pred4, newdata = new, se.fit = TRUE, interval = "prediction")
p4<- pred4_new$fit
pred4_new 

#get model rmse
cps_earn$p4a <- predict( pred4, cps_earn)
log_na <- is.na( cps_earn$p4a )
rmse4 <- RMSE(cps_earn$p4a[!log_na],cps_earn$wage[!log_na])
rmse4

# Result summary (prediction with 95% interval)
sum1 <- cbind(t(pred3_new[,c(1,3,4)]), t(pred4_new[,c(1,3,4)]))
colnames(sum1) <- c('Model3', 'Model4')
rownames(sum1) <- c('Predicted', 'PI_low (95%)', 'PI_high (95%)')

sum1

stargazer(sum1, summary = F, digits=0, float=F, type="text", out=paste(data_in,"table06.txt",sep=""))


# prediction with 80% interval

# summary of predictions and PI 80% version
# predict value for newly added obs

pred3_new80 <- predict(pred3, newdata = new, se.fit=TRUE, interval = "prediction", level=0.8)
p380<- pred3_new80$fit

pred4_new80 <- predict(pred4, newdata = new,se.fit = TRUE, interval = "prediction", level=0.8)
p480<- pred4_new80$fit

# Result summary
sum2 <- cbind(t(pred3_new80[,c(1,3,4)]), t(pred4_new80[,c(1,3,4)]))
colnames(sum2) <- c('Model3', 'Model4')
rownames(sum2) <- c('Predicted', 'PI_low (80%)', 'PI_high (80%)')
sum2

stargazer(sum2, summary = F, digits=0, float=F, type="text",  out=paste(data_in,"Table07.txt",sep=""))

### Conclusion:
#Model 3 and model 4 have comparatively higher R square value and lower RMSE and BIC. So, either of the model should be best model to predict earnings 
#per hour of the respondents. Evaluating by 4-fold cross validation, similarly we find lower Root Mean Squared Error (RMSE) for Model 3 and model 4.  
# By comparing predicted value at 80% and 95% prediction interval, we can take decision for the best model.Since, the range of interval is comparatively 
#lower in case of Model 4, we can conclude that, Model 4 is the best model to predict Earnings per hour. Finally, more complex model has more variables 
#that might generate higher R-Square but might not give best MSE in the live data. While predicting live data, I found the RMSE 15.34 which is also sort of 
#similar to for the analysis with original data. So, overfitting or underfitting is not observed for model 4. Therefore, we can conclude that performance of Model 4 
#is not affected for model complexity.
################## The End ################
