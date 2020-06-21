##############################################################################
## Final Project
## Authors: "Nimisha Goel & Saurabh Gupta"
## TODO: Add description : Why this data set, and process of the whole project
## Description: 
##  Exploratory analysis of birthwt dataset
##  Make plots to summarize some parameters of dataset with birthwt.
##  
##              
##############################################################################
## Clearing the environment
rm(list = ls())

## Loading the Libraries : MASS, data.table, ggplot2, stats and kknn
library(MASS)
library(data.table)
library(ggplot2)
library(stats)
library(kknn)

## Loading the data and converting it to data.table
data("birthwt")
birthwt <- data.table(birthwt)

## Converting categorical variables to numeric
birthwt[, race:= as.factor(race)]
birthwt[, smoke:=as.factor(smoke)]
birthwt[, ht:=as.factor(ht)]
birthwt[, ui:=as.factor(ui)]


## Descriptive analysis - Plot:  Number babies at certain weight ranges for each race
bwtcount <- ggplot(birthwt, aes(x=bwt)) + 
  geom_histogram(aes(x = bwt), color = "black", fill = "blue", bins = 50) +
  facet_grid(race~.)
labs(title = "Birthweight of Babies Distribution Faceted by Race[1- White,2-Black,3-Other",
     x = "Birthweight of babies (in grams)",
     y = "Number of babies born")
print(bwtcount)


## Mother's Age v/s Birthweight
## Analytical method -  Bivariate Regression to plot Mother's Age v/s Birthweight
regression_age <- lm(bwt~age, data=birthwt)
summary(regression_age)

## From the regression data, we can see that the Intercept (b0) is 2655.74 grams and Coefficient(b1) for
## age is 12.43. 
## The equation is bwt = b0 + b1*age. 
## The Intercept value means that at the lowest age (14 years) of the mother, 
## the baby's weight is 2655.74 grams. The coefficient value means that 1 year increase in mother's 
## age increases the baby's weight by 12.43 grams.
## The Adjusted R-squared value for the regression is 0.0029, which means that only 0.29% 
## variance in birthweight can be explained by "age".

birthwt[, predicted_wt_age := predict(regression_age)]
ggplot(birthwt, aes(x=age)) +
  geom_point(aes(y=bwt)) +
  geom_line(aes(y=predicted_wt_age), color="red") +
  labs(title = "Bivariate Regression to plot Mother's Age v/s Birthweight",
       x = "Mother's Age",
       y = "Birthweight of Baby(in grams)")


## Mother's weight vs Birthweight
## Analytical Method: Bivariate Regression to plot Mother's Weight v/s Baby's Weight
regression_motherwt <- lm(bwt~lwt, data=birthwt)
summary(regression_motherwt)

## From the regression data we can see that the Intercept(b0) is 2369.624 grams and Coefficient(b1) for
## mother's weight is 4.429. 
## The equation is bwt = b0 + b1*lwt 
## The Intercept value means that at the lowest weight (80 pounds) of the mother, the baby's 
## weight is 2369.624 grams. The coefficient value means that 1 pound increase in mother's 
## weight increases the baby's weight by 4.429 grams. The Adjusted R-squared value for the 
## regression is 0.02933, which means that around 2.9% variance in birthweight can be 
## explained by "mother's weight". Thus, the continuous variable mother's weight (lwt) will
## give a more accurate plot for predicting baby's weight

birthwt[, predicted_wt_motherwt := predict(regression_motherwt)]
ggplot(birthwt, aes(x=lwt)) +
  geom_point(aes(y=bwt)) +
  geom_line(aes(y=predicted_wt_motherwt), color="red") +
  labs(title = "Bivariate Regression to plot Mother's Weight v/s Baby's Weight",
       x = "Mother's Weight (lbs)",
       y = "Baby's Weight (in grams)")

## Race and Baby weight
## Descriptive Analysis : Bar Plot for mean weight v/s race
meanbwtvsrace <- ggplot(birthwt[, list(mean.bwt=mean(bwt)), 
                                 by=list(race)],
                            aes(x=race, y=mean.bwt)) +
  geom_bar(aes(fill=race), stat="identity") +
  labs(title = "Bar Plot for race of mothers v/s mean Weight of babies",
       x = "Race of mothers (1 - White, 2 - Black, 3 - Other",
       y = "Mean weight of the babies for each race (grams)")
print(meanbwtvsrace)

## Smoking and birth weight
## Descriptive analysis : Mean birthweight bar graph for smoking data
ggplot(birthwt[, list(mean.bwt=mean(bwt)), 
                                    by=list(smoke)],
                            aes(x=smoke, y=mean.bwt)) +
  geom_bar(aes(fill=smoke), stat="identity") + 
  labs(title = "Bar plot for smoking status of mother v/s mean weight of babies",
       x = "Smoking status of mother (1- smoker, 0 - non-smoker",
       y = "Mean weight of babies")

## Previous premature labours and birth weight
## Analytical method : Bivariate analysis to plot Number of previous premature labors v/s Birthweight
regression_prematlab <- lm(bwt~ptl, data=birthwt)
summary(regression_prematlab)

## From the regression data we can see that the Intercept(b0) is 2989.34 grams and Coefficient(b1) for
## previous premature labors(b1) is -228.59. 
## The equation is bwt = b0 + b1*lwt
## The Intercept value means that at the lowest number (0) of previous premature labors, the 
## baby's weight is 2989.34 grams. The coefficient value means that if the mother has 1 more 
## premature labor the baby's weight declines by 228.59 grams. The Adjusted R-squared value 
## for the regression is 0.0187, which means that around 1.8% variance in birthweight can be 
## explained by "Previous premature labors(ptl)". Thus, the continuous variable mother's
## weight (ptl) will decrease the weight of baby with increasing number of premature labours.

birthwt[, predicted_ptlwt := predict(regression_prematlab)]
ggplot(birthwt, aes(x=ptl)) +
  geom_jitter(aes(y=bwt)) +
  geom_line(aes(y=predicted_ptlwt), color="red") +
  labs(title = "Bivariate Regression of Previous Premature labours v/s Birthweight",
       x = "Number of Previous premature labours",
       y = "Birthweight of Babies (in grams)")

## ht vs bwt
## Descriptive analysis : Bar plots for history of hypertenstion v/s mean Baby weight
ggplot(birthwt[, list(mean.bwt=mean(bwt)),by=list(ht)],aes(x=ht, y=mean.bwt)) +
  geom_bar(aes(fill=ht), stat="identity") +
  labs(title = "Bar Plot of History of Hypertension v/s Mean Baby Weight",
       x = "History of Hypertension",
       y = "Mean Baby weight (in grams)")

## ui vs bwt
## Descriptive Analysis : Uterine irritability v/s Mean Baby Weight
ggplot(birthwt[, list(mean.bwt=mean(bwt)),by=list(ui)],aes(x=ui, y=mean.bwt)) +
  geom_bar(aes(fill=ui), stat="identity") +
  labs(title = "Bar Plot of Uterine Irritability v/s Mean Baby Weight",
       x = "Uterine Irritability",
       y = "Mean Baby Weight")

## Since Mother's age(age variable) has a less value of Adjusted R-squared value and from the plot
## we could see that the value doesn't affect the baby's birthweight much, that is why we have excluded
## the variable. From the different bar plots for race, smoking status(smoke), history of hypertension(ht) and
## uterine irritability(ui), we saw that there is an effect on mean baby's weight. 

## Analytical method - Multivariate Regression with bwt, lwt, race, smoke, ht, ui
## Equation : bwt = b0 + b1*lwt + b2*race + b3*smoke + b4*ht + b5*ui
regression_multi <- lm(bwt~lwt + race + smoke + ht + ui, data=birthwt)
summary(regression_multi)

## For the above multivariate regression, the Intercept value captures Race = 1(White), Non-Smoker(smoke=0),
## No Hypertension (ht=0) and No Uterine Irritability(ui=0)
## Intercept : For a non-smoker, mother (weight 80 pounds) with race White, no history of hypertension and no uterine irritability,
## the birthweight of baby is 2837.264 grams
## lwt : Continuous variable. For a one pound increase in mother's weight, the baby's weight increases by 4.242 grams. 
## race2 : Categorical variable for race Black. For the race=Black, the baby's weight decreases by 475.058 grams
## race3 : Categorical varibale for race Other. For the race=Other, the baby's weight decreases by 348.15 grams
## ht1 : Categorical variable for history of hypertension (value 1). If mother has history of hypertension, the baby's weight
## decreases by 585.193
## ui1 : Categorical variable for Uterine Irritability (value 1). If mother has uterine irritability, the baby's weight
## decreases by 525.524
## size : Continuous variable. For a size of party increase by 1, the tip for the table increases by $0.186131. 

birthwt[, predicted_wt := predict(regression_multi)]
  ggplot(birthwt, aes(x=lwt)) +
  geom_point(aes(y=bwt, color=race), alpha=0.7) +
  geom_line(aes(y=predicted_wt), size=1) +
  facet_grid(smoke~ht,scales = "free") + 
  labs(title="Multivariate Regression with for Mother's weight v/s baby's weight: smoke, ht, ui",
       x="Mother's Weight in pounds",
       y="Baby's weight in grams")
  
## From the Descriptive Analysis, we could see that the mean Birthweight is most affected by Uterine 
## irritability in mothers with a lower mean weight by around 600 gms. Hence, running a simplified 
## multivariate regression with Mother's weight and Uterine Irritability predictor variables
## Analytical method - Multivariate Regression with bwt~lwt,ui
## Equation : bwt = b0 + b1*lwt + b2*ui
regression_multi_simple <- lm(bwt~lwt + ui, data=birthwt)
summary(regression_multi_simple)

## From the summary of this regression, the intercept captures no uterine irritability in mothers.
## Intercept : The Birthweight is 2572.724 grams, for a mother with no uterine irritability and minimum weight in pounds.
## lwt : With a 1 pound increase in mother's weight, the baby's weight increases by 3.476
## ui1 : For a mother with uterine irritability, the baby's weight decreases by 535.687 grams.

birthwt[, predicted_wt_simple := predict(regression_multi_simple)]
ggplot(birthwt, aes(x=lwt)) +
    geom_point(aes(y=bwt), alpha=0.7) +
    geom_line(aes(y=predicted_wt_simple), size=1) +
    facet_grid(ui~.,scales = "free") + 
    labs(title="Multivariate Regression with for Mother's weight v/s baby's weight Faceted by Uterine Irritability",
         x="Mother's Weight in pounds",
         y="Baby's weight in grams")
  

### LOGISTIC REGRESSION / MACHINE LEARNING ###

## Likeliness of Healthy Baby weight(>2.5 KG) v/s Mother's Weight
logistic_reg <- glm(low ~ lwt, data=birthwt,family = binomial)
summary(logistic_reg)

## The intercept value in the above logistic regression is 0.998331 and the log odds of the
## birthweight of the baby to be less than 2.5 kg is  2.714. 
## The Coefficient value is -0.01406, which means the logs odds ratio for a pound increase in mother's 
## weight will increase the odds ratio of baby's weight to be above 2.5 kg by 0.986 times. 

birthwt[, predicted_wt:= predict(logistic_reg, type="response")]
ggplot(birthwt, aes(x=lwt)) +
  geom_point(aes(y=low)) +
  geom_line(aes(y=predicted_wt), size=1) +
  labs(title = "Logistic Regression curve for finding probability baby to be below 2.5 kg",
       x= "Mother's Weight",
       y= "Probability of baby being below 2.5 KG")

## Likeliness of Healthy Baby weight(>2.5 KG) v/s Mother's Weight and smoking status
logistic_reg_smoke <- glm(low ~ lwt + smoke, data=birthwt,family = binomial)
summary(logistic_reg_smoke)
## The intercept value in the above logistic regression is 0.62200 and the log odds of the
## birthweight of the baby to be less than 2.5 kg is  1.863. 
## The Coefficient value for lwt is -0.01332, which means the logs odds ratio for a pound increase in mother's 
## weight will increase the odds ratio of baby's weight to be above 2.5 kg by 0.986.
## The Coefficient for smoke1 is 0.67667, so for a smoker mother the baby being born below 2.5 kg is 1.967 times
## more likely than for a non-smoker mother.


### Out-of-Sample Validation for one variable and two variables ###

## 1. Splitting the data into training and testing set (85%-15%)
random_order <- sample(nrow(birthwt))
birthwt <- birthwt[random_order]
testing_set <- birthwt[1:30]
training_set <- birthwt[31:189]

## 2. Running both logistic regressions on the training set;
logistic_reg_bothpredvar <- glm(low ~ lwt + smoke, data=training_set,family = binomial)
logistic_reg_onepredvar <- glm(low ~ lwt,data=training_set,family = binomial)

## 3. Predicting values from each regression for the testing set and converting those values to binary variables;
testing_set[, predict_bothpredvar:= predict(logistic_reg_bothpredvar, newdata = testing_set, type = "response")]
testing_set[, predict_onepredvar:= predict(logistic_reg_onepredvar, newdata = testing_set, type = "response")]

## 4. Determining which regression predicts "better"-- because your outcome is binary, the error metric you should use is
##    "what percent of the time did my regression predict correctly?" 
## Take the Root Mean Square values for both regressions
testing_set[, squared_bothpredvar:= (predict_bothpredvar-low)^2]
testing_set[, squared_onepredvar:= (predict_onepredvar-low)^2]

rmse_bothpredvar <- sqrt(sum(testing_set$squared_bothpredvar)/nrow(testing_set))
print(rmse_bothpredvar)
rmse_onepredvar <- sqrt(sum(testing_set$squared_onepredvar)/nrow(testing_set))
print(rmse_onepredvar)
## For two predictor variables (lwt and smoke), the value is predicted correctly 42.99596% of times
## For one predictor variable, the value is predicted correctly 44.65154% of times.
## Thus, the logistic regression with ONE Predictor Variable predicts BETTER.

##For this, we make ggplot of both regression, with both variables(lwt+smoke in blue) and one varibale (lwt in red)
ggplot(testing_set, aes(x=lwt)) + 
  geom_point(aes(y=low)) +
  geom_line(aes(y=predict_bothpredvar), color="blue") + 
  geom_line(aes(y=predict_onepredvar), color="red") + 
  labs(title = "Out of Sample Validation for two variables(blue) and one variable(red)",
       x="Mother's Weight in pounds",
       y="Probability of baby being above 2.5 KG")






  