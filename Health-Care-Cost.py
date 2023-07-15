##      age    sex    bmi children smoker    region   charges
## 385   44   male 22.135        2     no northeast  8302.536
## 1054  47   male 29.800        3    yes southwest 25309.489
## 547   28   male 35.435        0     no northeast  3268.847
## 1179  23 female 34.865        0     no northeast  2899.489
## 1255  34 female 27.720        0     no southeast  4415.159


## Data 
## 
##  7  Variables      1338  Observations
## ---------------------------------------------------------------------------
## age 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1338        0       47    0.999    39.21    16.21       18       19 
##      .25      .50      .75      .90      .95 
##       27       39       51       59       62 
## 
## lowest : 18 19 20 21 22, highest: 60 61 62 63 64
## ---------------------------------------------------------------------------
## sex 
##        n  missing distinct 
##     1338        0        2 
##                         
## Value      female   male
## Frequency     662    676
## Proportion  0.495  0.505
## ---------------------------------------------------------------------------
## bmi 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1338        0      548        1    30.66    6.893    21.26    22.99 
##      .25      .50      .75      .90      .95 
##    26.30    30.40    34.69    38.62    41.11 
## 
## lowest : 15.960 16.815 17.195 17.290 17.385, highest: 48.070 49.060 50.380 52.580 53.130
## ---------------------------------------------------------------------------
## children 
##        n  missing distinct     Info     Mean      Gmd 
##     1338        0        6    0.899    1.095    1.275 
##                                               
## Value          0     1     2     3     4     5
## Frequency    574   324   240   157    25    18
## Proportion 0.429 0.242 0.179 0.117 0.019 0.013
## ---------------------------------------------------------------------------
## smoker 
##        n  missing distinct 
##     1338        0        2 
##                       
## Value         no   yes
## Frequency   1064   274
## Proportion 0.795 0.205
## ---------------------------------------------------------------------------
## region 
##        n  missing distinct 
##     1338        0        4 
##                                                   
## Value      northeast northwest southeast southwest
## Frequency        324       325       364       325
## Proportion     0.242     0.243     0.272     0.243
## ---------------------------------------------------------------------------
## charges 
##        n  missing distinct     Info     Mean      Gmd      .05      .10 
##     1338        0     1337        1    13270    12301     1758     2347 
##      .25      .50      .75      .90      .95 
##     4740     9382    16640    34832    41182 
## 
## lowest :  1121.874  1131.507  1135.941  1136.399  1137.011
## highest: 55135.402 58571.074 60021.399 62592.873 63770.428
## ---------------------------------------------------------------------------
from plotnine import ggplot

# Exploratory Data Analysis

x <- ggplot(Data, aes(age, charges)) + geom_jitter(color = "blue", alpha = 0.5) + theme_light()

y <- ggplot(Data, aes(bmi, charges)) + geom_jitter(color = "green", alpha = 0.5) + theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


x <- ggplot(Data, aes(sex, charges)) + geom_jitter(aes(color = sex), alpha = 0.7) + theme_light()

y <- ggplot(Data, aes(children, charges)) + geom_jitter(aes(color = children), alpha = 0.7) + theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



x <- ggplot(Data, aes(smoker, charges)) + geom_jitter(aes(color = smoker), alpha = 0.7) + theme_light()

y <- ggplot(Data, aes(region, charges)) + geom_jitter(aes(color = region), alpha = 0.7) + theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')


# Linear Regression Model

n_train <- round(0.8 * nrow(Data))
train_indices <- sample(1:nrow(Data), n_train)
Data_train <- Data[train_indices, ]
Data_test <- Data[-train_indices, ]

formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")

#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared

#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

# Train and Test New Model

formula_1 <- as.formula("charges ~ age + bmi + children + smoker + region")

model_1 <- lm(formula_1, data = Data_train)
summary(model_1)

r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))


# Model Performance

Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) +  geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("Prediction vs. Real values")

Data_test$residuals <- Data_test$charges - Data_test$prediction

ggplot(data = Data_test, aes(x = prediction, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +geom_hline(yintercept = 0, linetype = 3, color = "red") + ggtitle("Residuals vs. Linear model prediction")


ggplot(Data_test, aes(x = residuals)) + geom_histogram(bins = 15, fill = "blue") + ggtitle("Histogram of residuals")

GainCurvePlot(Data_test, "prediction", "charges", "Model")


