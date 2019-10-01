ClasseExercise
================
Swetha Jegannathan
01/10/2019

# PREDICT EXERCISE EFFECTIVENESS

**Objective** Predict the manner and effectiveness of exercise done by 6
participants on accelerometers on the belt, forearm, arm, and dumbell on
pml-testing.csv dataset for 20 different test cases. Dependant variable
is “classe” with 5 levels of classificaiton - A, B, C, D and E. The 6
users are - Adelmo Carlitos Charles Eurico Jeremy Pedro

The participants were asked to perform barbell lifts correctly and
incorrectly in 5 different ways.

**Dataset** The datasets pml-training.csv and pml-testung.csv are
imported into the system. The training dataset has 19622 rows and 160
variables and testing data set has 20 rows and 160 variables. There are
lot of NAs, blanks and character variable in the dataset. which requires
intensive
    cleaning.

``` r
getwd()
```

    ## [1] "C:/Users/SWETHA JEGANNATHAN/Desktop/WorkingDirectory/PROJECT/cPROJECT"

``` r
train <- read.csv("pml-training.csv", stringsAsFactors = FALSE)

test <- read.csv("pml-testing.csv", stringsAsFactors = FALSE)
set.seed(13243)

dim(train)
```

    ## [1] 19622   160

``` r
dim(test)
```

    ## [1]  20 160

**Create Subset** Since the dataset has 19622 rows, for easy of
calculation we take a subset of 5000 rows out of original training data

``` r
train.sample <- sample(nrow(train), 5000) 

train.subset <- train[train.sample,]
```

**Data Cleaning** Remove columns with NAs, blanks and unecessary
characters. There are only 52 columns that qualify for building our
predictive model. “classe” is the dependent variable,

``` r
train.dropna <- train.subset[,colSums(is.na(train.subset)) < 100]
train.dropchr <- train.dropna[, !sapply(train.dropna, is.character)] 
train.final <- train.dropchr[,4:56]
train.final$classe <- factor(train.subset$classe) #Re-insert classe column, convert to factor
```

**Data Partition in Training data** We partition our training data into
train1 - 80% and train2 - 20%. We shall build our model on train1 and
predict on train2 and use this model to predict the “classe” on 20 rows
in testing data.

``` r
#createDataPartition: Subset 20% of training data for cross-validation
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.6.1

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

``` r
trainIndex <- createDataPartition(y = train.final$classe, p=0.8, list = FALSE) 

train1 <- train.final[trainIndex,]

train2 <- train.final[-trainIndex,]

dim(train1)
```

    ## [1] 4002   54

``` r
str(train1)
```

    ## 'data.frame':    4002 obs. of  54 variables:
    ##  $ num_window          : int  275 808 624 134 157 274 209 252 433 575 ...
    ##  $ roll_belt           : num  1.66 1.61 131 119 153 1.6 121 0.73 1.13 124 ...
    ##  $ pitch_belt          : num  3.88 3.48 21.4 16.3 6.88 1.65 -43.9 0.35 5.28 -42 ...
    ##  $ yaw_belt            : num  -88.4 -88.3 21.8 -2.65 49.3 -88.2 174 -88.3 -88.5 166 ...
    ##  $ total_accel_belt    : int  2 2 20 18 25 2 17 3 5 17 ...
    ##  $ gyros_belt_x        : num  0.05 0.05 -0.35 0.08 0.31 -0.1 0.11 -0.05 -0.02 0.13 ...
    ##  $ gyros_belt_y        : num  0 0.02 -0.03 0.11 0.02 0.02 0.11 0.02 -0.02 0.11 ...
    ##  $ gyros_belt_z        : num  -0.02 0.05 -0.36 -0.15 -0.43 -0.03 -0.16 0 -0.02 -0.15 ...
    ##  $ accel_belt_x        : int  -7 -7 -51 -22 -28 -6 50 1 -16 49 ...
    ##  $ accel_belt_y        : int  1 2 69 64 63 4 42 2 5 43 ...
    ##  $ accel_belt_z        : int  19 23 -175 -168 -238 22 -157 30 41 -155 ...
    ##  $ magnet_belt_x       : int  46 29 20 25 47 30 171 24 44 159 ...
    ##  $ magnet_belt_y       : int  565 566 583 598 451 562 611 565 625 607 ...
    ##  $ magnet_belt_z       : int  -439 -434 -369 -339 -542 -431 -307 -431 -313 -302 ...
    ##  $ roll_arm            : num  74.9 87.2 138 -54.7 -35.7 85.4 10.4 88.3 0 -6.83 ...
    ##  $ pitch_arm           : num  5.24 10 -7.65 -24.4 -0.48 -41.2 2.91 7.21 0 -4.83 ...
    ##  $ yaw_arm             : num  113 109 -20.3 6.3 -50.3 22.9 -34.3 99 0 -41.4 ...
    ##  $ total_accel_arm     : int  31 30 44 25 19 18 6 31 31 14 ...
    ##  $ gyros_arm_x         : num  -1.96 -2.04 3.2 1.67 -3.56 2.89 2.41 1.45 -2.91 -0.22 ...
    ##  $ gyros_arm_y         : num  0.51 0.37 -2.42 -1 0.71 -0.56 -1.11 -0.43 1.73 -0.4 ...
    ##  $ gyros_arm_z         : num  -0.18 0 0.25 0.95 0.8 0.43 0.33 0.08 -0.62 0.77 ...
    ##  $ accel_arm_x         : int  -233 -223 368 130 -166 96 29 -226 -212 -87 ...
    ##  $ accel_arm_y         : int  187 191 -137 -142 -47 76 -44 190 220 -66 ...
    ##  $ accel_arm_z         : int  -60 -42 -170 -150 72 -132 -26 -67 -24 86 ...
    ##  $ magnet_arm_x        : int  -313 -350 680 737 435 711 664 -299 153 296 ...
    ##  $ magnet_arm_y        : int  303 333 -146 -61 111 -42 88 340 369 136 ...
    ##  $ magnet_arm_z        : int  572 556 -248 -46 506 162 278 561 520 579 ...
    ##  $ roll_dumbbell       : num  22.3 28.6 -59.7 109.2 -93.3 ...
    ##  $ pitch_dumbbell      : num  4.43 110.79 -22.93 41.53 -3.49 ...
    ##  $ yaw_dumbbell        : num  -134.3 35.4 92 -22.4 -63.6 ...
    ##  $ total_accel_dumbbell: int  2 27 16 5 3 1 16 17 29 17 ...
    ##  $ gyros_dumbbell_x    : num  -0.16 0.63 0.37 -0.1 0.24 -0.02 0.11 -1.48 0.24 0.16 ...
    ##  $ gyros_dumbbell_y    : num  -0.19 0.02 -0.58 -0.18 1 0.11 0.11 0.47 0.05 0.14 ...
    ##  $ gyros_dumbbell_z    : num  -0.08 -0.52 -0.43 -0.16 -0.05 0.2 -0.11 -0.11 -0.57 0.03 ...
    ##  $ accel_dumbbell_x    : int  1 235 -36 20 -1 -2 -47 109 -139 -47 ...
    ##  $ accel_dumbbell_y    : int  5 74 -89 44 -23 -3 80 48 159 90 ...
    ##  $ accel_dumbbell_z    : int  -22 91 126 -11 -17 -13 -127 115 -192 -131 ...
    ##  $ magnet_dumbbell_x   : int  -490 371 352 -363 -295 -492 -559 85 -512 -530 ...
    ##  $ magnet_dumbbell_y   : int  194 315 -657 501 543 153 280 334 364 312 ...
    ##  $ magnet_dumbbell_z   : num  263 336 -27 -49 1 269 52 436 -47 54 ...
    ##  $ roll_forearm        : num  -172 170 155 -61.7 41.1 174 0 -178 121 0 ...
    ##  $ pitch_forearm       : num  7.8 15.8 38.7 45.7 27.1 20.5 0 17.4 49.2 0 ...
    ##  $ yaw_forearm         : num  112 -49.4 177 -139 -37.6 -7.15 0 77.6 158 0 ...
    ##  $ total_accel_forearm : int  40 20 25 43 19 25 49 47 23 52 ...
    ##  $ gyros_forearm_x     : num  -0.66 -0.71 -1.25 -0.39 0.48 -0.03 0.61 -0.19 0.88 1.2 ...
    ##  $ gyros_forearm_y     : num  1.27 3.07 4.99 1.98 -1.45 -3.18 1.81 3.4 -3 -1.8 ...
    ##  $ gyros_forearm_z     : num  0.74 1.16 1.59 0.15 -0.02 0.07 1.05 1.2 -0.54 -1.18 ...
    ##  $ accel_forearm_x     : int  -76 110 -64 -409 -70 166 -159 49 -131 -285 ...
    ##  $ accel_forearm_y     : int  333 -3 192 -79 169 68 456 422 81 420 ...
    ##  $ accel_forearm_z     : int  187 159 -130 32 9 166 5 167 -163 -17 ...
    ##  $ magnet_forearm_x    : int  -688 302 -675 -676 -135 406 -446 -212 -665 -694 ...
    ##  $ magnet_forearm_y    : num  1180 -690 119 110 723 -288 814 819 199 472 ...
    ##  $ magnet_forearm_z    : num  -559 0 531 441 489 -130 754 -414 618 615 ...
    ##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 3 2 5 4 5 3 1 2 1 4 ...

``` r
dim(train2)
```

    ## [1] 998  54

``` r
str(train2)
```

    ## 'data.frame':    998 obs. of  54 variables:
    ##  $ num_window          : int  266 329 117 222 795 167 553 283 341 520 ...
    ##  $ roll_belt           : num  1.53 1.32 121 0.71 1.73 133 127 -6.28 1.7 126 ...
    ##  $ pitch_belt          : num  2.06 7.09 25.7 4.76 -2.39 10.5 -41.5 1.3 7.94 -43.1 ...
    ##  $ yaw_belt            : num  -88.4 -94 -5.29 -87.9 -85.5 55.3 161 -82.9 -93.4 164 ...
    ##  $ total_accel_belt    : int  2 3 20 3 4 17 17 2 3 18 ...
    ##  $ gyros_belt_x        : num  -0.03 -0.03 -0.48 0.06 0.13 0 0.13 -0.08 0.02 0.11 ...
    ##  $ gyros_belt_y        : num  0 -0.02 -0.02 0 0.19 0.08 0.11 -0.29 0 0.11 ...
    ##  $ gyros_belt_z        : num  -0.05 -0.02 -0.46 -0.03 -0.15 -0.44 -0.16 -1.15 -0.02 -0.16 ...
    ##  $ accel_belt_x        : int  -6 -17 -40 -15 25 -12 49 -13 -21 52 ...
    ##  $ accel_belt_y        : int  3 2 73 2 7 53 43 2 5 44 ...
    ##  $ accel_belt_z        : int  22 22 -173 30 31 -160 -158 17 18 -159 ...
    ##  $ magnet_belt_x       : int  38 -6 2 56 67 17 167 72 8 159 ...
    ##  $ magnet_belt_y       : int  565 601 586 577 575 602 608 568 607 607 ...
    ##  $ magnet_belt_z       : int  -440 -310 -369 -410 -414 -327 -310 -425 -299 -312 ...
    ##  $ roll_arm            : num  95.8 -141 9.83 72 51.3 -85.8 -7.77 74.9 68.1 -47.1 ...
    ##  $ pitch_arm           : num  12 4.3 25.5 43.3 -7.4 -20.3 2.86 6.42 -0.89 -55.6 ...
    ##  $ yaw_arm             : num  119 -171 -24.3 85.1 64.7 43.2 -47 119 49 27.4 ...
    ##  $ total_accel_arm     : int  30 34 37 29 16 41 14 31 23 41 ...
    ##  $ gyros_arm_x         : num  -0.83 -0.02 -2.44 -0.02 -1 -0.11 2.3 -1.04 2.44 0.53 ...
    ##  $ gyros_arm_y         : num  0.08 -0.02 0.56 0 0.22 -0.48 -1.25 0.22 -1.43 -0.5 ...
    ##  $ gyros_arm_z         : num  0.28 -0.03 0.28 0 -0.15 1.02 1.41 -0.11 -0.2 0.8 ...
    ##  $ accel_arm_x         : int  -217 -289 347 -217 -8 144 -125 -228 -151 96 ...
    ##  $ accel_arm_y         : int  178 112 -19 154 138 -178 -18 176 163 -142 ...
    ##  $ accel_arm_z         : int  -82 -126 86 -108 81 -325 62 -94 -5 -358 ...
    ##  $ magnet_arm_x        : int  -456 -367 692 -336 250 530 262 -348 272 373 ...
    ##  $ magnet_arm_y        : int  312 353 117 228 70 -152 217 311 276 -74 ...
    ##  $ magnet_arm_z        : int  472 510 214 598 617 -397 559 530 515 -529 ...
    ##  $ roll_dumbbell       : num  -101.5 65.7 -16 -85.5 58.4 ...
    ##  $ pitch_dumbbell      : num  -38.2 3.6 16 -66 10.6 ...
    ##  $ yaw_dumbbell        : num  -38.2 91.2 134.4 23.8 97.4 ...
    ##  $ total_accel_dumbbell: int  5 3 11 2 5 2 16 10 3 25 ...
    ##  $ gyros_dumbbell_x    : num  -0.03 0.03 0.43 0.02 -0.32 0.08 0.29 -0.48 -0.11 0.31 ...
    ##  $ gyros_dumbbell_y    : num  -0.24 -0.03 -0.16 -0.18 0.13 -0.16 -0.02 0.51 0.24 -0.27 ...
    ##  $ gyros_dumbbell_z    : num  0.08 -0.07 -0.21 0.1 0.11 -0.21 -0.1 0.02 -0.15 -0.41 ...
    ##  $ accel_dumbbell_x    : int  -18 1 17 -13 5 -7 -45 19 10 43 ...
    ##  $ accel_dumbbell_y    : int  -41 17 -17 -16 26 6 50 46 5 221 ...
    ##  $ accel_dumbbell_z    : int  -18 22 104 5 39 -18 -140 90 -22 -91 ...
    ##  $ magnet_dumbbell_x   : int  -504 -534 475 -441 -472 -280 -568 -234 -510 -184 ...
    ##  $ magnet_dumbbell_y   : int  126 321 -571 264 191 555 221 301 365 565 ...
    ##  $ magnet_dumbbell_z   : num  275 -38 -99 280 294 -53 72 404 -78 130 ...
    ##  $ roll_forearm        : num  174 13.5 138 -125 -77.1 -78.8 0 -177 73.6 0 ...
    ##  $ pitch_forearm       : num  11.8 -63.2 40.7 -22.3 35.6 -6 0 6.92 -18.2 0 ...
    ##  $ yaw_forearm         : num  4.52 -137 130 35.6 163 -177 0 112 164 0 ...
    ##  $ total_accel_forearm : int  24 36 39 31 36 55 50 44 36 38 ...
    ##  $ gyros_forearm_x     : num  -0.27 0.05 0.34 -0.56 -0.21 0.8 0.61 -0.72 0.03 1.35 ...
    ##  $ gyros_forearm_y     : num  2.3 0 -0.82 3.24 -0.19 -1.77 2.55 2.14 2.59 -3.02 ...
    ##  $ gyros_forearm_z     : num  0.39 -0.02 -0.13 0.54 0.3 0.46 0.25 0.94 0.94 -0.48 ...
    ##  $ accel_forearm_x     : int  159 190 -200 181 -182 -434 -215 -120 63 -214 ...
    ##  $ accel_forearm_y     : int  3 212 263 153 201 -310 436 356 287 302 ...
    ##  $ accel_forearm_z     : int  169 -215 -194 192 227 -36 4 208 -187 8 ...
    ##  $ magnet_forearm_x    : int  509 -22 -677 598 -1270 -353 -587 -819 -397 -688 ...
    ##  $ magnet_forearm_y    : num  -421 660 454 -448 1460 -467 713 1260 729 225 ...
    ##  $ magnet_forearm_z    : num  -15 470 474 87 -962 -47 692 -614 581 620 ...
    ##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 3 1 4 1 5 5 3 4 1 2 ...

**Cross Validation** We are using 10 fold cross validation to allow the
algorithm to build a model without biases due to adequate sampling. The
algorithm will run 10 times and create a optimum model using the
Gradient Decent machine learning algorithm. “classe” will be our
predictor variable.

**Build Model** We are using Gradient decent to build a machine learning
algorithm on the train1 dataset.

``` r
control <- trainControl(method="cv", number=10)
model_gbm <- train(classe~. , data = train1 , method="gbm" , trControl=control ,  verbose = FALSE )
model_gbm$finalModel
```

    ## A gradient boosted model with multinomial loss function.
    ## 150 iterations were performed.
    ## There were 53 predictors of which 53 had non-zero influence.

**Predict** Predict classes and probability on train2 model using the
Gradient decent model build above.

``` r
library(caret)
#library(ModelMetrics)
plsClasses <- predict(model_gbm, newdata = train2)
str(plsClasses)
```

    ##  Factor w/ 5 levels "A","B","C","D",..: 3 1 4 1 5 5 3 4 1 2 ...

``` r
plsProbs <- predict(model_gbm, newdata = train2, type = "prob")
head(plsProbs)
```

    ##             A           B           C            D            E
    ## 1 0.002733308 0.007000618 0.986819743 0.0013369465 0.0021093846
    ## 2 0.993422724 0.003279318 0.001432902 0.0009245479 0.0009405074
    ## 3 0.009284610 0.016753364 0.042021707 0.9257889357 0.0061513832
    ## 4 0.977934977 0.005207053 0.010320707 0.0022989593 0.0042383028
    ## 5 0.007053136 0.006826518 0.008140920 0.0441244009 0.9338550254
    ## 6 0.006649222 0.004736997 0.001504482 0.0082307682 0.9788785302

**Important Variables** We identify the important variables contrubuting
to understanding our predictor variable. Further, we plot the variable
in the order of the importance.

``` r
# Visualization
library(gbm)
```

    ## Warning: package 'gbm' was built under R version 3.6.1

    ## Loaded gbm 2.1.5

``` r
library(caret)
library(splines)
library(parallel)
library(plyr)
require(RCurl)
```

    ## Loading required package: RCurl

    ## Loading required package: bitops

``` r
varImp(model_gbm)
```

    ## gbm variable importance
    ## 
    ##   only 20 most important variables shown (out of 53)
    ## 
    ##                   Overall
    ## num_window        100.000
    ## roll_belt          70.292
    ## pitch_forearm      40.983
    ## yaw_belt           37.704
    ## magnet_dumbbell_z  36.707
    ## magnet_dumbbell_y  27.984
    ## pitch_belt         19.318
    ## magnet_belt_z      18.833
    ## roll_forearm       16.551
    ## accel_dumbbell_z   15.175
    ## roll_dumbbell      14.716
    ## gyros_belt_z       13.184
    ## accel_forearm_x    11.720
    ## accel_dumbbell_y    9.903
    ## magnet_forearm_z    9.687
    ## accel_forearm_z     6.579
    ## accel_dumbbell_x    6.283
    ## gyros_dumbbell_y    5.923
    ## gyros_dumbbell_x    5.346
    ## yaw_arm             5.341

``` r
plot(varImp(model_gbm))
```

![](ClasseExercise_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(model_gbm)
```

![](ClasseExercise_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
model_gbm
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 4002 samples
    ##   53 predictor
    ##    5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 3600, 3601, 3601, 3602, 3602, 3602, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7636432  0.7001063
    ##   1                  100      0.8368450  0.7937422
    ##   1                  150      0.8705846  0.8364387
    ##   2                   50      0.8800666  0.8483144
    ##   2                  100      0.9285375  0.9096118
    ##   2                  150      0.9497828  0.9364961
    ##   3                   50      0.9232950  0.9029687
    ##   3                  100      0.9592816  0.9485037
    ##   3                  150      0.9757636  0.9693568
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were n.trees = 150,
    ##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

**Predict 20 classe variables on Test sample** Since we got good results
on the model built on train1 and testing it on train2, we shall now use
the same model to predict the “classe” on the 20 rows of testing data
into A, B, C, D and E.

``` r
cross_validation_results_boost <- predict(model_gbm , newdata = test)

predict.test.gbm <- predict(model_gbm , newdata = test )
predict.test.gbm
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

``` r
plsClasses1 <- predict(model_gbm, newdata = test)
plsClasses1
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

``` r
str(plsClasses)
```

    ##  Factor w/ 5 levels "A","B","C","D",..: 3 1 4 1 5 5 3 4 1 2 ...

``` r
plsProbs1 <- predict(model_gbm, newdata = test, type = "prob")
head(plsProbs1)
```

    ##             A           B           C           D           E
    ## 1 0.038630091 0.634467400 0.194670468 0.101515263 0.030716778
    ## 2 0.961296535 0.026867265 0.007294512 0.002215277 0.002326411
    ## 3 0.081180273 0.813224864 0.059323320 0.024071092 0.022200452
    ## 4 0.981793082 0.001536142 0.011474870 0.004177138 0.001018767
    ## 5 0.929769738 0.037428401 0.017211141 0.005618721 0.009971998
    ## 6 0.001100094 0.013064783 0.040716967 0.003793686 0.941324469

**Prediction of “Classe” on 20 rows**

``` r
plsClasses1 <- predict(model_gbm, newdata = test)

data.frame("Predictions" = plsClasses1)
```

    ##    Predictions
    ## 1            B
    ## 2            A
    ## 3            B
    ## 4            A
    ## 5            A
    ## 6            E
    ## 7            D
    ## 8            B
    ## 9            A
    ## 10           A
    ## 11           B
    ## 12           C
    ## 13           B
    ## 14           A
    ## 15           E
    ## 16           E
    ## 17           A
    ## 18           B
    ## 19           B
    ## 20           B
