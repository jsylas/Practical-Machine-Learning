PRACTICAL MACHINE LEARNING
==========================

#### Loading the data and creating the pairs plot for the classifier to be used to go through the visual representation of the classifier i am going to use.To predict the classes of output from the human recognition data. Th class value reprsent, class-A throws elbows to the front,Class-B lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).To predict the class value identified the releveant classifier and created the formula to pass in the random forest algorithm.

    setwd("C:/Coursera/Practical Machine Learning")
    train <- read.csv("pml-training.csv",header=T,stringsAsFactors=T, sep=",")
    test <- read.csv("pml-testing.csv",header=T,stringsAsFactors=T, sep=",")
    pairs(classe ~ roll_belt+pitch_belt+yaw_belt+total_accel_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_forearm,data=train)

![](PracticalMachineLearning_files/figure-markdown_strict/train%20and%20test-1.png)

    classif <- formula(classe  ~ roll_belt+pitch_belt+yaw_belt+total_accel_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+roll_arm+pitch_arm+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm)
    model <-randomForest(classif,data=train,importance=TRUE)
    importance(model)

    ##                            A        B        C        D        E
    ## roll_belt           66.42667 78.98877 76.59964 72.62490 93.32570
    ## pitch_belt          44.92373 90.15030 61.61974 61.45566 48.68108
    ## yaw_belt            58.86059 76.87709 68.57400 82.92652 45.76576
    ## total_accel_arm     29.11207 53.37526 57.87700 52.01919 41.18381
    ## roll_dumbbell       37.88327 49.93124 52.28679 43.76147 46.90406
    ## pitch_dumbbell      20.20183 31.13421 31.60469 23.06941 31.23649
    ## yaw_dumbbell        27.78934 41.71636 43.05209 37.21184 44.35212
    ## roll_arm            36.53076 55.81535 45.20200 47.43572 35.18284
    ## pitch_arm           28.61178 50.44861 39.93938 41.32870 35.12724
    ## magnet_dumbbell_x   35.33469 44.19367 46.72083 38.13051 38.62112
    ## magnet_dumbbell_y   45.78232 57.09527 62.82625 54.70427 50.87394
    ## magnet_dumbbell_z   63.10748 62.64396 87.07264 62.41303 60.95555
    ## roll_forearm        41.31264 36.63764 47.65834 36.73878 35.25879
    ## pitch_forearm       52.07501 63.19483 58.45094 80.92070 60.84548
    ## yaw_forearm         29.42100 38.95763 40.47632 39.41489 43.49891
    ## total_accel_forearm 28.07006 37.72550 40.42299 34.80003 29.48830
    ##                     MeanDecreaseAccuracy MeanDecreaseGini
    ## roll_belt                      106.88848        2479.7078
    ## pitch_belt                      83.94560        1457.5660
    ## yaw_belt                        90.01264        1790.0424
    ## total_accel_arm                 61.50070         372.8416
    ## roll_dumbbell                   54.37913         780.5171
    ## pitch_dumbbell                  31.33095         400.1594
    ## yaw_dumbbell                    49.62200         589.2114
    ## roll_arm                        59.03742         719.9358
    ## pitch_arm                       50.99911         388.7978
    ## magnet_dumbbell_x               44.35763         896.9140
    ## magnet_dumbbell_y               63.72479        1149.5484
    ## magnet_dumbbell_z               83.51374        1302.3091
    ## roll_forearm                    43.32428        1073.9327
    ## pitch_forearm                   79.93928        1388.8858
    ## yaw_forearm                     53.77072         442.2193
    ## total_accel_forearm             44.22344         281.0962

    print(model)

    ## 
    ## Call:
    ##  randomForest(formula = classif, data = train, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 0.53%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 5574    6    0    0    0 0.001075269
    ## B   15 3746   34    2    0 0.013431657
    ## C    0    9 3406    7    0 0.004675628
    ## D    0    0   16 3197    3 0.005907960
    ## E    0    3    5    4 3595 0.003326864

    test_predict <- predict (model,test)

##### The above summary of results says about the predicted value and the error rate in the confusion matrix.Also the out of sample error rate of 0.53% if we consider the all the training data to construct the model.

CROSS VALIDATION ALGORITHM
--------------------------

#### Used the 3 cross validation to verify the out of sample error and applied the predicted value to the test data.

    first_seed <- 1234
    accuracies <-c()
    for (i in 1:3){
           set.seed(first_seed)
           first_seed <- first_seed+1
           trainIndex <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
           trainingSet<- train[trainIndex,]
           testingSet<- train[-trainIndex,]
           modelFit <- randomForest(classif, data = trainingSet,importance=TRUE)
           prediction <- predict(modelFit, testingSet)
           testingSet$rightPred <- prediction == testingSet$classe
           t<-table(prediction, testingSet$classe)
           print(modelFit)
           accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
           accuracies <- c(accuracies,accuracy)
           print(accuracy)
    }

    ## 
    ## Call:
    ##  randomForest(formula = classif, data = trainingSet, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 0.73%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 4176    7    0    0    2 0.002150538
    ## B   14 2805   24    5    0 0.015098315
    ## C    0    9 2551    7    0 0.006232957
    ## D    0    0   18 2391    3 0.008706468
    ## E    0    4    6    9 2687 0.007021434
    ## [1] 0.9946982
    ## 
    ## Call:
    ##  randomForest(formula = classif, data = trainingSet, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 0.77%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 4178    6    0    1    0 0.001672640
    ## B   15 2799   32    2    0 0.017205056
    ## C    0   12 2547    8    0 0.007791196
    ## D    0    0   21 2389    2 0.009535655
    ## E    0    4    4    6 2692 0.005173688
    ## [1] 0.9908238
    ## 
    ## Call:
    ##  randomForest(formula = classif, data = trainingSet, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 0.79%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 4172   11    0    1    1 0.003106332
    ## B   14 2802   32    0    0 0.016151685
    ## C    0   11 2543   13    0 0.009349435
    ## D    0    0   21 2388    3 0.009950249
    ## E    0    2    2    5 2697 0.003325942
    ## [1] 0.9924551

    predict_test <- predict (modelFit,test)
    predict_test

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

#### In the above method of cross validation sample the out of bag error is in the range of 0.75 and it is very close to the full model OOB error, also the accuracy rate comingout of the cross validation method random forest application is in the good mark.This model is not overfitted it produce the accurate results in the test data prediction.
