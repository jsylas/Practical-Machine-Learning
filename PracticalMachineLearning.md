PRACTICAL MACHINE LEARNING
==========================

#### Reading the data and creating the pairs part for the classifier to be used to go through the visual representation of the classifier i am going to use.To predict the classes of output from the human recognition data. Th class value reprsent, class-A throws elbows to the front,Class-B lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).To predict the class value identified the releveant classifier and created the formula to pass in the random forest algorithm.

    setwd("C:/Coursera/Practical Machine Learning")
    train <- read.csv("pml-training.csv",header=T,stringsAsFactors=T, sep=",")
    test <- read.csv("pml-testing.csv",header=T,stringsAsFactors=T, sep=",")
    pairs(classe ~ roll_belt+pitch_belt+yaw_belt+total_accel_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_forearm,data=train)

![](PracticalMachineLearning_files/figure-markdown_strict/train%20and%20test-1.png)

    classif <- formula(classe  ~ roll_belt+pitch_belt+yaw_belt+total_accel_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+roll_arm+pitch_arm+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm)
    model <-randomForest(classif,data=train,importance=TRUE)
    importance(model)

    ##                            A        B        C        D        E
    ## roll_belt           60.24627 83.98397 79.79924 70.95037 95.06618
    ## pitch_belt          40.23555 89.38118 61.89484 58.01389 49.50458
    ## yaw_belt            62.16708 75.97888 72.01640 85.81619 47.68046
    ## total_accel_arm     28.85928 52.11837 52.13976 54.01787 41.27630
    ## roll_dumbbell       37.74008 50.60703 52.69838 47.89971 48.26013
    ## pitch_dumbbell      19.98638 33.46647 29.62479 24.74176 31.29711
    ## yaw_dumbbell        28.44086 46.68185 43.26129 39.22014 48.69881
    ## roll_arm            36.20744 56.81908 46.32824 45.39302 34.60879
    ## pitch_arm           26.73103 48.38616 39.91699 37.36222 33.62349
    ## magnet_dumbbell_x   34.59434 40.64609 50.86878 37.41326 38.48013
    ## magnet_dumbbell_y   49.90095 59.53865 69.99530 55.30990 50.83393
    ## magnet_dumbbell_z   65.72656 61.13496 84.78495 57.73035 61.62557
    ## roll_forearm        41.28300 37.43593 49.33124 39.39367 35.80834
    ## pitch_forearm       51.29640 59.23236 56.45169 76.88710 57.58460
    ## yaw_forearm         27.23268 35.87296 41.11629 40.09536 42.31400
    ## total_accel_forearm 31.95742 39.93119 42.16360 35.82703 32.44374
    ##                     MeanDecreaseAccuracy MeanDecreaseGini
    ## roll_belt                      108.06450        2496.5713
    ## pitch_belt                      80.87672        1420.8637
    ## yaw_belt                        96.52288        1783.7349
    ## total_accel_arm                 62.70496         381.3025
    ## roll_dumbbell                   56.55440         794.0926
    ## pitch_dumbbell                  31.57673         404.0678
    ## yaw_dumbbell                    52.35176         598.1888
    ## roll_arm                        58.64617         716.5549
    ## pitch_arm                       46.37915         376.1261
    ## magnet_dumbbell_x               44.42948         885.8540
    ## magnet_dumbbell_y               70.25853        1142.3543
    ## magnet_dumbbell_z               81.60422        1297.9415
    ## roll_forearm                    44.34546        1092.7089
    ## pitch_forearm                   74.50410        1396.6791
    ## yaw_forearm                     51.24844         444.9032
    ## total_accel_forearm             50.25788         279.9829

    print(model)

    ## 
    ## Call:
    ##  randomForest(formula = classif, data = train, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 0.54%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 5572    7    0    1    0 0.001433692
    ## B   14 3747   34    2    0 0.013168291
    ## C    0    8 3407    7    0 0.004383402
    ## D    0    0   18 3195    3 0.006529851
    ## E    0    3    4    5 3595 0.003326864

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
