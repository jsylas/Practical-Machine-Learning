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

    ##                            A        B        C        D         E
    ## roll_belt           62.29118 88.86731 81.96621 72.15317 103.08804
    ## pitch_belt          42.98242 86.09476 64.33709 61.51408  49.43682
    ## yaw_belt            61.70123 82.97650 72.83940 80.56336  46.80986
    ## total_accel_arm     26.54528 53.43234 53.93084 52.91549  40.52533
    ## roll_dumbbell       38.15291 52.06119 53.20146 44.00045  47.75782
    ## pitch_dumbbell      21.13486 33.26272 28.87134 25.03165  33.08533
    ## yaw_dumbbell        28.49412 44.73692 44.86357 40.87600  46.67610
    ## roll_arm            38.59402 54.81909 47.61576 48.99933  34.59457
    ## pitch_arm           28.62538 48.00057 42.13231 38.97439  36.54027
    ## magnet_dumbbell_x   35.38373 40.80984 46.98739 37.41844  38.90073
    ## magnet_dumbbell_y   47.01989 59.61528 66.32156 56.59194  49.97322
    ## magnet_dumbbell_z   61.69391 58.56551 81.54843 54.72872  58.27447
    ## roll_forearm        40.18500 36.94961 46.87064 35.91956  35.27765
    ## pitch_forearm       53.63346 62.09782 59.54367 87.73455  61.52572
    ## yaw_forearm         29.90274 36.63611 40.69179 38.93237  39.85376
    ## total_accel_forearm 31.71984 39.15760 41.29597 34.51238  30.78710
    ##                     MeanDecreaseAccuracy MeanDecreaseGini
    ## roll_belt                      112.53818        2538.9646
    ## pitch_belt                      82.71610        1416.2094
    ## yaw_belt                        99.24785        1742.4135
    ## total_accel_arm                 61.07714         375.7740
    ## roll_dumbbell                   55.48884         801.3088
    ## pitch_dumbbell                  33.01011         402.9477
    ## yaw_dumbbell                    51.94810         590.9698
    ## roll_arm                        64.30050         719.4231
    ## pitch_arm                       49.96329         377.3342
    ## magnet_dumbbell_x               44.00818         901.7901
    ## magnet_dumbbell_y               66.37905        1163.5084
    ## magnet_dumbbell_z               75.62283        1288.2667
    ## roll_forearm                    42.35620        1060.4582
    ## pitch_forearm                   82.07248        1411.4485
    ## yaw_forearm                     53.27460         442.6560
    ## total_accel_forearm             49.83790         278.5616

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
    ## A 5571    6    1    1    1 0.001612903
    ## B   15 3749   31    2    0 0.012641559
    ## C    0    7 3407    8    0 0.004383402
    ## D    0    1   19 3194    2 0.006840796
    ## E    0    2    3    5 3597 0.002772387

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
           print(t)
           accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
           accuracies <- c(accuracies,accuracy)
           print(accuracy)
    }

    ##           
    ## prediction    A    B    C    D    E
    ##          A 1394    2    0    0    0
    ##          B    1  939    6    0    2
    ##          C    0    8  846    4    0
    ##          D    0    0    3  800    0
    ##          E    0    0    0    0  899
    ## [1] 0.9946982
    ##           
    ## prediction    A    B    C    D    E
    ##          A 1393    5    0    0    0
    ##          B    1  928    0    0    1
    ##          C    0   14  854   12    1
    ##          D    1    2    1  790    5
    ##          E    0    0    0    2  894
    ## [1] 0.9908238
    ##           
    ## prediction    A    B    C    D    E
    ##          A 1392    4    0    0    0
    ##          B    2  937    1    0    0
    ##          C    0    6  850    9    4
    ##          D    1    2    4  794    3
    ##          E    0    0    0    1  894
    ## [1] 0.9924551

    predict_test <- predict (modelFit,test)
    predict_test

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

\`\`\`
