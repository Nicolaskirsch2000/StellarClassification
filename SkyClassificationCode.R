#Load necessary packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos="http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos="http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos="http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest")

######################################
#Initial Data Exploration and cleaning
######################################

#Load the dataset
sky <- read.csv("https://raw.githubusercontent.com/Nicolaskirsch2000/StellarClassification/master/GalaxyStarsQuasars.csv")

#Check for NA values
colSums(is.na(sky))

#Get first insight on the data distribution in the dataset
summary(sky)

#Get how much observation there is for each classes
summary(sky$class)
ggplot(sky, aes(class, fill = class)) + geom_bar()

#Get all the column
colnames(sky)

#Get rid of all useless columns
sky <- sky[,-c(1,9,10,11,12,13)]

#Get the redshift repartitions for stars quasars and galaxies
star_r <- sky %>% filter(class == 'STAR') %>% 
  ggplot(aes(redshift))+ geom_histogram(bins = 30, color = "black") +ggtitle("Stars")
galaxy_r <- sky %>% filter(class == 'GALAXY') %>% 
  ggplot(aes(redshift))+ geom_histogram(bins = 30, color = "black") + ggtitle("Galaxies")
qso_r <- sky %>% filter(class == 'QSO') %>% 
  ggplot(aes(redshift))+ geom_histogram(bins = 30, color = "black") + ggtitle("Quasars")

#Get the three graphs in a grid
p <- plot_grid(star_r,galaxy_r,qso_r)
title <- ggdraw() + draw_label("Redshift distribution for Stars , Galaxies and Quasars", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

#Density distribution of the different wavelength u,g,r,i,z
plot_grid(ggplot(sky, aes(u, fill = class)) + geom_density(alpha = 0.7), 
          ggplot(sky, aes(g, fill = class)) + geom_density(alpha = 0.7),
          ggplot(sky, aes(r, fill = class)) + geom_density(alpha = 0.7),
          ggplot(sky, aes(i, fill = class)) + geom_density(alpha = 0.7),
          ggplot(sky, aes(z, fill = class)) + geom_density(alpha = 0.7), 
          ncol=2)

#Create test and train set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = sky$class, times = 1, p = 0.1, list = FALSE)
train_set <- sky[-test_index,]
test_set <- sky[test_index,]

##################################
#Randomly classifying observations
##################################

#Creating a vector of the same length as the test set randomly selcting between Star, Galaxy and Quasar
random_classification <- as.factor(sample(c("GALAXY","STAR","QSO"),nrow(test_set),replace=TRUE))

#Creating a confusion matrix between the predicted classes and the actual classes
cM_random <- confusionMatrix(random_classification,as.factor(test_set$class))

#Recuperating the accuracy of this model
accuracy_random <- cM_random$overall['Accuracy']

#Creating a table regrouping the accuracy from the diffrent models
accuracy_table <- tibble(method = "Random Classification", Accuracy = cM$overall['Accuracy'])


################################
#Classifying using KNN algorithm
################################

#Train a KNN algorithm to get the best k value 
knn_model <- train(class ~ ., 
                method='knn', 
                data = train_set,
                tuneGrid=data.frame(k=seq(0,200,20)))

#Representation of the evolution of the accuracy following the k value
ggplot(modele, highlight = TRUE)

#Getting the best k value
modele$bestTune

#Predict the classification using the model
prediction_knn <- predict(modele, test_set, type = "raw")%>% 
  as.data.frame() %>%
  cbind(test_set, .) %>% pull(.)

#Creating a confusion matrix between the predicted classes and the actual classes
cm_knn <- confusionMatrix(yhatknn, test_set$class)

#Recuperating the accuracy of this model
accuracy_knn <- cm_knn$overall['Accuracy']

#Add this model to the Accuracy Table
accuracy_table <- bind_rows(accuracy_table, tibble(method="KNN model", Accuracy = accuracy_knn))


###############################################
#Classify using Quadratic Discriminant Analysis
###############################################

#Train the qda model
train_qda <- train(class ~ ., method = "qda", data = train_set)

#Predict classes of the test set using the trained qda model
prediction_qda <- predict(train_qda, test_set, type = "raw")%>% 
  as.data.frame() %>%
  cbind(test_set, .) %>% pull(.)

#Creating a confusion matrix between the predicted classes and the actual classes
cm_qda <- confusionMatrix(prediction_qda, test_set$class)

#Get the accuracy of this model
accuracy_qda <- cm_qda$overall['Accuracy']

#Add this model to the Accuracy Table
accuracy_table <- bind_rows(accuracy_table, tibble(method="QDA model", Accuracy = accuracy_qda))

##############################
#Classify using decision trees 
##############################

#Train the Decision Tree model
train_dt <- train(class ~ ., method = "rpart",
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), 
                  data = train_set) 

#Using the decision tree model to predict the test set classes
prediction_dt <- predict(train_dt, test_set, type = "raw")%>% 
  as.data.frame() %>%
  cbind(test_set, .) %>% pull(.)

#Creating a confusion matrix between the predicted classes and the actual classes
cm_dt <- confusionMatrix(prediction_dt, test_set$class)

#Get the accuracy of this model
accuracy_dt <- cm_dt$overall['Accuracy']

#Add this model to the Accuracy Table
accuracy_table <- bind_rows(accuracy_table, tibble(method="Decision tree model", Accuracy = accuracy_dt))

#############################
#Classify Using Random Forest
#############################

#Train the random forest model
train_rf <- randomForest(class~.,data = train_set)

#Using the random forest model to predict the classes of the test set
prediction_rf <- predict(train_rf, test_set)%>% 
  as.data.frame() %>%
  cbind(test_set, .) %>% pull(.)

#Creating a confusion matrix between the predicted classes and the actual classes
cm_rf <- confusionMatrix(prediction_rf, test_set$class)

#Get the accuracy of this model
accuracy_rf <- cm_rf$overall['Accuracy']

#Add this model to the Accuracy Table
accuracy_table <- bind_rows(accuracy_table, tibble(method="Random forest model", Accuracy = accuracy_rf))
accuracy_table

