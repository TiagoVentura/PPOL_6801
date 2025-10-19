##############################################################################
# File-Name: week5_supervised_learning.r
# author: Tiago Ventura
# course: PPOL 6801 - text as data
# topics: supervised learning
# Machine: MacOS High Sierra
##############################################################################

# 0 -  Introduction -------------------------------------------------------------

## This script provides covers topics related to training your own supervised learning models in R

## We will focus mostly in using Quanteda + glmnet

# loading packages

pacman::p_load(tidyverse, quanteda, quanteda.corpora, quanteda.textstats, quanteda.textmodels, 
               rjson)


# 1 - Dataset ----------------------------------------------------

# We will work with a news dataset from HuffPost available at Kaggle
# download here: "https://www.kaggle.com/datasets/rmisra/news-category-dataset#News_Category_Dataset_v2.json"

# create json
json_file="data/news.json"

# readlines
temp = readLines(json_file)

# open by entry (some entries were corrupted)
news_data <- map(temp, ~ fromJSON(.x) %>% as_tibble()) %>% bind_rows() 

# see the data
table(news_data$category)



# 2 - Naive Bayes ------------------------------------------

# In class, we did not discuss Naive Bayes at length. 
# Naive Bayes is the simplest text classifier you can build. 
# Intuitively, NB uses training data  to build prototypes of classes based on the words frequency.
# The naive assumption is that features occur independently conditional on the class.

# subset data and keep relevant variables
news_samp <- news_data %>% 
  filter(category %in% c("CRIME", "SPORTS")) %>% 
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "CRIME"])
head(news_samp$text[news_samp$class == "SPORTS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
head(news_samp$text[news_samp$class == "SPORTS"])

# what's the distribution of classes?
prop.table(table(news_samp$class))

# split sample into training & test sets
set.seed(1310)

prop_train <- 0.8
ids <- 1:nrow(news_samp)

# get training data
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
train_set <- news_samp[ids_train,]

# get test data
ids_test <- ids[-ids_train]
test_set <- news_samp[ids_test,]

# get dfm for each set
train_dfm <- tokens(train_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_remove(stopwords("en")) 

test_dfm <- tokens(test_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_remove(stopwords("en")) 

# how does this look?
as.matrix(train_dfm)[1:5,1:5]
featnames(train_dfm)

# Are the features of these two DFMs necessarily the same? Yes/No Why?
# match test set dfm to train set dfm features
?dfm_match
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))


# 2.1 - Naive Bayes Without Smoothing parameters ------------------------------------------

# first, let's understand the modeling functions from quanteda
# see here: https://github.com/quanteda/quanteda.textmodels

# let's see the naive bayes models
?textmodel_nb

# train model on the training set
nb_model <- textmodel_nb(train_dfm, 
                         train_set$class, 
                         smooth = 0, 
                         prior = "uniform")
# summary
summary(nb_model)
?textmodel_nb

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm)

# baseline
actual_class <- test_set$class
tab_class <- table(actual_class, predicted_class)

# accuracy for baseline
baseline_acc <- max(prop.table(table(test_set$class)))

# get confusion matrix
nb_acc <- sum(diag(tab_class))/sum(tab_class) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- tab_class[2,2]/sum(tab_class[2,]) # recall = TP / (TP + FN)
nb_precision <- tab_class[2,2]/sum(tab_class[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# another way to get the confusion matrix from caret package
library(caret)
confusionMatrix(tab_class, mode = "everything")


# 2.1 - Naive Bayes WITH Smoothing parameters ------------------------------------------

# train model on the training set using Laplace smoothing
nb_model_sm <- textmodel_nb(train_dfm, 
                            train_set$class, 
                            smooth = 1, # here
                            prior = "uniform")

# evaluate on test set
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

# get confusion matrix
tabclass_sm <- table(test_set$class, predicted_class_sm)#

confusionMatrix(tabclass_sm, mode = "everything")


# take a look at the most discriminant features (get some face validity)
posterior <- tibble(feature = colnames(nb_model_sm$param), 
                    post_CRIME = t(nb_model_sm$param)[,1],
                    post_SPORTS = t(nb_model_sm$param)[,2])

posterior %>% arrange(-post_SPORTS) %>% head(10)
posterior %>% arrange(-post_CRIME) %>% head(10)

# what does smoothing do? 
# avoids zeros on your terms. 
# This is an issue for naive bayes likelihood function. 

# how many terms get zero probabilities in the model with no smoothing?
sum(nb_model$param[1,]==0)

# how many terms get zero probabilities in the model with smoothing?
sum(nb_model_sm$param[1,]==0)


# 3 - Regularization ----------------------------------------------------------

# let's now learn more about a work-horse model in supervised learning with text: 
# Logistic Regression + Regularization parameters. 

# let's start with Lasso ~ shrinks parameters towards zero. Here we need to use 
# cross validation to find the optimal value for the lambda parameter
# we will use glmnet to estimate the lasso model and to do cross validation

# convert the data to a factor
train_set$sports = fct_relevel(as_factor(train_set$class), "SPORTS")
test_set$sports = fct_relevel(as_factor(test_set$class), "SPORTS")
levels(train_set$sports)

# call glmnet
library(glmnet)

# Simplified call to cv.glmnet to debug
lasso <- cv.glmnet(x=train_dfm, 
                   y=train_set$sports, 
	                 family="binomial", 
                   alpha=1, # l-1 lasso
                   nfolds=5,
	intercept=TRUE,	
	type.measure="class")

# see the values for the lambda parameter used in the regularization
plot(lasso)

# We can now compute the performance metrics on the test set.
predicted_lasso <- predict(lasso, newx=test_dfm, type="class")

# get confusion matrix
tabclass_lasso <- table(fct_rev(test_set$sports), predicted_lasso) 
lasso_cmat<-confusionMatrix(tabclass_lasso, mode = "everything")

# function to extract main measures
print_cmat <- function(confusion_matrix){
	message("Positive Class: ", confusion_matrix$positive, "\n",
	        "Accuracy = ", round(confusion_matrix$overall["Accuracy"], 2), "\n",
	        "Precision = ", round(confusion_matrix$byClass["Precision"], 2), "\n",
	        "Recall = ", round(confusion_matrix$byClass["Recall"], 2))
}

# print
print_cmat(lasso_cmat)

# With LASSO, it is interesting to look at the actual estimated coefficients and
# see which of these have the highest or lowest values:

# minimal lambada
best.lambda <- which(lasso$lambda==lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[,best.lambda]
head(beta)

## identifying predictive features
df <- data.frame(coef = as.numeric(beta),
                 word = names(beta), stringsAsFactors=F)

## Most predictive for crime
df %>% arrange(desc(coef)) %>% slice(0:20)

## Most predictive for csports
df %>% arrange(coef) %>% slice(0:20)


# 3 - Regularization - Elastic Net and Ridge ----------------------------------------------------------

#We can easily modify our code to experiment with Ridge or ElasticNet models:
?cv.glmnet  

ridge <- cv.glmnet(x=train_dfm, 
                   y=train_set$sports, 
                   family="binomial", 
                   alpha=0, # alpha = 0, ridge someting in between. you can find with CV
                   nfolds=5,
                   intercept=TRUE,	type.measure="class")


# We can now compute the performance metrics on the test set.
predicted_ridge<- predict(ridge, newx=test_dfm, type="class")

# get confusion matrix
tabclass_ridge <- table(fct_rev(test_set$sports), predicted_ridge) 
ridge_cmat<-confusionMatrix(tabclass_ridge, mode = "everything")

# print
print_cmat(ridge_cmat)

# 4 - Tidy Models ---------

## Some R developers have been working on a unified approach for ML in R called `tidymodels`
## It basically incorporates principles of the tidyverse package into ML in R
## read here: https://www.tidymodels.org/

## Let me show one example using the tidymodels approach
## Because of the tidy approach, you need to convert the matrix to a DF


#install.packages("tidymodels")
library(tidymodels)
set.seed(123)

# Convert dfm to sparse matrix
train_mat <- as.matrix(train_dfm)  
test_mat  <- as.matrix(test_dfm)

# Combine into tibble with outcome
train_data <- as_tibble(train_mat) %>%
  mutate(sports = train_set$sports)

test_data <- as_tibble(test_mat) %>%
  mutate(sports = test_set$sports)

# Split data
data_split <- initial_split(train_data , prop = 0.8, strata="sports")
train_data <- training(data_split)
test_data  <- testing(data_split)

# Cross-validation
folds <- vfold_cv(train_data, v = 5, strata = sports)

# Recipe: you can add steps for text/numeric preprocessing here
rec <- recipe(sports ~ ., data = train_data)

# elastic net (as we saw with glmnet)

## build model
el_net <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")

## build workflow
el_net_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(el_net)

## train the model
el_net_tuned <- tune_grid(el_net_wf,
                        resamples = folds,
                        grid = 5, 
                        metrics = metric_set(accuracy, precision, recall))

?tune_grid

## Get metrics
best_metric = el_net_tuned %>%
  select_best(metric = "accuracy")

##  finalize the model
final_wf <- 
  el_net_wf   %>% 
  finalize_workflow(best_metric)

## fit in the test set
## last_fit() with our finalized model;
## this function fits the finalized model on the full 
## training data set and evaluates the finalized model on the testing data.
## I AM NOT A FAN OF THIS!!
el_net_fit <- 
  final_wf %>%
  last_fit(data_split) 

##
el_net_fit %>%
  collect_metrics()

## Notice it is from here, quite easy to use other other models. 
## Let's see with tree based models

## build model
tree_net <- decision_tree(cost_complexity = tune(), 
                          tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

## build workflow
tree_net_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tree_net)

## train the model
tree_net_tuned <- tune_grid(tree_net_wf,
                          resamples = folds,
                          grid = 5, 
                          metrics = metric_set(accuracy, precision, recall))

## Get metrics
best_metric = tree_net_tuned %>%
  select_best(metric = "accuracy")

##  finalize the model
tree_final_wf <- 
  tree_net_wf   %>% 
  finalize_workflow(best_metric)

## fit in the test set
tree_fit <- 
  tree_final_wf %>%
  last_fit(data_split) 

##
tree_fit %>%
  collect_metrics()


# 5 - Where to learn more?  -----------------------------------------------

# This is a brief intro to supervised learning. After you get the idea, 
# switching between models is easy. 

# Here are some links for materials for you to learn more: 

# here for more of glmnet which focus mostly on penalized regresion: https://glmnet.stanford.edu/index.html

# caret for a more general ML framework: https://topepo.github.io/caret/

# Here if you want to explore a tidy approach for ML in R: https://smltar.com/
## strongly recommend this last book. It provides a more integrative pipeline for ML in R

# and this book for ML:https://www.statlearning.com/
