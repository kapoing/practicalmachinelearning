# plot(density(data$num_window[data$classe=="A",]),
#      col="blue",main="",xlab="User'")
# lines(density(spam$your[spam$type=="spam"]),col="red")


# If you have a large sample size
# 60% training
# 20% test
# 20% validation
# If you have a medium sample size
# 60% training
# 40% testing
# If you have a small sample size
# Do cross validation
# Report caveat of small sample size
# accuracy =  (TP+TNegatives ) / (True Positives + TN + FP + FN)
# Cross-validation
# Approach:
#     
#     Use the training set
# 
# Split it into training/test sets
# 
# Build a model on the training set
# 
# Evaluate on the test set
# 
# Repeat and average the estimated errors
# 
# Used for:
#     
#     Picking variables to include in a model
# 
# Picking the type of prediction function to use
# 
# Picking the parameters in the prediction function
# 
# Comparing different predictors


lda MASS predict(obj) (no options needed)
glm stats predict(obj, type = "response")
gbm gbm predict(obj, type = "response", n.trees)
mda mda predict(obj, type = "posterior")
rpart rpart predict(obj, type = "prob")
Weka RWeka predict(obj, type = "probability")
LogitBoost caTools predict(obj, type = "raw", nIter)



Metric options

Categorical outcomes:
    
    Accuracy = Fraction correct
Kappa = A measure of concordance