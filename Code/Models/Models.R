# Libraries #

options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('klaR')
library(ggord)
library(nnet)
library(caTools)
library(caret)
library(pROC)
library(klaR)
library(class)

# Training and Test Sets #

df$Species <- relevel(df$Species, ref = 'setosa')
str(df)

set.seed(101)
split <- sample.split(df$Species, 0.3)
training <- subset(df, split == T)
test <- subset(df, split == F)

# Logistic Model #

logmodel <- train(Species ~ ., data = training, method = 'multinom')
summary(logmodel)
z_stat <- summary(logmodel)$coefficients/summary(logmodel)$standard.errors
p_values <- (1-pnorm(abs(z_stat), 0, 1))*2

logmodel_pred <- predict(logmodel, test, type = 'raw')
confusionMatrix(logmodel_pred, test$Species)

logmodel_prob <- predict(logmodel, test, type = 'prob')
rocs <- multiclass.roc(test$Species, logmodel_prob, percent = T)
rs <- rocs[['rocs']]
r1 <- rs[[1]][[2]] # Comparing Setosa vs Versicolor
r2 <- rs[[2]][[2]] # Comparing Setosa vs Virginica
r3 <- rs[[3]][[2]] # Comparing Versicolor vs Virginica
plot.roc(r1, col = 'forestgreen', lty = 5)
plot.roc(r2, col = 'red', add = T, lty = 3)
plot.roc(r3, col = 'blue', add = T, lty = 2)

# Comments
# - I used a smaller training set to get more interesting results (3 errors)
# - Unfortunately, coefficients do not make much sense and the coefficients actually have little interpretation due to their high standard error
# - Fortunately though, the predictions are pretty good still, and the ROC curve shows that the model is good for predictions
# - The only trouble it has is through classifying versicolor vs virginica, makes sense since they are fairly similar

# Linear Discriminant Analysis #

ldamod <- lda(Species ~ ., data = training)
ldamod_pred <- predict(ldamod, test)$class
ldamod_plotted <- predict(ldamod,test)$x
confusionMatrix(ldamod_pred, test$Species)
ldahist(ldamod_plotted[,1], g = test$Species)
ldahist(ldamod_plotted[,2], g = test$Species)

plot(ldamod)
ldamod_plotted_matrix <- data.frame(ldamod_plotted, Species = test$Species)
ggplot(ldamod_plotted_matrix, aes(x = LD1, y = LD2, col = Species)) + geom_point() + geom_density_2d() + theme_clean() + ggtitle(label = 'Test Values Plotted on LDA Axis') + theme(plot.title = element_text(hjust = 0.5))

partimat(Species ~., data = training, method = 'lda') # Still can't install.
ggord(linear, training$Species, ylim = c(-10, 10)) # Still can't install

# Comments
# - The LDA method shows good accuracy, only missclassifying 1 reading
# - We see that the LD1 axis shows good separation between the three classes, but LD2 does not which is not a good sign.
# - From the 2D density plot, we can also see that one versicolor point seems to fit in more with virginica points.
# - Model performs well because predictors are roughly normally distributed
# - However, correlation/covariance matrix not fixed, although differences are not crazy

# Quadratic Discriminant Analysis #

qdamod <- qda(Species ~., data = training)
qdamod_pred <- predict(qdamod, test)$class
confusionMatrix(qdamod_pred, test$Species)

# Comments
# - This model got 4 observations wrong, 3 more than the LDA model and 1 more than Logistic Model
# - This is likely due to the lack of observations (only 30% of dataset used, which represents 45 data points)
# - No gain from having low bias since relationships are not incredibly complex, but loses out due to the variance and overfitting of the model
# - Unfortunately can't be plotted either, so really low use.

# KNN #

training_scaled <- as.data.frame(scale(training[, -5]))
test_scaled <- as.data.frame(scale(test[, -5]))
training_results <- training$Species
test_results <- test$Species

errors <- c()
for(i in 1:45){
  test_pred <- knn(training_scaled, test_scaled, training_results, k = i)
  errors[i] <- mean(test_pred != test_results)
}
errors <- data.frame(error = errors, k = 1:45)
ggplot(errors, aes(x = k, y = error)) + geom_point(aes(size = error), col = 'lightblue', show.legend = F) + geom_line() + theme_clean() + labs(x = 'K', y = 'Error Rate', title = 'Error Rates for K-Nearest Neighbour Models') + theme(plot.title = element_text(hjust = 0.5))

test_pred <- knn(training_scaled, test_scaled, training_results, k = 8)
confusionMatrix(test_pred, test_results)

# Comments
# - Performs better than QDA, same as LDA and worse than Logistic Model (3 errors)
# - This poorer performance is likely due to low number of observations
# - Optimal model is at K = 8, which is fairly flexible

# Conclusions
# - LDA Model performs the best with an accuracy of 99.0% (1 error). Predictors are normally distributed, relationships are not crazy complicated and covariance matrix are not too different from each other
# - Logistic Model comes in second. It likely suffers from being strict, also means between different classes are fairly distinguishable and observations normally distributed, so LDA will perform better
# - KNN comes in next, this suffers from having low observations but 4 predictors limits damage.
# - QDA performs woprse, likely overfitting and low observations.


