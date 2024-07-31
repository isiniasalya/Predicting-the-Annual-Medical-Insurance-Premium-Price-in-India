library(ISLR)
library(dplyr)
head(Hitters)

Hitters %>%
  select(Salary) %>%
  is.na() %>%
  sum()

# Print the dimensions of the original Hitters data (322 rows x 20 columns)
dim(Hitters)

# Drop any rows the contain missing values
Hitters = Hitters %>%
  na.omit()

# Print the dimensions of the modified Hitters data (263 rows x 20 columns)
dim(Hitters)

# One last check: should return 0
Hitters %>%
  is.na() %>%
  sum()

library(leaps)
regfit_full = regsubsets(Salary~., data = Hitters)
summary(regfit_full)

regfit_full = regsubsets(Salary~., data = Hitters, nvmax = 19)
reg_summary = summary(regfit_full)

names(reg_summary)
reg_summary$rsq
reg_summary$rsqadjr2
reg_summary$which

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg_summary$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

plot(regfit_full, scale = "r2")

plot(regfit_full, scale = "adjr2")
coef(regfit_full, 11)

plot(regfit_full, scale = "Cp")
coef(regfit_full, 10)

plot(regfit_full, scale = "bic")
coef(regfit_full, 6)



# k fold cross validation
k <- 10
n <- nrow (Hitters)
set.seed (1)
folds <- sample ( rep (1:k, length = n))
cv.errors <- matrix (NA, k, 19,dimnames = list (NULL , paste (1:19)))

predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix(form , newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}
for (j in 1:k) {
  best.fit <- regsubsets (Salary ~ .,
                            data = Hitters[folds != j, ],
                            nvmax = 19)
  for (i in 1:19) {
    pred <- predict (best.fit , Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-mean ((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply (cv.errors , 2, mean)
mean.cv.errors
par (mfrow = c(1, 1))
plot (mean.cv.errors , type = "b")

reg.best <- regsubsets (Salary ~ ., data = Hitters ,nvmax = 19)
coef (reg.best , 10)










set.seed (2)
x <- matrix ( rnorm (50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
km.out <- kmeans (x, 2, nstart = 20)
par (mfrow = c(1, 2))
plot (x, col = (km.out$cluster + 1),
        main = "K- Means Clustering Results with K = 2",
        xlab = "", ylab = "", pch = 20, cex = 2)





set.seed (4)
km.out <- kmeans (x, 3, nstart = 20)
km.out
plot (x, col = (km.out$cluster + 1),
      main = "K- Means Clustering Results with K = 3",
      xlab = "", ylab = "", pch = 20, cex = 2)



library(GGally)
library(ggplot2)

# Perform kmeans clustering
set.seed(123)
data <- iris[,1:4]
k <- kmeans(data, 3)

# Add cluster variable to data
data$cluster <- factor(k$cluster)

# Create scatterplot matrix
ggpairs(data, columns=1:4, aes(color=cluster))





# Create a data frame
df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

# Use transform() to create a new column
df <- transform(df, z = x + y)

# Print the result
df




library(factoextra)
library(cluster)

# Load data
data("iris")
iris <- iris[, 1:4] # Use only the numeric variables

# Determine the optimal number of clusters
sil <- silhouette(kmeans(iris, 3)$cluster, dist(iris))
summary(sil)

# Perform K-means clustering with 3 clusters
set.seed(123)
kmeans.fit <- kmeans(iris, 3)

# Get the cluster assignments
cluster.labels <- kmeans.fit$cluster

# Add the cluster assignments to the original data
iris$cluster <- cluster.labels

# View the cluster assignments
table(iris$cluster)

library(ggplot2)

# Create a scatterplot of the iris dataset with cluster assignments
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = factor(cluster))) +
  geom_point(size = 4) +
  labs(title = "K-means Clustering (k = 3)", x = "Sepal Length", y = "Petal Length")



