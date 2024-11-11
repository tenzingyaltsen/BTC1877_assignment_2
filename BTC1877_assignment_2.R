#' BTC1877 Assignment 2
#' Tenzin Gyaltsen

#### Preparation ####
# Install and load required packages.
install.packages("dplyr")
install.packages("tidyr")
install.packages("funModeling")
install.packages("ggplot2")
install.packages("glmnet")
install.packages("pROC")
install.packages("tree")
library(funModeling)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(pROC)
library(tree)

# Read data and briefly explore.
working <- read.csv("bc_data.csv", header = FALSE)
dim(working)
summary(working)
str(working)

# Assess and assign NAs.
which(working == "?")
working <- as.data.frame(lapply(working, function(x) 
  ifelse(x == "?", NA, x)))
status(working)
# Lymph node status is the only variable with zeroes and NAs.

# Rename variables.
names(working) <- c("id", "outcome", "time", "radius_mean", "texture_mean",
                    "perimeter_mean", "area_mean", "smoothness_mean", 
                    "compactness_mean", "concavity_mean", "concave_points_mean",
                    "symmetry_mean", "fractal_dimension_mean", "radius_se",
                    "texture_se", "perimeter_se", "area_se", "smoothness_se",
                    "compactness_se", "concavity_se", "concave_points_se",
                    "symmetry_se", "fractal_dimension_se", "radius_worst",
                    "texture_worst", "perimeter_worst", "area_worst",
                    "smoothness_worst", "compactness_worst", "concavity_worst",
                    "concave_points_worst", "symmetry_worst", "fractal_dimension_worst",
                    "tumour_size", "lymph_nodes")

#### Regression ####
# Consider only patients with recurrence.
workingr <- working[which(working$outcome == "R"),]
# Consider only mean feature values.
workingr <- workingr[,c(1:13,34,35)]
# Refactor number of axillary nodes.
workingr$lymph_nodes <- ifelse(is.na(workingr$lymph_nodes), NA, 
                        ifelse(workingr$lymph_nodes == "0", 0, 
                               ifelse(as.integer(workingr$lymph_nodes) < 4, "1-3",
                                      "4 or more")))

# Generate descriptive statistics for numerical variables.
num_variables <- c(3:14)
num_table <- workingr %>%
  summarise(across(all_of(num_variables), list(
    mean = ~mean(.x,na.rm = T), median = ~median(.x,na.rm = T),
    sd = ~sd(.x, na.rm = T), min = ~min(.x, na.rm = T),
    max = ~max(.x, na.rm = T), iqr = ~IQR(.x, na.rm = T)))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
num_table

# Generate descriptive statistics for one categorical variable.
cat_table <- workingr %>%
  count(lymph_nodes) %>%
  mutate(proportion = n/sum(n))
cat_table

#' Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(workingr)) {
  if (is.numeric(workingr[[var]]) && var != "id") {
    histogram <- ggplot(workingr, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue") +
      labs(title = var, x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.character(workingr[[var]]) && var != "outcome") {
    barchart <- ggplot(workingr, aes(x = .data[[var]])) +
      geom_bar(fill = "salmon") +
      labs(title = var, x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(, hjust = 0.5, size = 18))
    print(barchart)
  }
}

#' Create matrix of feature values. Note that observation with NA 
#' automatically removed.
x <- model.matrix(time ~ radius_mean + texture_mean + perimeter_mean +
                    area_mean + smoothness_mean + compactness_mean + 
                    concavity_mean + concave_points_mean + symmetry_mean +
                    fractal_dimension_mean + tumour_size + lymph_nodes,
                  workingr)[,-1]
# Create vector with response values, but remove observation with NA.
y <- workingr$time[!is.na(workingr$lymph_nodes)]
# Train model, but first remove observation with NA.
lasso.mod <- glmnet(x, y, family = "gaussian")

# Plot the results against different values of log(lambda).
plot(lasso.mod, label = T, xvar = "lambda")
# Coefficients not standardized due to differing units of predictors.

# Decide on an optimal value for lambda using cross-validation.
set.seed(123)
cv.lasso <- cv.glmnet(x, y, nfolds = 5)
# Extract lambda values that gives lowest cross-validated MSE.
cv.lasso$lambda.min
# Examine MSE for that value, among other outputs.
print(cv.lasso)
# Examine value of features that stay in model when using optimal lambda.
coef.min <- coef(cv.lasso, s = "lambda.min")
coef.min
# Print the list using Dr. Mitsakakis' "awkward" code.
rownames(coef.min)[coef.min[,1] != 0][-1]

#### Classification ####
#'Make same data cleaning adjustments as before, but do not remove 
#' non-recurrence observations.
workingc <- working[,c(1:13,34,35)]
workingc$lymph_nodes <- ifelse(is.na(workingc$lymph_nodes), NA, 
                               ifelse(workingc$lymph_nodes == "0", 0, 
                                      ifelse(as.integer(workingc$lymph_nodes) < 4, "1-3",
                                             "4 or more")))

#'Refactor binary outcome, assigning recurrence as "1". Also factor lymph 
#'nodes variable.
workingc$outcome <- factor(ifelse(workingc$outcome == "R", 1, 0), 
                           levels = c(0,1))
workingc$lymph_nodes <- factor(workingc$lymph_nodes)

# Remove observations with NAs as this will interfere with matrix creation.
workingc <- workingc[complete.cases(workingc),]
#'Split the data into training and test, of equal size. Note that the seed
#' will have to be changed 4 more times, so come back to this code to
#' complete that particular question.
set.seed(123)
train.I <- sample(nrow(workingc), round(nrow(workingc)/2))

# Create matrices (x-values) and responses (y-values) for training.
x <- model.matrix(outcome ~ radius_mean + texture_mean + perimeter_mean + 
                     area_mean + smoothness_mean + compactness_mean + 
                     concavity_mean + concave_points_mean + symmetry_mean +
                     fractal_dimension_mean + tumour_size + lymph_nodes, 
                   workingc)[train.I,-1]
y <- workingc$outcome[train.I]

# Create lasso model, specifying "binomial".
lasso.mod <- glmnet(x, y, family = "binomial")
# Plot coefficients of predictors for different values of lambda.
plot(lasso.mod, label = T, xvar = "lambda")
#'Cross-validate model using AUC. Default of 10 folds is too large to 
#' reliably calculate AUC, so deviance used instead.
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", 
                      type.measure = "auc")
#'Plot deviances for different lambda values and find minimum lambda that 
#'provides lowest deviance.
plot(cv.lasso)
cv.lasso$lambda.min

# Extract predictors.
coef.min <- coef(cv.lasso, s = "lambda.min")
coef.min
rownames(coef.min)[coef.min[,1] != 0][-1]

#'Test the model using test set and minimum lambda and plot predicted
#' probabilities using histogram.
pred.lasso <- as.numeric(predict(lasso.mod, 
                                 newx = model.matrix(outcome ~ radius_mean + texture_mean + perimeter_mean + 
                                                       area_mean + smoothness_mean + compactness_mean + 
                                                       concavity_mean + concave_points_mean + symmetry_mean +
                                                       fractal_dimension_mean + tumour_size + lymph_nodes, 
                                                     workingc)[-train.I,-1], 
                                 s = cv.lasso$lambda.min, 
                                 type = "response"))
hist(pred.lasso)
# Plot the ROC curve.
myroc_lasso <- roc(outcome ~ pred.lasso, data = workingc[-train.I,])
plot(myroc_lasso)
# Extract the AUC, a measure of discrimination.
auc.lasso <- myroc_lasso$auc
auc.lasso

# Next create unpruned tree model.
tree.mod <- tree(outcome ~ radius_mean + texture_mean + perimeter_mean + 
                   area_mean + smoothness_mean + compactness_mean + 
                   concavity_mean + concave_points_mean + symmetry_mean +
                   fractal_dimension_mean + tumour_size + lymph_nodes, 
                 data = workingc, subset = train.I)
# Plot tree.
plot(tree.mod)
text(tree.mod, pretty = 0, cex = 0.35)

# Get probabilities for test set based on unpruned tree model.
preds.unpruned <- predict(tree.mod, newdata = workingc[-train.I,], type = "vector")
pred.probs.unpruned <- as.numeric(preds.unpruned[,2])
# Create and plot ROC curve and get AUC for unpruned tree.
myroc_unpruned <- roc(outcome ~ pred.probs.unpruned, data = workingc[-train.I,])
plot(myroc_unpruned)
myroc_unpruned$auc

# Next, prune tree by determining best size via cross-validation.
cv.res <- cv.tree(tree.mod, FUN = prune.tree)
cv.res
#'Get best size based on least deviance. Assign value of 2 if it is 1,
#' according to assignment instructions.
best.size <- cv.res$size[which.min(cv.res$dev)]
if (best.size == 1) {
  best.size <- 2
}
# Prune tree.
pruned <- prune.misclass(tree.mod, best = best.size)
plot(pruned)
text(pruned, pretty = 0, cex = 0.7)

# Get probabilities for test set based on pruned tree model.
preds.pruned <- predict(pruned, newdata = workingc[-train.I,], type = "vector")
pred.probs.pruned <- as.numeric(preds.pruned[,2])
# Create and plot ROC curve and get AUC.
myroc_pruned <- roc(outcome ~ pred.probs.pruned, data = workingc[-train.I,])
plot(myroc)
myroc$auc

#'Create table summarizing AUC values for each model assuming different
#' splits via random seed change.
results <- data.frame(
  set_seed = c(123, 234, 345, 456, 567),
  Lasso_AUC = c(0.5312, 0.5571, 0.6172, 0.6475, 0.6598),
  Unpruned_AUC = c(0.545, 0.5841, 0.5161, 0.5605, 0.5185),
  Pruned_AUC = c(0.5709, 0.5709, 0.5709, 0.5709, 0.5709)
)
print(results)

#### Survival Analysis ####

median_value <- median(workingc$time)
median_table <- data.frame(
  Statistic = "Median Time to Recurrence",
  Value = median_value
)
print(median_table)

# First name variable appropriately for recognition by function.
names(workingc)[2] <- "status"
# Create KM surival curves, stratified by lymph_nodes.
sf <- survfit(Surv(time, status==1) ~ lymph_nodes, data = workingc)
# Plot curves.
plot(sf, xlab = "Time From Surgery", ylab = "Recurrence", col = 1:3, 
     conf.int = 0.95)
legend("topright", legend = levels(factor(workingc$lymph_nodes)), 
       col = 1:3, lty = 1, title = "Lymph Node Levels", cex = 0.5)

# Check PH assumption to conduct log rank test.
plot(survfit(Surv(time, status==1) ~ lymph_nodes, data = workingc), 
     fun = "S")
# No obvious deviation. Also check cloglog plot.
plot(survfit(Surv(time, status==1) ~ lymph_nodes, data = workingc), 
     fun = "cloglog")
# Curves are somewhat parallel, but do not cross over. Proceed.

# Log rank test. 
survdiff(Surv(time, status==1) ~ lymph_nodes ,data = workingc)

# Create Cox PH model.
coxmod <- coxph(Surv(time, status==1) ~ radius_mean + texture_mean + 
                  perimeter_mean + area_mean + smoothness_mean + 
                  compactness_mean + concavity_mean + concave_points_mean + 
                  symmetry_mean + fractal_dimension_mean + tumour_size + 
                  lymph_nodes, 
                data = workingc)
summary(coxmod)
# Testing for the PH assumption using the cox.zph() function.
cox.zph(coxmod)