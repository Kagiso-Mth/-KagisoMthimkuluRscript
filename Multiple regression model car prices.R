# Load Data
library(readxl)

# File Path
Car_Price_Data = read_xls("/Users/kennyg/Downloads/CarPrice_Assignment.csv.xls")


# with readr
library(readr)
Car_Price_Data = read_csv("/Users/kennyg/Downloads/CarPrice_Assignment.csv.xls")

head(Car_Price_Data)

# Glimpse the data
library(dplyr)

glimpse(Car_Price_Data)

# Cleaning columns
library(tidyr)
library(janitor)
Car_Price_Data = Car_Price_Data %>%
  clean_names()

# Checking for any loading issues
any(is.na(Car_Price_Data))

# Summary of statisitics
summary(Car_Price_Data)

# Heatmap
install.packages("pheatmap")
library(pheatmap)

numeric_cols = Car_Price_Data[, sapply(Car_Price_Data, is.numeric)]
scaled_data = scale(numeric_cols)

pheatmap(scaled_data,
         main = "Car Features Heatmap",
         show_rownames = FALSE,
         clustering_method = "complete",
         color = colorRampPalette(c("blue", "white", "red"))(50)
         )
        
 
# Histogram of all numeric values 
library(ggplot2)
library(tidyr)
numeric_cols %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 10, fill = "steelblue", colour ="black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of all numeric values")
         
str(Car_Price_Data)  

# Convert Approriate columns to factor
Car_Price_Data$fueltype = as.factor(Car_Price_Data$fueltype)
Car_Price_Data$aspiration = as.factor(Car_Price_Data$aspiration)
Car_Price_Data$doornumber = as.factor(Car_Price_Data$doornumber)
Car_Price_Data$enginelocation = as.factor(Car_Price_Data$enginelocation)
Car_Price_Data$drivewheel = as.factor(Car_Price_Data$drivewheel)
Car_Price_Data$carbody = as.factor(Car_Price_Data$carbody)

# correlation for numeric values
numeric_vars <- Car_Price_Data %>%
  select(where(is.numeric), -price)

corr_matrix = cor(numeric_vars, use = "complete.obs")

print(round(corr_matrix, 2))

# find highly correlated pairs (correlations > 0.8)
high_cor = which(abs(corr_matrix) > 0.8 & upper.tri(corr_matrix), arr.ind = TRUE)
 if(nrow(high_cor) > 0) {
   for(i in 1:nrow(high_cor)) {
     cat("High Correlation Between", 
         rownames(corr_matrix)[high_cor[i,1]], "and", 
         colnames(corr_matrix)[high_cor[i,2]], ":", 
         corr_matrix[high_cor[i,1], high_cor[i,2]], "\n")
   }
 } else {
   cat("No highly correlated pairs found.\n")
 }

# Vsiualise correlation
install.packages("corrplot")
library(corrplot)
corrplot(corr_matrix, method = "color", type = "upper",
         tl.cex = 0.7, tl.col = "black", 
         title = "Correlation Matrix of Numeric Predictions", 
         mar = c(0, 0, 2, 0))



# Get the names of variables involved
vars_involved = unique(c(
  rownames(corr_matrix)[high_cor[,1]], 
  colnames(corr_matrix)[high_cor[,2]]
))

# Compute the absolute correlation with price
cor_with_price = sapply(vars_involved, function(v) {
  abs(cor(Car_Price_Data[[v]], Car_Price_Data$price, use = "complete.obs"))
})

# view sorted 
sort(cor_with_price, decreasing = TRUE)

# Variables to drop
vars_to_drop = c()
for(i in 1:nrow(high_cor)) {
  v1 = rownames(corr_matrix)[high_cor[i,1]]
  v2 = colnames(corr_matrix)[high_cor[i,2]]
  
  if(cor_with_price[v1] < cor_with_price[v2]) {
    vars_to_drop = c(vars_to_drop, v1)
    cat("Drop", v1, "(cor with price =", round(cor_with_price[v1],3), 
        "(, keep", v2, "(cor =", round(cor_with_price[v2],3), ")\n")
  } else {
    vars_to_drop = c(vars_to_drop, v2)
    cat("Drop", v2, "(cor with price =", round(cor_with_price[v2],3), 
        "(, keep", v1, "(cor =", round(cor_with_price[v1],3), ")\n")
  }
}

vars_to_drop = unique(vars_to_drop)
cat("\nVariables to drop:\n")
print(vars_to_drop)

# Drop Columns
library(dplyr)
Car_Price_Data = Car_Price_Data %>%
  select(-car_id, -car_name, -citympg, -carlength, -wheelbase, -carwidth, -curbweight, -horsepower)
head(Car_Price_Data)
str(Car_Price_Data)
glimpse(Car_Price_Data)

# Prepare data (combine rare levels)
library(dplyr)
library(forcats)
Car_Price_Data <- Car_Price_Data %>%
  mutate(cylindernumber = fct_lump_min(cylindernumber, min = 5, other_level = "rare"))
# fix
factor_cols <- names(Car_Price_Data)[sapply(Car_Price_Data, is.factor)]
Car_Price_Data <- Car_Price_Data %>%
  mutate(across(all_of(factor_cols), 
                ~ fct_lump_min(.x, min = 5, other_level = "rare")))
# Build
model = lm(price ~ ., data = Car_Price_Data)
summary(model)

# Check model assumptions and multicollinearity
install.packages("car")
library(car)
vif(model)
plot(model)


# Split data
set.seed(123)
n <- nrow(Car_Price_Data)
train_idx <- sample(1:n, 0.7 * n)
train <- Car_Price_Data[train_idx, ]
test <- Car_Price_Data[-train_idx, ]

# Remove any factors with only one level
for(col in names(train)[sapply(train, is.factor)]) {
  if(length(unique(train[[col]])) < 2) {
    train <- train %>% select(-all_of(col))
    test <- test %>% select(-all_of(col))
    cat("Removed", col, "- only one level\n")
  }
}

# fit model
model_temp = lm(price ~ ., data = train)

# Look for aliased coefficients 
aliased_info = alias(model_temp)
print(aliased_info$Complete)

# Stepwise
model_step = step(lm(price ~ ., data = train), direction = "both", trace = 1)

# Compare original vs. stepwise
cat("Original variables:", length(coef(lm(price ~ ., data = train))), "\n")
cat("Stepwise variables:", length(coef(model_step)), "\n")
cat("Variables dropped due to aliasing:", 
    length(coef(lm(price ~ ., data = train))) - length(coef(model_step)), "\n")

# Prediction
pred_step = predict(model_step, newdata = test)
rmse_step = sqrt(mean((test$price - pred_step)^2))
r2_step = cor(test$price, pred_step)^2
cat("RMSE:", rmse_step, "\nR²:", r2_step, "\n")
Residuals = test$price - pred_step
plot(test$price, pred_step, xlab = "Actual Price", ylab = "Predicted Price")
abline(0, 1, col = "red")

# Saving Working directory
png("myplot.png")
plot(test$price, pred_step)
abline(0,1,col="red")
dev.off()
