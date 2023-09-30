# Load Housing Data
library(AmesHousing)
# Viz Package
library(DataExplorer)
library(ggplot2)
# Regression tools
library(car)
# Categorical data tools
library(vcd)
# Data manipulation tools
library(tidyverse)
library(reshape2)
# LASSO
library(glmnet)

# COPY THIS FUNCTION:
get_anova <- function(df, target){
  anova_res <- data.frame(Variable = character(0), P_Value = numeric(0))
  # Loop through columns (excluding the target variable)
  for (feature in names(df)) {
    if (feature != target) {
      # Perform ANOVA test
      anova_result <- anova(lm(df[[target]] ~ df[[feature]]))
      # Extract the p-value
      p_value <- anova_result$"Pr(>F)"[1]
      # Add the p-value to the p_values_df data frame
      anova_res <- rbind(anova_res, data.frame(Variable = feature, P_Value = p_value))
    }
  }
  return(anova_res)
}

# Load in our housing data
set.seed(123)
ames <- data.frame(make_ordinal_ames())
ames <- ames %>% 
  mutate(id = row_number(ames))

# Split into train and test
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

# Easy way for high level overview of the data
introduce(train)

# Split the data into continuous and discrete, function isnt perfect
output <- split_columns(train, binary_as_factor = TRUE)
c <- output$continuous

# Take a look at correlation first
plot_correlation(na.omit(c))

# After seeing how messy that was we can probably eliminate a few variables by hand 
plot_scatterplot(na.omit(c), by = "Sale_Price")

# We will select these variables to look at based on their plots with the target
c <- subset(c, select = c("Total_Bsmt_SF","First_Flr_SF","Gr_Liv_Area","Second_Flr_SF","Garage_Area","Sale_Price"))
plot_correlation(na.omit(c))

# Now we will examine our discrete variables
d <- output$discrete
d$Sale_Price <- train$Sale_Price

# And calculate ANOVA
anova_res <- get_anova(d, "Sale_Price")
d <- subset(d, select = c("Neighborhood", "Overall_Qual", "Sale_Price"))
# Don't forget to check assumptions!
# The observations are independent
# The model residuals are normally distributed
# The variances for each group are equal

# Take a look at the visuals
ggplot(data = d, aes(y = Sale_Price/1000, x = Overall_Qual, fill = Overall_Qual)) +
  geom_boxplot() + 
  labs(y = "Sales Price (Thousands $)", x = "Quality of Home") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
  scale_fill_brewer(palette="Blues") + theme_classic() + coord_flip()

# Combine our discrete and continuous back together
selected_features = cbind(d,select(c, -Sale_Price))

# Check for multicollinearity
linear_model <- lm(Sale_Price ~ ., data = selected_features)
vif(linear_model)

# Remove problematic terms intuitively
selected_features <- select(selected_features, -First_Flr_SF, -Second_Flr_SF)
linear_model2 <- lm(Sale_Price ~ ., data = selected_features)
vif(linear_model2)
summary(linear_model2)


# Discrete Example

# Creating Bonus Eligible col
train <- train %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

# Visualizing relationships
ggplot(data = train) +
  geom_bar(mapping = aes(x = Bonus, fill = Overall_Qual))

ggplot(data = train) +
  geom_bar(mapping = aes(x = Bonus, fill = Central_Air))

# Chi-Sq test
table(train$Central_Air, train$Bonus)
csq <- chisq.test(table(train$Central_Air, train$Bonus))
# Check Assumption
csq$expected
csq

# Statistical strength tests are correlation measures. 
# For the Chi-square, the most commonly used strength test is the Cramer’s V test.
assocstats(table(train$Central_Air, train$Bonus))

# Goal model
logistic_model <- glm(Bonus ~ ., data = train, family = binomial(link = "logit"))
# We can run vif here as well
vif(logistic_model)

# Get a summary of our data set
create_report(
  data = c,
  output_file = "practice_report.html",
  output_dir = getwd(),
  config = configure_report()
)
create_report(
  data = d,
  output_file = "practice_report.html",
  output_dir = getwd(),
  config = configure_report()
)

# LASSO
train_x <- model.matrix(Sale_Price ~ ., train)[, -1]
train_y <- train$Sale_Price
lasso_model <- glmnet(x = train_x, y = train_y, alpha = 1)
fit_model = predict(lasso_model, data = train)






