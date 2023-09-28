# Load Libraries
library(AmesHousing)
library(tidyverse)
library(DataExplorer)
library(car)
library(vcd)

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
plot_density(train, by = "Sale_Price")

# Show point biserial-correlation?
plot_correlation(na.omit(d), type = "all")

# Check for multicollinearity
linear_model <- lm(Sale_Price ~ ., data = c)
vif(linear_model)

# Remove problematic terms intuitively
linear_model2 <- lm(Sale_Price ~ Gr_Liv_Area + Total_Bsmt_SF + Garage_Area, data = c)
vif(linear_model2)
summary(linear_model2)

# Discrete Example

# Creating Bonus Eligible col
train <- train %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

# Visualizing relationships
plot_bar(train, by = "Bonus")

# Chi-Sq test
# Check Assumption
table(train$Central_Air, train$Bonus)
csq <- chisq.test(table(train$Central_Air, train$Bonus))
csq$expected
csq

# Statistical strength tests are correlation measures. 
# For the Chi-square, the most commonly used strength test is the Cramer’s V test.
assocstats(table(train$Central_Air, train$Bonus))

# Goal model
logistic_model <- glm(Bonus ~ Central_Air + Electrical, data = train, family = binomial(link = "logit"))
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


create_boxplots <- function(df, target_variable) {
  # Get the list of predictor variables (exclude the target variable)
  predictors <- setdiff(colnames(df), target_variable)
  # Loop through each predictor
  for (predictor in predictors) {
    # Create a boxplot for the current predictor
    current_plot <- ggplot(data = df, aes(y = factor(df[predictor]), x = df[target_variable], fill = df[target_variable])) +
      geom_boxplot() + 
      labs(y = target_variable, x = predictor) +
      stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
      scale_fill_brewer(palette="Blues") + theme_classic() + coord_flip()
    show(current_plot)
  }
}

create_boxplots(train, "Sale_Price")





