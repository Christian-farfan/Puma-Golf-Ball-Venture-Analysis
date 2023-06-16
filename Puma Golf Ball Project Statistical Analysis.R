###                                                   Golf Balls Population Means Project

# Setting up the hypothesis test
# H0: differences of population mean = 0 ----> No significant difference between current and proposed new ball
# Ha: differences of population mean =/= 0 -----> Significant difference between current and proposed new ball

# libraries
library(dplyr)

### Checking imported data and ensuring data integrity
sports_data <- Sports
str(sports_data)

# To clarify which ball produced what distance in yards, the "Current" and "New" columns will be renamed to "current_ball" and "new_ball"
sports_data <- sports_data %>% 
  rename("current_ball" = "Current",
         "new_ball" = "New")

# Checking renamed columns
head(sports_data) 

# Checking for any null and/or missing values
complete.cases(sports_data)
anyNA(sports_data)
is.na(sports_data$current_ball)
is.na(sports_data$new_ball)

### Exploratory data analysis
summary(sports_data)
range_current_ball = range(sports_data$current_ball)
range_new_ball = range(sports_data$new_ball)
range_current_ball
range_new_ball

current_ball_hist <- hist(sports_data$current_ball, breaks = 7, main = "Current Ball's Distance in Yards Histogram", 
                          xlab =  "Yards", ylab = "Frequency")

new_ball_hist <- hist(sports_data$new_ball, breaks = 7, main = "New Ball's Distance in Yards Histogram", 
                      xlab = "Yards", ylab = "Frequency")

current_ball_boxplot <- boxplot(sports_data$current_ball, main = "Current Ball Boxplot", ylab = "Yards")
new_ball_boxplot <- boxplot(sports_data$new_ball, main = "New Ball Boxplot", ylab = "Yards")

current_ball_qqplot <- qqnorm(sports_data$current_ball, main = "Current Ball QQ Plot")
qqline(sports_data$current_ball)

new_ball_qqplot <- qqnorm(sports_data$new_ball, main = "New Ball QQ Plot")
qqline(sports_data$new_ball)

### Partitioning the current and new ball data in order to be able to perform calculations and further analysis, and 
###   creating a new data frame where the difference between the current and new golf ball distances in yards is included

# Pulling first column as Current data
current_data <- sports_data[,1]
current_data

# Pulling second column as New data
new_data <- sports_data[,2]
new_data

# Taking the difference between the current ball and the old ball
differences_data = (current_data - new_data)
differences_data

# Creating a new data frame where the yard difference of the current ball and new ball are placed in a column
differences_df <- data.frame(current_data = current_data, new_data = new_data, differences_data = differences_data)
differences_df

# Renaming the differences column from "current_ball.1" to "difference"
colnames(differences_df)[which(names(differences_df) == "current_ball.1")] <- "difference"
differences_df

# Mean of difference data
mean_differences <- mean(differences_df$difference)
mean_differences

# Sample standard deviation of difference data
sample_sd_differences <- sd(differences_df$difference) 
sample_sd_differences

### Setting up our variables in order to proceed with our test 

# Creating variables for alpha, sample size, and degrees of freedom
alpha = 0.05
sample_size = 40
degrees_of_freedom = 40 - 1
alpha
sample_size
degrees_of_freedom
degrees_of_freedom

# Creating our test statistic
test_statistic = (mean_differences - 0) / (sample_sd_differences/sqrt(sample_size))
test_statistic # The test statistic is 1.277.

# Identifying t-critical value
t_critical_value = qt(1-alpha/2,degrees_of_freedom)
t_critical_value # The t-critical value is 2.023.

# Identifying two-tailed p-value
p_value = 2*pt(test_statistic, degrees_of_freedom, lower.tail=FALSE)
p_value # The p-value is 0.209.

#### Assessing whether we reject H0 or not

# Critical Value Method
# Reject H0 if: test_statistic <= -t(alpha/2, degrees_of_freedom)
#   OR t >= t(alpha/2, degrees_of_freedom)
if (test_statistic <= -t_critical_value | test_statistic >= t_critical_value) {
  print("Reject H0")
} else {
  print("Do Not Reject H0")
}

# P-value method
# Reject H0 if: p-value <= alpha (which is 0.05)
if (p_value <= alpha) {
  print("Reject H0")
} else {
  print("Do Not Reject H0")
}

# Do Not Reject H0
# We can determine with 95% confidence that the driving distances for new and
#   current-model golf balls does not have a significant increase from new to current model.

# Descriptive statistics for current ball, new ball, and their difference
summary(current_data)
summary(new_data)
summary(differences_data)

### Creating confidence intervals, point estimates, margin of errors, and lower and upper bounds for current ball, new ball, 
###   and their difference

# 95% confidence interval for the population mean of current golf ball model
mean_current_ball <- mean(differences_df$current_ball)
mean_current_ball
sd_current_ball <- sd(differences_df$current_ball)
sd_current_ball
point_estimate_current_ball = mean_current_ball

z_value = qnorm(1-alpha/2) #z_value = 1.96 and z-value for alpha/2 = 0.025
z_value
standard_error_current_ball = sd_current_ball/sqrt(sample_size)
standard_error_current_ball
margin_of_error_current_ball = z_value * standard_error_current_ball
margin_of_error_current_ball

lower_bound_current_ball = point_estimate_current_ball - margin_of_error_current_ball # Calculate lower bound
upper_bound_current_ball = point_estimate_current_ball + margin_of_error_current_ball # Calculate upper bound
lower_bound_current_ball
upper_bound_current_ball

# We can determine with 95% confidence that the driving distance for the current
#   golf ball model ranges from 267.56 and 272.99 yards.

# 95% confidence interval for the population mean of new golf ball model
mean_new_ball <- mean(differences_df$new_ball)
mean_new_ball
sd_new_ball <- sd(differences_df$new_ball)
sd_new_ball
point_estimate_new_ball = mean_new_ball
point_estimate_new_ball

standard_error_new_ball = sd_new_ball/sqrt(sample_size)
standard_error_new_ball
margin_of_error_new_ball = z_value * standard_error_new_ball
margin_of_error_new_ball

lower_bound_new_ball = point_estimate_new_ball - margin_of_error_new_ball # Calculate lower bound
upper_bound_new_ball = point_estimate_new_ball + margin_of_error_new_ball # Calculate upper bound
lower_bound_new_ball
upper_bound_new_ball

# We can determine with 95% confidence that the driving distance for the new
#   golf ball model ranges from 264.43 and 270.57 yards.

# 95% confidence interval for the population mean of differences of current vs new golf ball models
point_estimate_differences = mean(differences_df$difference)
point_estimate_differences

standard_error_differences = sample_sd_differences/sqrt(sample_size)
standard_error_differences
margin_of_error_differences = z_value * standard_error_differences
margin_of_error_differences

lower_bound_differences = point_estimate_differences - margin_of_error_differences # Calculate lower bound
upper_bound_differences = point_estimate_differences + margin_of_error_differences # Calculate upper bound
lower_bound_differences
upper_bound_differences

# We can determine with 95% confidence that the differences of the driving distance
#   for current vs new model of golf ball ranges from -1.48 and 7.03 yards.

