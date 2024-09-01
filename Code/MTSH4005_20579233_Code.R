# Question 1 :
library(tidyr)
library(ggplot2)

# Loading the datasets
obs_data <- read.csv("Kittiwake_Observation_20579233.csv")
head(obs_data)
# Getting the structure of the data
str(obs_data)

# Calculating the Sumarry statistics 
std1 = sqrt(var(obs_data$dawn))
std1
std2 = sqrt(var(obs_data$noon))
std2
std3 = sqrt(var(obs_data$mid.afternoon))
std3
std4 = sqrt(var(obs_data$dusk))
std4
summary(obs_data)

# Loading the mid-afternoon data
mid_afternoon <- obs_data$mid.afternoon

# PLotting a QQ plot to check if the mid-afternoon is normally distributed
qqnorm(mid_afternoon)
qqline(mid_afternoon)

# Calculating the confidence interval
ci <- t.test(mid_afternoon, conf.level = 0.98)$conf.int
ci
# The confidence interval comes out to be 56.67450 - 63.39693

# Boxplot of mid-afternoon observations
boxplot(mid_afternoon, main = "Mid-Afternoon Observations", ylab = "Number of Observations")
# Adding a horizontal line for the mean
abline(h = mean(mid_afternoon), col = "red", lty = 2)
# Adding the confidence interval as a horizontal line
abline(h = ci, col = "blue", lty = 2)
# Adding the legend
legend("topright", legend = c("Mean", "98% CI"), col = c("red", "blue"), lty = 2)



# Question 2 :
# Task A

# Loading the Historical data
hist_data <- read.csv("Kittiwake_Historical_20579233.csv")
hist_data

# Exploring the structure and summary of the data
str(hist_data)
summary(hist_data)
std1 = sqrt(var(hist_data$Site.A))
std1
std2 = sqrt(var(hist_data$Site.B))
std2
std3 = sqrt(var(hist_data$Site.C))
std3
std4 = sqrt(var(hist_data$Site.D))
std4
std5 = sqrt(var(hist_data$Site.E))
std5
std6 = sqrt(var(hist_data$Site.F))
std6

# Performing the Chi-Squared Test
chisq_test <- chisq.test(hist_data[, -1])
print(chisq_test)

# Reshaping the data to long format
hist_data_new <- gather(hist_data, key = "Site", value = "BreedingPairs", -X)

# Converting 'X' to a factor for better x-axis labeling
hist_data_new$X <- as.factor(hist_data_new$X)

# Plotting breeding pairs over time for each site
ggplot(hist_data_new, aes(x = X, y = BreedingPairs, color = Site, group = Site)) +
  geom_line() +
  labs(title = "Breeding Pairs Over Time by Site") +
  scale_x_discrete(name = "Year")

# Task B
# Linear regression model
predicting_model <- lm(Site.F ~ X, data = hist_data)

# This predicts the breeding pairs for Site F in 2006
prediction <- predict(predicting_model, newdata = data.frame(X = 2006))
print(round(prediction))


# Question 3 : 

# Task A
# Reading the measurement data 
measurement_data <- read.csv("Kittiwake_Measurement_20579233.csv")
head(measurement_data)

# Visual summary
ggplot(measurement_data, aes(x = Weight, y = Wingspan, color = factor(Sub.species))) +
  geom_point() +
  labs(title = "Scatterplot of Weight vs. Wing Span",
       x = "Weight (g)",
       y = "Wing Span (cm)",
       color = "Subspecies") +
  theme_minimal()

ggplot(measurement_data, aes(x = factor(Sub.species), y = Culmen)) +
  geom_boxplot() +
  labs(title = "Boxplot of Culmen Length by Subspecies",
       x = "Subspecies",
       y = "Culmen Length (mm)") +
  theme_minimal()

# Task B
# Correlation for black-legged kittiwakes
cor(measurement_data$Wingspan[measurement_data$Sub.species == "Black-legged"],
    measurement_data$Culmen[measurement_data$Sub.species == "Black-legged"])

# Correlation for red-legged kittiwakes
cor(measurement_data$Wingspan[measurement_data$Sub.species == "Red-legged"],
    measurement_data$Culmen[measurement_data$Sub.species == "Red-legged"])

# Result -> Independent

#Task C
# Performing Welch's t-test for difference in weights between sub-species
t_test <- t.test(measurement_data$Weight ~ measurement_data$Sub.species)

t_test
# Result = There is not enough evidence that the weights of birds of the two sub-species are different

# Task D
# Performming MANOVA
manova <- manova(cbind(Wingspan, Culmen, Weight) ~ Sub.species, data = measurement_data)
manova
# Displays the MANOVA results
summary(manova, test = "Pillai")
# Result = Not enough evidence that there is a significant difference between the two species



# Question 4
# Task A
# Loading the data
loc_data <- read.csv("Kittiwake_Location_20579233.csv")
head(loc_data)
# Fitting a linear model
model <- lm(Breeding.pairs ~ Summer.temp + cliff.height + sandeel + Coast.direction, data = loc_data)
# Prints a summary of the model
summary(model)

residuals <- residuals(model)
fitted <- fitted(model)
plot(fitted, residuals, main = "Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Task B
# Taking the natural logarithm of the breeding pairs variable
loc_data$log_breeding_pairs <- log(loc_data$Breeding.pairs)

# Fits a linear model to the logarithm of the breeding pairs
log_model <- lm(log_breeding_pairs ~ Summer.temp + cliff.height + sandeel + Coast.direction, data = loc_data)

# Prints a summary of the log-transformed model
summary(log_model)

residuals <- residuals(log_model)
fitted <- fitted(log_model)
plot(fitted, residuals, main = "Residual Plot For The Log Model", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Task E
# Creating a new data frame with the specified covariate values
new <- data.frame(
  Summer.temp = 18.9,
  cliff.height = log(3.74),  # Log-transform cliff height
  sandeel = 2.66,
  Coast.direction = "South"
)

# Predicting the natural logarithm of breeding pairs
result <- predict(log_model, newdata = new, interval = "confidence", level = 0.80)

# Transforming the confidence interval back to the original scale
confidence_interval <- exp(result[, 2:3])

# Printing the 80% confidence interval
print(confidence_interval)

