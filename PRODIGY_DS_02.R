#LOAD THE DATA

train <- read.csv("~/Desktop/INTERNSHIP TASK/TASK 2/train.csv", stringsAsFactors=TRUE)
View(train)
df <- train

head(df)

###  DATA CLEANING 

#HANDLING MISSING VALUES
colSums(is.na(df))

df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

df <- df[, !names(df) %in% c("Cabin")]

# CONVERT CATEGORICAL VARIABLES
df$Sex <- as.numeric(factor(df$Sex, levels = c("male", "female")))

df$Embarked <- as.numeric(factor(df$Embarked, levels = c("C", "Q", "S")))

### EXPLORATORY DATA ANALYSIS
#VISUALIZATION
install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = factor(Survived))) +
  geom_bar() +
  labs(title = "Distribution of Survived", x = "Survived", y = "Count")

#EXPLORE THE RELATIONSHIP
ggplot(df, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Proportion")


ggplot(df, aes(x = factor(Sex), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Gender", x = "Sex", y = "Proportion")


ggplot(df, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Survival", x = "Age", y = "Count")


#CORRELATION ANALYSIS
# Calculate correlation matrix
correlation_matrix <- cor(df[, sapply(df, is.numeric)], use = "complete.obs")
library(corrplot)

corrplot(correlation_matrix, method = "color", tl.cex = 0.8)

