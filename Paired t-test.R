#load required packages
library(readxl)
library(psych)

#load study data
data <- read_excel("A:/ECU/Data Science Masters/Biostatistics/Assignment 1/Assignment 1 biostat/lhelend.xlsx", sheet = "Q1")

alpha <- 0.05

#Paired t-test we find the diff
diff <- data$After_Score - data$Before_Score

# normality test
shapiro.test(diff)

# boxplot for outlier detection
boxplot(diff,
        main = "After - Before Score Differences",
        xlab = "Sample",
        ylab = "Score Difference",
        las = 1) 

# Obtain and print descriptive stats
descriptive_stats.describe <- describe(diff, skew = FALSE, ranges = FALSE)
descriptive_stats.summary <- summary(diff)

score_diff_ci <- t.test(diff, conf.level=0.95)


combined_results <- list(
  n = descriptive_stats.describe["n"],
  mean = descriptive_stats.describe["mean"],
  sd = descriptive_stats.describe["sd"],
  Min = descriptive_stats.summary["Min."],
  Q1 = descriptive_stats.summary["1st Qu."],
  Median = descriptive_stats.summary["Median"],
  Q3 = descriptive_stats.summary["3rd Qu."],
  Max = descriptive_stats.summary["Max."],
  LowerBound = score_diff_ci$conf.int[1],
  UpperBound = score_diff_ci$conf.int[2]

)
cat("Descriptive Statistics:\n")
cat(paste(names(combined_results), ": ", unlist(combined_results), "\n", sep = ""))

# paired t-test
t.test(data$After_Score, data$Before_Score, mu=0, alternative = "greater", paired = T, conf.level = 1-alpha)


