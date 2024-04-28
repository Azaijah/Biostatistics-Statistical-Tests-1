# load required packages
library(psych)
library(car)
library(readxl)

# load study data
fertiliser_data <- read_excel("A:/ECU/Data Science Masters/Biostatistics/Assignment 1/Assignment 1 biostat/lhelend.xlsx", sheet = "Q2")


# format for anova test
fertiliser_long <- stack(fertiliser_data)

# normality test for each group
tapply(fertiliser_long$values, 
       fertiliser_long$ind, 
       shapiro.test)

# boxplot for outerlier check
bp <- boxplot(fertiliser_long$values ~ fertiliser_long$ind,
        main = "Yield Grouped by Fertiliser Type",
        xlab = "Fertiliser Type",
        ylab = "Yield",
        names = c("Fertiliser 1", "Fertiliser 2", "Fertiliser 3"),
        las = 1)

# obtain and print outliers and data needed for extreme outlier calculation
print(bp$out)

IQR(fertiliser_data$`Fertiliser 1`) 

quantile(fertiliser_data$`Fertiliser 1`, probs = 0.25)
quantile(fertiliser_data$`Fertiliser 1`, probs = 0.75)

IQR(fertiliser_data$`Fertiliser 3`) 

quantile(fertiliser_data$`Fertiliser 3`, probs = 0.25)
quantile(fertiliser_data$`Fertiliser 3`, probs = 0.75)

# obtain and print descriptive stats for all 3 fertilizer groups

combined_results_fertilisers <- list()

for (i in 1:3) {

  fertiliser_col <- paste("Fertiliser", i)
  descriptive_stats.describe <- describe(fertiliser_data[[fertiliser_col]], skew = FALSE, ranges = FALSE)
  descriptive_stats.summary <- summary(fertiliser_data[[fertiliser_col]])
  

  score_diff_ci <- t.test(fertiliser_data[[fertiliser_col]], conf.level=0.95)
  

  combined_results_fertilisers[[fertiliser_col]] <- list(
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
}


for (fertiliser_col in names(combined_results_fertilisers)) {
  cat("Descriptive Statistics for", fertiliser_col, ":\n")
  results <- combined_results_fertilisers[[fertiliser_col]]
  cat(paste(names(results), ": ", unlist(results), "\n", sep = ""))
  cat("\n") 
}


# anova test 
fertiliser_aov <- aov(fertiliser_long$values ~ as.factor(fertiliser_long$ind))
summary(fertiliser_aov)


# equal variance test
leveneTest(fertiliser_aov, 
           center = "mean")

# post hoc bonferroni test
pairwise.t.test(fertiliser_long$values, 
                fertiliser_long$ind, 
                p.adjust = "bonferroni")

