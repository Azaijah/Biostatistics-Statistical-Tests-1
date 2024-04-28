# load required packages
library(readxl)
library(car)


# load study data
mulch_data <- read_excel("A:/ECU/Data Science Masters/Biostatistics/Assignment 1/Assignment 1 biostat/lhelend.xlsx", sheet = "Q3")

# normality tests
shapiro.test(mulch_data$`55_deg`)
shapiro.test(mulch_data$`65_deg`)

# equal variance test
values <- c(mulch_data$`55_deg`, mulch_data$`65_deg`)
group <- factor(rep(c("55_deg", "65_deg"), each = nrow(mulch_data)))
leveneTest(values, group, center = mean)


# obtain and print out descriptive stats for 55 and 65 degrees
combined_results_mulch <- list()

temperatures <- c("55_deg", "65_deg")

for (temp in temperatures) {

  data <- mulch_data[[temp]]
  descriptive_stats.describe <- describe(data, skew = FALSE, ranges = FALSE)
  descriptive_stats.summary <- summary(data)
  
  score_diff_ci <- t.test(data, conf.level = 0.95)
  
  combined_results_mulch[[temp]] <- list(
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


for (temp in names(combined_results_mulch)) {
  cat("Descriptive Statistics for", temp, ":\n")
  results <- combined_results_mulch[[temp]]
  cat(paste(names(results), ": ", unlist(results), "\n", sep = ""))
  cat("\n") 
}

# boxplot for outlier dectection
bp <- boxplot(mulch_data$`55_deg`, mulch_data$`65_deg`,
        main = "Days to Pathogen-free Mulch by Temperature",
        xlab = "Temperature Group",
        ylab = "Days to Pathogen-free Mulch",
        names = c("55°C", "65°C"),
        las = 1)

# obtain and print outliers and data needed for extreme outlier calculation
print(bp$out)

IQR(mulch_data$`65_deg`) 

quantile(mulch_data$`65_deg`, probs = 0.25)


# independent Samples t-test (welch... var.equal set to false)
t.test(mulch_data$`55_deg`, mulch_data$`65_deg`, var.equal=FALSE)


