df = read.csv("/Users/tiyanigurusinghe/Downloads/code_smells_refactoring_dataset_120k.csv")
head(df)
df$delta_refraction_complexity = df$post_refactor_complexity - df$pre_refactor_complexity
head(df)

hist(df$delta_refraction_complexity, main = "Histogram of Delta Refraction Complexity", xlab = "Delta Complexity", col = "skyblue")
hist(df$delta_refraction_complexity[df$refactoring_applied == 1],
     breaks = 50,
     main = "Delta Complexity (Refraction Applied = 1)",
     xlab = "Delta Complexity",
     col = "green")

hist(df$delta_refraction_complexity[df$refactoring_applied == 0],
     breaks = 50,
     main = "Delta Complexity (Refraction Applied = 0)",
     xlab = "Delta Complexity",
     col = "red")
# Both the above hist shows a bell curve indicating we can use delta complexity 
# of all data available as we can mititagte researcher might have been lazy when 
# calculating post refractor complexity in codes that where refraction was not 
# applied
mean_applied_1 = mean(df$delta_refraction_complexity[df$refactoring_applied == 1], na.rm = TRUE)
mean_applied_1

mean_applied_0 = mean(df$delta_refraction_complexity[df$refactoring_applied == 0], na.rm = TRUE)
mean_applied_0
mean_delta = mean(df$delta_refraction_complexity, na.rm = TRUE)
mean_delta
#means are also pretty simila so we can just go ahead and use all of delta_refraction_complexity

summary(df$delta_refraction_complexity)


# QQ Plot
qqnorm(df$delta_refraction_complexity)
qqline(df$delta_refraction_complexity, col="red")

# K-S Test for large datasets
# The Kolmogorov-Smirnov (K-S) Test

ks.test(scale(df$delta_refraction_complexity), "pnorm")





# -------------------------------------

one_sample_twosided_ttest = t.test(df$delta_refraction_complexity, mu = 0)


print(one_sample_twosided_ttest)


one_sample_onesided_ttest = t.test(df$delta_refraction_complexity, mu = 0, alternative = "less")

print(one_sample_onesided_ttest)

# ==========================================
# 1. Check Distribution: Bug Prone Score
# ==========================================

# Histogram
hist(df$bug_prone_score, 
     main="Histogram of Bug Prone Score", 
     xlab="Bug Prone Score", 
     col="lightblue")

# Q-Q Plot
qqnorm(df$bug_prone_score, main="Q-Q Plot for Bug Prone Score")
qqline(df$bug_prone_score, col="red")

# K-S Test
ks_result_bug = ks.test(scale(df$bug_prone_score), "pnorm")
print("K-S Test for Bug Prone Score:")
print(ks_result_bug)


# ==========================================
# 2. Check Distribution: Technical Debt Minutes
# ==========================================

# Histogram
hist(df$technical_debt_minutes, 
     main="Histogram of Technical Debt", 
     xlab="Technical Debt (Minutes)", 
     col="lightcoral")

# Q-Q Plot
qqnorm(df$technical_debt_minutes, main="Q-Q Plot for Technical Debt")
qqline(df$technical_debt_minutes, col="red")

# K-S Test
ks_result_debt = ks.test(scale(df$technical_debt_minutes), "pnorm")
print("K-S Test for Technical Debt Minutes:")
print(ks_result_debt)

# ==========================================
# 3. Check Distribution: Maintainability Index
# ==========================================
# 1. Histogram
hist(df$maintainability_index, 
     main="Histogram of Maintainability Index", 
     xlab="Maintainability Index", 
     col="lightgreen")

# 2. Q-Q Plot
qqnorm(df$maintainability_index, main="Q-Q Plot for Maintainability Index")
qqline(df$maintainability_index, col="red")

# 3. K-S Test for Large Datasets
ks_result_maintainability = ks.test(scale(df$maintainability_index), "pnorm")
print(ks_result_maintainability)
# View the exact counts of 0s and 1s
counts_refactoring = table(df$refactoring_applied)
print(counts_refactoring)

# View it as percentages
proportions = prop.table(counts_refactoring) * 100
print(proportions)

# Create a simple bar plot to visualize it
barplot(counts_refactoring, 
        main="Distribution of Refactoring Applied", 
        xlab="Refactoring Applied (0 = No, 1 = Yes)", 
        ylab="Count", 
        col=c("salmon", "skyblue"))

# ==========================================
# 1. Test: Maintainability Index
# ==========================================
wilcox_maintainability = wilcox.test(maintainability_index ~ refactoring_applied, data = df)
print("--- Wilcoxon Test: Maintainability Index ---")
print(wilcox_maintainability)

# ==========================================
# 2. Test: Bug Prone Score
# ==========================================
wilcox_bug = wilcox.test(bug_prone_score ~ refactoring_applied, data = df)
print("--- Wilcoxon Test: Bug Prone Score ---")
print(wilcox_bug)

# ==========================================
# 3. Test: Technical Debt Minutes
# ==========================================
wilcox_debt = wilcox.test(technical_debt_minutes ~ refactoring_applied, data = df)
print("--- Wilcoxon Test: Technical Debt Minutes ---")
print(wilcox_debt)


# Create the true complexity column based on reality
df$true_complexity = ifelse(df$refactoring_applied == 1, 
                            df$post_refactor_complexity, 
                            df$pre_refactor_complexity)
head(df[, c("refactoring_applied", "pre_refactor_complexity", "post_refactor_complexity", "true_complexity")])

# Build the multiple linear regression model
# We are predicting delta_complexity using all the variables you mentioned
model = lm(true_complexity ~ refactoring_applied + 
             lines_of_code + 
             developer_experience_years + 
             language + 
             framework, 
           data = df)

# Display the full results
summary(model)


# Convert to factor just to be safe
df$refactoring_suggested = as.factor(df$refactoring_suggested)


# Run the ANOVA
anova_type = aov(delta_refraction_complexity ~ refactoring_suggested, data = df)

# View the results
summary(anova_type)
df$smell_severity = as.factor(df$smell_severity)

anova_severity = aov(delta_refraction_complexity ~ smell_severity, data = df)
summary(anova_severity)
# The asterisk (*) tests Type, Severity, AND their combination
anova_combined = aov(delta_refraction_complexity ~ refactoring_suggested * smell_severity, data = df)

summary(anova_combined)

# (categorical)
df$refactoring_applied = as.factor(df$refactoring_applied)
df$refactoring_suggested = as.factor(df$refactoring_suggested)
df$smell_severity = as.factor(df$smell_severity)


anova_true_type = aov(true_complexity ~ refactoring_applied * refactoring_suggested, data = df)
print("--- ANOVA: True Complexity by Applied and Type ---")
summary(anova_true_type)


anova_true_severity = aov(true_complexity ~ refactoring_applied * smell_severity, data = df)
print("--- ANOVA: True Complexity by Applied and Severity ---")
summary(anova_true_severity)

