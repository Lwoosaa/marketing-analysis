# ============================================================
# Correlation Analysis: Marketing Dataset
# Income vs Total Spending
#
# This script explores the association between customer annual
# household income and total product spending using:
#   - Random Forest imputation for missing income values
#   - Pearson, Log-Pearson, and Spearman correlation comparison
#   - Cook's Distance and IQR outlier detection
# ============================================================

library(tidyverse)
library(corrplot)
library(ggplot2)
library(ranger)
library(scales)
library(gridExtra)

# ── 1. LOAD DATA ─────────────────────────────────────────────
# Read dataset, clean column names, derive Age and TotalSpend

df <- read.csv("/kaggle/input/datasets/ahsan81/superstore-marketing-campaign-dataset/superstore_data.csv",
               stringsAsFactors = FALSE)

names(df) <- trimws(names(df))
df$Income <- as.numeric(trimws(df$Income))

df$Age        <- 2025 - df$Year_Birth
df$TotalSpend <- df$MntWines + df$MntFruits + df$MntMeatProducts +
                 df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds

cat("Observations:", nrow(df), "| Variables:", ncol(df), "\n")

# ── 2. DATA CLEANING ──────────────────────────────────────────
# Remove Income = 666666 (data entry error — $500k gap to next highest value)
# Remove Year_Birth < 1930 (ages 125+ are biologically impossible)

n_before <- nrow(df)

df <- df[!(df$Income == 666666 & !is.na(df$Income)), ]
df <- df[!(df$Year_Birth < 1930), ]
df$Age <- 2025 - df$Year_Birth

cat("Removed:", n_before - nrow(df), "rows | Remaining:", nrow(df), "\n")

# ── 3. MISSING VALUES ─────────────────────────────────────────
# Income has 24 missing values (~1%). TotalSpend has none.
# We impute rather than drop to retain all customers in the analysis.

cat("\nMissing values:\n")
cat("  Income    :", sum(is.na(df$Income)),
    paste0("(", round(mean(is.na(df$Income)) * 100, 1), "%)"), "\n")
cat("  TotalSpend:", sum(is.na(df$TotalSpend)), "\n")

# ── 4. INCOME IMPUTATION (Random Forest) ──────────────────────
# Train a Random Forest on complete cases to predict missing income.
# Predictors chosen for domain relevance:
#   Education, Age     → earning capacity
#   TotalSpend         → reflects disposable income
#   Kidhome, Teenhome  → household composition
#   NumCatalogPurchases, NumStorePurchases → purchasing behaviour

complete_rows <- df[!is.na(df$Income), ]
missing_rows  <- df[ is.na(df$Income), ]

set.seed(42)
train_idx <- sample(nrow(complete_rows), floor(0.8 * nrow(complete_rows)))
train_df  <- complete_rows[ train_idx, ]
test_df   <- complete_rows[-train_idx, ]

rf_model <- ranger(
  formula   = Income ~ Education + Age + TotalSpend +
                       Kidhome + Teenhome +
                       NumCatalogPurchases + NumStorePurchases,
  data      = train_df,
  num.trees = 500,
  seed      = 42
)

# Evaluate on held-out test set
test_preds <- predict(rf_model, data = test_df)$predictions
rmse <- round(sqrt(mean((test_preds - test_df$Income)^2)), 0)
r2   <- round(1 - sum((test_preds - test_df$Income)^2) /
                   sum((test_df$Income - mean(test_df$Income))^2), 3)

cat("\nImputation model (test set):\n")
cat("  RMSE:", paste0("$", format(rmse, big.mark = ",")), "\n")
cat("  R²  :", r2, "\n")

# Create income_noNA: original values where observed, RF predictions where missing
df$income_noNA <- df$Income
df$income_noNA[is.na(df$Income)] <- predict(rf_model,
                                             data = missing_rows)$predictions
cat("  Missing in income_noNA:", sum(is.na(df$income_noNA)), "\n")

# Distribution: observed vs imputed
dist_df <- data.frame(
  Income = c(df$Income[!is.na(df$Income)],
             df$income_noNA[is.na(df$Income)]),
  Group  = c(rep("Observed", sum(!is.na(df$Income))),
             rep("Imputed",  sum( is.na(df$Income))))
)
ggplot(dist_df, aes(x = Income, fill = Group)) +
  geom_histogram(binwidth = 5000, alpha = 0.65,
                 position = "identity", color = "white") +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("Observed" = "#2c7bb6", "Imputed" = "#d7191c")) +
  labs(title = "Income Distribution: Observed vs Imputed",
       x = "Annual Income (USD)", y = "Number of customers", fill = NULL) +
  theme_minimal(base_size = 13)

# ── 5. DESCRIPTIVE STATISTICS ─────────────────────────────────
# income_noNA: mean $51,933 | SD $21,464 | range $1,730–$162,397
# TotalSpend : mean $606    | SD $602    | range $5–$2,525 (right-skewed)

cat("\nDescriptive Statistics:\n")
desc <- function(x, name) {
  cat(name, "— n:", sum(!is.na(x)),
      "| Mean:", round(mean(x, na.rm=TRUE), 0),
      "| SD:", round(sd(x, na.rm=TRUE), 0),
      "| Median:", round(median(x, na.rm=TRUE), 0),
      "| Min:", round(min(x, na.rm=TRUE), 0),
      "| Max:", round(max(x, na.rm=TRUE), 0), "\n")
}
desc(df$income_noNA, "income_noNA")
desc(df$TotalSpend,  "TotalSpend ")

# ── 6. SCATTERPLOT ────────────────────────────────────────────
# Clear upward trend. Point cloud widens at higher incomes
# (heteroscedasticity) — motivates testing Spearman below.

ggplot(df, aes(x = income_noNA, y = TotalSpend)) +
  geom_point(alpha = 0.3, color = "#2c7bb6", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d7191c", linewidth = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Annual Income vs Total Spending",
       subtitle = "Each point = one customer | Red line: OLS with 95% CI",
       x = "income_noNA (USD)", y = "TotalSpend (USD)") +
  theme_minimal(base_size = 13)

# ── 7. ASSUMPTION CHECKS ──────────────────────────────────────
# Left : LOESS vs OLS — checks linearity
# Right: Residuals vs fitted — checks homoscedasticity
# A fan shape on the right confirms variance grows with income.

lm_raw   <- lm(TotalSpend ~ income_noNA, data = df)
resid_df <- data.frame(fitted    = fitted(lm_raw),
                       residuals = residuals(lm_raw))

p1 <- ggplot(df, aes(x = income_noNA, y = TotalSpend)) +
  geom_point(alpha = 0.2, color = "#2c7bb6", size = 1.4) +
  geom_smooth(method = "lm",    se = FALSE, color = "grey50",
              linewidth = 0.9, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#d7191c", linewidth = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Linearity check",
       subtitle = "Red = LOESS | Dashed = OLS",
       x = "income_noNA", y = "TotalSpend") +
  theme_minimal(base_size = 12)

p2 <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.2, color = "#2c7bb6", size = 1.4) +
  geom_hline(yintercept = 0, color = "#d7191c", linewidth = 0.9) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  scale_x_continuous(labels = comma) +
  labs(title = "Homoscedasticity check",
       subtitle = "Fan shape = heteroscedasticity",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 12)

grid.arrange(p1, p2, ncol = 2)

# ── 8. CORRELATION: PEARSON, LOG-PEARSON, SPEARMAN ────────────
# Three methods are compared to find the most appropriate measure:
#   Pearson     — linear association on raw values
#   Log-Pearson — Pearson after log1p transformation (tests log-linearity)
#   Spearman    — rank-based, robust to outliers and non-normality
#
# Result: Spearman (0.853) > Pearson (0.793) > Log-Pearson (0.759)
# Spearman is selected as the primary measure — higher than Pearson
# because a few influential observations (Cook's Distance) and
# non-normal residual tails pull Pearson downward.

ct_pearson  <- cor.test(df$income_noNA,        df$TotalSpend,
                        method = "pearson")
ct_log      <- cor.test(log1p(df$income_noNA), log1p(df$TotalSpend),
                        method = "pearson")
ct_spearman <- cor.test(df$income_noNA,        df$TotalSpend,
                        method = "spearman", exact = FALSE)

cat("\nCorrelation Results:\n")
cat("  Pearson     r =", round(ct_pearson$estimate,  3),
    "| 95% CI [", round(ct_pearson$conf.int[1], 3), ",",
                  round(ct_pearson$conf.int[2], 3), "]\n")
cat("  Log-Pearson r =", round(ct_log$estimate,      3),
    "| 95% CI [", round(ct_log$conf.int[1], 3), ",",
                  round(ct_log$conf.int[2], 3), "]\n")
cat("  Spearman    r =", round(ct_spearman$estimate, 3),
    "| p =", format.pval(ct_spearman$p.value, digits=3), "\n")

# Log scale scatterplot
ggplot(df, aes(x = log1p(income_noNA), y = log1p(TotalSpend))) +
  geom_point(alpha = 0.3, color = "#5ab4ac", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d7191c", linewidth = 1) +
  labs(title    = "Income vs Spending — Log Scale",
       subtitle = "log1p applied to both axes",
       x = "log1p(income_noNA)", y = "log1p(TotalSpend)") +
  theme_minimal(base_size = 13)

# Comparison bar chart
bar_df <- data.frame(
  Method = c("Pearson\n(raw)", "Log-Pearson\n(log)", "Spearman\n(rank)"),
  r      = c(ct_pearson$estimate, ct_log$estimate, ct_spearman$estimate)
)
ggplot(bar_df, aes(x = Method, y = r, fill = Method)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = round(r, 3)), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("#2c7bb6", "#5ab4ac", "#d7191c")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Comparison of Correlation Coefficients",
       x = NULL, y = "Correlation") +
  theme_minimal(base_size = 13)

# ── 9. OUTLIERS & INFLUENTIAL OBSERVATIONS ────────────────────
# Cook's Distance: 122 observations (5.5%) exceed the 4/n threshold.
# Visually only a few bars stand out, but the threshold (4/2236 = 0.0018)
# is very small — many bars that appear flat still exceed it mathematically.
# This, combined with non-normal tails in the Q-Q plot, strongly supports
# Spearman as the primary correlation measure.
# IQR method: 7 outliers in income_noNA, 3 in TotalSpend.

model     <- lm(TotalSpend ~ income_noNA, data = df)
cooksd    <- cooks.distance(model)
threshold <- 4 / nrow(df)

cat("\nInfluential observations (Cook's D > 4/n):", sum(cooksd > threshold), "\n")

par(mfrow = c(1, 2))
plot(cooksd, type = "h",
     main = "Cook's Distance", ylab = "Cook's Distance",
     xlab = "Observation Index",
     col  = ifelse(cooksd > threshold, "red", "steelblue"))
abline(h = threshold, col = "red", lty = 2, lwd = 1.5)
plot(model, which = 2, main = "Normal Q-Q Plot")
par(mfrow = c(1, 1))

iqr_out <- function(x) {
  q  <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iq <- IQR(x, na.rm = TRUE)
  sum(x < q[1] - 1.5*iq | x > q[2] + 1.5*iq, na.rm = TRUE)
}
cat("  IQR outliers — income_noNA:", iqr_out(df$income_noNA), "\n")
cat("  IQR outliers — TotalSpend :", iqr_out(df$TotalSpend),  "\n")

# ── 10. CORRELATION MATRIX & HEATMAP ──────────────────────────
# Income most strongly correlated with: MntWines (0.69),
# NumStorePurchases (0.64), NumCatalogPurchases (0.63).
# Recency shows virtually no association with income (0.01).

num_vars <- df[ , c("income_noNA", "MntWines", "MntFruits", "MntMeatProducts",
                    "MntFishProducts", "MntSweetProducts", "MntGoldProds",
                    "NumWebPurchases", "NumCatalogPurchases",
                    "NumStorePurchases", "TotalSpend", "Recency")]

cor_matrix <- cor(num_vars, use = "complete.obs")

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         order       = "hclust",
         tl.col      = "black",
         tl.srt      = 45,
         tl.cex      = 0.85,
         addCoef.col = "black",
         number.cex  = 0.7,
         col         = colorRampPalette(c("#d7191c", "white", "#2c7bb6"))(200),
         title       = "Pearson Correlation Matrix",
         mar         = c(0, 0, 2, 0))
