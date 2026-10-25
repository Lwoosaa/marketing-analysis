# Correlation Analysis — Marketing Dataset

Correlation analysis between customer annual income and spending behavior, using the Customer Personality Analysis dataset. Available in both R and Python.

## Files

| File | Description |
|------|-------------|
| `correlation_analysis_v0.3.Rmd` | Main R Markdown source file |
| `correlation_analysis_v0.3.html` | Rendered HTML report |
| `correlation_analysis.ipynb` | Python version (pandas, scikit-learn, SciPy, seaborn) with the children and wine analysis |
| `marketing_data.csv` | Raw dataset (2,240 customer records) |
| `marketing_data_dictionary.csv` | Variable descriptions |

## What the analysis covers

1. Dataset description
2. Data cleaning — removal of erroneous records (Income = $666,666; Year_Birth < 1930)
3. Missing value imputation for `Income` using Random Forest (`ranger`)
4. Descriptive statistics
5. Scatterplot with fitted line
6. Correlation analysis — Pearson, Log-Pearson, and Spearman compared
7. Outlier detection — Cook's Distance and IQR method
8. Correlation matrix and heatmap

## Key findings

- Strong positive association between income and total spending (Spearman ρ = 0.853, p < 2e-16)
- Spearman selected as primary measure due to influential observations and non-normal residual tails
- Wine and meat spending most strongly correlated with income; Recency shows virtually no association

## Python version: children and alcohol spending

The notebook repeats the R pipeline (same cleaning rules, Random Forest imputation with the same predictors) and adds a second question: do households with children spend less on wine?

- Raw association between number of children and wine spending: Spearman rho = -0.34
- But households with children also earn less (rho = -0.37 between income and children)
- After controlling for income (partial Spearman), the link nearly disappears: rho = -0.05
- Young children (Kidhome) go with lower wine spending (rho = -0.58), teenagers (Teenhome) slightly higher (rho = +0.11)

Conclusion: income is the main confounder behind the apparent children and alcohol relationship.

```bash
pip install pandas numpy scipy scikit-learn matplotlib seaborn jupyter
jupyter notebook correlation_analysis.ipynb
```

## Requirements (R)

```r
install.packages(c("tidyverse", "corrplot", "knitr", "kableExtra",
                   "ranger", "scales", "gridExtra"))
```

## How to run

Open `correlation_analysis_v0.3.Rmd` in RStudio and click **Knit**, or run:

```r
rmarkdown::render("correlation_analysis_v0.3.Rmd")
```
