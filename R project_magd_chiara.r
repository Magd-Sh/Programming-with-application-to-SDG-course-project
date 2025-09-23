install.packages(c("tidyverse","readxl","janitor","fixest"))
library(tidyverse)
library(readxl)
library(janitor)
library(fixest)

# Importing the dataset

path <- "/Users/majdshamashan/Desktop/SDG R project/interactions_dataset.xlsx"
raw <- read_excel(path, sheet = "Sheet1") |> 
  clean_names()

# Reshape the dataset for panel 

raw <- raw |>
  rename(country = any_of(c("country","Country","COUNTRY"))) |>
  mutate(country = as.character(country))

names(raw) <- names(raw) |>
  str_replace("^avg_non_market_stringecy_", "avg_non_market_stringency_")

years <- 2018:2023
suffix_rx <- paste0("(", paste(years, collapse="|"), ")$")

panel <- raw |>
  pivot_longer(
    cols = matches(paste0("_", suffix_rx)),
    names_to = c(".value", "year"),
    names_pattern = "^(.*)_(\\d{4})$"
  ) |>
  mutate(year = as.integer(year)) |>
  arrange(country, year)

num_cols <- panel |>
  select(-country, -year) |>
  names()
panel <- panel |>
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))

View(panel)

# renaming the dataset
interactions_panel_2018_2023 <- panel

# Summary statistics
install.packages('modelsummary')
library(modelsummary)

datasummary_skim(
  panel[, c("c02_per_gdp",
            "avg_market_stringency",
            "avg_non_market_stringency",
            "ghg_emissions_targets",
            "fossil_prod_policies",
            "international_coop",
            "ghg_emissions_reporting")],
  type = "numeric",
  histogram=FALSE,
  output = "/Users/majdshamashan/Desktop/SDG R project/stat_table.tex"
)


# Creating a pair plot to see correlation between variables

install.packages('GGally')
library(GGally)

vars_subset <- c(
  "c02_per_gdp",
  "avg_market_stringency",
  "avg_non_market_stringency",
  "ghg_emissions_targets",
  "fossil_prod_policies",
  "international_coop",
  "ghg_emissions_reporting"
)

GGally::ggpairs(
  data = interactions_panel_2018_2023,
  columns = vars_subset,     # the vars you want in the plot
  progress = FALSE,
  upper = list(continuous = wrap("cor", size = 3)),
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
  diag  = list(continuous = wrap("densityDiag"))
)

# Creating correlation matrix
ggcorr(interactions_panel_2018_2023, method = c("pairwise", "pearson"),
       nbreaks = NULL, digits = 3, low = "blue",
       mid = "white", high = "red",
       geom = "tile", label = T,
       label_alpha = F)


# reshape data to long format and create data visualisations
library(ggplot2)
library(reshape2)

vars_subset2 <- c(
  "avg_market_stringency",
  "avg_non_market_stringency",
  "ghg_emissions_targets",
  "fossil_prod_policies",
  "international_coop",
  "ghg_emissions_reporting"
)

df_long <- melt(panel[, vars_subset2])
ggplot(df_long, aes(x = value)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()



# Declare panel data structure and deciding on fixed effects vs. random effects model
library(plm)
pdata <- pdata.frame(interactions_panel_2018_2023, index = c("country", "year"))

fe_model <- plm(
  log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency + 
    avg_market_stringency * avg_non_market_stringency +
    ghg_emissions_targets + fossil_prod_policies +
    international_coop + ghg_emissions_reporting,
  data = pdata,
  model = "within"
)
summary(fe_model)


re_model <- plm(
  log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency +
    avg_market_stringency * avg_non_market_stringency +
    ghg_emissions_targets + fossil_prod_policies +
    international_coop + ghg_emissions_reporting,
  data = pdata,
  model = "random"   
)
summary(re_model)


# Conducting a Hausman test to decide fixed vs. random effects 
# since p-value is 0.69 > 0.05, the random effects model if preferred
phtest(fe_model, re_model)


# Try to cluster SE at the country level to see the effect on results
library(lmtest)
library(sandwich)
coeftest(re_model, vcov = vcovHC(re_model, type = "HC1", cluster = "group"))

# Running different regressions with different varieties of variables to see hidden effects ========================

# re_model1 : only with market and non-market instruments
re_model1<- plm(
  log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency,
  data = pdata,
  model = "random"  
)
summary(re_model1)

# re_model2 : re_model1 with an interaction term between market and non-market instruments
re_model2<- plm(
  log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency +
    avg_market_stringency * avg_non_market_stringency,
  data = pdata,
  model = "random"  
)
summary(re_model2)

# re_model3 : re_model2 with an interaction term between market and non-market instruments
re_model3<- plm(
  log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency +
    avg_market_stringency * avg_non_market_stringency +
    ghg_emissions_targets + fossil_prod_policies +
    international_coop + ghg_emissions_reporting,
  data = pdata,
  model = "random"  
)
summary(re_model3)


# Exporting the results of different regressions

models <- list(
  "Model 1" = re_model1,
  "Model 2" = re_model2,
  "Model 3" = re_model3
)

vcovs <- list(
  vcovHC(re_model1, type = "HC1", cluster = "group"),
  vcovHC(re_model2, type = "HC1", cluster = "group"),
  vcovHC(re_model3, type = "HC1", cluster = "group")
)

msummary(models, stars = c('*'=.10, '**'=.05, '***'=.01), coef_omit = "^\\(Intercept\\)$", output = '/Users/majdshamashan/Desktop/SDG R project/reg_table.tex')
# with clustered SE:
# msummary(models, vcov=vcovs, stars = c('*'=.10, '**'=.05, '***'=.01), coef_omit = "^\\(Intercept\\)$", output = '/Users/majdshamashan/Desktop/SDG R project/clustered_reg_table.tex')



#  Checking assumptions: ========================================================

# 1. Linearity
vars_subset2 <- c(
  "avg_market_stringency",
  "avg_non_market_stringency",
  "ghg_emissions_targets",
  "fossil_prod_policies",
  "international_coop",
  "ghg_emissions_reporting"
)

df_long <- panel %>%
  mutate(log_co2_per_gdp = log(c02_per_gdp)) %>%
  pivot_longer(cols = all_of(vars_subset2),
               names_to = "variable",
               values_to = "x_value") %>%
  filter(is.finite(log_co2_per_gdp), is.finite(x_value))

ggplot(df_long, aes(x = x_value, y = log(c02_per_gdp))) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "log(CO2 per GDP)", title = "Log CO2 intensity vs regressors") +
  theme_minimal()


# 2. Normality of residuals
install.packages("patchwork")
library(patchwork)
res_df <- data.frame(residual = as.numeric(residuals(re_model, type = "response")))

p1 <- ggplot(res_df, aes(x = residual)) +
  geom_histogram(bins = 30, color = "white") +
  labs(title = "Histogram of Residuals", x = "Residual Value", y = "Count") +
  theme_minimal()

p2 <- ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

p1 | p2 

# 3. homoscedasticity
df_plot <- data.frame(
  fitted = as.numeric(fitted(re_model)),
  resid  = as.numeric(residuals(re_model, type = "response"))
)

ggplot(df_plot, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0) +
  labs(title = "Fitted Values vs Residuals",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 4. Multicollinearity
library(car)

model <- lm(log(c02_per_gdp) ~ avg_market_stringency + avg_non_market_stringency +
              ghg_emissions_targets + fossil_prod_policies +
              international_coop + ghg_emissions_reporting,
            data = interactions_panel_2018_2023)

vif(model)
summary(model)















