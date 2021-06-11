library(tidyverse)
library(haven)
library(scales)
library(dineq) #used for question 5
library(readxl)

# Question 4 -------
# Learning about sampling through COVID cases

# Explore the dataset
covid <- read_xls("../3 - to post - ps2/COVID Data - Our World in Data - Sept 2 2020.xls")
# Top 5 countries

top5_cases <- covid %>%
  arrange(desc(total_cases)) %>%
  select(Country, total_cases) %>%
  slice(1:5)
top5_cases

# Histogram of cases  ----
ggplot(covid, aes(x = total_cases, y = stat(density*width))) +
  geom_histogram(color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Total COVID Cases",
       y = "Proportion")


# Summary statistics
covid %>%
  summarize(mean = mean(total_cases , na.rm = TRUE),
            sd = sd(total_cases , na.rm = TRUE),
            n = sum(!is.na(total_cases)))


# Distribution of Y in the sample
# Random sample  ----
covid_sample <- sample_n(covid, 30)

# Histogram of the new sample
ggplot(covid_sample, aes(x = total_cases, y = stat(density*width))) +
  geom_histogram(color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Total COVID Cases",
       y = "Proportion")

# Summary statistics
covid_sample %>%
  summarize(mean = mean(total_cases, na.rm = TRUE),
            sd = sd(total_cases, na.rm = TRUE),
            n = n())


# Question 5 ----
# Learning about inequality in the world

# 1: Explore the dataset ---
wdi <- read_dta("../3 - to post - ps2//WDI - API-209.dta")

# 1.a ----

# mean and number of observations
# mean, median, 25th percentile, 75th percentile
# for GDP per capita (PPP-adjusted) in 2017

wdi %>%
  summarize(mean = mean(gdppc2017, na.rm = TRUE),
            p25 = quantile(gdppc2017, 0.25, na.rm = TRUE),
            median = quantile(gdppc2017, 0.50, na.rm = TRUE),
            p75 = quantile(gdppc2017, 0.75, na.rm = TRUE),
            n = n())

# 1.b ----
# Unweighted theil index
theil.wtd(wdi$gdppc1992)
theil.wtd(wdi$gdppc2017)

# 1.c ----

# Weighted theil index
theil.wtd(x = wdi$gdppc1992, weights = wdi$pop1992)
theil.wtd(x = wdi$gdppc2017, weights = wdi$pop2017)

# 1.d -----
n_wdi <- nrow(filter(wdi, !is.na(gini)))
wdi %>%
  filter(!is.na(gini)) %>%
  arrange(desc(gini)) %>%
  select(country, gini) %>%
  slice(c(1:3), c((n_wdi - 2):n_wdi))


# 1.f -----
gini_fmt <- wdi %>%
  filter(!is.na(gini), !is.na(corruption)) %>%
  mutate(unequal = gini > 40,
         corrupt = corruption <= quantile(corruption, 0.2))

# Note, this uses the "formula notation" of the ~ corrupt + unequal
xtabs(~ corrupt + unequal, gini_fmt)/nrow(gini_fmt)

round(prop.table(xtabs(~ corrupt + unequal, gini_fmt), 2), 3)
round(prop.table(xtabs(~ corrupt + unequal, gini_fmt), 1), 3)
0.1222222/(0.1222222+0.2000000)

# Optional Graphs -----
ggplot(wdi, aes(x = log(gdppc2017) , y =  nnmort)) +
  geom_point(color = "grey") +
  geom_smooth() +
  labs(x = "GDP per capita in 2017, logs",
       y = "Neonatal Mortality")

ggplot(wdi, aes(x = log(gdppc2017) , y =  lifeexp)) +
  geom_point(color = "grey") +
  geom_smooth() +
  labs(x = "GDP per capita in 2017, logs",
       y = "Life Expectancy, Years")

ggplot(wdi, aes(x = log(gdppc2017) , y =  corruption)) +
  geom_point(color = "grey") +
  geom_smooth() +
  labs(x = "GDP per capita in 2017, logs",
       y = "Corruption, Index")

