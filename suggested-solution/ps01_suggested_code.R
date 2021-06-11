library(tidyverse)
library(readxl)
library(scales)


# Question 1 (if making the graph in R) ----

x_range <- tibble(x = c(0, 0.4))

# draw a continuous function using stat_function based on Bayes' Rule calculation

bayes_function <- function(p){
  (0.88*p)/(0.88*p + (1-0.988)*(1-p))
}

ggplot(x_range, aes(x = x)) +
  stat_function(fun = bayes_function) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Prevalence of COVID",
       y = "Positive Predicted Value of Antibody Test")+
  ylim(c(0,1))+
  theme_light()
ggsave("COVID_pred-value-curve.pdf", width = 4, height = 4)
ggsave("COVID_pred-value-curve.png", width = 4, height = 4)

# Question 4
# data -----
wdi_raw <- read_excel("../4 - to post/WDI Data Extract API-209 - PS 1.xlsx")

# transform data to analysis ----
wdi <- wdi_raw %>%
  mutate(
    gdp_1993 = gdp_1993 / 1e6,
    gdp_2018 = gdp_2018 / 1e6,
    pop_1993 = pop_1993 / 1e6,
    pop_2018 = pop_2018 / 1e6,
  ) %>%
  mutate(gdp_pc_1993  = gdp_1993 / pop_1993,
         gdp_pc_2018 = gdp_2018 / pop_2018) %>%
  filter(!is.na(gdp_pc_2018))


# (1) ----
mean(wdi$gdp_2018, na.rm = TRUE)
nrow(wdi)


# (2) ----
# a
sum(wdi$gdp_2018, na.rm = TRUE)
sum(wdi$pop_2018, na.rm = TRUE)


# b
arrange(wdi, desc(gdp_2018)) %>%
  select(country, gdp_2018)

arrange(wdi, desc(pop_2018)) %>%
  select(country, pop_2018)

# (3) ----
wdi %>%
  summarize(mean_gdp_pc_2018 = mean(gdp_pc_2018, na.rm = TRUE),
            tot_gdp_pc_2018 = sum(gdp_2018, na.rm = TRUE) / sum(pop_2018, na.rm  = TRUE),
            med_gdp_pc_2018 = median(gdp_pc_2018, na.rm = TRUE),
            pop_belo_mean = sum(pop_2018*(scale(gdp_pc_2018) < 0), na.rm = TRUE))

wdi %>%
  ggplot(aes(x = gdp_pc_2018)) +
  geom_histogram(binwidth = 2000) +
  scale_x_continuous(labels = dollar) +
  labs(x = "GDP per capita",
       y = "Number of Countries")


# (4) ----

wdi_fct <- wdi %>%
  mutate(income_group = fct_relevel(income_group,
                                    "Low income",
                                    "Lower middle income",
                                    "Upper middle income",
                                    "High income"
  ))

wdi_fct %>%
  group_by(income_group) %>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  select(Income = income_group, Proportion = prop)

wdi_fct %>%
  filter(region == "Sub-Saharan Africa") %>%
  count(income_group)


# (5) -------

sum(is.na(wdi$gdp_pc_1993))

sum(is.na(wdi$gdp_1993))
sum(is.na(wdi$pop_1993))

mean(wdi$gdp_pc_1993, na.rm = TRUE)

wdi %>%
  mutate(is_na = is.na(gdp_1993)) %>%
  group_by(is_na) %>%
  summarize(
    mean_2018 = mean(gdp_pc_2018, na.rm = TRUE)
  )



