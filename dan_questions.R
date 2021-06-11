
# Percent format ggplot2 --------------------------------------------------

library('tidyverse')

ggplot(data = diamonds) +
  geom_bar (mapping = aes(x = cut, y = stat(prop), group = 1)) +

  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,
                                                     scale = 1)) +
  gghighlight::gghighlight(max_highlight = 1)


# How to highlight

d <- data.frame(
  idx = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  value = c(1, 2, 3, 10, 11, 12, 9, 10, 11),
  category = rep(c("a", "b", "c"), 3),
  stringsAsFactors = FALSE
)

# Highlight the lines whose max values are larger than 10
ggplot(d, aes(idx, value, colour = category)) +
  geom_line() + gghighlight(max(value) > 10)

# Rounding multiple variables ---------------------------------------------

# How to round multiple numeric variables
#
# Approach 1: Using anonymous function ~

anscombe %>%
  mutate(across(where(is.numeric),
                ~ round(mean(.x, na.rm = TRUE), digits = 2)))

anscombe %>%
  mutate(across(where(is.numeric),
                ~ mean(.x, na.rm = TRUE) %>% round(digits = 2)))


# Aproach 2: Using ...
anscombe %>%
  mutate(across(where(is.numeric),
                mean,
                na.rm = TRUE))

# Aproach 3: Iterating using purrr map
anscombe %>%
  purrr::map_dbl(round(2))

