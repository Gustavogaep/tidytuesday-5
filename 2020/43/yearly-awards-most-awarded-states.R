

library(tidyverse)
library(gghighlight)
library(wesanderson)

# reading as csv b/c had an issue with tidytuesdayR package
beer_awards <- read_csv(paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/",
  "data/2020/2020-10-20/beer_awards.csv"
)) %>%
  mutate(
    state = str_to_upper(state) # standardize state codes (e.g 'WA' vs. 'wa')
  )

# number of awards for each state and year
# and total awards received over all years for each state
award_counts <- beer_awards %>%
  count(state, year, sort = TRUE) %>%
  group_by(state) %>%
  mutate(total_awards = sum(n)) %>%
  ungroup()

# highlight states with lifetime awards in top 25%
# 155 is 3rd quartile of lifetime awards received
award_counts %>%
  ggplot(aes(year, n, color = state)) +
  geom_line() +
  gghighlight(
    total_awards > 155,
    use_direct_label = FALSE,
    use_group_by = FALSE
  ) +
  facet_wrap(~state) +
  scale_color_manual(
    values = wes_palette("GrandBudapest1", n = 7, type = "continuous")
  ) +
  labs(
    x = "Year",
    y = "Number of awards",
    title = "Yearly Great American Beer Festival awards",
    subtitle = "Highlighted are states in top 25% of total lifetime awards"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("2020/43/yearly-awards-most-awarded-states.png")
