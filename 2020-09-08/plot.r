library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(here)
library(ggplot2)
library(patchwork)
library(waffle)
library(hrbrthemes)

tt <- tt_load('2020-09-08')


main_friends <- tt$friends %>% 
  tabyl(speaker) %>% 
  arrange(desc(n)) %>% 
  head(6) %>% 
  pull(speaker)

# df <- tt
df <- tt$friends %>%
  count(season, speaker) %>%
  mutate(n = n/10) %>%
  filter(speaker %in% main_friends)

p1 <- ggplot(df, aes(fill = speaker, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 5, flip = TRUE) +
  facet_wrap(~season, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    # title = "TidyTuesday: Friends",
    # subtitle = "Number of lines ",
    x = "Season",
    y = "Count (each square = 10 lines)"
  ) +
  theme_minimal(base_family = "Roboto Condensed" ) +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  # theme_ft_rc() +
  guides(fill = guide_legend(reverse = TRUE))

p1 + plot_annotation(
  title = "Friends!",
  subtitle = "Who had the most lines?",
  caption = "Created by @andrewcstewart, Data from the {friends} package, #TidyTuesday",
  # theme = theme_ft_rc()
  ) 

ggsave(
  path = here::here('2020-09-08'),
  filename = "friends.png",
  device = "png")
