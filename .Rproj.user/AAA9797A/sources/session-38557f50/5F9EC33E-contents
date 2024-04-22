library(tidyverse)
library(patchwork)
library(haven)
library(lubridate)
install.packages("extrafont")
library(extrafont)
font_import()

l

laturnout = read_dta('la_turnout_basic.dta')

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
}

laturnouttest = laturnout

#Question 1-------------------------

laturnout = 
  laturnout |>
  mutate(
    UnderstandingClause = FctWhen(
      understandingclause2 == 0 ~ "Control",
      understandingclause2 == 1 ~ "Treated"
    ),
    .keep = 'unused'
  )

Black 
  laturnout |>
  group_by(year, UnderstandingClause) |>
  mutate(
    across(where(is.labelled), as_factor),
    brrate = mean(blackregrate, na.rm = TRUE),
  ) |>
  filter(between(year, 1950, 1970)) |>
  ggplot(aes(x = year, 
           y = brrate, 
           group = UnderstandingClause)
      ) +
  geom_line(aes(
    colour = UnderstandingClause
    )
    ) +
  geom_point(aes(
    shape = UnderstandingClause
    )
    ) +
  scale_colour_manual(
    values = c("Control" = "purple4", "Treated" = "yellow1")
  ) +
  labs(
    y = "Black Regression Rate",
    x = "Year",
    caption = "(a) Black Registration"
  ) +
  guides(
      shape = guide_legend(
        override.aes = list(size = 3)
      )
    ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.key = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 12),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey"),
    panel.border = element_rect(colour = "black", fill = NA, size = 2)
  ) 

White =
  laturnout |>
  group_by(year, UnderstandingClause) |>
  mutate(
    across(where(is.labelled), as_factor),
    wrrate = mean(whiteregrate, na.rm = TRUE),
  ) |>
  filter(between(year, 1950, 1970)) |>
  ggplot(aes(x = year, 
             y = wrrate, 
             group = UnderstandingClause)
  ) +
  geom_line(aes(
    colour = UnderstandingClause
  )
  ) +
  geom_point(aes(
    shape = UnderstandingClause
  )
  ) +
  scale_colour_manual(
    values = c("Control" = "purple4", "Treated" = "yellow1")
  ) +
  labs(
    y = "White Regression Rate",
    x = "Year",
    caption = "(b) White Registration"
  ) +
  guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.key = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 12),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey"),
    panel.border = element_rect(colour = "black", fill = NA, size = 2)
  )

Black + White  

#Question 2--------------------------
laturnout = laturnouttest

laturnouttest2 = laturnouttest

laturnout |>
  group_by(year, UnderstandingClause) |>
  mutate(
    brrate = mean(blackregrate, na.rm = TRUE),
    wrrate = mean(whiteregrate, na.rm = TRUE),
    .keep = 'unused'
  ) |>
  pivot_longer(
    cols = ends_with('rate'),
    names_to = 'regrate',
    values_to = 'regratevalues'
  ) |>
  mutate(
    RegRate = FctWhen(
      regrate == 'brrate' ~ '(a) Black Registration Rate',
      regrate == 'wrrate' ~ '(b) White Registration Rate'
    ),
    .keep = 'unused'
  ) |>
  filter(between(year, 1950, 1970)) |>
  ggplot(aes(x = year, y = regratevalues, group = UnderstandingClause)) +
  geom_rect(aes( xmin = 1954, xmax= 1964, ymin= 0, ymax=1), fill = "gray90", alpha = 0.5)+
  facet_wrap(~RegRate) +
  geom_line(aes(
    colour = UnderstandingClause
    )
  ) +
  geom_point(aes(
    shape = UnderstandingClause
    )
  ) +
  scale_colour_manual(
      values = c("Control" = "aquamarine4", "Treated" = "tan3") 
    )+
  guides(
      shape = guide_legend(
        override.aes = list(size = 2)
      )
    )+
  theme(
    legend.box.spacing = unit(0, "pt"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 1),
    strip.background = element_blank()
  )+
  labs(
    y = "Regression Rate",
    x = "Year",
    caption = "Shaded space denotes use of understanding clause between 1954 and 1964"
  )

