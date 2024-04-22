#Presentation

library(tidyverse)
library(haven)
library(patchwork)
library(forcats)

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
}

#load data
USD <- read_dta("USD 30 Dataset/events.dta")

USD2 = USD

USD = 
  USD |>
  mutate(
    TYPE = FctWhen(
      PTYPE == 10 ~ "General Warfare",
      PTYPE == 20 ~ "Inter-communal Warfare",
      PTYPE == 30 ~ "Armed Battle/Clash",
      PTYPE == 31 ~ "Armed Attack",
      PTYPE == 40 ~ "Pro-Government Terrorism (Repression)",
      PTYPE == 41 ~ "Anti-Government Terrorism",
      PTYPE == 42 ~ "Communal Terrorism",
      PTYPE == 50 ~ "Organised Violent Riot",
      PTYPE == 51 ~ "Spontaneous Violent Riot",
      PTYPE == 60 ~ "Organised Demonstration",
      PTYPE == 61 ~ "Pro-Government Demonstration",
      PTYPE == 62 ~ "Spontaneous Demonstration",
      PTYPE == 70 ~ "Other"
    )
  )

count(USD, TYPE)
#Slide 3

USD |>  
  ggplot(aes(x = fct_infreq(TYPE), fill = TYPE))+
  coord_flip(ylim = c(0,5000)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count), hjust = -0.5)) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank()
  ) +
  labs(
    y = "Frequency"
  )

#Slide 4 do the same calculation for global sample and then the rest. bar chart,], class scripts review in visualisation. 
USD2 =
  USD |>
  summarise(
    mean = mean(TARGET1 == "Government"),
  ) |>
  mutate(
    REGION = "Global"
  )
  
USD |>
  group_by(REGION) |>
  summarise(
    mean = mean(TARGET1 == "Government")
  ) |>
  bind_rows(USD2) |>
  ggplot(aes(
    x = fct_rev(fct_infreq(REGION, mean)),
    y = mean,
    fill = (REGION == "Global")
  )) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    panel.background = element_blank()
  ) +
  labs(
    y = "Mean",
    x = "Region"
  )


