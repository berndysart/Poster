#Problem Set 4

#load librarys
library(tidyverse)
library(haven)
library(readr)
library(knitr)

#Question 1 --------------------

#load data
SSD <- read_csv("senate_seat_distribution_2022.csv")

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
}

SSD |>
  filter(expression == '_deluxe') |>
  mutate(
    SenateSeats = FctWhen(
      seatsheld < 49 ~ "GOP",
      seatsheld %in% 49:50  ~ "Prob",
      seatsheld > 50 ~ "DEM"
    ),
    .keep = 'all'
  ) |>
  ggplot(aes(x = seatsheld, y = (seatprob_Dparty * 100),
             fill = SenateSeats)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(
    values = c("GOP" = "tomato3", "Prob" = "dodgerblue3", "DEM" = "lightblue1")) +
  scale_x_continuous(
    breaks = c(38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60),
    labels = c('60','58','54','56','54','52','50','52','54','56','58','60')
  ) +
  scale_y_continuous(
    breaks = c(15, 10, 5, 0),
    labels  = c("15%", "10", "5", "0"),
    expand = c(0,0)
    ) +
  theme(
    axis.title.y = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(), 
    axis.line.x.bottom = element_line(colour = 'black'),
    panel.grid.major.y = element_line(colour = 'gray'),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 7),
    plot.subtitle = element_text(hjust = 0)
  ) +
    labs(
      title = "How Many Senate Seats We Expect Each Party to Win",
      subtitle = "Party seat counts based on who wins the Senate in our Deluxe model's 40,000 simulations",
      caption = "FIVETHIRTYEIGHT DELUXE FORECAST, OCT 12, 2022, 3PM EST"
    )

#Question 2----------------

load('blue_jays.rda')


blue_jays |>
  mutate(
    sex = FctWhen(
      KnownSex == "F" ~ "female",
      KnownSex == "M" ~ "male"
    ),
    .keep = "unused"
  ) |>
  ggplot(aes(x = Mass, y = Head, 
    size = Skull,
    colour = sex
    )
  ) +
  geom_point() +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.box.spacing = unit(0, "pt"),
    legend.key = element_blank(),
    legend.justification = "right",
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey")
  ) +
  scale_y_continuous(
    breaks = c(60, 58, 56, 54, 52)
  ) +
  scale_size_continuous(
    limits = c(28,34),
    breaks = c(28, 30, 32, 34)
  ) +
  scale_colour_manual(
    values = c("female" = "orangered", "male" = "navyblue")
  ) +
  guides(
    colour = guide_legend(
      order = 1, 
      title.position = "top", 
      title.hjust = 0.5,
      override.aes = list(size = 3)
      ),
    size = guide_legend(
      order = 2, 
      title = "skull size (mm)", 
      title.position = "top", 
      title.hjust = 0.5)
  ) +
  labs(
    y = "head length (mm)",
    x = "body mass (g)"
  )