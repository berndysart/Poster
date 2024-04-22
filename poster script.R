#script

library(tidyverse)
library(haven)
library(stargazer)
library(knitr)
library(scales)
library(lfe)
library(modelsummary)

df = read_sav("ZA5960_v1-0-0.sav")

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
}

df = 
  df |>
  filter(COUNTRY == 348)|>
  mutate(
    v42.fixed = (6-v42),
    .keep = 'all'
  )
  
df |>
group_by(C_SAMPLE_YEAR) |>
count(v42.fixed)

df |> 
  mutate(across(where(is.labelled), as_factor)) |>
  filter(COUNTRY == "HU-Hungary") |>
  drop_na(DEGREE) |>
  ggplot(aes(x = DEGREE, fill = C_SAMPLE_YEAR)) +
  facet_wrap(~C_SAMPLE_YEAR) +
  geom_bar() +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  scale_x_discrete(
    labels = c("None", "Elementary", "Middle", "High School", "University", "Graduate School"),
    guide = 
      guide_axis(angle = 45)
  ) +
  scale_fill_manual(
    name = "Year", 
    labels = c("1995", "2003", "2013"),
    values = c("#c2b280", "#c19a6b", "#645452")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = 'black'),
    axis.title.x = element_blank(), 
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  labs(
    title = "Education Level of Respondents",
    y = "Frequency"
  )

df |>
  filter(COUNTRY == 348) |>
  count(C_SAMPLE_YEAR)
-----------------
df1 = 
  df |>
  filter(COUNTRY == 348) |>
  filter(C_SAMPLE_YEAR == 3481995) |>
  mutate(
    Degree = FctWhen(
      DEGREE == 0 ~ "None",
      DEGREE == 1 ~ "Elementary",
      DEGREE == 2 ~ "Middle",
      DEGREE == 3 ~ "Secondary",
      DEGREE == 4 ~ "University",
      DEGREE == 5 ~ "Graduate School"
    ),
    Wave = FctWhen(
      C_SAMPLE_YEAR == 3481995 ~ "1995"
    ),
    .keep = 'all'
  )|>
  drop_na(Degree) |>
  group_by(Degree, Wave)|>
  summarise(
    Observations = n(),
    AverageScore = mean(v42.fixed, na.rm = TRUE)
  )
  
df2 =
  df |>
  filter(COUNTRY == 348) |>
  filter(C_SAMPLE_YEAR == 3482003) |>
  mutate(
    Degree = FctWhen(
      DEGREE == 0 ~ "None",
      DEGREE == 1 ~ "Elementary",
      DEGREE == 2 ~ "Middle",
      DEGREE == 3 ~ "Secondary",
      DEGREE == 4 ~ "University",
      DEGREE == 5 ~ "Graduate School"
    ),
    Wave = FctWhen(
      C_SAMPLE_YEAR == 3482003 ~ "2003"
    ),
    .keep = 'all'
  )|>
  drop_na(Degree) |>
  group_by(Degree, Wave) |>
  summarise(
    Observations = n(),
    AverageScore = mean(v42.fixed, na.rm = TRUE)
  )

df3 = 
  df |>
  filter(COUNTRY == 348) |>
  filter(C_SAMPLE_YEAR == 3482013) |>
  mutate(
    Degree = FctWhen(
      DEGREE == 0 ~ "None",
      DEGREE == 1 ~ "Elementary",
      DEGREE == 2 ~ "Middle",
      DEGREE == 3 ~ "Secondary",
      DEGREE == 4 ~ "University",
      DEGREE == 5 ~ "Graduate School"
    ),
    Wave = FctWhen(
      C_SAMPLE_YEAR == 3482013 ~ "2013"
    ),
    .keep = 'all'
  )|>
  drop_na(Degree) |>
  group_by(Degree, Wave) |>
  summarise(
    Observations = n(),
    AverageScore = mean(v42.fixed, na.rm = TRUE)
  )  

totals = left_join(df1, df2, by = 'Degree')

totals2 = left_join(totals, df3, by = 'Degree')

finaltotals = 
  totals2 |>
  rename(
    Wave.1 = Wave.x,
    Observations.1 = Observations.x,
    AverageScore.1 = AverageScore.x,
    Wave.2 = Wave.y,
    Observations.2 = Observations.y,
    AverageScore.2 = AverageScore.y,
    Wave.3 = Wave,
    Observations.3 = Observations,
    AverageScore.3 = AverageScore
  )

finaltotals |>
  kable(digits = 1L)

mods =
  list(
    m1 = felm(v42.fixed ~ DEGREE + AGE + SEX|C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df),
    m2 = felm(v42.fixed ~ DEGREE + AGE + SEX + EDUCYRS|C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df),
    m3 = felm(v42.fixed ~ DEGREE + AGE + SEX + EDUCYRS + WRKHRS |C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df)
  )
modelsummary(mods,
             type = 'text', 
             gof_map = 'nobs',
             stars = c('*' = 0.05),
             keep.stat = 'n',
             statistic = NULL,
             title = 'DV: Immigration Crime Rate Response')

stargazer(mods, type = 'html', keep.stat = 'n')
