#script

#libraries loaded---------
library(tidyverse)
library(haven)
library(stargazer)
library(knitr)
library(scales)
library(lfe)
library(modelsummary)
library(gt)
library(kableExtra)

#data from working directory------------
df = read_sav("ZA5960_v1-0-0.sav")

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
} #function to create new variables from old one with new labels

df = #filter out all other countries except Hungary
  df |>
  filter(COUNTRY == 348)|>
  mutate( #corrects the scale to be negative to positive with the greater number being positive
    v42.fixed = (6-v42),
    .keep = 'all'
  )
  
#education level-----------------
df |> 
  mutate(across(where(is.labelled), as_factor)) |>#instead of using the numeric number, uses factor label
  drop_na(DEGREE) |>
  group_by(DEGREE) |>
  ggplot(aes(x = DEGREE, y = v42.fixed, fill = C_SAMPLE_YEAR)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,5)
  ) +
  scale_x_discrete(
    labels = c("None", "Elementary", "Middle", "High School", "University", "Graduate School"), #shortens the name
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
    y = "Average Score on Survey"
  )

df |>
  count(PARTY_LR)
#general information-----------------------

df |>
  mutate(across(where(is.labelled), as_factor)) |>
  drop_na(PARTY_LR) |>
  ggplot(aes(x = PARTY_LR)) +
  geom_bar(position = "dodge2", fill = '#d5a499') +
  coord_flip() +
  geom_text(stat = 'count', aes(label = after_stat(count), hjust = -0.5)) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,700)
  ) +
  scale_x_discrete(
    labels = c("Far Left", "Left", "Moderate", "Right", "Far Right") #shortens the name
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_line(colour = 'black'),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  labs(
    title = "Political Ideology of Respondents"
  )
#regression tables--------------------
mods =
  list(
    m1 = felm(v42.fixed ~ DEGREE + AGE|C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df),
    m2 = felm(v42.fixed ~ DEGREE + AGE + SEX + URBRURAL|C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df),
    m3 = felm(v42.fixed ~ DEGREE + AGE + SEX + URBRURAL + WRKSUP|C_SAMPLE_YEAR|0|C_SAMPLE_YEAR, data = df)
  )
modelsummary(mods,
             type = 'text', 
             gof_map = 'nobs',
             stars = c('*' = 0.05),
             keep.stat = 'n',
             statistic = NULL,
             title = 'DV: Immigration Crime Rate Response')
  tab_header(
    title = 'Immigrant Crime Rate Response'
  ) |>
  tab_options(
    table.font.size = 50,
    heading.title.font.size = 50,
    table.width = "100%"
  )

modelplot(mods, coef_omit = 'Interc') +
  scale_color_manual(
    name = "Model", 
    labels = c("Small Model", "Medium Model", "Large Model"),
    values = c("#f0e130", "#100c08", "#ff0800")
  ) + 
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = 'black'),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  labs(
    x = 'Coefficients',
    title = 'Linear Regression of "Immigrants Raise Crime Rate Question"'
  )

count(df, MARITAL)
