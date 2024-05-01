# Poster Assignment

## Introduction

This was an assignment for SIS-750 Data Analysis at American University in Washington DC. The project was to create a poster using data of our choosing to evaluate a research interest. The assignment required that the entire poster be produced within RStudio using Posterdown then posted to github, being evaluated for its visualisations, analysis, and end product

## Research Topic

My research interest is in nationalism, national identity, and populism. Current trends on nationalist-populist political campaigns and parties heavily focus on certain political interests such as immigration and globalisation. In an effort to understand the supporter base of these kinds of parties, I decided to look into whether the education level influenced views on immigrants. In the field of nationalism research and voting behavior, it is of interest to know the educational background of those who are not supportive of immigrants. In knowing this, it would help to inform new policies or approaches by campaigns or parties to ease the fears and the negative effects of xenophobic views. 

## Data

The data that was used was from the International Social Survey Programme (ISSP) national identity module. This is a cross-national collaboration program that conducts annual surveys on social science topics. The survey data was collected through in-person interviews internationally. This survey was replicated three different times in 1995, 2003, and 2013 with the same questions. These questions asked about feelings about immigration, democracy, and feelings toward the nation. 

## Approach
- Cleaning
  - Created a new variable that reversed the scale of the particular survey question
- Visualisation
  - Graph showing the distribution of self identified political ideological alignment
  - Graph showing the average score on survey question asking about agreement with statement on whether Immigrants increase crime rates
  - Graph showing the coefficients of regression models for the variables
- Data Analysis
  - Regression table looking at variables to determine correlation between educational level and increase in agreement with statement
  
## Exemplary Code
- Filtered out the country from the entire data set to limit constant cleaning throughout visualisation
- Reversed the scale so that strongly disagree was associated with the number 1 and strongly agree was associated with 5, for ease of interpretation
  - Please see the rmd script in the r set up chunk

```
df =
  df |>
  filter(COUNTRY == 348)|>
  mutate(
    v42.fixed = (6-v42),
    .keep = 'all'
  )
```
