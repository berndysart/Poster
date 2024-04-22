#Problem Set 3
#B. DySart
#February 09, 2024

library(tidyverse)
library(knitr)
library(readr)
library(xtable)
#Question 1----------------
load('ssa_water.Rdata')

water1.0 = 
  water |>
  select(
    countryModule,
    interviewYear,
    waterTimeMins
  ) 

water2.0 = 
  water1.0 |>
  group_by(countryModule) |>
  print(n = 28) |>
  summarise(
    n = n(),
    year = list(unique(interviewYear)),
    wtdmean = mean(waterTimeMins, na.rm = TRUE),
    wtdsd = sd(waterTimeMins, na.rm = TRUE), 
    wtdmed = median(waterTimeMins, na.rm = TRUE), 
    perplot = mean(waterTimeMins == 0, na.rm = TRUE) * 100
  )
  kable(water2.0, digits = 0)

rm(water, water1.0, water2.0)
#Question 2--------------
Crid = read_csv('CRid.csv') #id file
Exp10 = read_csv('exp10.csv') #bonus
Exp11 = read_csv('exp11.csv') #pay

 Exp11.fixed = 
   Exp11|>
   rename(
     Bonus = Pay
   ) |>
   select (start, CompCode, Bonus)
 
 Exp10.fixed = 
   Exp10 |>
   select(start, CompCode, Bonus)

 Exp1011 = 
   full_join(
     Exp11.fixed,
     Exp10.fixed,
     by = join_by(CompCode)
   )
 
 full = 
   inner_join(
     Exp1011, 
     Crid, 
     by = join_by(CompCode)
   )
 
 full2.0 = 
   full |>
   select(AssignmentID, rID, CompCode, Bonus.x)
 
 str(full2.0)
 
 rm(test1.1, test2.1, test3.1, test4.1)
  
rm(Exp1011, test1, test1.1, test2, test2.1, test3.1)
#Question 3, 4, 5 ----------------
load('WordleDictionary.Rdata')

wordle1.0 = 
  wordle |>
  pivot_longer(
    cols = starts_with('l'),
    names_to = 'id',
    values_to = 'letters'
  )

wordle2.0 =
    wordle1.0 |>
    count(letters) |>
    arrange(desc(n)) |>
    print(n = 26) #show all 26
  
kable(wordle2.0)

wordle3.0 = 
  left_join(
    wordle1.0, 
    wordle2.0, 
    by = join_by(letters)
  )

wordle4.0 =
  wordle3.0 |>
  group_by(word) |>
  summarise(
    n = sum(n)
  ) |>
  filter(n == max(n) | n == min(n)) |>
  arrange(desc(n))

kable(wordle4.0)
  
wordle5.0 = 
  wordle3.0 |>
  group_by(word) |>
  filter(dups == FALSE) |>
  summarise(
    n = sum(n)
  ) |>
  filter(n == max(n) | n == min(n)) |>
  arrange(desc(n))

kable(wordle5.0)

rm(wordle, wordle1.0, wordle2.0, wordle3.0, wordle4.0, wordle5.0)
