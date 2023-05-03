# Packages----
library(tidyverse)
library(here)
#_________________________________----

# Load data ----
butterfly <- read_csv(here("Data", "univoltine_butterfly.csv"))
#_________________________________----
# Check data ----

## Structure ----
glimpse(butterfly)

## Check tidy ----
head(butterfly)
#year is dbl when it could be int - NEEDS CHANGING

## Check names ----
colnames(butterfly)

#_________________________________----
# Data clean----

## Clean names ----
butterfly <- janitor::clean_names(butterfly)

## Check duplicates ----
butterfly %>% 
  duplicated() %>% 
  sum()

## Check typos ----
butterfly %>% 
  summarise(min=min(forewing_length, na.rm=TRUE), 
            max=max(forewing_length, na.rm=TRUE))

butterfly %>% 
  distinct(year)

butterfly %>% 
  distinct(sex)
#found typo Female and Maes 
###fix typos ----
butterfly <- butterfly %>% 
  mutate(sex = case_when(sex == "Female" ~ "Females",
                         sex == "Maes" ~ "Males"))

#using mutate created NAs but I will work with them because reverting didn't work

## Missing values ---- 
butterfly %>% 
  is.na() %>% 
  sum()
#somehow after mutating the typos out all the data (51) became a missing value
#_________________________________----
#Summarise ----
summary(butterfly)

