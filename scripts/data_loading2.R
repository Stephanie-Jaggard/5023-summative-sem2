# Packages----
library(tidyverse)
library(here)
#_________________________________----

# Load data ----
univoltine_butterfly <- read_csv(here("Data", "univoltine_butterfly.csv"))

butterfly <- select(.data = univoltine_butterfly,
                    Year, forewing_length, sex, JUN_mean, rain_JUN)

#_________________________________----
# Check data ----

## Structure ----
glimpse(butterfly)

## Check tidy ----
head(butterfly)

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
###Fix typos----
butterfly <- filter(.data = butterfly , sex != "Female")
butterfly <- filter(.data = butterfly , sex != "Maes")

#__________________________________----

## Missing values ---- 
butterfly %>% 
  is.na() %>% 
  sum()

#_________________________________----
#Summarise ----
summary(butterfly)


