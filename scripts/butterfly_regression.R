#Set up ----

library(rstatix)
library(performance)
library(patchwork)
library(knitr)
library(kableExtra)

#_____________________________________________________________________________----

#Load scripts and data----

source("scripts/data_loading2.R")
source("scripts/data_visualisation.R")

#_____________________________________________________________________________----
#Pearson's R ----
library(rstatix)

butterfly %>% 
  cor_test(forewing_length, jun_mean)
# 0.28 (round to 0.3 weak positive correlation)

#_____________________________________________________________________________----
#Linear model for regression ----
butterfly_ls1 <- lm(forewing_length ~ jun_mean, data = butterfly) 

##Plot for model ----

reg_temp_plot <- butterfly %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point(colour = "black")+
  geom_smooth(method="lm",
              colour = "darkgrey")+
  labs(x = "Temperature (°C)",
       y = "Forewing Length (mm)")+
  facet_wrap(~ sex)+
  theme_classic()

reg_temp_plot
#_____________________________________________________________________________----
#Confidence intervals ----

confint(butterfly_ls1)
#2.5% - 0.00168
#97.5% - 0.39521
#Tells us that it's not stable and is not a strong relationship
#_____________________________________________________________________________----
#Effect size ----

butterfly_ls1 %>% 
  broom::glance()
#r squared - 0.0773 < 0.1 small effect size weak relationship

#_____________________________________________________________________________----
#Regression plot----

augbutterfly_ls1 <- butterfly_ls1 %>% 
  broom::augment()

resi_plot <- augbutterfly_ls1 %>% 
  ggplot(aes(x=jun_mean, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=jun_mean, 
                 y=forewing_length))+
  geom_segment(aes(x=jun_mean, 
                   xend=jun_mean, 
                   y=.fitted, 
                   yend=forewing_length), 
               linetype="longdash", colour="darkgrey")+
  labs(x = "Temperature (°C)",
       y = "Fitted model value for forewing (mm)")+
  theme_classic()

resi_plot
#_____________________________________________________________________________----
#Check assumptions for model ----

##Distribution QQ----
performance::check_model(butterfly_ls1, check=c("normality","qq"))
#2 peak split distibution

##Equal variance----
performance::check_model(butterfly_ls1, check="homogeneity")
#groups do not have constant/similar variance 

##Outliers----
plot(butterfly_ls1, which=c(4,5))
#many outliers but I think it's because the relationship is weak

#_________________________________________________________________________________________________----
#Predictions for conclusion ----

coef(butterfly_ls1)
#Intercept - 10.7850
#temp- 0.1984

butterfly %>% 
  summarise(min=min(jun_mean, na.rm=TRUE), 
            max=max(jun_mean, na.rm=TRUE))

broom::augment(butterfly_ls1, newdata=tibble(jun_mean=c(11,12,13,14,15,16,17)), interval="confidence")
#11 deg = 13 mm 
#12=13.2 mm
#13=13.4 mm
#14=13.6 mm
#15=13.8 mm
#16=14.0 mm
#17 deg = 14.2 mm

##Prediction table ----

pred_table <- butterfly_ls1 %>% 
  broom::augment(butterfly_ls1, newdata=tibble(jun_mean=c(11,12,13,14,15,16,17)), interval="confidence") %>%
  kbl(caption = "Table 1 - Predictions for the size of Silver Spotted Skipper butterfly forewings in new temperature environments. Indicates that for every 1°C, forewing length increases by 0.2 mm (Confidence intervals = 13.6 - 14.8)",
      col.names = c("Temperature (°C)", "Predicted forewing size (mm)", "Lower Confidence Interval","Upper Confidence Interval")) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

pred_table
#11 deg = 13 mm   ~ 12.33323	13.60268
#17 deg = 14.2 mm ~ 13.53551	14.78179

#_________________________________________________________________________________________________----
#Male regression ----
##Create data----
male_butterflydata <- filter(.data = butterfly,
                             sex == "Males")

##Pearson's R----
male_butterflydata %>% 
  cor_test(forewing_length, jun_mean)
#0.62 - medium-strong positive correlation

##Linear model for regression ----
male_butterfly_ls1 <- lm(forewing_length ~ jun_mean, data = male_butterflydata) 

###Plot ----
male_butterflydata %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point(colour = "black")+
  geom_smooth(method="lm",
              colour = "darkgrey")+
  labs(x = "Temperature (°C)",
       y = "Forewing Length (mm)",
       title = "Male Silver Spotted Skipper Butterfly",
       subtitle = "Male forewing size and temperature to explore the effects of climate change on male butterflies" )+
  theme_classic()

## Confidence intervals----
confint(male_butterfly_ls1)
#2.5% - 0.13466
#97.5% - 0.42391
#Still not stable/strong

##Effect size R squared----
male_butterfly_ls1 %>% 
  broom::glance()
#R squared - 0.388 >3 has a medium effect size

##Male resi plot ----
male_augbutterfly_ls1 <- male_butterfly_ls1 %>% 
  broom::augment()

male_resi_plot <- male_augbutterfly_ls1 %>% 
  ggplot(aes(x=jun_mean, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=jun_mean, 
                 y=forewing_length))+
  geom_segment(aes(x=jun_mean, 
                   xend=jun_mean, 
                   y=.fitted, 
                   yend=forewing_length), 
               linetype="longdash", colour="darkgrey")+
  labs(x = "Temperature (°C)",
       y = "Fitted model value for male forewing (mm)")+
  theme_classic()

male_resi_plot
#Residuals still far from the fitted but better than combined sex

residuals_plot <- resi_plot + male_resi_plot

residuals_plot

##Male prediction table----

male_butterfly_ls1 %>% 
  broom::augment(male_butterfly_ls1, newdata=tibble(jun_mean=c(11,12,13,14,15,16,17)), interval="confidence") 
#11=12.1
#12=12.4
#13=12.7
#14=13.0
#15=13.2
#16=13.5
#17=13.8

male_pred_table <- male_butterfly_ls1 %>% 
  broom::augment(male_butterfly_ls1, newdata=tibble(jun_mean=c(11,12,13,14,15,16,17)), interval="confidence") %>%
  kbl(caption = "Table 2 - Predictions for the size of Male Silver Spotted Skipper butterfly forewings in new temperature environments. Indicates that for every 1°C, forewing length increases by 0.3 mm (Confidence intervals = 12.6 - 14.3)",
      col.names = c("Temperature (°C)", "Predicted forewing size (mm)", "Lower Confidence Interval","Upper Confidence Interval")) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

male_pred_table
#11 deg - 12.1 mm ~ 11.65659	12.59410
#17 deg - 13.9 mm ~ 13.34541	14.25678 
#Very similar confidence intervals - males smaller than average as already known

#_________________________________________________________________________________________________----
