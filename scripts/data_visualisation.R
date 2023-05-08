#Load scripts and data----
source("scripts/data_loading2.R")

#________________________________________________________________________-----

#Plot data ----

##Testing temperature with wing length ----
butterfly %>% 
  ggplot(aes(x = jun_mean,
             y = forewing_length))+
  geom_point()+
  geom_smooth(method="lm",
              se=FALSE)
#________________________________________________________________________-----
#Compare groups ----

##Sex/Size----
butterfly %>% 
  group_by(sex) %>% 
  summarise(mean=mean(forewing_length),
            sd=sd(forewing_length))
#SDS are ~ 0.5 so are <1 and is more significant

##S/S Linear model ----

lsmodel2 <- lm(forewing_length ~ sex, data=butterfly)

broom::tidy(lsmodel2)
#tells me that the Males have wings 1.26 mm shorter than Females
###Plot for sex/size----
butterfly %>% 
  ggplot(aes(x=sex, 
             y=forewing_length,
             colour=sex))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()
#sex may have an effect on the data
#_________________________________________________________----
#S/S Confidence intervals----
GGally::ggcoef_model(lsmodel2,
                     show_p_values=FALSE, 
                     conf.level=0.95)

GGally::ggcoef_model(lsmodel2,
                     show_p_values=TRUE, 
                     conf.level=0.99)
#Reject Null that sex has no effect
#S/S emmeans plot ----
means1 <- emmeans::emmeans(lsmodel2, specs = ~ sex)

emmean_plot1 <- means1 %>% 
  as_tibble() %>% 
  ggplot(aes(x=sex, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))+
  labs(x = "Sex",
       y = "Emmean",
       title = "Silver Spotted Skipper Butterfly",
       subtitle = "Effect of sex on forewing length (mm) of butterflies regardless of temperature")+
  theme_classic()
emmean
#S/S performance check----
performance::check_model(lsmodel2)
#IS normally distributed
#Error size isn't terrible
#Outliers aren't having a great effect on data
#________________________________________________________________________-----
#T/S linear model ----
lsmodel3 <- lm(forewing_length ~ jun_mean, data=butterfly)
broom::tidy(lsmodel3)

##T/S Plot----
butterfly %>% 
  ggplot(aes(x=jun_mean, 
             y=forewing_length,
             colour=sex))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

#T/S performance check ----
performance::check_model(lsmodel3)
#Distribution is ok
#Error size is large
#Outliers may affect data

#__________________________________________________________----
#Nice plot----

temp_size_plot1 <- butterfly %>% 
  ggplot(aes(x = jun_mean,
             y = forewing_length))+
  geom_point(colour = "black")+
  geom_smooth(method="lm",
              se=FALSE,
              colour = "darkgrey")+
  labs(x = "Temperature (Celsius)",
       y = "Forewing Length (Millimeters)",
       title = "Silver Spotted Skipper Butterfly",
       subtitle = "Forewing size and temperature to explore the effects of climate change on butterflies" )+
  facet_wrap(~ sex)+
  theme_classic()

temp_size_plot1 #to show a possible relationship between temp and size
#______________________________________________________________________________________________________________-----
#Notes ----
#lsmodel2 is S/S
#lsmodedl3 is T/S
#means1 is S/S
#

#________________________________________________________________________-----
#Callable plots ----
#temp_size_plot1
#emmean_plot1


#________________________________________________________________________-----