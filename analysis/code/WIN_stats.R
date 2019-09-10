library("tidyverse")
library("knitr")
library("pander")
library("car")
demo<-read_csv('/data/dataDB/WIN/database/specific_variables/jasp_demo.csv')

#converts gender to categorical variable
demo <- demo %>% mutate(Sex = factor(Sex)) 

plot(v_incongruency_effect~base_bmi_measured, data=demo,
     main="Incongruency effect vs BMI",
     xlab='BMI',
     ylab='Incongruency effect')

ggplot(aes(y = v_incongruency_effect, x = base_bmi_measured, col = Sex), data = demo) +
  geom_point(aes(shape = Sex)) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "BMI",
       y = "Incongruency Effect",
       col = "Sex",
       shape = "Sex",
       title = "Relationship between Incongruency effect and BMI given Sex")+
      scale_shape_identity()
bmi_sex <- lm(v_incongruency_effect ~ base_bmi_measured+Sex,
               data = demo)
pander(summary(bmi_sex))


ggplot(aes(y = v_incongruency_effect, x = Age, col = Sex), data = demo) +
  geom_point(aes(shape = Sex)) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Age",
       y = "Incongruency Effect",
       col = "Sex",
       shape = "Sex",
       title = "Relationship between Incongruency effect and Age given Sex")+
  scale_shape_identity()

hist(demo$Age)



