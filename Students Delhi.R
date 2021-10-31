library(dplyr)
library(ggplot2)
library(scales)
library(tidyr )
library(plotly)


data_stress_delhi <- read.csv("C:/Users/HP/Desktop/desktop/Pooja Reaearch Methods/COVID-19 Survey Student Responses Delhi.csv" )

summary(data_stress_delhi)
#data_stress_delhi$Health.issue.during.lockdown[NO] <- 0

names(data_stress_delhi)
dim(data_stress_delhi)


g<- ggplot(data_stress_delhi, 
       aes(x =data_stress_delhi$Change.in.your.weight, 
           y = data_stress_delhi$Time.spent.on.Online.Class
             )) +
  geom_boxplot() +labs(title = "Change in Weight Based on Time Spent on Online Classes",
                       subtitle = "Data of Delhi Students during Lockdown ",
                       x = "Change in Weight",
                       y = "Time spent on online classes")


ggplotly(g)


g2<- ggplot(data_stress_delhi, 
           aes(x =data_stress_delhi$Change.in.your.weight, 
               y = data_stress_delhi$Time.spent.on.self.study
           )) +
  geom_boxplot()+labs(title = "Change in Weight based on Time Spent on Self Study",
                      subtitle = "Data of Delhi Students during Lockdown ",
                      x = "Change in Weight",
                      y = "Time Spent on Self Study")

ggplotly(g2)



g3<- ggplot(data_stress_delhi, 
            aes(x =data_stress_delhi$Change.in.your.weight, 
                y = data_stress_delhi$Time.spent.on.fitness
            )) +
  geom_boxplot()+labs(title = "Change in Weight based on Time Spent on Fitness",
                       subtitle = "Data of Delhi Students during Lockdown ",
                       x = "Change in Weight",
                       y = "Time Spent on Fitness")

ggplotly(g3)



g4<- ggplot(data_stress_delhi, 
            aes(x =data_stress_delhi$Change.in.your.weight, 
                y = data_stress_delhi$Time.spent.on.sleep
            )) +
  geom_boxplot()+labs(title = "Change in Weight based on Time Spent on Sleep",
                      subtitle = "Data of Delhi Students during Lockdown ",
                      x = "Change in Weight",
                      y = "Time Spent on Sleep") 

ggplotly(g4)



g5<- ggplot(data_stress_delhi, 
            aes(x =data_stress_delhi$Change.in.your.weight, 
                y = data_stress_delhi$Time.spent.on.social.media
            )) +
  geom_boxplot()+labs(title = "Change in Weight based on Time Spent on Social Media",
                      subtitle = "Data of Delhi Students during Lockdown ",
                      x = "Change in Weight",
                      y = "Time Spent on Social Media")

ggplotly(g5)


g6<- ggplot(data_stress_delhi, 
            aes(x =data_stress_delhi$Change.in.your.weight, 
                y = data_stress_delhi$Time.spent.on.TV
            )) +
  geom_boxplot()+labs(title = "Change in Weight based on Time Spent on TV",
                      subtitle = "Data of Delhi Students during Lockdown ",
                      x = "Change in Weight",
                      y = "Time Spent on TV") 

ggplotly(g6)


g7<- ggplot(data_stress_delhi, 
            aes(x =data_stress_delhi$Change.in.your.weight, 
                y = data_stress_delhi$Number.of.meals.per.day
            )) +
  geom_boxplot()+labs(title = "Change in Weight based on No.of Meals per Day",
                      subtitle = "Data of Delhi Students during Lockdown ",
                      x = "Change in Weight",
                      y = "No.of Meals per Day") 

ggplotly(g7)




anova1way_1<-aov(data_stress_delhi$Number.of.meals.per.day~as.factor(data_stress_delhi$Change.in.your.weight),data=data_stress_delhi)
summary(anova1way_1)

pairwise.t.test(data_stress_delhi$Number.of.meals.per.day, data_stress_delhi$Change.in.your.weight, p.adj = "bonferroni")

tukey.diet_1<-TukeyHSD(anova1way_1)
tukey.diet_1
par(mar = c(3, 12, 3, 4)) 
plot(tukey.diet_1, las = 2)

anova1way_1_online<-aov(data_stress_delhi$Time.spent.on.Online.Class~as.factor(data_stress_delhi$Change.in.your.weight),data=data_stress_delhi)
summary(anova1way_1_online)

pairwise.t.test(data_stress_delhi$Time.spent.on.Online.Class, data_stress_delhi$Change.in.your.weight, p.adj = "bonferroni")

tukey.diet_1_online<-TukeyHSD(anova1way_1_online)
tukey.diet_1_online
par(mar = c(3, 12, 3, 4)) 
plot(tukey.diet_1_online,las = 2)

anova1way_1_fitness<-aov(data_stress_delhi$Time.spent.on.fitness~as.factor(data_stress_delhi$Change.in.your.weight),data=data_stress_delhi)
summary(anova1way_1_fitness)


pairwise.t.test(data_stress_delhi$Time.spent.on.fitness, data_stress_delhi$Change.in.your.weight, p.adj = "bonferroni")


tukey.diet_1_fitness<-TukeyHSD(anova1way_1_fitness)
tukey.diet_1_fitness
#plot(tukey.diet_1_fitness,las = 2,cex = 1)
#plot(tukey.diet, las = 2,cex = 1)

par(mar = c(3, 12, 3, 4)) 
plot(tukey.diet_1_fitness,las = 2)
#################################################################################################################################################







#library(MASS)

#
#data_stress_delhi <- read.csv("C:/Users/HP/Desktop/desktop/Pooja Reaearch Methods/COVID-19 Survey Student Responses Delhi.csv")


#lda.default <- lda(data_stress_delhi$Health.issue.during.lockdown ~ data_stress_delhi$Region.of.residence+data_stress_delhi$Age.of.Subject+data_stress_delhi$Time.spent.on.Online.Class+
 #                    data_stress_delhi$Rating.of.Online.Class.experience+data_stress_delhi$Medium.for.online.class+data_stress_delhi$Time.spent.on.self.study+data_stress_delhi$Time.spent.on.fitness+
#                     data_stress_delhi$Time.spent.on.sleep+data_stress_delhi$Time.spent.on.social.media+data_stress_delhi$Prefered.social.media.platform+data_stress_delhi$Time.spent.on.TV+
#                     data_stress_delhi$Number.of.meals.per.day+data_stress_delhi$Change.in.your.weight+data_stress_delhi$Stress.busters+
 #                    data_stress_delhi$Time.utilized+data_stress_delhi$Do.you.find.yourself.more.connected.with.your.family..close.friends...relatives...+data_stress_delhi$What.you.miss.the.most)
#lda.default
