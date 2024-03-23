 train <- read.csv("C:/Users/pc/Desktop/analiz ödev/train.csv")
 str(orneklem)

 orneklem <- sample(1:nrow(train),2000)
 veri <- train[orneklem,]
 write.csv(veri,"C:/Users/pc/Desktop/analiz ödev/veri.csv", row.names = FALSE)

class(veri) 
names(veri)
attach(veri)

ozet <- summary(veri)
ozet

grupped_cb_age <- group_by(veri, age)
grupped_cb_age

summarise(grupped_cb_smoking, mean(age))

summarise_at(vars(id:smoking),mean, na.rn = TRUE)

no_smoking <- filter(veri, smoking == 1)
no_smoking

library(shiny)
library(ggiraph) 
library(e1071)
library(dbplyr)


require(ggiraphExtra)
require(ggplot2)
require(ggiraph)
require(plyr)

ggplot(veri,aes(fasting.blood.sugar,Cholesterol)) + geom_point() + geom_smooth()

ggPoints(veri, aes(fasting.blood.sugar, Cholesterol)) +
  geom_point() +
  facet_wrap( ~ smoking)


ggplot(veri, aes(fasting.blood.sugar, Cholesterol)) +
  geom_point() +
  facet_wrap( ~ Urine.protein)

require(ggiraphExtra)
require(ggplot2)
require(ggiraph)  


ggplot(veri) +
  aes(x = as.factor(Urine.protein), y = age) +
  geom_boxplot(fill='#A4A4A4', color="black")  +
  labs(
    title = "Urine Protein yaş dağılım",
    x = "Urine Protein",
    y = "Yaş"
  )
  

ggplot(veri) +
  aes(x = as.factor(smoking), y = relaxation) +
  geom_boxplot(fill='red', color="black")  +
  labs(
    title = "Sigara - Rahatlık ",
    x = "Sigara Durumu",
    y = "Rahaltlık"
  )


ggplot(veri, aes(x = age)) +
  geom_histogram(bins = 10) 


ggplot(data = veri, aes(x = age)) +
  geom_histogram(bins=10,color = "white", fill = "red")
mean(age)
mode(age)
Mod(age)
max(age); min(age); sum(age); mean(age); median(age); range(age); prod(age)



ggplot(veri) +
  aes(x = age) +
  geom_histogram(aes(y = ..density..), bins = 10,color = "black", fill = "red") +
  geom_density(color = "blue", linewidth = 2)

ggplot(veri) +
  aes(x = Cholesterol) +
  geom_histogram(aes(y = ..density..), bins = 10,color = "black", fill = "red") +
  geom_density(color = "blue", linewidth = 2)

max(Cholesterol); min(Cholesterol); sum(Cholesterol); mean(Cholesterol); median(Cholesterol); range(Cholesterol); prod(Cholesterol)

install.packages("e1071")


skewness(age,na.rm = FALSE, type = 3)
kurtosis(age, na.rm = FALSE, type = 3)



skewness(Cholesterol,na.rm = FALSE, type = 3)
kurtosis(Cholesterol, na.rm = FALSE, type = 3)

ggplot(veri, aes(weight.kg., age)) +
  geom_line(linetype = 5,
            size = 4,
            col = "blue")


gg <- ggplot(veri,aes(x=age, y = systolic )) +
  geom_line(linetype = 1) +
  geom_point(shape = 25, size = 3, fill = "red",color ="white")



ggsave("gg.jpg", plot = gg, units = "in", width = 5, height = 4)



