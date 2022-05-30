# 1
noResponden <- c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100, 95,70,90,90,90,89,90,100)
tabel <- data.frame(noResponden, X, Y)

# a. standar deviasi dari selisih pasangan tabel pengamatan
selisih <- tabel$Y - tabel$X
sd_selisih <- sd(selisih) #sd = standar deviasi
sd_selisih

# b. t (p-value)
t.test(x=tabel$X, y=tabel$Y,
       alternative="greater",
       mu=0.5, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

# c.
# Telah didapatkan hasil bahwa p-value < 0.05 (alpha) dan meannya != 0
# sehingga H0 : "tidak ada pengaruh signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A" dapat ditolak
# maka kesimpulannya : terdapat pengaruh signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A

# 2
install.packages("BSDA")
library(BSDA)

zsum.test(mean.x = 23500, sigma.x = 3900, n.x = 100, alternative = "less", mu=20000)


# 3
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y = 2.79, s.y = 1.32, n.y = 27, 
          alternative = "two.sided", var.equal = TRUE, conf.level = 0.95)

# 4
# a.

dataoneway <- read.table("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("Orange", "Hitam", "Putih"))

class(dataoneway$Group)

Group1 <- subset(dataoneway, Group == "Orange")
Group2 <- subset(dataoneway, Group == "Hitam")
Group3 <- subset(dataoneway, Group == "Putih")

qqnorm(Group1$Length)
qqline(Group1$Length)

# b. nilai p
bartlett.test(Length ~ Group, data = dataoneway)

# c.
model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)

# e.
model1 <- lm(Length~Group, data=myFile)

anova(model1)

TukeyHSD(aov(model1))

# f.
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
        scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")


# 5
# a.
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

my.data <- read_csv("GTL.csv")

qplot(x = Temp, y = Light, geom = "point", data = my.data) + facet_grid(.~Glass, labeller = label_both)

# b. uji ANOVA 2 arah
my.data$Glass <- as.factor(my.data$Glass)
my.data$Temp_Factor <- as.factor(my.data$Temp)

anova <- aov(Light ~ Glass*Temp_Factor, data = my.data)
summary(anova)

# c.
data.sum <- group_by(my.data, Glass, Temp) %>%
  summarise(mean = mean(Light), sd = sd(Light)) %>%
  arrange(desc(mean))

print(data.sum)

# d.
tukey <- TukeyHSD(anova)
print(tukey)

# e.
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data.sum$Tukey <- cld$Letters
print(data.sum)