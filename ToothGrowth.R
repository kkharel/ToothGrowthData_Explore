library(ggplot2)
library(datasets)
Tooth_Growth <- ToothGrowth
str(Tooth_Growth)
head(ToothGrowth)
unique(Tooth_Growth$supp)
unique(Tooth_Growth$dose)
summary(Tooth_Growth)

#There are two levels of supplement(possibly supplement type) 
# and three levels of dose.

#Plot the data
ggplot(aes(x=dose, y = len), data = Tooth_Growth) + 
  geom_point(aes(color = supp)) 

#Boxplot of dose and len
ggplot(aes(x=dose,y=len),data = Tooth_Growth)+
  geom_boxplot(aes(fill=factor(dose)))

#Boxplot of supplement type and len
ggplot(aes(x=factor(supp),y=len),data = Tooth_Growth)+
  geom_boxplot(aes(fill=factor(supp)))

#Everything together
ggplot(aes(x = supp, y = len), data = Tooth_Growth) +
  geom_boxplot(aes(fill = supp)) + facet_wrap(~ dose)

Lower <- subset(Tooth_Growth, dose %in% c(0.5, 1.0))
Middle <- subset(Tooth_Growth, dose %in% c(0.5, 2.0))
Upper <- subset(Tooth_Growth, dose %in% c(1.0,2.0))
Upper
# Hypothesis:
# H0 = True difference in mean is equal to 0
# H1 = True difference in mean is not equal to 0.
t.test(len~dose, paired = F, var.equal = F, data = Lower)
t.test(len~dose, paired = F, var.equal = F, data = Middle)
t.test(len~dose, paired = F, var.equal = F, data = Upper)
# we reject the null hypothesis. The result shows that p-value
# is really really small < 0.05 which states that there is 
# a statistical difference between two means.
# Hence, increasing the dose level does lead to enhancement
#  of tooth growth

# H0 = True difference in mean is equal to 0
# H1 = True difference in mean is not equal to 0
t.test(len~supp, paired = F, var.equal = F, data = Tooth_Growth)


# On the other hand, we fail to reject null hypothesis. The 
# result shows that p-value is > 0.05 which states that there
# is no statistical difference between two means.
# Hence, supplement type has no effect on tooth growth.
