#1

tobacco = c(176,51,0,299,74,2,23,205,6,155)
alcohol = c(88,33,113,51,0,3,46,73,5,74)
differences = tobacco-alcohol

#assumptions
outlier = function(data) {
  lower.bound = quantile(data,0.25)-1.5*IQR(data)
  upper.bound = quantile(data,0.75)+1.5*IQR(data)
  return((data < lower.bound) |(data > upper.bound))
}
#remove outliers
tobacco = tobacco[!outlier(differences)]
alcohol = alcohol[!outlier(differences)]
#skewness index
I = function(data) {
  return((3*(mean(data)-median(data)))/sd(data))
}
I(differences)
print("sample is now normally distributed without skew")

#t test
t.test(x = tobacco, y = alcohol, alternative = "greater", conf.level = 0.95, paired = TRUE)





#2

chocolate = c(17,24,25,25,27,29,29,29,32,34,36,38,41)
non.chocolate = c(10,12,29,29,30,37,38,39,41,41)

#assumptions
#remove outliers 
chocolate = chocolate[!outlier(chocolate)]
non.chocolate = non.chocolate[!outlier(non.chocolate)]
#skewness index
I(chocolate)
I(non.chocolate) 
print("non-chocolate sample is skewed so proceed with caution")

#a
#F test/ variance test
vartest = var.test(x = chocolate, y = non.chocolate, alternative = "two.sided", conf.level = 0.95)
ifelse(vartest$p.value > 0.05,"fail to reject because p-value > sig.level, the variances are not sig. different", "reject because p-value < sig.level, variances are sig. different")

#b
#t test
ttest = t.test(x = chocolate, y = non.chocolate, alternative = "two.sided", conf.level = 0.975)
ifelse(ttest$p.value>0.05, "fail to reject because p-value > sig.level, there is not enough evidence to claim that there is a difference in carb. content of the two candies", "reject because p-value < sig.level, there is enough evidence to claim that there is a difference in carb. content of the two candies")





#3

#assumptions
print("since N>30, population is normally distributed")

#binomial test
binomtest = binom.test(x = 46, n = 384, p = 0.05, alternative = "greater", conf.level = 0.95)
ifelse(binomtest$p.value > 0.05, "fail to reject because p-value > sig.level, there is not enough evidence to claim that the level of autism has increased", "reject because p-value < sig.level, there is enough evidence to claim that the level of autism has increased")





#4

calories = c(100, 125, 150, 160, 185, 125, 155, 145, 160, 100, 150, 140, 135, 120, 110)

#assumptions
#remove outliers
calories = calories[!outlier(calories)]
#skewness index
I(calories)
print("sample is now normally distributed without skew")

#t test
population.sd = sd(calories)/sqrt(length(calories))

t.test.one = function(x, mu, conf.level){
  t = (mean(x)-mu)/sd(x)/sqrt(length(x))
  p.value = pt(-abs(t), df = length(x)-1)
  return(ifelse(p.value>conf.level, "fail to reject because p-value > sig.level", "reject because p-value < sig.level"))
}
t.test.one(calories,110,0.01)
print("there is not enough evidence to support the claim that the average caloric content is greater than 110 calories")




#5

daily.jogger = 34
weekly.jogger = 52
needed.jogger = 23
daily.nonjogger = 18
weekly.nonjogger = 65
needed.nonjogger = 18
total.jogger = sum(34,52,23)
total.nonjogger = sum(18,65,18)
total.daily = sum(34,18)
total.weekly = sum(52,65)
total.needed = sum(23,18)
total = sum(34,52,23,18,65,18)

#assumptions
(total.jogger * total.daily)/total
(total.jogger * total.weekly)/total
(total.jogger * total.needed)/total
(total.nonjogger * total.daily)/total
(total.nonjogger * total.weekly)/total
(total.nonjogger * total.needed)/total
print("sample is normally distrubuted becuase all expected values > 5")

#chi squared test
table = matrix(c(34,52,23,18,65,18), nrow = 2, ncol = 3, byrow = TRUE)
chisqtest = chisq.test(table)
ifelse(chisqtest$p.value>0.05, "fail to reject because p-value > sig.level, not enough evidence to claim that jogging and the consumption of supplements are not related", "reject because p-value < sig.level, enough evidence to claim that jogging and the consumption of supplements are not related")

