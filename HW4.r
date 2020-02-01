#1.4 Predicting Flu Epidemics
setwd(dir = '/Users/ChrisMacAir/Documents/Chicago Booth/2019Q2/Regressions/WK4/HW4')
flu = read.csv(file = "flu.csv")
#i Boxplot winter months to check for greater mortality
#Winter <- subset(FluData, Week > 47 | Week < 9)
#NotWinter <- subset(FluData, Week < 48 & Week > 8)
#WinterFlu <- log(lm Winter ~ NotWinter)

is_winter = (flu$Week < 9 | flu$Week > 47)
flu = cbind(flu, is_winter)
# winter = subset(flu, Week < 9 | Week > 47)
# not_winter = subset(flu, Week > 8 & Week < 48 )
boxplot(
  flu$All.Deaths ~ flu$is_winter,
  xlab = "Winter",
  ylab = "Deaths",
  main = "Seasons vs. Deaths",
  pch = 1,
)

#ii Relationship between All Deaths and Flu/Pneumonia
winter_model = lm(formula = flu$All.Deaths ~ flu$is_winter)
abline(winter_model)
ordered_flu = flu[order(is_winter),]
plot(
  ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza ~ ordered_flu$Week,
  xlab = "Week of the Year",
  ylab = "% of Death to Flu & Pnemonia",
  pch = 1, 
)
## The data doesn't follow a linear relationship. Death rates are parabolic,
## much higher in winter months, reaching a nadir in late summer, then rising again.

#iii 
PandI <- lm(ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza ~ ordered_flu$Week)
abline(PandI)
r <- rstudent(PandI)
plot(ordered_flu$Week, r, pch=20, col=4); abline(h=0, lty=2)
qqnorm(r, col=3, pch=20)
abline(a=0, b=1)

#iv Fit a quadratic polynomial to the data
PandI <- lm(ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza ~ ordered_flu$Week)
#Deaths2 <- ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza^2
#PandI2 <- lm(ordered_flu$Week ~ ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza +
#               Deaths2)
#summary(PandI2)
DeathsAlt <- ordered_flu$Week^2
PandI2Alt <- lm(ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza ~ 
                  ordered_flu$Week + DeathsAlt)
summary(PandI2Alt)
par(mfrow=c(1,1)); plot(ordered_flu$Week, 
    ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza, 
    pch=20, col=4)
lines(ordered_flu$Week,fitted(PandI2Alt))
plot(ordered_flu$Week, rstudent(PandI2Alt), pch=20, col=4); abline(h=0, lty=2)
