install.packages(gridExtra)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

#These are a stack of 6 plots for some initial statistics.
p1 = ggplot (whr, aes(Log.GDP.per.capita, Life.Ladder, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "Log GDP per Capita", y = "Life Ladder")
p2 = ggplot (whr, aes(gini.of.household.income.reported.in.Gallup..by.wp5.year, Life.Ladder, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "GINI of Household Income", y = "Life Ladder")
p3 = ggplot (whr, aes(Social.support, Life.Ladder, color = Country.Status, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "Social Support", y = "Life Ladder")
p4 = ggplot (whr, aes(Freedom.to.make.life.choices, Life.Ladder, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "Freedom to make life choices", y = "Life Ladder")
p5 = ggplot (whr, aes(Perceptions.of.corruption, Life.Ladder, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "Perceptions of Corruption", y = "Life Ladder")
p6 = ggplot (whr, aes(Confidence.in.national.government, Life.Ladder, na.rm = FALSE)) + geom_smooth(color = "red", se = FALSE) + geom_point(color = 'darkblue', alpha = .5) + labs(x = "Confidence in National Government", y = "Life Ladder")

#A lot of the rows were ommmitted for some graphs since only the 2016 values were present.

#This shows all of the 6 plots.
grid.arrange(p1,p3,p5,p2,p4,p6, ncol = 3)

#whr_nonunique is a table with only those countries mentioned who have data for both 2016 and 2017
whr_nonunique <- whr %>%
  group_by(country) %>%
  filter(n() == 2 ) %>%
  ungroup()

#regression with happiness as the dependent variable and GINI of household income as the independent variable.
ggplot (whr, aes(gini.of.household.income.reported.in.Gallup..by.wp5.year, Life.Ladder)) + geom_point() + geom_smooth(method='lm', se = FALSE) +facet_wrap(~year) + labs(x = "Gini of Household Income", y = "Life Ladder")

#residual plot for regressing happiness and GINI for household income.
data2016 = filter(whr, year == 2016)
data2017 = filter(whr, year == 2017)
GINI.lm2016 = lm(gini.of.household.income.reported.in.Gallup..by.wp5.year ~ Life.Ladder, data2016)
GINI.lm2017 = lm(gini.of.household.income.reported.in.Gallup..by.wp5.year ~ Life.Ladder, data2017)
GINI.lmf2016 = fortify(GINI.lm2016)
GINI.lmf2017 = fortify(GINI.lm2017)
Gini2016.res = ggplot(GINI.lmf2016, aes(.fitted, .resid)) + geom_point(color = "blue") + geom_abline(intercept = 0, slope =0, size = 1) + labs(x = "GINI of household income 2016", y = "Residuals 2016")
Gini2017.res = ggplot(GINI.lmf2017, aes(.fitted, .resid)) + geom_point(color = "blue") + geom_abline(intercept = 0, slope = 0, size = 1) + labs(x = "GINI of household income 2017", y = "Residuals 2017")
grid.arrange(Gini2016.res, Gini2017.res, ncol = 2)

#correlation between happiness and gini
cor(whr$gini.of.household.income.reported.in.Gallup..by.wp5.year,whr$Life.Ladder)

#whr_nonunique is a table with only those countries mentioned who have data for both 2016 and 2017
whr_nonunique <- whr %>%
  group_by(country) %>%
  filter(n() == 2 ) %>%
  ungroup()

#Creating the boxplot for countries
select(whr_unique)
ggplot(whr_nonunique, aes(x = as.character(year), y = Life.Ladder))+ geom_boxplot() + labs(x = "Years", y = "Life Ladder")

#Creating column differentials
happiness_diff = select(filter(whr_nonunique, year == 2017), Life.Ladder) - select(filter(whr_nonunique, year == 2016), Life.Ladder)

#Data frame with every variables differential value.
whr_differential = data.frame(happiness_diff)

#Creating a histogram for the plot
happiness_16 = select(filter(whr_nonunique, year == 2016), Life.Ladder)
happiness_17 = select(filter(whr_nonunique, year == 2017), Life.Ladder)
happiness_dataframe = data.frame(happiness_16, happiness_17)
ggplot(whr_differential, aes(Life.Ladder, ..density..)) + geom_histogram(bins = 20, fill = "pink", color = "blue") + labs(x = "Change in Life Ladder values", y = "Frequency Density")


#Running the hypothesis test on Life Ladder values.
count(happiness_16)
count(happiness_17)
mean2016 = mean(happiness_16$Life.Ladder)
mean2017 = mean(happiness_17$Life.Ladder)
sd(happiness_16$Life.Ladder)
sd(happiness_17$Life.Ladder)
standard_error2016 = (sd(happiness_16$Life.Ladder)) / (sqrt(length(happiness_16$Life.Ladder)))
standard_error2017 = (sd(happiness_17$Life.Ladder)) / (sqrt(length(happiness_17$Life.Ladder)))
se_diff = sqrt((standard_error2016 ** 2) + (standard_error2017 ** 2))
z = (0 - (mean2017 - mean2016)) / se_diff
pnorm(z)

