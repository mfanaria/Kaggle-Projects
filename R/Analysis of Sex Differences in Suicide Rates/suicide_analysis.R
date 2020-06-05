# Import data
# setwd(...)
suicide_data = read.csv('master.csv')

###############################################################################

# Import packages
library(tidyverse)
library(dplyr)
library(gridExtra)
library(data.table)
library(car)

###############################################################################

# Data cleaning

# overview of data
head(suicide_data)

# rename some variables
colnames(suicide_data)[1] = 'country'
colnames(suicide_data)[10] = 'gdp_for_year'
colnames(suicide_data)[11] = 'gdp_per_capita'

# age groups need to be ordered
suicide_data$age <- factor(suicide_data$age, ordered = T,
                           levels = c('5-14 years',
                                      '15-24 years', 
                                      '25-34 years', 
                                      '35-54 years', 
                                      '55-74 years', 
                                      '75+ years'))

# dimensions of data
# dim(suicide_data)
# 27820 rows and 12 columns
# the names of the columns
# names(suicide_data)

# are any data missing?
sum(is.na(suicide_data)) # 19456 values
# what variables are missing data?
for(i in 1:12) {
  cat('Variable', i, 'is missing', sum(is.na(suicide_data[i])), 'values\n')
}
# variable 9 is the only variable with missing data in 70% of the rows
# therefore it is removed
suicide_data = suicide_data[-9]
#head(suicide_data)

# is there enough data available for each year
years <- suicide_data %>% # using suicide_data
  group_by(year) %>% # group all the data from the same year
  summarise(rows = n()) # collate the number of rows for each year
# drop in rows from 744 to 160 from 2015 to 2016

plot_years_theme <- theme(axis.title = element_text(size = 22.5), 
                          axis.text = element_text(size = 20))
plot_years <- ggplot(data = years, 
                     aes(x = year, y = rows, 
                         fill = factor(ifelse(year == '2016',
                                              'Highlighted',
                                              'Normal')))) +
  # fill ifelse separates 2016 and allows it to be coloured red
  geom_bar(stat = 'identity', show.legend = F) + # create bar chart,
  # remove legend
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Rows of Data') + # change y axis label
  plot_years_theme + # change the size of axis titles and axis text
  scale_x_continuous(breaks = c(seq(1985, 2016, 2)),
                     labels = c(seq(1985, 2016, 2)),
                     limits = c(1984, 2017)) +
  scale_y_continuous(breaks = c(seq(0, 1250, 250)),
                     labels = c(seq(0, 1250, 250)),
                     limits = c(0, 1250)) +
  # change x and y axis values
  scale_fill_manual(name = 'year', values = c('red', 'black'))
  # fill every bar black, except in 2016 which is red
plot_years

# therefore remove 2016
suicide_data <- subset(suicide_data, subset = year < 2016)

# additionally going to remove countries with less than 10 years data
minimum_years <- suicide_data %>% # using suicide_data
  group_by(country) %>% # group all the data from the same country
  summarise(rows = n(), years = rows / 12) %>% # collate number of rows 
  # for each country, with second column denoting the number of years
  # worth of data for each country
  arrange(years) # arrange the table by years (ascending)

suicide_data <- suicide_data %>% # using suicide_data
  filter(!(country %in% head(minimum_years$country, 10))) # remove the 
  # countries that are in the first 10 rows of the minimum_years table
# to check:
# minimum_years10 <- data %>%
#   group_by(country) %>%
#   summarise(rows = n(), 
#             years = rows / 12) %>%
#   arrange(years)

###############################################################################

# EDA

# look at how global suicides per 100k population have changed
# calculate the global mean
global_mean = (sum(as.numeric(suicide_data$suicides_no)) / 
                 sum(as.numeric(suicide_data$population))) * 100000
suicides_global = suicide_data %>% # using suicide_data
  group_by(year) %>% # group all the data from the same year
  summarise(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
  # create rows with the sum of the global population, sum of suicides,
  # and the suicides per 100k population, for each year

plot_global_theme <- theme(axis.title = element_text(size = 22.5), 
                           axis.text = element_text(size = 20))
plot_global <- ggplot(data = suicides_global, 
                      aes(x = year, y = suicides_per_100k)) +
  geom_point(lwd = 2) +
  # add data as points
  geom_line(lwd = 1) +
  # join points together
  geom_hline(yintercept = global_mean, linetype = 2) +
  # add horizontal line denoting the global mean for the period
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Mean Global Suicides per 100k Population') + 
  # change y axis label
  plot_global_theme + # change the size of axis titles and axis text
  scale_x_continuous(breaks = c(seq(1985, 2015, 2)),
                     labels = c(seq(1985, 2015, 2)),
                     limits = c(1984, 2016)) +
  scale_y_continuous(breaks = c(seq(11, 16, 1)),
                     labels = c(seq(11, 16, 1)),
                     limits = c(11, 16))
  # change x and y axis values
plot_global

#######################################

# Sex differences
# differences between the sexes for global suicides
suicides_sex = suicide_data %>% # using suicide_data
  group_by(year, sex) %>% # group all the data from the same year and sex
  summarise(population = sum(as.numeric(population)), 
            suicides = sum(as.numeric(suicides_no)))
# create rows with the sum of the global population and the sum of 
# global suicides

plot_sex_theme <- theme(axis.title = element_text(size = 22.5), 
                        axis.text = element_text(size = 20),
                        legend.position = 'none')
options(scipen = 250000) # remove scientific notation
plot_sex <- ggplot(data = suicides_sex, aes(x = sex, y = suicides,
                                            fill = sex)) +
  geom_boxplot() + # create bar chart
  theme_classic() + # white background, no gridlines
  xlab('Sex') + # change x axis label
  ylab('Total Suicides') + # change y axis label
  plot_sex_theme + # change the size of axis titles, axis text
  # and remove legend
  scale_x_discrete(labels = c('Female', 'Male')) +
  scale_y_continuous(breaks = c(seq(25000, 225000, 25000)),
                     labels = c(seq(25000, 225000, 25000)),
                     limits = c(25000, 225000)) +
  # change x and y values
  scale_fill_manual(values = c('tomato2', '#44749D'))
  # change fill colour for bars, female then male

# differences between the sexes over time
suicides_sex_years = suicide_data %>% # using suicide_data
  group_by(year, sex) %>% # group all the data from the same year and sex
  summarise(population = sum(as.numeric(population)), 
            suicides = sum(as.numeric(suicides_no)), 
            suicides_per_100k = (suicides / population) * 100000)
# create rows with the sum of the global population, sum of suicides,
# and the suicides per 100k population, for each sex

plot_sex_years_theme <- theme(axis.title = element_text(size = 22.5), 
                        axis.text = element_text(size = 20),
                        legend.position = 'none',
                        strip.background = element_blank(),
                        strip.text = element_blank())
plot_sex_years <- ggplot(data = suicides_sex_years, 
                         aes(x = year, y = suicides_per_100k, 
                             col = sex)) +
  facet_grid(sex ~ ., scales = 'free_y') +
  # create a grid of two graphs split by sex, with the y axis set to
  # free, in which the scale is automatically calculated
  geom_point(lwd = 2) +
  # add data as points
  geom_line(lwd = 1) +
  # join points together
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Mean Global Suicides per 100k Population') + # change y axis label
  plot_sex_years_theme + # change the size of axis titles, axis text,
  # remove legend and facet grid labels
  scale_x_continuous(breaks = c(seq(1985, 2015, 5)),
                     labels = c(seq(1985, 2015, 5)),
                     limits = c(1984, 2016)) +
  # change x values
  scale_colour_manual(values = c('tomato2', '#44749D'))
# change fill colour for lines and points

# arrange both sex plots into one graph
grid.arrange(plot_sex, plot_sex_years, ncol = 2)


#######################################

# differences between the sexes in different age groups
suicides_sex_ages = suicide_data %>% # using suicide_data
  group_by(year, sex, age) %>% # group all the data from the same year, 
  # sex and age group
  summarise(population = sum(as.numeric(population)), 
            suicides = sum(as.numeric(suicides_no)), 
            suicides_per_100k = (suicides / population) * 100000)
# create rows with the sum of the global population, sum of suicides,
# and the suicides per 100k population, for each sex

# create subsets of male and female data
suicide_male_ages <- suicides_sex_ages[which(suicides_sex_ages$sex == 'male'),]
suicide_female_ages <- suicides_sex_ages[which(suicides_sex_ages$sex == 'female'),]

# female plots
plot_female_ages_theme <- theme(axis.title = element_text(size = 22.5), 
                                axis.text = element_text(size = 20),
                                legend.position = 'none',
                                strip.text.y = element_text(size = 15))
# panel.spacing.y = unit(4.5, "mm") added on kaggle to remove overlap
# of lefthand y-axis labels
plot_female_ages <- ggplot(data = suicide_female_ages, 
                           aes(x = year, y = suicides_per_100k, 
                               col = sex)) +
  facet_grid(age ~ ., scales = 'free_y') +
  # create a grid of two graphs split by sex, with the y axis set to
  # free, in which the scale is automatically calculated
  geom_point(lwd = 2) +
  # add data as points
  geom_line(lwd = 1) +
  # join points together
  xlab('Year') + # change x axis label
  ylab('Mean Global Female Suicides per 100k Population') + 
  # change y axis label
  plot_female_ages_theme + # change the size of axis titles, axis text
  # and remove legend 
  scale_x_continuous(breaks = c(seq(1985, 2015, 5)),
                     labels = c(seq(1985, 2015, 5)),
                     limits = c(1984, 2016)) +  
  # change x values
  scale_colour_manual(values = c('tomato2'))
  # change colour

# male plots 
plot_male_ages_theme <- theme(axis.title = element_text(size = 22.5), 
                              axis.text = element_text(size = 20),
                              legend.position = 'none',
                              strip.text.y = element_text(size = 15))
# panel.spacing.y = unit(4.5, "mm") added on kaggle to remove overlap
# of lefthand y-axis labels
plot_male_ages <- ggplot(data = suicide_male_ages, 
                         aes(x = year, y = suicides_per_100k, 
                             col = sex)) +
  facet_grid(age ~ ., scales = 'free_y') +
  # create a grid of two graphs split by sex, with the y axis set to
  # free, in which the scale is automatically calculated
  geom_point(aes(x = year, y = suicides_per_100k), lwd = 2) +
  # add data as points
  geom_line(aes(x = year, y = suicides_per_100k), lwd = 1) +
  # join points together
  xlab('Year') + # change x axis label
  ylab('Mean Global Male Suicides per 100k Population') + 
  # change y axis label
  plot_male_ages_theme + # change the size of axis titles, axis text
  # and remove legend
  scale_x_continuous(breaks = c(seq(1985, 2015, 5)),
                     labels = c(seq(1985, 2015, 5)),
                     limits = c(1984, 2016)) +
  # change x values 
  scale_colour_manual(values = c('#44749D'))
  # change colour

# arrange both sex plots into one graph
grid.arrange(plot_female_ages, plot_male_ages, ncol = 2)

###############################################################################

# Statistical analysis

# does the number of global female suicides differ from the number of 
# global male suicides across the time period?
# first, check normality assumptions:
with(suicides_sex, shapiro.test(suicides[sex == 'female'])) # p = 0.000001
with(suicides_sex, shapiro.test(suicides[sex == 'male'])) # p = 0.000029
# as both p-values are less than 0.05, the distributions are significantly
# different from normal

# second, check variance assumptions:
var.test(suicides ~ sex, data = suicides_sex)
# p-value less than 0.05, there is a significant difference in variance

# so the non-parametric version must be used:
wilcox_sex_diff = wilcox.test(suicides ~ sex, data = suicides_sex)
wilcox_sex_diff$p.value # 0.000000000000000004297117
# the p-value is less than 0.05, therefore the median number of global suicides
# for females is significantly different than the median number of global 
# suicides for males in the period

#######################################

# do the number of suicides in each age group differ from each other in
# females?
aov_females_age = aov(suicides ~ age, data = suicide_female_ages)
# test assumptions of one-way analysis of variance (ANOVA):
leveneTest(suicides ~ age, data = suicide_female_ages)
# p-value less than 0.05, therefore variance across groups is statistically
# significant, violating the homogeneity of variance assumptions
aov_females_age_residuals = residuals(object = aov_females_age)
shapiro.test(aov_females_age_residuals)
# p-value less than 0.05 therefore the distribution of the residuals is 
# significantly different from normal, violating the normality of the 
# distribution of the residuals assumption

# must therefore use the non-parametric Kruskal-Wallis test:
kw_females_age = kruskal.test(suicides ~ age, data = suicide_female_ages)
kw_females_age$p.value # 0.0000000000000000000000000000000004308701
# p-value less than 0.05, therefore there are significant differences between
# age groups

# to find out which age groups are different, a multiple pairwise-comparison
# is used:
pw_females_age = pairwise.wilcox.test(suicide_female_ages$suicides,
                                      suicide_female_ages$age,
                                      p.adjust.method = 'BH')
pw_females_age
# because all p-values are less than 0.05, all age groups are
# significantly different from each other

# do the number of suicides in each age group differ from each other in
# males?
aov_males_age = aov(suicides ~ age, data = suicide_male_ages)
# test assumptions of one-way ANOVA:
leveneTest(suicides ~ age, data = suicide_male_ages)
# p-value less than 0.05, therefore variance across groups is statistically
# significant, violating the homogeneity of variance assumptions
aov_males_age_residuals = residuals(object = aov_males_age)
shapiro.test(aov_males_age_residuals)
# p-value less than 0.05 therefore the distribution of the residuals is 
# significantly different from normal, violating the normality of the 
# distribution of the residuals assumption

# use the non-parametric Kruskal-Wallis test:
kw_males_age = kruskal.test(suicides ~ age, data = suicide_male_ages)
kw_males_age$p.value # 0.000000000000000000000000000000002525514
# p-value less than 0.05, therefore there are significant differences between
# age groups

# use the multiple pairwise-comparison:
pw_males_age = pairwise.wilcox.test(suicide_male_ages$suicides,
                                    suicide_male_ages$age,
                                    p.adjust.method = 'BH')
pw_males_age
# again, because all p-values are less than 0.05, all age groups are
# significantly different from each other

# both of these conclusions are supported by the line plots

#######################################

# does the number of suicides differ with age and/or sex?
aov_sex_age = aov(suicides ~ sex + age, data = suicides_sex_ages)
summary(aov_sex_age)
# both sex and age are statistically significant, with sex being more
# significant (higher F value)
# test with interaction effect:
aov_sex_age_interaction = aov(suicides ~ sex * age, data = suicides_sex_ages)
summary(aov_sex_age_interaction)
# the interaction between sex and age is also significant, therefore this
# model will be preferred

# test assumptions of two-way ANOVA:
plot(aov_sex_age_interaction, 1)
leveneTest(suicides ~ sex * age, data = suicides_sex_ages)
# p-value less than 0.05, therefore variance across groups is statistically
# significant, violating the homogeneity of variance assumptions
plot(aov_sex_age_interaction, 2)
aov_sex_age_interaction_residuals = residuals(object = aov_sex_age_interaction)
shapiro.test(aov_sex_age_interaction_residuals)
# p-value less than 0.05 therefore the distribution is significantly different
# from normal, violating the normality of the distribution of the residuals
# assumption

# there is not an easy way to do a non-parametric version of two-way ANOVA,
# so i am going to remove the identified outliers (10, 22 and 46)
suicides_sex_ages_outliers = suicides_sex_ages[-c(10, 22, 46),]
aov_sex_age_interaction_outliers = aov(suicides ~ sex * age,
                                       data = suicides_sex_ages_outliers)
plot(aov_sex_age_interaction_outliers, 1)
leveneTest(suicides ~ sex * age, data = suicides_sex_ages_outliers)
plot(aov_sex_age_interaction_outliers, 2)
aov_sex_age_interaction_outliers_residuals = 
  residuals(object = aov_sex_age_interaction_outliers)
shapiro.test(aov_sex_age_interaction_outliers_residuals)
# still fails, so this should be kept in mind when discussing conclusions

# Tukey HSD can be used to perform multiple pairwise-comparison to assess
# if the mean difference between pairs of groups is statistically significant
# which = 'age' is used because sex only has two levels, which have already 
# been seen to be statistically significant
TukeyHSD(aov_sex_age_interaction_outliers, which = 'age')
# all adjusted p-values are less than 0.05, therefore all pairwise comparisons
# are significant

#######################################

# does the size of the population affect the number of suicides?
# gender difference
sex_population = suicide_data %>% # using suicide_data
  group_by(country, sex) %>% # group all the data from the same country
  # and sex
  summarise(suicides = mean(sum(as.numeric(suicides_no))),
            population = mean(population))
# create rows with the mean of the number of suicides and the mean of
# the population, for each sex in each country across the time period
# change some country names

# rename some countries
levels(sex_population$country) <- gsub('^Russian Federation$', 'Russia', 
                                     levels(sex_population$country))
levels(sex_population$country) <- gsub('^United States$', 'USA', 
                                       levels(sex_population$country))
levels(sex_population$country) <- gsub('^Republic of Korea$', 'South Korea', 
                                       levels(sex_population$country))

# females
# create a subset of sex_population with female data
female_population <- sex_population[which(sex_population$sex == 'female'),]
# change country to a character
female_population$country <- as.character(female_population$country)
female_pop_lm = lm(log(suicides) ~ log(population), 
                   data = female_population)
summary(female_pop_lm)
# population is highly significant with a p-value less than 0.00001
# there is a 1.25% increase in the mean number of female suicides for every
# 1% increase in population

plot_female_population_theme <- theme(axis.title = element_text(size = 22.5), 
                                      axis.text = element_text(size = 20))

# create a vector with the countries below the regression line
# to label on the plot
highlighted_countries_below_f = c('Antigua and Barbuda', 'Grenada',  
                                  'Jamaica', 'USA', 'Brazil',
                                  'Azerbaijan', 'Maldives', 'Mexico',
                                  'South Africa', 'Philippines', 
                                  'Guatemala', 'Bahamas', 'Qatar')
# and above the line
highlighted_countries_above_f = c('Iceland', 'Japan', 'Russia',
                                  'Luxembourg', 'Suriname', 'Estonia',
                                  'Mauritius', 'Guyana', 'Slovenia',
                                  'Trinidad and Tobago', 'Latvia',
                                  'Lithuania', 'Singapore', 'Finland',
                                  'Belgium', 'Hungary', 'Austria',
                                  'South Korea', 'France')

plot_female_population <- ggplot(data = female_population, 
                          aes(x = population, y = suicides,
                              label = country)) +
  geom_point(lwd = 2, col = 'tomato2') +
  # add data as points
  geom_smooth(method = 'lm', col = 'black') + 
  # add regression line
  theme_classic() + # white background, no gridlines
  xlab('Mean Female Population') + # change x axis label
  ylab('Mean Number of Female Suicides') + # change y axis label
  plot_female_population_theme + # change the size of axis titles and axis text
  annotate('text', x = 25000, y = 1000000, label = 'R-squared = 0.85', size = 7.5) +
  annotate('text', x = 25000, y = 500000, label = 'p < 0.00001', size = 7.5) +
  # add text denoting the R-squared and p-value
  scale_x_continuous(trans = 'log10',
                     breaks = c(10000, 100000, 1000000, 10000000),
                     labels = c('10,000', '100,000', '1,000,000', '10,000,000')) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000',
                                '1,000,000'),
                     limits = c(1, 1000000)) +
  # log transform both axis, but use labels to show the actual values 
  # increasing by a power of 10
  geom_text_repel(data = female_population[which(female_population$country %in% 
                                                   highlighted_countries_below_f),],
                  direction = 'y',
                  nudge_y = -0.25,
                  size = 5) +
  geom_text_repel(data = female_population[which(female_population$country %in% 
                                                   highlighted_countries_above_f),],
                  direction = 'y',
                  nudge_y = 0.25,
                  force = 6,
                  size = 5)
  # add country labels
                   
plot_female_population
# any country above the regression line has a relatively higher increase in
# mean female suicides i.e. the increase in suicides is greater than 1.25% for
# a 1% increase in population, and vice versa

# males
# create a subset of sex_population with male data
male_population <- sex_population[which(sex_population$sex == 'male'),]
# change country to a character
male_population$country <- as.character(male_population$country)
# perform linear regression
male_pop_lm = lm(log(suicides) ~ log(population), 
                   data = male_population)
summary(male_pop_lm)
# population is highly significant with a p-value less than 0.00001
# there is a 1.18% increase in the mean number of male suicides for every
# 1% increase in population

plot_male_population_theme <- theme(axis.title = element_text(size = 22.5), 
                                    axis.text = element_text(size = 20))

# create a vector with the countries below the regression line
# to label on the plot
highlighted_countries_below_m = c('Antigua and Barbuda', 'Grenada',  
                                  'Jamaica', 'USA', 'Brazil',
                                  'Azerbaijan', 'Maldives', 'Mexico',
                                  'South Africa', 'Philippines', 
                                  'Guatemala', 'Bahamas', 'Qatar')
# and above the line
highlighted_countries_above_m = c('Iceland', 'Japan', 'Russia',
                                  'Luxembourg', 'Suriname', 'Estonia',
                                  'Mauritius', 'Guyana', 'Slovenia',
                                  'Trinidad and Tobago', 'Latvia',
                                  'Lithuania', 'Singapore', 'Finland',
                                  'Belgium', 'Hungary', 'Austria',
                                  'South Korea', 'France')

plot_male_population <- ggplot(data = male_population, 
                               aes(x = population, y = suicides,
                                   label = country)) +
  geom_point(lwd = 2, col = '#44749D') +
  # add data as points
  geom_smooth(method = 'lm', col = 'black') + 
  # add regression line
  theme_classic() + # white background, no gridlines
  xlab('Mean Male Population') + # change x axis label
  ylab('Mean Number of Male Suicides') + # change y axis label
  plot_male_population_theme + # change the size of axis titles and axis text
  annotate('text', x = 25000, y = 1000000, label = 'R-squared = 0.83', size = 7.5) +
  annotate('text', x = 25000, y = 500000, label = 'p < 0.00001', size = 7.5) +
  # add text denoting the R-squared and p-value
  scale_x_continuous(trans = 'log',
                     breaks = c(10000, 100000, 1000000, 10000000),
                     labels = c('10,000', '100,000', '1,000,000', '10,000,000')) +
  scale_y_continuous(trans = 'log', 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000', 
                                '1,000,000'),
                     limits = c(1, 1000000)) +
  # log transform both axis, but use labels to show the actual values 
  # increasing by a power of 10
  geom_text_repel(data = male_population[which(male_population$country %in% 
                                                 highlighted_countries_below_m),],
                  direction = 'y',
                  nudge_y = -0.25,
                  size = 5) +
  geom_text_repel(data = male_population[which(male_population$country %in% 
                                                 highlighted_countries_above_m),],
                  direction = 'y',
                  nudge_y = 0.25,
                  force = 6,
                  size = 5)
  # add country labels
plot_male_population
# any country above the regression line has a relatively higher increase in
# mean male suicides i.e. the increase in suicides is greater than 1.18% for
# a 1% increase in population, and vice versa

# grid.arrange(plot_female_population, plot_male_population, ncol = 2)
# arrange both sex plots into one graph

# we can use analysis of covariance (ANCOVA) to test if adding sex is
# significant, after controlling for population
# first model is the most complicated, with a different intercept
# and slope for females and males
sex_population_lm1 = lm(log(suicides) ~ sex * log(population), 
                        data = sex_population)
summary(sex_population_lm1)
# population has a large effect on suicide rate (t = 22.458),
# there is no significant difference between the sexes (p-value of
# 0.3696 is greater than 0.05)

# second model is simpler, with two intercepts for the two sexes,
# but they both have the same slope
sex_population_lm2 = lm(log(suicides) ~ sex + log(population), 
                        data = sex_population)
# compare the two models
anova(sex_population_lm1, sex_population_lm2)
# the simpler model (without the interaction term) is justified
# as the p-value is greater than 0.05
# essentially there is no significant difference in explanatory power
# when using different slopes for each sex, and therefore there is no
# need to have different slopes for each sex

# now test whether sex had a significant effect on suicide rate after
# controlling for population
# this model has one intercept and one slope
sex_population_lm3 = lm(log(suicides) ~ log(population), 
                        data = sex_population)
# and compare the simpler model with this new model
anova(sex_population_lm2, sex_population_lm3)
# removing sex reduces the explanatory power (F value increases to 69.02),
# and the p-value is less than 0.05
# therefore the effect of sex in increasing suicides is significant
# there should be two slopes for the different sexes, but not different
# slopes
# sex_population_lm2 is the preferred model
summary(sex_population_lm2)

# calculate sex-specific intercepts
intercepts = c(coef(sex_population_lm2)['(Intercept)'],
               coef(sex_population_lm2)['(Intercept)'] + 
                 coef(sex_population_lm2)['sexmale'])
lines = data.frame(intercepts = intercepts,
                   slopes = rep(coef(sex_population_lm2)['log(population)'],
                                     2),
                   sex = levels(sex_population$sex))

plot_sex_population_theme <- theme(axis.title = element_text(size = 22.5), 
                                  axis.text = element_text(size = 20),
                                  legend.position = 'none')

plot_sex_population <- ggplot(data = sex_population,
                              aes(x = population, y = suicides,
                                  col = sex)) +
  geom_point(lwd = 2) +
  # add data as points
  geom_abline(aes(intercept = intercepts, slope = slopes, colour = sex),
              lwd = 1,
              colour = c('tomato2', '#44749D'),
              data = lines) +
  # add regression line
  theme_classic() + # white background, no gridlines
  xlab('Mean Population') + # change x axis label
  ylab('Mean Number of Suicides') + # change y axis label
  plot_sex_population_theme + # change the size of axis titles and axis text
  # and remove legend
  scale_colour_manual(values = c('tomato2', '#44749D')) +
  # change colour of points
  scale_x_continuous(trans = 'log',
                     breaks = c(10000, 100000, 1000000, 10000000),
                     labels = c('10,000', '100,000', '1,000,000', '10,000,000')) +
  scale_y_continuous(trans = 'log', 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000', 
                                '1,000,000'),
                     limits = c(1, 1000000))
# log transform both axis, but use labels to show the actual values 
# increasing by a power of 10
plot_sex_population

#######################################

# does having less wealth per person on average increase suicides
# gender difference
sex_gdp = suicide_data %>% # using suicide_data
  group_by(country, sex) %>% # group all the data from the same country
  # and sex
  summarise(suicides = mean(sum(as.numeric(suicides_no))),
            gdp_per_capita = mean(as.numeric(gdp_per_capita)))
# create rows with the mean of the number of suicides and the mean of
# the gdp per capita, for each sex in each country across the time period

# females
female_gdp <- sex_gdp[which(sex_gdp$sex == 'female'),]
# create a subset of sex_gdp with female data
female_gdp_lm = lm(log(suicides) ~ gdp_per_capita, 
                   data = female_gdp)
summary(female_gdp_lm)
# gdp per capita is not significant

plot_female_gdp_theme <- theme(axis.title = element_text(size = 22.5), 
                               axis.text = element_text(size = 20))
plot_female_gdp <- ggplot(data = female_gdp, aes(x = gdp_per_capita, 
                                                 y = suicides)) +
  geom_point(lwd = 2, col = 'tomato2') +
  # add data as points
  geom_smooth(method = 'lm', col = 'black') + 
  # add regression line
  theme_classic() + # white background, no gridlines
  xlab('Mean GDP per Capita') + # change x axis label
  ylab('Mean Number of Female Suicides') + # change y axis label
  plot_female_gdp_theme + # change the size of axis titles and axis text
  annotate('text', x = 25000, y = 1000000, label = 'R-squared = 0.02', size = 7.5) +
  annotate('text', x = 25000, y = 500000, label = 'p = 0.18', size = 7.5) +
  # add text denoting the R-squared and p-value
  scale_x_continuous(breaks = c(seq(0, 70000, 10000)),
                     labels = c(seq(0, 70000, 10000))) +
  # change x axis labels
  scale_y_continuous(trans = 'log', 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000',
                                '1,000,000'),
                     limits = c(1, 1000000))
  # log transform y axis, but use labels to show the actual values 
  # increasing by a power of 10
plot_female_gdp

# males
# create a subset of sex_gdp with female data
male_gdp <- sex_gdp[which(sex_gdp$sex == 'male'),]
# perform linear regression
male_gdp_lm = lm(log(suicides) ~ gdp_per_capita, data = male_gdp)
summary(male_gdp_lm)
# gdp per capita is not significant

plot_male_gdp_theme <- theme(axis.title = element_text(size = 22.5), 
                             axis.text = element_text(size = 20))
plot_male_gdp <- ggplot(data = male_gdp, aes(x = gdp_per_capita, 
                                             y = suicides)) +
  geom_point(lwd = 2, col = '#44749D') +
  # add data as points
  geom_smooth(method = 'lm', col = 'black') + 
  # add regression line
  theme_classic() + # white background, no gridlines
  xlab('Mean GDP per Capita') + # change x axis label
  ylab('Mean Number of Male Suicides') + # change y axis label
  plot_male_gdp_theme + # change the size of axis titles and axis text
  annotate('text', x = 25000, y = 1000000, label = 'R-squared = 0.02', size = 7.5) +
  annotate('text', x = 25000, y = 500000, label = 'p = 0.23', size = 7.5) +
  # add text denoting the R-squared and p-value
  scale_x_continuous(breaks = c(seq(0, 70000, 10000)),
                     labels = c(seq(0, 70000, 10000))) +
  # change x axis labels
  scale_y_continuous(trans = 'log', 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000',
                                '1,000,000'),
                     limits = c(1, 1000000))
 # log transform y axis, but use labels to show the actual values 
 # increasing by a power of 10
plot_male_gdp

# grid.arrange(plot_female_gdp, plot_male_gdp, ncol = 2)
# arrange both sex plots into one graph

###############################################################################
