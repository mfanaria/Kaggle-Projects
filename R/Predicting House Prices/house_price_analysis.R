# Import data
#setwd()
house_data = read.csv('kc_house_data.csv')

#### Packages ################################################################# 

# Import packages
library(dplyr)
library(gbm)
library(glmnet)
library(gridExtra)
library(leaflet)
library(leaps)
library(pls)
library(randomForest)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(tree)

#### Data cleaning ############################################################

# Data cleaning

# overview of data
head(house_data)

# I take issue with the number of floors and number of bathrooms, 
# but I'll leave it

# are any data missing?
sum(is.na(house_data)) # 0 values

#### EDA - price ##############################################################

# EDA

# Price
options(scipen = 100000000)
price_plot_theme = theme(axis.title = element_text(size = 22.5),
                         axis.text = element_text(size = 20))
price_plot = ggplot(data = house_data, aes(x = price)) + 
  geom_histogram(col = 'black') +
  # makes a boxplot
  geom_vline(xintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Price (US$)') + # change x axis label
  ylab('Frequency') + # change y axis label
  price_plot_theme + # change size of axis titles and text
  scale_x_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) +
  scale_y_continuous(breaks = c(seq(0, 3000, 500)),
                     labels = c(seq(0, 3000, 500)),
                     limits = c(0, 3000))
 # change x and y axis values
price_plot

#### EDA - built house vs time and house age vs price #########################

# Trend of houses built and age of house vs price

# houses built over time
# collate number of houses built per year
yearBuilt <- house_data %>% # using house_data
  group_by(yr_built) %>% # group all the data from the same year
  summarise(rows = n()) # collate the number of rows for each year

yearBuilt_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                              axis.text = element_text(size = 20))
yearBuilt_plot <- ggplot(data = yearBuilt, aes(x = yr_built, y = rows)) +
  geom_bar(stat = 'identity', col = 'black') + # create bar chart
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Houses Built') + # change y axis label
  yearBuilt_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(breaks = c(seq(1900, 2015, 5)),
                     labels = c(seq(1900, 2015, 5)),
                     limits = c(1899, 2016)) +
  scale_y_continuous(breaks = c(seq(0, 600, 50)),
                     labels = c(seq(0, 600, 50)),
                     limits = c(0, 600))
  # change x and y axis values

# mean price of house per year built
# collate current mean price of house per year built
yearBuilt_price <- house_data %>% # using house_data
  group_by(yr_built) %>% # group all the data from the same year
  summarise(mean_price = mean(as.numeric(price))) %>%
  # calculate current mean price for houses built in that year
  arrange(yr_built)

yearBuilt_price_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                                    axis.text = element_text(size = 20))
yearBuilt_price_plot <- ggplot(data = yearBuilt_price, 
                               aes(x = yr_built, y = mean_price)) +
  geom_bar(stat = 'identity', col = 'black') + # create bar chart
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Mean Price (US$)') + # change y axis label
  yearBuilt_price_plot_theme + 
  # change the size of axis titles and axis text
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  scale_x_continuous(breaks = c(seq(1900, 2015, 5)),
                     labels = c(seq(1900, 2015, 5)),
                     limits = c(1899, 2016)) +
  scale_y_continuous(breaks = c(seq(100000, 800000, 100000)),
                     labels = c('100,000', '200,000', '300,000',
                                '400,000', '500,000', '600,000',
                                '700,000', '800,000'))
 # change x and y axis values

grid.arrange(yearBuilt_plot, yearBuilt_price_plot, ncol = 2)

#### EDA - surrounding 15 houses vs price #####################################

# How do the surrounding 15 houses affect house price

# Square footage of lot of surrounding 15 houses
sqft_lot15_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                               axis.text = element_text(size = 20))
sqft_lot15_plot <- ggplot(data = house_data, aes(x = sqft_lot15, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Lot of Nearest 15 Houses') + 
  # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_lot15_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(1000, 10000, 100000, 1000000),
                     labels = c('1,000', '10,000', '100,000', '1,000,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', '1,000,000', 
                                '2,500,000', '5,000,000'))
 # change x and y axis values

# Square footage of living area of surrounding 15 houses
sqft_liv15_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                               axis.text = element_text(size = 20))
sqft_liv15_plot <- ggplot(data = house_data, 
                          aes(x = sqft_living15, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Living Area of Nearest 15 Houses') + 
  # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_liv15_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(500, 1000, 2500, 5000),
                     labels = c('500', '1,000', '2,500', '5,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) 
 # change x and y axis values

grid.arrange(sqft_lot15_plot, sqft_liv15_plot, ncol = 2)

#### EDA - waterfront, view, grade and condition vs price #####################

# waterfront
wft_plot_theme = theme(axis.title = element_text(size = 22.5),
                       axis.text = element_text(size = 20))
wft_plot = ggplot(data = house_data, aes(x = factor(waterfront), y = price)) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('') +
  ylab('Price (US$)') + # change y axis label
  wft_plot_theme + # change axis title and text size
  scale_x_discrete(labels = c('Not on Waterfront', 
                              'On Waterfront')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

# view
view_plot_theme = theme(axis.title = element_text(size = 22.5),
                        axis.text = element_text(size = 20))
view_plot = ggplot(data = house_data, aes(x = factor(view), y = price)) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('View Rating') +
  ylab('Price (US$)') + # change y axis label
  view_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

# grade
grd_plot_theme = theme(axis.title = element_text(size = 22.5),
                       axis.text = element_text(size = 20))
grd_plot = ggplot(data = house_data, aes(x = factor(grade), y = price)) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Grade') +
  ylab('Price (US$)') + # change y axis label
  grd_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

# condition
cond_plot_theme = theme(axis.title = element_text(size = 22.5),
                        axis.text = element_text(size = 20))
cond_plot = ggplot(data = house_data, aes(x = factor(condition), y = price)) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Condition') +
  ylab('Price (US$)') + # change y axis label
  cond_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

grid.arrange(wft_plot, view_plot, grd_plot, cond_plot, ncol = 2, nrow = 2)

#### EDA - Sq. foot. of interior, lot, abv and blw ground vs price ############

# Square footage of interior living space, land space, above ground
# and below ground vs price

# Square footage of interior living space
sqft_liv_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                             axis.text = element_text(size = 20))
sqft_liv_plot <- ggplot(data = house_data, aes(x = sqft_living, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Living Area') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_liv_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(500, 1000, 2500, 5000, 10000),
                     labels = c('500', '1,000', '2,500', '5,000',
                                '10,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) 
# change x and y axis values

# Square footage of land space
sqft_lot_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                             axis.text = element_text(size = 20))
sqft_lot_plot <- ggplot(data = house_data, aes(x = sqft_lot, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Lot') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_lot_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(1000, 10000, 100000, 1000000),
                     labels = c('1,000', '10,000', '100,000', '1,000,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) 
# change x and y axis values

# Square footage of above ground interior
sqft_abv_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                             axis.text = element_text(size = 20))
sqft_abv_plot <- ggplot(data = house_data, aes(x = sqft_above, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Interior (Above Ground)') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_abv_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(500, 1000, 2500, 5000, 7500),
                     labels = c('500', '1,000', '2,500', '5,000', '7,500')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) 
# change x and y axis values

# Square footage of below ground interior
# create a subset with houses that have a below ground area
blw_data <- subset(house_data, subset = sqft_basement > 0)

sqft_blw_plot_theme <- theme(axis.title = element_text(size = 22.5), 
                             axis.text = element_text(size = 20))
sqft_blw_plot <- ggplot(data = blw_data, aes(x = sqft_basement, y = price)) +
  geom_point(lwd = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Interior (Below Ground)') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_blw_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = c(10, 100, 1000, 5000),
                     labels = c('10', '100', '1,000', '5,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) 
# change x and y axis values

grid.arrange(sqft_liv_plot, sqft_lot_plot, sqft_abv_plot, sqft_blw_plot,
             ncol = 2, nrow = 2)

#### EDA - number of floors, bedrooms and bathrooms vs price ##################

# floors
floor_plot_theme = theme(axis.title = element_text(size = 22.5),
                         axis.text = element_text(size = 20))
floor_plot = ggplot(data = house_data, aes(x = factor(floors), y = price)) + 
  geom_boxplot() +
  # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Floors') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  floor_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
 # change y-axis values

# bedrooms
bed_plot_theme = theme(axis.title = element_text(size = 22.5),
                       axis.text = element_text(size = 20))
bed_plot = ggplot(data = house_data, aes(x = factor(bedrooms), y = price)) + 
  geom_boxplot() +
  # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Bedrooms') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  bed_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

# bathrooms
bath_plot_theme = theme(axis.title = element_text(size = 22.5),
                        axis.text = element_text(size = 20))
bath_plot = ggplot(data = house_data, aes(x = factor(bathrooms), y = price)) + 
  geom_boxplot() +
  # makes a boxplot
  geom_hline(yintercept = mean(house_data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Bathrooms') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  bed_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000'))
# change y-axis values

grid.arrange(floor_plot, bed_plot, bath_plot, ncol = 2, nrow = 2)
# number of floors doesn't seem to affect mean price much
# more than 5 bedrooms has higher than mean price
# mean price increases with number of bedrooms and bathrooms, but
# more with bathrooms
# given that this is housing, doesn't make sense to have 0 bedrooms,
# so they are removed, as well as the 33 bedroom house
# also remove any house with 0 bathrooms
house_data <- subset(house_data, subset = bedrooms < 33 & bedrooms > 0 
                     & bathrooms > 0)

#### EDA - map of house locations #############################################

# map of houses
# outline price groups
price_group1 <- subset(house_data, subset = price < 250000)
price_group2 <- subset(house_data, subset = price > 250000 & 
                         price < 500000)
price_group3 <- subset(house_data, subset = price > 500000 & 
                         price < 750000)
price_group4 <- subset(house_data, subset = price > 750000 & 
                         price < 1000000)
price_group5 <- subset(house_data, subset = price > 1000000)
# in order to make the legend work, I've specified a vector of five 
# blank strings of text with different spaces (no spaces to four)
# this ensures nothing is outputted onto the legend by 
# values = legend_order, and allows the price groups denoted by
# legend_labels to be pasted instead in the correct order
legend_order = c('', ' ', '  ', '   ', '    ')
legend_labels = c('78,000 - 250,000', '250,000 - 500,000', 
                  '500,000 - 750,000', '750,000 - 1,000,000', 
                  '1,000,000 - 7,700,000')

# define colours
pal <- colorFactor(palette = c('lawngreen', 'mediumseagreen', 
                               'deepskyblue', 'blue', 'navy'),
                   levels = legend_order)
# create ma
leaflet(options = leafletOptions(minZoom = 9, dragging = T)) %>% 
  # set the limit on zooming out and allow the map to be moved
  addProviderTiles(provider = 'CartoDB') %>%
  # add mao
  addCircleMarkers(data = price_group1, radius = 1, opacity = 0.75,
                   popup = ~paste0('<b>', 'Price: $', price, '</b>', 
                                   '<br/>', 'House area (sqft): ', sqft_living, 
                                   '<br/>', 'Lot area (sqft): ', sqft_lot),
                   color = 'lawngreen',  group = legend_labels[1]) %>%
  # add circles for the first price group, and give information when a
  # circle is clicked on
  addCircleMarkers(data = price_group2, radius = 1, opacity = 0.75,
                   popup = ~paste0('<b>', 'Price: $', price, '</b>', 
                                   '<br/>', 'House area (sqft): ', sqft_living, 
                                   '<br/>', 'Lot area (sqft): ', sqft_lot),
                   color = 'mediumseagreen',  group = legend_labels[2]) %>%
  addCircleMarkers(data = price_group3, radius = 1, opacity = 0.75,
                   popup = ~paste0('<b>', 'Price: $', price, '</b>', 
                                   '<br/>', 'House area (sqft): ', sqft_living, 
                                   '<br/>', 'Lot area (sqft): ', sqft_lot),
                   color = 'deepskyblue',  group = legend_labels[3]) %>%
  addCircleMarkers(data = price_group4, radius = 1, opacity = 0.75,
                   popup = ~paste0('<b>', 'Price: $', price, '</b>', 
                                   '<br/>', 'House area (sqft): ', sqft_living, 
                                   '<br/>', 'Lot area (sqft): ', sqft_lot),
                   color = 'blue',  group = legend_labels[4]) %>%
  addCircleMarkers(data = price_group5, radius = 1, opacity = 0.75,
                   popup = ~paste0('<b>', 'Price: $', price, '</b>', 
                                   '<br/>', 'House area (sqft): ', sqft_living, 
                                   '<br/>', 'Lot area (sqft): ', sqft_lot),
                   color = 'navy',  group = legend_labels[5]) %>%
  setView(lng = -122.25, lat = 47.4, zoom = 9) %>% # set initial map location
  addLegend(pal = pal, values = legend_order,
            labFormat = labelFormat(paste(values = legend_labels)),
            opacity = 0.75, title = 'Price Range ($)', 
            position = 'bottomleft') %>%
  # as mentioned previously, legend_order denotes the order of the 
  # legend by giving strings with different numbers of blank spaces,
  # allowing the legend_labels to pasted in the correct order
  # corresponding to the correct colours
  addLayersControl(overlayGroups = legend_labels, position = 'bottomright')
  # add an option to toggle a certain price group

#### Multiple linear regression models ########################################

# set up

# first remove unnecessary variables (first 2 columns and 17th)
model_data = house_data[-1][-1][-15]
# also going to log transform some variables
model_data$price = log(model_data$price)
model_data$sqft_living = log(model_data$sqft_living)
model_data$sqft_lot = log(model_data$sqft_lot)
model_data$sqft_above = log(model_data$sqft_above)
model_data$sqft_basement = log(model_data$sqft_basement + 1)
model_data$sqft_living15 = log(model_data$sqft_living15)
model_data$sqft_lot15 = log(model_data$sqft_lot15)

# create a matrix to track model stats
model_stats = matrix(NA, 16, 1, 
                     dimnames = list(c('Best subset selection (Lowest)',
                                       'Best subset selection (1 S.E.)',
                                       'Forward stepwise selection (Lowest)',
                                       'Forward stepwise selection (1 S.E.)',
                                       'Backward stepwise selection (Lowest)',
                                       'Backward stepwise selection (1 S.E.)',
                                       'Ridge regression (Lowest)',
                                       'Ridge regression (1 S.E.)',
                                       'Lasso regression (Lowest)',
                                       'Lasso regression (1 S.E.)', 'PCR', 
                                       'PLS', 'Single regression tree', 
                                       'Bagging', 'Random forest', 'Boosting'),
                                     c('Test MSE')))

#### Best subset selection using cross-validation #############################

# create a vector allocating each observation to one of k = 10 folds
k = 10
set.seed(35)
folds = sample(1:k, nrow(model_data), replace = T)
crossVal_errors_bss = matrix(NA, k, 17, dimnames = list(NULL, paste(1:17)))

# create a predict function to use in cross-validation 
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

# now write a for loop performing cross-validation
# in the jth fold, the elements of folds that equal j are in the test
# set and the remainder are in the training
for(j in 1:k){
  best_ss_cv = regsubsets(price ~ ., model_data[folds != j,], nvmax = 17)
  for(i in 1:17){
    predictions_bss = predict(best_ss_cv, model_data[folds == j,], id = i)
    crossVal_errors_bss[j, i] = mean(
      (model_data$price[folds == j] - predictions_bss)^2)
  }
}

# this results in a matrix of test MSE, where the (i,j)th element is 
# equal to the test MSE for the ith cross-validation fold for the best
# j-variable model
# obtain a vector for which the jth element is the cross-validation
# error for the j-variable model
mean_crossVal_errors_bss = apply(crossVal_errors_bss, 2, mean)
which.min(mean_crossVal_errors_bss)
# cross-validation has selected a 16 variable model

# check the standard errors
mean_crossVal_errors_bss = data.frame(mean_crossVal_errors_bss)
# standard error is the standard deviation of the test MSE for a particular
# model size divided by the square root of the number of folds
mean_crossVal_errors_bss$se = ((apply(crossVal_errors_bss, 2, sd)) / 
                                 (sqrt(10)))
mean_crossVal_errors_bss$se = unlist(mean_crossVal_errors_bss$se)

plot_bss_cv_theme <- theme(axis.title = element_text(size = 22.5),
                           axis.text = element_text(size = 20))
plot_bss_cv <- ggplot(data = mean_crossVal_errors_bss,
                      aes(x = c(seq(1, 17)), y = mean_crossVal_errors_bss)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(aes(ymin = mean_crossVal_errors_bss - se,
                    ymax = mean_crossVal_errors_bss + se)) + 
  # add standard error bars
  geom_hline(yintercept = 0.06397989, linetype = 2,
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_bss_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0.05, 0.15)) # change y-axis limits
plot_bss_cv
# 13 variable model is within one standard error above the chosen best model

# now perform best subset selection on full data set to obtain a 13
# variable model
best_ss_cv_full = regsubsets(price ~ ., model_data, nvmax = 17)
coef(best_ss_cv_full, 13)
summary_best_ss_cv_full = summary(best_ss_cv_full)

# it would also be useful to see how the adjusted R-squared changed
# as model sized increased
bss_adjRsq = data.frame(summary_best_ss_cv_full$adjr2)
bss_adjRsq$summary_best_ss_cv_full.adjr2 = 
  unlist(bss_adjRsq$summary_best_ss_cv_full.adjr2)

plot_bss_adjRsq_theme <- theme(axis.title = element_text(size = 22.5),
                               axis.text = element_text(size = 20))
plot_bss_adjRsq <- ggplot(data = bss_adjRsq,
                           aes(x = c(seq(1, 17)), 
                               y = bss_adjRsq$summary_best_ss_cv_full.adjr2)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = 0.7715858, 
             linetype = 2, colour = 'red') + # add line denoting the 
  # highest adjusted R-squared
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Adjusted R-Squared') + # change y axis label
  plot_bss_adjRsq_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0.4, 0.8)) # change y-axis limits
plot_bss_adjRsq
# model size 16 has highest adjusted R-squared
# but not much change after model size 5
round(bss_adjRsq$summary_best_ss_cv_full.adjr2, digits = 2)

# put test MSE in matrix
model_stats['Best subset selection (1 S.E.)', 
            'Test MSE'] = mean_crossVal_errors_bss$mean_crossVal_errors_bss[13]
model_stats['Best subset selection (Lowest)', 
            'Test MSE'] = mean_crossVal_errors_bss$mean_crossVal_errors_bss[16]

#### Forward stepwise selection with cross-validation #########################

# create a vector allocating each observation to one of k = 10 folds
k = 10
set.seed(35)
folds = sample(1:k, nrow(model_data), replace = T)
crossVal_errors_fwd = matrix(NA, k, 17, dimnames = list(NULL, paste(1:17)))

# now write a for loop performing cross-validation
# in the jth fold, the elements of folds that equal j are in the test
# set and the remainder are in the training
for(j in 1:k){
  fwd_ss_cv = regsubsets(price ~ ., model_data[folds != j,], nvmax = 17,
                         method = 'forward')
  for(i in 1:17){
    predictions_fwd = predict(fwd_ss_cv, model_data[folds == j,], id = i)
    crossVal_errors_fwd[j, i] = mean(
      (model_data$price[folds == j] - predictions_fwd)^2)
  }
}

# this results in a matrix of test MSE, where the (i,j)th element is 
# equal to the test MSE for the ith cross-validation fold for the best
# j-variable model
# obtain a vector for which the jth element is the cross-validation
# error for the j-variable model
mean_crossVal_errors_fwd = apply(crossVal_errors_fwd, 2, mean)
which.min(mean_crossVal_errors_fwd)
# cross-validation has selected a 16 variable model

# check the standard errors
mean_crossVal_errors_fwd = data.frame(mean_crossVal_errors_fwd)
mean_crossVal_errors_fwd$se = ((apply(crossVal_errors_fwd, 2, sd)) / 
                                 (sqrt(10)))
mean_crossVal_errors_fwd$se = unlist(mean_crossVal_errors_fwd$se)

plot_fwd_cv_theme <- theme(axis.title = element_text(size = 22.5),
                           axis.text = element_text(size = 20))
plot_fwd_cv <- ggplot(data = mean_crossVal_errors_fwd,
                      aes(x = c(seq(1, 17)), y = mean_crossVal_errors_fwd)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(aes(ymin = mean_crossVal_errors_fwd - se,
                    ymax = mean_crossVal_errors_fwd + se)) + 
  # add standard error bars
  geom_hline(yintercept = 0.06397989, linetype = 2,
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_fwd_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0.05, 0.15)) # change y-axis limits
plot_fwd_cv
# 12 variable model is within one standard error above the chosen best model

# now perform best subset selection on full data set to obtain a 12
# variable model
fwd_ss_cv_full = regsubsets(price ~ ., model_data, nvmax = 17,
                            method = 'forward')
coef(fwd_ss_cv_full, 12)
summary_fwd_ss_cv_full = summary(fwd_ss_cv_full)

# it would also be useful to see how the adjusted R-squared changed
# as model sized increased
fwd_adjRsq = data.frame(summary_fwd_ss_cv_full$adjr2)
fwd_adjRsq$summary_fwd_ss_cv_full.adjr2 = 
  unlist(fwd_adjRsq$summary_fwd_ss_cv_full.adjr2)

plot_fwd_adjRsq_theme <- theme(axis.title = element_text(size = 22.5),
                               axis.text = element_text(size = 20))
plot_fwd_adjRsq <- ggplot(data = fwd_adjRsq,
                          aes(x = c(seq(1, 17)), 
                              y = fwd_adjRsq$summary_fwd_ss_cv_full.adjr2)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = 0.7715858, 
             linetype = 2, colour = 'red') + # add line denoting the 
  # highest adjusted R-squared
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Adjusted R-Squared') + # change y axis label
  plot_fwd_adjRsq_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0.4, 0.8)) # change y-axis limits
plot_fwd_adjRsq
# model size 16 has highest adjusted R-squared
# but not much change after model size 5
round(bss_adjRsq$summary_best_ss_cv_full.adjr2, digits = 2)

# put test MSE in matrix
model_stats['Forward stepwise selection (1 S.E.)', 
            'Test MSE'] = mean_crossVal_errors_fwd$mean_crossVal_errors_fwd[12]
model_stats['Forward stepwise selection (Lowest)', 
            'Test MSE'] = mean_crossVal_errors_fwd$mean_crossVal_errors_fwd[16]

#### Backward stepwise selection with cross-validation  #######################

# create a vector allocating each observation to one of k = 10 folds
k = 10
set.seed(35)
folds = sample(1:k, nrow(model_data), replace = T)
crossVal_errors_bwd = matrix(NA, k, 17, dimnames = list(NULL, paste(1:17)))

# now write a for loop performing cross-validation
# in the jth fold, the elements of folds that equal j are in the test
# set and the remainder are in the training
for(j in 1:k){
  bwd_ss_cv = regsubsets(price ~ ., model_data[folds != j,], nvmax = 17,
                         method = 'backward')
  for(i in 1:17){
    predictions_bwd = predict(bwd_ss_cv, model_data[folds == j,], id = i)
    crossVal_errors_bwd[j, i] = mean(
      (model_data$price[folds == j] - predictions_bwd)^2)
  }
}

# this results in a matrix of test MSE, where the (i,j)th element is 
# equal to the test MSE for the ith cross-validation fold for the best
# j-variable model
# obtain a vector for which the jth element is the cross-validation
# error for the j-variable model
mean_crossVal_errors_bwd = apply(crossVal_errors_bwd, 2, mean)
which.min(mean_crossVal_errors_bwd)
# cross-validation has selected a 16 variable model

# check the standard errors
mean_crossVal_errors_bwd = data.frame(mean_crossVal_errors_bwd)
mean_crossVal_errors_bwd$se = ((apply(crossVal_errors_bwd, 2, sd)) / 
                                 (sqrt(10)))
mean_crossVal_errors_bwd$se = unlist(mean_crossVal_errors_bwd$se)

plot_bwd_cv_theme <- theme(axis.title = element_text(size = 22.5),
                           axis.text = element_text(size = 20))
plot_bwd_cv <- ggplot(data = mean_crossVal_errors_bwd,
                      aes(x = c(seq(1, 17)), y = mean_crossVal_errors_bwd)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(aes(ymin = mean_crossVal_errors_bwd - se,
                    ymax = mean_crossVal_errors_bwd + se)) + 
  # add standard error bars
  geom_hline(yintercept = 0.06397989, linetype = 2,
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_bwd_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0, 0.15)) # change y-axis limits
plot_bwd_cv
# 12 variable model is within one standard error above the chosen best model

# now perform best subset selection on full data set to obtain a 12
# variable model
bwd_ss_cv_full = regsubsets(price ~ ., model_data, nvmax = 17,
                            method = 'backward')
coef(bwd_ss_cv_full, 12)
summary_bwd_ss_cv_full = summary(bwd_ss_cv_full)

# it would also be useful to see how the adjusted R-squared changed
# as model sized increased
bwd_adjRsq = data.frame(summary_bwd_ss_cv_full$adjr2)
bwd_adjRsq$summary_bwd_ss_cv_full.adjr2 = 
  unlist(bwd_adjRsq$summary_bwd_ss_cv_full.adjr2)

plot_bwd_adjRsq_theme <- theme(axis.title = element_text(size = 22.5),
                               axis.text = element_text(size = 20))
plot_bwd_adjRsq <- ggplot(data = bwd_adjRsq,
                          aes(x = c(seq(1, 17)), 
                              y = bwd_adjRsq$summary_bwd_ss_cv_full.adjr2)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = 0.7715858, 
             linetype = 2, colour = 'red') + # add line denoting the 
  # highest adjusted R-squared
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Adjusted R-Squared') + # change y axis label
  plot_bwd_adjRsq_theme + # change the size of axis titles and axis text
  scale_y_continuous(limit = c(0.4, 0.8)) # change y-axis limits
plot_bwd_adjRsq
# model size 16 has highest adjusted R-squared
# but not much change after model size 5
round(bss_adjRsq$summary_best_ss_cv_full.adjr2, digits = 2)

# put test MSE in matrix
model_stats['Backward stepwise selection (1 S.E.)', 
            'Test MSE'] = mean_crossVal_errors_bwd$mean_crossVal_errors_bwd[12]
model_stats['Backward stepwise selection (Lowest)', 
            'Test MSE'] = mean_crossVal_errors_bwd$mean_crossVal_errors_bwd[16]

#### Ridge regression #########################################################

# create an x matrix and a y vector
x = model.matrix(price ~ ., model_data)[, -1]
y = model_data$price

# ridge regression is similar to least squares in that it tries to 
# make the RSS small
# it also has a shrinkage penalty, lambda
# when lambda is 0, the penalty has no effect and ridge regression
# produces the least squares estimate
# as lambda increases, the impact of the shrinkage penalty grows and
# the coefficient estimates will approach zero
# when lambda is very large, all coefficients will be essentially zero,
# which is a null model with no predictors
# how to determine which lambda value to use?
# use a grid of lambda values, compute the cross-validation error for
# each value, select the lambda with the lowest cross-validation error

# use ridge regression with a range of lambda values
grid = 10^seq(10, -2, length = 100)
# alpha = 0 denotes the ridge regression model
ridge = glmnet(x, y, alpha = 0, lambda = grid)

# perform 10-fold cross-validation to find the best lambda value
set.seed(35)
# split data into train and test
train = sample(c(TRUE, FALSE), nrow(model_data), rep = TRUE)
test = (!train)
y.test = y[test]
# cross-validation (10-fold is default)
ridge_crossVal = cv.glmnet(x[train,], y[train], alpha = 0)
plot(ridge_crossVal)
best_lambda_ridge = ridge_crossVal$lambda.min
best_lambda_ridge

# the test MSE with this lambda value is:
ridge_prediction = predict(ridge, s = best_lambda_ridge, 
                           newx = x[test,])
mean((ridge_prediction - y.test)^2)

# coefficients of best with best lambda value
ridge_full = glmnet(x, y, alpha = 0)
ridge_full_coef = predict(ridge_full, type = 'coefficients', 
                          s = best_lambda_ridge)

# can also use the lowest lambda that is within one standard error of
# the best lambda
best_lambda_se_ridge = ridge_crossVal$lambda.1se
# and the test MSE with this value:
ridge_prediction_se = predict(ridge, s = best_lambda_se_ridge, 
                              newx = x[test,])
mean((ridge_prediction_se - y.test)^2)

# put test MSE in matrix
model_stats['Ridge regression (Lowest)', 
            'Test MSE'] = mean((ridge_prediction - y.test)^2)
model_stats['Ridge regression (1 S.E.)', 
            'Test MSE'] = mean((ridge_prediction_se - y.test)^2)

#### Lasso regression #########################################################

# lasso regression is similar to to best subset, forward stepwise and
# backward stepwise regression, in that it performs variable selection
# using lambda as in ridge, coefficient estimates will be shrunk towards
# zero, but unlike ridge if a coefficient is exactly zero it will be
# removed

# alpha = 1 denotes the lasso regression model, lambda grid is the 
# same as the ridge regression
lasso = glmnet(x[train,], y[train], alpha = 1, lambda = grid) 

# perform 10-fold cross-validation to find the best lambda value
set.seed(35)
lasso_crossVal = cv.glmnet(x[train,], y[train], alpha = 1)
plot(lasso_crossVal)
best_lambda_lasso = lasso_crossVal$lambda.min
best_lambda_lasso

# the test MSE with this lambda value is:
lasso_prediction = predict(lasso, s = best_lambda_lasso, 
                           newx = x[test,])
mean((lasso_prediction - y.test)^2)

# coefficients  with best lambda value
lasso_full = glmnet(x, y, alpha = 1, lambda = grid)
lasso_full_coef = predict(lasso_full, type = 'coefficients', 
                          s = best_lambda_lasso)

# can also use the lowest lambda that is within one standard error of
# the best lambda
best_lambda_se_lasso = lasso_crossVal$lambda.1se
# and the test MSE with this value:
lasso_prediction_se = predict(lasso, s = best_lambda_se_lasso, 
                              newx = x[test,])
mean((lasso_prediction_se - y.test)^2)

# put test MSE in matrix
model_stats['Lasso regression (Lowest)', 
            'Test MSE'] = mean((lasso_prediction - y.test)^2)
model_stats['Lasso regression (1 S.E.)', 
            'Test MSE'] = mean((lasso_prediction_se - y.test)^2)

#### Principal components regression ##########################################

# Principal components regression (PCR)
# scale = T standardises each predictor, validation = 'CV' causes pcr to 
# compute the ten-fold cross-validation error for each value of M, the number
# of principal components used
set.seed(35)
pcr_house = pcr(price ~ ., data = model_data, subset = train, scale = T,
                validation = 'CV')

validationplot(pcr_house, val.type = 'MSEP')
pcr_house$validation$adj
# doesn't seem to be much difference after about 7 components

# therefore going to add 7 component model to matrix
model_stats['PCR', 'Test MSE'] = 
  (mean((predict(pcr_house, x[test,], ncomp = 7) - y.test)^2))

#### Partial least squares ####################################################

# Partial least squares (PLS)
set.seed(35)
pls_house = plsr(price ~ ., data = model_data, subset = train, scale = T,
               validation = 'CV')

validationplot(pls_house, val.type = 'MSEP')
pls_house$validation$adj
# doesn't seem to be much difference after about 4 components

# therefore going to add 4 component model to matrix
model_stats['PLS', 'Test MSE'] = 
  (mean((predict(pls_house, x[test,], ncomp = 4) - y.test)^2))

#### Single regression tree ###################################################

# one regression tree
tree_house = tree(price ~ ., model_data, subset = train)
summary(tree_house)
# three variables have been used to construct the tree
# deviance is the sum of squared errors for the tree
# plot tree
# default plot is below
#plot(tree_house)
#text(tree_house, pretty = 0)

# but i am going to convert it to an rpart object so it looks better
tree_plot = rpart(price ~ ., model_data, subset = train)
# delete node 7 (lat <48), so that it matches the original plot
tree_plot = snip.rpart(tree_plot, toss = c(7))
# plot the rpart object, with 4 digits and no rounding of integers,
# each node shows the variable responsible for that split and each branch
# has the condition displayed
rpart.plot(tree_plot, digits = 4, type = 5)
# the terminal nodes shows the predicted house price (as a log),
# and the percentage of (training) observations in it

# use cv.tree() to see if pruning improves performance
cv_tree_house = cv.tree(tree_house)
plot(cv_tree_house$size, cv_tree_house$dev, type = 'b')
# in this case, the most complex tree is selected

# in keeping with the cross-validation results, use unpruned tree to make
# predictions on test set
yhat_tree_house = predict(tree_house, newdata = model_data[test,])
house_test = model_data[test, 'price']
plot(yhat_tree_house, house_test)
abline(0, 1)
mean((yhat_tree_house - house_test)^2) 

# put test MSE in matrix
model_stats['Single regression tree', 
            'Test MSE'] = mean((yhat_tree_house - house_test)^2)

#### Bagging ##################################################################

# bagging is a special case of a random forest, where the subset of predictors
# used is equal to the number of predictors - mtry = 17 denotes this
set.seed(35)
bag_house = randomForest(price ~ ., data = model_data, subset = train, 
                         mtry = 17, importance = T)
bag_house

# test on test set
yhat_bag = predict(bag_house, newdata = model_data[test,])
mean((yhat_bag - house_test)^2) 

# put test MSE in matrix
model_stats['Bagging', 'Test MSE'] = mean((yhat_bag - house_test)^2)

#### Random forest ############################################################

# growing a random forest is similar, but a lower value of mtry is used
# the default value is used here
set.seed(35)
rf_house = randomForest(price ~ ., data = model_data, subset = train,
                        importance = T)
yhat_rf = predict(rf_house, newdata = model_data[test,])
mean((yhat_rf - house_test)^2) 

# put test MSE in matrix
model_stats['Random forest', 'Test MSE'] = mean((yhat_rf - house_test)^2)

# importance() can be used to view the importance of each variable
randomForest::importance(rf_house)
# first column is based on the mean decrease of accuracy in predictions on the
# OOB samples when a given variable is excluded from the model
# second column is a measure of the total decrease in node impurity that results
# from splits over that variable, averaged over all trees
# for regression trees, node impurity is measured by the training RSS
varImpPlot(rf_house)
# shows that latitude is the most important variable, longitude is important
# for accuracy whereas the square footage of the living area and the grade of
# the house are important for node purity

#### Boosting #################################################################

# Boosting
# distribution = 'gaussian' is used because this is a regression problem
# n.trees indicates the number of trees, and interaction.depth the depth of
# each tree
set.seed(35)
boost_house = gbm(price ~., data = model_data[train,], 
                  distribution = 'gaussian', n.trees = 5000, 
                  interaction.depth = 4)
summary(boost_house)
# again latitude, square footage of the living area and the grade of the house
# are by far the most important

# partial dependence plots can be produced for these three variables, they
# illustrate the marginal effect of the selected variables on the response
# after integrating out the other variables
par(mfrow = c(1, 3))
plot(boost_house, i = 'lat') # house prices generally increase with latitude,
# though there is a drop off
plot(boost_house, i = 'sqft_living') # house prices increase as the square
# footage of the living area increases
plot(boost_house, i = 'grade') # house prices increase as the grade of the house
# increases

# now use boosted model to predict prices on test set
yhat_boost = predict(boost_house, newdata = model_data[test,], n.trees = 5000)
mean((yhat_boost - house_test)^2) 

# boosting has a number of hyperparameters that can be tuned to improve 
# performance even more, these are number of trees, depth of trees, learning 
# rate and subsampling
# going to perform a grid search to search over different hyperparameter values
hyper_parameter_grid = expand.grid(
  shrinkage = c(0.001, 0.01, 0.1), # changes the learning rate
  interaction.depth = c(1, 2, 3, 4, 5), # changes how deep the tree is
  n.minobsinnode = c(5, 10, 15), # changes the minimum number of observations
  # in each terminal node
  bag.fraction = c(0.65, 0.8, 1), # allows stochastic gradient descent, helps
  # prevent overfitting
  optimal_trees = 0, # track the optimal number of trees
  min_RMSE = 0 # track the minimum RMSE
) # 135 models to consider

# to increase the speed of this process, the models will be trained on a 
# training set (75% of the observations) and tested on the remaining 25%, as
# opposed to using cross-validation
for(i in 1:nrow(hyper_parameter_grid)){
  set.seed(35)
  boost_house_tuning = gbm(
    price ~., 
    data = model_data[train,], 
    distribution = 'gaussian', 
    n.trees = 5000, 
    shrinkage = hyper_parameter_grid$shrinkage[i],
    interaction.depth = hyper_parameter_grid$interaction.depth[i],
    n.minobsinnode = hyper_parameter_grid$n.minobsinnode[i],
    bag.fraction = hyper_parameter_grid$bag.fraction[i],
    train.fraction = 0.75,
    verbose = F)
  
  # add training error and optimal number of trees to grid
  hyper_parameter_grid$optimal_trees[i] = 
    which.min(boost_house_tuning$valid.error)
  hyper_parameter_grid$min_RMSE[i] = sqrt(min(boost_house_tuning$valid.error))
  
  # tells me how many models have been considered so far
  print(i)
}

hyper_parameter_grid %>%
  arrange(min_RMSE) %>%
  head(10)
# none of the top 10 models used a shrinkage rate of 0.001
# all of the top 10 models had deep trees (>= 4)
# adding bag.fraction < 1 helped, but not with the top 3 models
# only 1 of the top 10 models had a minimum observations per node of 5

# modify the hyperparameter grid
hyper_parameter_grid = expand.grid(
  shrinkage = c(0.01, 0.05, 0.1, 0.15), 
  interaction.depth = c(3, 4, 5), 
  n.minobsinnode = c(5, 10, 15), 
  bag.fraction = c(0.65, 0.8, 1), 
  optimal_trees = 0,
  min_RMSE = 0 
) # 108 models to consider
# run for loop again
for(i in 1:nrow(hyper_parameter_grid)){
  set.seed(35)
  boost_house_tuning = gbm(
    price ~., 
    data = model_data[train,], 
    distribution = 'gaussian', 
    n.trees = 5000, 
    shrinkage = hyper_parameter_grid$shrinkage[i],
    interaction.depth = hyper_parameter_grid$interaction.depth[i],
    n.minobsinnode = hyper_parameter_grid$n.minobsinnode[i],
    bag.fraction = hyper_parameter_grid$bag.fraction[i],
    train.fraction = 0.75,
    verbose = F)
  
  # add training error and optimal number of trees to grid
  hyper_parameter_grid$optimal_trees[i] = 
    which.min(boost_house_tuning$valid.error)
  hyper_parameter_grid$min_RMSE[i] = sqrt(min(boost_house_tuning$valid.error))
  
  # tells me how many models have been considered so far
  print(i)
}
hyper_parameter_grid %>%
  arrange(min_RMSE) %>%
  head(10)

# train the model with the best hyperparameters
set.seed(35)
boost_house_tuned = gbm(price ~., data = model_data[train,], 
                        distribution = 'gaussian', n.trees = 3556, 
                        interaction.depth = 4, shrinkage = 0.05,
                        n.minobsinnode = 15, bag.fraction = 0.8,
                        verbose = F)
# test model
yhat_boost_tuned = predict(boost_house_tuned, newdata = model_data[test,], 
                           n.trees = 3556)
mean((yhat_boost_tuned - house_test)^2) 

# put test MSE in matrix
model_stats['Boosting', 'Test MSE'] = mean((yhat_boost_tuned - house_test)^2)

# can also plot the relative influence of each predictor again
par(mfrow = c(1, 1))
summary(boost_house_tuned)
# latitude, square footage of living area and grade of the house are most
# important









