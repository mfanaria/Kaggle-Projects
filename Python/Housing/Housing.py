## Packages
# Import packages
import os
import pandas as pd
import numpy as np
from plotnine import * # using * saves me from writing plotnine before every
# ggplot use
from dfply import *
import folium
import statsmodels.api as sm
import itertools
from sklearn.cross_decomposition import PLSRegression
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.linear_model import LinearRegression, Ridge, RidgeCV, Lasso, LassoCV
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split, cross_val_score, KFold, ParameterGrid
from sklearn.preprocessing import scale
from sklearn.tree import DecisionTreeRegressor, plot_tree
from mlxtend.feature_selection import ExhaustiveFeatureSelector as EFS



## Import data
# set working directory
os.chdir('/Users/olliethwaites/Documents/OneDrive - Swansea University/Other/Projects/Python/Housing')

# import csv file
house_data = pd.read_csv('kc_house_data.csv')


## Data cleaning
# overview of data
house_data.info()

# issue with 'floors' and 'bathrooms' as floats

# are any data missing?
print('Number of missing data:', house_data.isnull().sum().sum()) 
# 0 missing data


## EDA - price
# note that ggplot can be used in python, using the plotnine library
# interpretation of the graphs is found in the kaggle project

price_plot_theme = theme(axis_title = element_text(size = 22.5), 
                         axis_text = element_text(size = 20))

price_plot = (ggplot(data = house_data, mapping = aes(x = 'price')) + 
  # note that the whole object is in brackets - (ggplot...)
  # also note that aes is preceded by mapping = and the column name is in quotes
  geom_histogram(color = 'black', bins = 30) +
  # makes a boxplot, bins = 30 was the value defaulted by R
  geom_vline(xintercept = house_data['price'].mean(), linetype = 'dashed', 
             color = 'red', size = 2) +
  # add line denoting mean house price - note how the mean value is called is
  # different than R, you use the data first and select the column using [''],
  # followed by .mean()
  # size = 2 replaces lwd = 2 from R
  theme_classic() + # white background, no gridlines
  xlab('Price (US$)') + # change x axis label
  ylab('Frequency') + # change y axis label
  price_plot_theme + # change size of axis titles and text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', 
                                        '5,000,000'])) +
  scale_y_continuous(breaks = np.array(range(0, 3500, 500)),
                     labels = np.array(range(0, 3500, 500)),
                     limits = np.array([0, 3000]))
 # change x and y axis values
 # note that instead of using c() in R, numpy arrays are used
 # also note that in range(), the second number denotes when the range stops,
 # but that final number is not included - so if I want the numbers 0-3000
 # every 500, the stop number has to be 3500
 )
price_plot


## EDA - houses built over time and house age vs price
# dfply is a python equivalent to dplyr and has pipes that can be used to
# chain multiple bits of code together

# houses built over time
# collate number of houses built per year
yearBuilt = (house_data >> # note that >> replaces %>% as the pipe
  group_by(X.yr_built) >>  # X refers to the data frame from the first line
  # and has to be called explicitly unlike in R
  summarize(rows = n(X.yr_built))
  )

yearBuilt_plot_theme = theme(axis_title = element_text(size = 22.5), 
                             axis_text = element_text(size = 20))
yearBuilt_plot = (ggplot(data = yearBuilt, 
                         mapping = aes(x = 'yr_built', y = 'rows')) +
  geom_bar(stat = 'identity', color = 'black') + # create bar chart
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Houses Built') + # change y axis label
  yearBuilt_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(breaks = np.array(range(1900, 2020, 5)),
                     labels = np.array(range(1900, 2020, 5)),
                     limits = np.array([1899, 2016])) +
  scale_y_continuous(breaks = np.array(range(0, 650, 50)),
                     labels = np.array(range(0, 650, 50)),
                     limits = np.array([0, 600]))
  # change x and y axis values
  )
yearBuilt_plot

# mean price of house per year built
# collate current mean price of house per year built
yearBuilt_price = (house_data >> # using house_data
  group_by(X.yr_built) >> # group all the data from the same year
  summarize(mean_price = mean(X.price)) >>
  # calculate current mean price for houses built in that year
  arrange(X.yr_built)
  )

yearBuilt_price_plot_theme = theme(axis_title = element_text(size = 22.5), 
                                   axis_text = element_text(size = 20))
yearBuilt_price_plot = (ggplot(data = yearBuilt_price, 
                               mapping = aes(x = 'yr_built', 
                                             y = 'mean_price')) +
  geom_bar(stat = 'identity', color = 'black') + # create bar chart
  theme_classic() + # white background, no gridlines
  xlab('Year') + # change x axis label
  ylab('Mean Price (US$)') + # change y axis label
  yearBuilt_price_plot_theme + 
  # change the size of axis titles and axis text
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  scale_x_continuous(breaks = np.array(range(1900, 2020, 5)),
                     labels = np.array(range(1900, 2020, 5)),
                     limits = np.array([1899, 2016])) +
  scale_y_continuous(breaks = np.array(range(0, 900000, 100000)),
                     labels = np.array(['0', '100,000', '200,000', '300,000',
                                        '400,000', '500,000', '600,000',
                                        '700,000', '800,000']))
 # change x and y axis values
 )
yearBuilt_price_plot


## EDA - surrounding 15 houses vs price 

# Square footage of lot of surrounding 15 houses
sqft_lot15_plot_theme = theme(axis_title = element_text(size = 22.5), 
                              axis_text = element_text(size = 20))
sqft_lot15_plot = (ggplot(data = house_data, 
                          mapping = aes(x = 'sqft_lot15', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Lot of Nearest 15 Houses') + 
  # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_lot15_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([1000, 10000, 100000, 1000000]),
                     labels = np.array(['1,000', '10,000', '100,000', 
                                        '1,000,000'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
 # change x and y axis values
 )
sqft_lot15_plot

# Square footage of living area of surrounding 15 houses
sqft_liv15_plot_theme = theme(axis_title = element_text(size = 22.5), 
                              axis_text = element_text(size = 20))
sqft_liv15_plot = (ggplot(data = house_data, 
                          mapping = aes(x = 'sqft_living15', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Living Area of Nearest 15 Houses') + 
  # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_liv15_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([500, 1000, 2500, 5000]),
                     labels = np.array(['500', '1,000', '2,500', '5,000'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000'])) 
 # change x and y axis values
 )
sqft_liv15_plot


# EDA - waterfront, view, grade and condition vs price

# waterfront
wft_plot_theme = theme(axis_title = element_text(size = 22.5),
                       axis_text = element_text(size = 20))

wft_plot = (ggplot(data = house_data, 
                   mapping = aes(x = house_data['waterfront'].astype('category'), 
                                 y = 'price')) + # note that instead of using
            # factor() in R, in python you specify the column and use 
            # astype('category')
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('') +
  ylab('Price (US$)') + # change y axis label
  wft_plot_theme + # change axis title and text size
  scale_x_discrete(labels = np.array(['Not on Waterfront', 'On Waterfront'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change y-axis values
  )
wft_plot

# view
view_plot_theme = theme(axis_title = element_text(size = 22.5),
                        axis_text = element_text(size = 20))
view_plot = (ggplot(data = house_data, 
                    mapping = aes(x = house_data['view'].astype('category'), 
                                  y = 'price')) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('View Rating') +
  ylab('Price (US$)') + # change y axis label
  view_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change y-axis values
  )
view_plot

# grade
grd_plot_theme = theme(axis_title = element_text(size = 22.5),
                       axis_text = element_text(size = 20))
grd_plot = (ggplot(data = house_data, 
                   mapping = aes(x = house_data['grade'].astype('category'), 
                                 y = 'price')) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Grade') +
  ylab('Price (US$)') + # change y axis label
  grd_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change y-axis values
  )
grd_plot

# condition
cond_plot_theme = theme(axis_title = element_text(size = 22.5),
                        axis_text = element_text(size = 20))
cond_plot = (ggplot(data = house_data, 
                    mapping = aes(x = house_data['condition'].astype('category'), 
                                  y = 'price')) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Condition') +
  ylab('Price (US$)') + # change y axis label
  cond_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change y-axis values
  )
cond_plot


## EDA - Sq. foot. of interior, abv and blw ground vs price

# Square footage of interior living space
sqft_liv_plot_theme = theme(axis_title = element_text(size = 22.5), 
                            axis_text = element_text(size = 20))
sqft_liv_plot = (ggplot(data = house_data, 
                        mapping = aes(x = 'sqft_living', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Living Area') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_liv_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([500, 1000, 2500, 5000, 10000]),
                     labels = np.array(['500', '1,000', '2,500', '5,000',
                                        '10,000'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000'])) 
  # change x and y axis values
  )
sqft_liv_plot

# Square footage of land space
sqft_lot_plot_theme = theme(axis_title = element_text(size = 22.5), 
                            axis_text = element_text(size = 20))
sqft_lot_plot = (ggplot(data = house_data, 
                        mapping = aes(x = 'sqft_lot', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Lot') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_lot_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([1000, 10000, 100000, 1000000]),
                     labels = np.array(['1,000', '10,000', '100,000', 
                                        '1,000,000'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000'])) 
  # change x and y axis values
  )
sqft_lot_plot

# Square footage of above ground interior
sqft_abv_plot_theme = theme(axis_title = element_text(size = 22.5), 
                            axis_text = element_text(size = 20))
sqft_abv_plot = (ggplot(data = house_data, 
                        mapping = aes(x = 'sqft_above', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Interior (Above Ground)') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_abv_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([500, 1000, 2500, 5000, 7500]),
                     labels = np.array(['500', '1,000', '2,500', '5,000', 
                                        '7,500'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000'])) 
  # change x and y axis values
  )
sqft_abv_plot

# Square footage of below ground interior
# create a subset with houses that have a below ground area
blw_data = house_data[house_data.sqft_basement > 0]

sqft_blw_plot_theme = theme(axis_title = element_text(size = 22.5), 
                            axis_text = element_text(size = 20))
sqft_blw_plot = (ggplot(data = blw_data, 
                        mapping = aes(x = 'sqft_basement', y = 'price')) +
  geom_point(size = 1) +
  # add data as points
  theme_classic() + # white background, no gridlines
  xlab('Square Footage of Interior (Below Ground)') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  sqft_blw_plot_theme + # change the size of axis titles and axis text
  scale_x_continuous(trans = 'log',
                     breaks = np.array([10, 100, 1000, 5000]),
                     labels = np.array(['10', '100', '1,000', '5,000'])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000'])) 
  # change x and y axis values
  )
sqft_blw_plot


## EDA - no. of floors, bedrooms and bathrooms vs price

# floors
floor_plot_theme = theme(axis_title = element_text(size = 22.5),
                         axis_text = element_text(size = 20))
floor_plot = (ggplot(data = house_data, 
                     mapping = aes(x = house_data['floors'].astype('category'), 
                                   y = 'price')) + 
  geom_boxplot() +
  # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Floors') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  floor_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
 # change y-axis values
 )
floor_plot

# bedrooms
bed_plot_theme = theme(axis_title = element_text(size = 22.5),
                       axis_text = element_text(size = 20))
bed_plot = (ggplot(data = house_data, 
                   mapping = aes(x = house_data['bedrooms'].astype('category'), 
                                 y = 'price')) + 
  geom_boxplot() +
  # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Bedrooms') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  bed_plot_theme + # change axis title and text size
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                        2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change y-axis values
  )
bed_plot

# bathrooms
bath_plot_theme = theme(axis_title = element_text(size = 22.5),
                        axis_text = element_text(size = 20))
bath_plot = (ggplot(data = house_data, 
                    mapping = aes(x = house_data['bathrooms'].astype('category'), 
                                  y = 'price')) + 
  geom_boxplot() + # makes a boxplot
  geom_hline(yintercept = house_data['price'].mean(),
             linetype = 'dashed', color = 'red', size = 2) +
  # add line denoting mean house price
  theme_classic() + # white background, no gridlines
  xlab('Number of Bathrooms') + # change x axis label
  ylab('Price (US$)') + # change y axis label
  bed_plot_theme + # change axis title and text size
  scale_x_discrete(breaks = np.array([0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 
                                      5, 5.5, 6, 6.5, 7.5, 8]),
                   labels = np.array(['0', 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 
                                      5, 5.5, 6, 6.5, 7.5, 8])) +
  scale_y_continuous(trans = 'log',
                     breaks = np.array([100000, 250000, 500000, 1000000, 
                                2500000, 5000000]),
                     labels = np.array(['100,000', '250,000', '500,000', 
                                        '1,000,000', '2,500,000', '5,000,000']))
  # change x and y-axis values
  )
bath_plot

# given that this is housing, doesn't make sense to have 0 bedrooms,
# so they are removed, as well as the 33 bedroom house
# also remove any house with 0 bathrooms
house_data = house_data[(house_data.bedrooms < 33) & (house_data.bedrooms > 0) 
                        & (house_data.bathrooms > 0)]


## EDA - map of house locations

# create map
house_map = folium.Map([47.4, -122.25], tiles = 'cartodbpositron',
                       zoom_start = 9)

# create subsets of the original dataset, for each of the (arbitrarily)
# chosen price groups
price_group1_subset = house_data[house_data.price < 250000]
price_group2_subset = house_data[(house_data.price > 250000) 
                                 & (house_data.price < 500000)]
price_group3_subset = house_data[(house_data.price > 500000) 
                                 & (house_data.price < 750000)]
price_group4_subset = house_data[(house_data.price > 750000) 
                                 & (house_data.price < 1000000)]
price_group5_subset = house_data[house_data.price > 1000000]

# template for text shown in LayerControl
legend_text = '<span style = "color: {col};"> {txt} </span>'

# create price groups as FeatureGroups, so that each price groups is its 
# own layer on the map
price_group1 = folium.FeatureGroup(name = legend_text.format(
    txt = 'Price: < $250,000', col = 'lawngreen'))
price_group2 = folium.FeatureGroup(name = legend_text.format(
    txt = 'Price: $250,000 - $500,000', col = 'mediumseagreen'))
price_group3 = folium.FeatureGroup(name = legend_text.format(
    txt = 'Price: $500,000 - $750,000', col = 'deepskyblue'))
price_group4 = folium.FeatureGroup(name = legend_text.format(
    txt = 'Price: $750,000 - $1,000,000', col = 'blue'))
price_group5 = folium.FeatureGroup(name = legend_text.format(
    txt = 'Price: > $1,000,000', col = 'navy'))

# add price_group1 to map
for i in range(0, len(price_group1_subset)):
    # for each house in this price group subset
    
    # template for text shown when the marker is clicked on 
    popup_text = '<b> Price: ${:,} </b> <br> House area (sqft): {:,} <br> Lot area (sqft): {:,}'
    popup_text = popup_text.format(
        price_group1_subset['price'].iloc[i], # find the price for this house
        price_group1_subset['sqft_living'].iloc[i], # find the sqft_living 
        price_group1_subset['sqft_lot'].iloc[i] # find the sqft_lot
        )
    
    # create popup with the text created previously and set the width & height
    iframe = folium.IFrame(popup_text, width = 175, height = 100)
    
    # add marker of this particular house
    folium.CircleMarker(
        location = [price_group1_subset.iloc[i]['lat'], 
                    price_group1_subset.iloc[i]['long']],
        popup = folium.Popup(iframe), # created previously
        radius = 1,
        opacity = 0.75,
        color = 'lawngreen').add_to(price_group1) # add to FeatureGroup
price_group1.add_to(house_map) # add FeatureGroup to Map

# repeat this for the other price groups

for i in range(0, len(price_group2_subset)):
    
    popup_text = '<b> Price: ${:,} </b> <br> House area (sqft): {:,} <br> Lot area (sqft): {:,}'
    popup_text = popup_text.format(
        price_group2_subset['price'].iloc[i],
        price_group2_subset['sqft_living'].iloc[i],
        price_group2_subset['sqft_lot'].iloc[i]
        )
    
    iframe = folium.IFrame(popup_text, width = 175, height = 100)
    
    folium.CircleMarker(
        location = [price_group2_subset.iloc[i]['lat'], 
                    price_group2_subset.iloc[i]['long']],
        popup = folium.Popup(iframe),
        radius = 1,
        opacity = 0.75,
        color = 'mediumseagreen').add_to(price_group2)
price_group2.add_to(house_map)

for i in range(0, len(price_group3_subset)):
    
    popup_text = '<b> Price: ${:,} </b> <br> House area (sqft): {:,} <br> Lot area (sqft): {:,}'
    popup_text = popup_text.format(
        price_group3_subset['price'].iloc[i],
        price_group3_subset['sqft_living'].iloc[i],
        price_group3_subset['sqft_lot'].iloc[i]
        )
    
    iframe = folium.IFrame(popup_text, width = 175, height = 100)
    
    folium.CircleMarker(
        location = [price_group3_subset.iloc[i]['lat'], 
                    price_group3_subset.iloc[i]['long']],
        popup = folium.Popup(iframe),
        radius = 1,
        opacity = 0.75,
        color = 'deepskyblue').add_to(price_group3)
price_group3.add_to(house_map)

for i in range(0, len(price_group4_subset)):
    
    popup_text = '<b> Price: ${:,} </b> <br> House area (sqft): {:,} <br> Lot area (sqft): {:,}'
    popup_text = popup_text.format(
        price_group4_subset['price'].iloc[i],
        price_group4_subset['sqft_living'].iloc[i],
        price_group4_subset['sqft_lot'].iloc[i]
        )
    
    iframe = folium.IFrame(popup_text, width = 175, height = 100)
    
    folium.CircleMarker(
        location = [price_group4_subset.iloc[i]['lat'], 
                    price_group4_subset.iloc[i]['long']],
        popup = folium.Popup(iframe),
        radius = 1,
        opacity = 0.75,
        color = 'blue').add_to(price_group4)
price_group4.add_to(house_map)

for i in range(0, len(price_group5_subset)):
    
    popup_text = '<b> Price: ${:,} </b> <br> House area (sqft): {:,} <br> Lot area (sqft): {:,}'
    popup_text = popup_text.format(
        price_group5_subset['price'].iloc[i],
        price_group5_subset['sqft_living'].iloc[i],
        price_group5_subset['sqft_lot'].iloc[i]
        )
    
    iframe = folium.IFrame(popup_text, width = 175, height = 100)
    
    folium.CircleMarker(
        location = [price_group5_subset.iloc[i]['lat'], 
                    price_group5_subset.iloc[i]['long']],
        popup = folium.Popup(iframe),
        radius = 1,
        opacity = 0.75,
        color = 'navy').add_to(price_group5)
price_group5.add_to(house_map)

# add option to toggle the layers (different price groups) on the map,
# the colour and price range of each price group is shown too
folium.LayerControl(position = 'bottomleft').add_to(house_map)

# when writing the script on Spyder, map has to be saved to your working
# directory - it can then be opened in a browser
#house_map.save('house_map.html')


## Multiple linear regression models

# first remove unnecessary variables ('id', 'date' and 'zipcode')
model_data = house_data.drop(columns = ['id', 'date', 'zipcode'])

# log transform some variables using NumPy
model_data['price'] = np.log(model_data['price'])
model_data['sqft_living'] = np.log(model_data['sqft_living'])
model_data['sqft_lot'] = np.log(model_data['sqft_lot'])
model_data['sqft_above'] = np.log(model_data['sqft_above'])
model_data['sqft_basement'] = np.log(model_data['sqft_basement'] + 1)
model_data['sqft_living15'] = np.log(model_data['sqft_living15'])
model_data['sqft_lot15'] = np.log(model_data['sqft_lot15'])

# create a way to track model stats, all values are initially nan but these 
# will be replaced when then test MSE's are calculated
# panda's DataFrame replacing matrix() in R
model_stats = pd.DataFrame(columns = ['Test MSE'],
                           index = [
                               'Best subset selection',
                               'Forward stepwise selection', 
                               'Backward stepwise selection',
                               'Ridge regression', 
                               'Lasso regression', 'PCR', 'PLS', 
                               'Single regression tree', 'Bagging', 
                               'Random forest', 'Boosting'
                               ]
                           )

# denote the x (predictors) and y (response) variables, for train and test set
# training set will be 75% of original dataset, test set is 25%
x = model_data.drop('price', axis = 1)
y = pd.DataFrame(model_data.price)
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.25,
                                                    random_state = 35)


## Best subset selection using cross-validation (takes a while to run)

# EFS will consider every possible combination of features
# the 'best' number of features is determined by 10-fold cross-validation,
# the mean squared error on the held-out fold
lm = LinearRegression()
efs = EFS(lm, min_features = 1, max_features = 17, 
          scoring = 'neg_mean_squared_error', cv = 10)

# fit to training data
efs.fit(x_train, y_train)
efs.best_feature_names_
# 16 features selected, only 'long' was not selected

# use the selected features to predict values in test set
x_train_efs = efs.transform(x_train)
x_test_efs = efs.transform(x_test)
lm_house = lm.fit(x_train_efs, y_train)
y_pred = lm_house.predict(x_test_efs)

# calculate test set MSE
lm_test_MSE = float(np.mean((y_test - y_pred)**2))
print('Test set MSE: %.5f' % lm_test_MSE)
# test set MSE: 0.06352

# extract stats of efs
efs_stats = pd.DataFrame.from_dict(efs.get_metric_dict()).T
# dataframe to add the test mse and standard errors for the best model at
# each size
efs_best_models = pd.DataFrame(columns = ['CV_score', 'std_error'])

# for each sized model
for i in range(1, 17, 1):

    indexes = [] # list to track the indexes in efs_stats of the models with
    # size i
    
    # iterate over all the models in efs_stats
    for j in range(0, len(efs_stats)):
        
        # if the length of the model is equal to i
        if len(efs_stats['feature_names'][j]) == i:
            
            # add the index (j) to indexes
            indexes.append(j)
    
    # extract the index of the model with the lowest cross val score
    min_avg_score_index = efs_stats[min(indexes):max(indexes)]['avg_score'].astype(float).idxmin(axis = 1)
    
    # use that index to extract the cv_score and std_error
    efs_best_models.at[i, 'CV_score'] = np.square(efs_stats['avg_score'][min_avg_score_index])
    efs_best_models.at[i, 'std_error'] = efs_stats['std_err'][min_avg_score_index]

# change columns to floats
efs_best_models = efs_best_models.astype(float)

# plot scores with std errors
plot_bss_cv_theme = theme(axis_title = element_text(size = 22.5),
                          axis_text = element_text(size = 20))
plot_bss_cv = (ggplot(data = efs_best_models, 
                      mapping = aes(x = np.array(range(1, 17, 1)), 
                                    y = 'CV_score')) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(mapping = aes(ymin = efs_best_models['CV_score'] - 
                              efs_best_models['std_error'],
                              ymax = efs_best_models['CV_score'] + 
                              efs_best_models['std_error'])) +
  
  # add standard error bars
  geom_hline(yintercept = 0.00994000519371302, linetype = 'dashed',
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_bss_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(breaks = np.array(np.arange(0, 0.09, 0.01)),
                     labels = np.array(np.arange(0, 0.09, 0.01)))
  # change y-axis limits
  )
plot_bss_cv

# add test set MSE to matrix
model_stats.loc['Best subset selection', 'Test MSE'] = lm_test_MSE
# for reference, this test MSE is very similar to the number in R


## Forward stepwise selection
# taken and adapted from:
# http://www.science.smith.edu/~jcrouser/SDS293/labs/lab9-py.html

# using 10-fold cross-validation, so set that up first
k = 10
np.random.seed(seed = 35)
folds = np.random.choice(k, size = len(y), replace = True)

# dataframe to track cross-validation errors
crossVal_errors_fwd = pd.DataFrame(
    columns = range(1, k + 1),
    index = range(1, 18))

# function to fit a linear model
def processSubset(features, X_train, Y_train, X_test, Y_test):
    # fit to training set
    model = sm.OLS(Y_train, X_train[list(features)])
    model_fit = model.fit()
    
    # calculate test set MSE by using the fitted model to make predictions on 
    # the test set
    predictions = model_fit.predict(X_test[list(features)])
    test_MSE = np.mean((Y_test.subtract(predictions, axis = 0))**2)
    test_MSE = test_MSE.tolist()

    return {'Model': model_fit, 'Test MSE': test_MSE}

# function to perform forward stepwise selection
def forward_stepwise_selection(predictors, X_train, Y_train, X_test, Y_test):
    results = [] # empty for now
    
    # extract predictors that still need to be checked
    # checks if each predictor in x_train is also not in predictors
    remaining_predictors = [p for p in X_train.columns if p not in predictors]
    
    # for each of the remaining predictors
    for p in remaining_predictors:
        
        # call processSubset to fit a linear model using predictors + each
        # of the remaining predicotrs
        results.append(processSubset(predictors + [p], 
                                     X_train, Y_train, X_test, Y_test))
        
    # turn results into a dataframe
    models = pd.DataFrame(results)
    
    # choose the best model (with the lowest test set MSE)    
    models.sort_values('Test MSE', inplace = True, axis = 0)
    best_model = models[:1]

    return best_model
    
# now write a for loop performing cross-validation
# in the ith fold, the elements of folds that equal i are in the test set and 
# the remainder are in the training
# dataframe to track the cross_validation errors for each model size
models_crossVal_errors_fwd = pd.DataFrame(columns = ['Model', 'Test MSE'])

# iterate over each fold
for i in range(1, k + 1):
    
    # reset predictors
    predictors = []
    
    # iterate over each model size
    for j in range(1, len(x.columns) + 1):
        
        # create train sets using all but fold i and test sets using the
        # remaining fold i
        x_train = x[folds != (i - 1)]
        y_train = y[folds != (i - 1)]
        x_test = x[folds == (i - 1)]
        y_test = y[folds == (i - 1)]
        
        # call forward_stepwise_selection, training on every fold except i
        # test on the ith fold
        fss = forward_stepwise_selection(
            predictors, x_train, y_train, x_test, y_test)
        
        # add model and test MSE for this fold i to models_crossVal_errors
        models_crossVal_errors_fwd = models_crossVal_errors_fwd.append(fss)

        # in order to add the test MSE for this model, the index of fss needs
        # to be called but this changes over time, so the following code
        # extracts the relevant index
        # convert column to series
        fss_test_MSE_column = pd.Series(fss['Test MSE'])
        # extract index value as a list
        fss_test_MSE_index = fss.index.values.tolist()
        # convert element in fss_index to str
        index_str = [str(i) for i in fss_test_MSE_index]
        fss_index = str(''.join(index_str))                                    
         
        # add test MSE for this model size (j) and fold (i) to 
        # crossVal_errors_fwd - fss_index is finally converted to int
        crossVal_errors_fwd.at[j, i] = pd.Series(fss['Test MSE'][int(fss_index)])

        # extract predictors
        predictors = list(fss['Model'])[0].model.exog_names
        # this ensures that when the j loop runs again, the predictors start
        # with the best selection at the previous model size
       
# this results in a matrix of test MSE, where the (i,j)th element is 
# equal to the test MSE for the ith cross-validation fold for the best
# j-variable model
# obtain a vector for which the jth element is the cross-validation
# error for the j-variable model by averaging all the errors for that size
crossVal_errors_fwd_mean = crossVal_errors_fwd.apply(np.mean, axis = 1)

# standard error of each model size test MSE (standard deviation of test set
# MSE divided by the square root of the number of folds) before mean is found
crossVal_errors_fwd_se = pd.DataFrame(crossVal_errors_fwd_mean,
                                      columns = ['Test MSE'])
crossVal_errors_fwd_se['SE'] = crossVal_errors_fwd.sem(axis = 1)

# plot
plot_fwd_cv_theme = theme(axis_title = element_text(size = 22.5),
                          axis_text = element_text(size = 20))
plot_fwd_cv = (ggplot(data = crossVal_errors_fwd_se, 
                      mapping = aes(x = np.array(range(1, 18, 1)), 
                                    y = 'Test MSE')) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(mapping = aes(ymin = crossVal_errors_fwd_se['Test MSE'] - 
                              crossVal_errors_fwd_se['SE'],
                              ymax = crossVal_errors_fwd_se['Test MSE'] + 
                              crossVal_errors_fwd_se['SE'])) +
  
  # add standard error bars
  geom_hline(yintercept = 0.0659094255830648, linetype = 'dashed',
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_fwd_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(limits = np.array([0.05, 0.25])) 
  # change y-axis limits
  )
plot_fwd_cv
# the 12 variable model is the simplest model that has a test MSE within one
# standard error of the best model (16 variables)

# therefore add the test set MSE of the 12 variable model to the matrix
model_stats.loc['Forward stepwise selection', 
                'Test MSE'] = crossVal_errors_fwd_se['Test MSE'][12]


## Backwards stepwise selection
# taken and adapted from:
# http://www.science.smith.edu/~jcrouser/SDS293/labs/lab8-py.html

# using 10-fold cross-validation, so set that up first
k = 10
np.random.seed(seed = 35)
folds = np.random.choice(k, size = len(y), replace = True)

# dataframe to track cross-validation errors
crossVal_errors_bwd = pd.DataFrame(
    columns = range(1, k + 1),
    index = range(1, 18)
    )

# function to perform backward stepwise selection
def backward_stepwise_selection(predictors, X_train, Y_train, X_test, Y_test):
    results = [] # empty for now
    
    # for each combination of predictors (up to 16)
    for combo in itertools.combinations(predictors, len(predictors) - 1):
            # call processSubset on each combination and append to results
            results.append(processSubset(combo, X_train, Y_train, X_test, Y_test))
        
    # turn results into a dataframe
    models = pd.DataFrame(results)
    
    # choose the best model (with the lowest test set MSE)    
    models.sort_values('Test MSE', inplace = True, axis = 0)
    best_model = models[:1]

    return best_model

models_crossVal_errors_bwd = pd.DataFrame(columns = ['Model', 'Test MSE'])

# iterate over each fold
for i in range(1, k + 1):
    
    predictors = x_train.columns
    
    # iterate over each model size
    while(len(predictors) > 1):

        # create train sets using all but fold i and test sets using the
        # remaining fold i
        x_train = x[folds != (i - 1)]
        y_train = y[folds != (i - 1)]
        x_test = x[folds == (i - 1)]
        y_test = y[folds == (i - 1)]
        
        # call backward_stepwise_selection, training on every fold except i
        # test on the ith fold
        bss = backward_stepwise_selection(
            predictors, x_train, y_train, x_test, y_test)
        
        # add model and test MSE for this fold i to models_crossVal_errors
        models_crossVal_errors_bwd = models_crossVal_errors_bwd.append(bss)

        # in order to add the test MSE for this model, the index of bss needs
        # to be called but this changes over time, so the following code
        # extracts the relevant index
        # convert column to series
        bss_test_MSE_column = pd.Series(bss['Test MSE'])
        # extract index value as a list
        bss_test_MSE_index = bss.index.values.tolist()
        # convert element in fss_index to str
        index_str = [str(i) for i in bss_test_MSE_index]
        bss_index = str(''.join(index_str))                                    
         
        # add test MSE for this model size (j) and fold (i) to 
        # crossVal_errors_bwd - bss_index is finally converted to int
        crossVal_errors_bwd.at[(len(predictors) - 1), i] = pd.Series(
            bss['Test MSE'][int(bss_index)])

        # extract predictors        
        predictors = list(bss['Model'])[0].model.exog_names
        
# note that this means a 17 variable model is never looked at, but the point
# of this is that it should be a subset of predictors anyway, so I'm content
# with leaving the 17 variable model empty (best subset selection identified
# that the 17 variable was not the best anyway)

# this results in a matrix of test MSE, where the (i,j)th element is 
# equal to the test MSE for the ith cross-validation fold for the best
# j-variable model
# obtain a vector for which the jth element is the cross-validation
# error for the j-variable model by averaging all the errors for that size
crossVal_errors_bwd_mean = crossVal_errors_bwd.apply(np.mean, axis = 1)

# standard error of each model size test MSE (standard deviation of test set
# MSE divided by the square root of the number of folds) before mean is found
crossVal_errors_bwd_se = pd.DataFrame(crossVal_errors_bwd_mean,
                                      columns = ['Test MSE'])
crossVal_errors_bwd_se['SE'] = crossVal_errors_bwd.sem(axis = 1)

plot_bwd_cv_theme = theme(axis_title = element_text(size = 22.5),
                          axis_text = element_text(size = 20))
plot_bwd_cv = (ggplot(data = crossVal_errors_bwd_se, 
                      mapping = aes(x = np.array(range(1, 18, 1)), 
                                    y = 'Test MSE')) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_errorbar(mapping = aes(ymin = crossVal_errors_bwd_se['Test MSE'] - 
                              crossVal_errors_bwd_se['SE'],
                              ymax = crossVal_errors_bwd_se['Test MSE'] + 
                              crossVal_errors_bwd_se['SE'])) +
  
  # add standard error bars
  geom_hline(yintercept = 0.06589432278232081, linetype = 'dashed',
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Model Size') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_bwd_cv_theme + # change the size of axis titles and axis text
  scale_y_continuous(limits = np.array([0.05, 0.3])) 
  # change y-axis limits
  )
plot_bwd_cv
# the 12 variable model is the simplest model that has a test MSE within one
# standard error of the best model (16 variables)

# therefore add the test set MSE of the 12 variable model to the matrix
model_stats.loc['Backward stepwise selection', 
                'Test MSE'] = crossVal_errors_fwd_se['Test MSE'][12]


## Ridge regression
# from: https://nbviewer.jupyter.org/github/JWarmenhoven/ISL-python/blob/master/Notebooks/Chapter%206.ipynb

# set grid of alpha values
grid = 10**np.linspace(10, -2, 100)*0.5

# perform ridge regression with 10-fold cross-validation to find the best alpha,
# scoring is mean squared error (MSE)
ridge_model = RidgeCV(alphas = grid, scoring = 'neg_mean_squared_error', cv = 10)
# fit to training data
ridge_model.fit(x_train, y_train)
# extract best alpha
ridge_best_alpha = ridge_model.alpha_

# new ridge model with best alpha
ridge2 = Ridge(alpha = ridge_best_alpha)
# fit to training data
ridge2.fit(x_train, y_train)
# predict on test data
mean_squared_error(y_test, ridge2.predict(x_test))
# best so far and broadly similar to R results

# add test MSE to matrix
model_stats.loc['Ridge regression', 
                'Test MSE'] = mean_squared_error(y_test, ridge2.predict(x_test))


## Lasso regression
# from: https://nbviewer.jupyter.org/github/JWarmenhoven/ISL-python/blob/master/Notebooks/Chapter%206.ipynb

# perform lasso regression with 10-fold cross-validation to find the best alpha,
# 'random_state = 35' ensures reproducible results
lasso_model = LassoCV(alphas = grid, cv = 10, random_state = 35)
# fit to training data
lasso_model.fit(x_train, y_train.values.ravel())
# extract best alpha
lasso_best_alpha = lasso_model.alpha_

# new ridge model with best alpha
lasso2 = Lasso(alpha = lasso_best_alpha)
# fit to training data
lasso2.fit(x_train, y_train)
# predict on test data
mean_squared_error(y_test, lasso2.predict(x_test))
# worse than ridge and best subset but better than forward and backward
# similar to R

# add test MSE to matrix
model_stats.loc['Lasso regression', 
                'Test MSE'] = mean_squared_error(y_test, lasso2.predict(x_test))


## Principal components regression
# from: https://nbviewer.jupyter.org/github/JWarmenhoven/ISL-python/blob/master/Notebooks/Chapter%206.ipynb

pca = PCA()
lm = LinearRegression()
# scale the predictors
x_transformed = pca.fit_transform(scale(x_train))
n = len(x_transformed)

# using 10-fold cross-validation - different way of choosing folds so not
# necessarily comparable with the other method, but it shouldn't make too
# much difference
folds2 = KFold(n_splits = 10, shuffle = True, random_state = 35)

# track MSE
pcr_mse = []

# calculate MSE for the intercept
pcr_intercept = -1*cross_val_score(lm, np.ones((n, 1)), y_train, cv = folds2,
                                   scoring = 'neg_mean_squared_error').mean()
pcr_mse.append(pcr_intercept)

# now calculate MSE cross-validation for all 17 principal components
for i in range(1, 18, 1):
    pcr_value = -1*cross_val_score(lm, x_transformed[:, :i], y_train, 
                                   cv = folds2,
                                   scoring = 'neg_mean_squared_error').mean()
    pcr_mse.append(pcr_value)


# plot
pcr_mse = pd.DataFrame(pcr_mse)
plot_pcr_cv_theme = theme(axis_title = element_text(size = 22.5),
                          axis_text = element_text(size = 20))
plot_pcr_cv = (ggplot(data = pcr_mse, 
                      mapping = aes(x = np.array(range(0, 18, 1)), 
                                    y = pcr_mse)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = min(pcr_mse[0]), linetype = 'dashed',
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Number of Principal Components') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_pcr_cv_theme # change the size of axis titles and axis text
  )
plot_pcr_cv

# the lowest test MSE is with all 17 principal components (therefore it is
# essentially least squares)
# transform test data
x_transformed_test = pca.transform(scale(x_test))[:, :18]
# train on training data
lm = LinearRegression()
lm.fit(x_transformed[:, :18], y_train)
# test on test data
pcr_predictions = lm.predict(x_transformed_test)
mean_squared_error(y_test, pcr_predictions)

# add test MSE to matrix
model_stats.loc['PCR', 'Test MSE'] = mean_squared_error(y_test, pcr_predictions)
# second best so far
# similar value to R, but was one of the worst overall in R


## Principal least squares
# from: https://nbviewer.jupyter.org/github/JWarmenhoven/ISL-python/blob/master/Notebooks/Chapter%206.ipynb

n = len(x_train)

# using 10-fold cross-validation 
folds2 = KFold(n_splits = 10, shuffle = True, random_state = 35)

# track MSE
pls_mse = []

# now calculate MSE cross-validation for all 17 principal components
for i in range(1, 18, 1):
    pls = PLSRegression(n_components = i)
    pls_value = cross_val_score(pls, scale(x_train), y_train, cv = folds2,
                                scoring = 'neg_mean_squared_error').mean()
    pls_mse.append(pls_value)

# plot
pls_mse = pd.DataFrame(pls_mse)
pls_mse[0] = pls_mse[0]**2
plot_pls_cv_theme = theme(axis_title = element_text(size = 22.5),
                          axis_text = element_text(size = 20))
plot_pls_cv = (ggplot(data = pls_mse, 
                      mapping = aes(x = np.array(range(0, 17, 1)), 
                                    y = pls_mse)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = min(pls_mse[0]), linetype = 'dashed',
             colour = 'red') + # add line denoting the standard error
  # above the best model
  theme_classic() + # white background, no gridlines
  xlab('Number of Principal Components') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_pls_cv_theme # change the size of axis titles and axis text
  )
plot_pls_cv

# the lowest test MSE is with all 17 principal components but there is very
# little change after 4, so I will stick with 4
pls = PLSRegression(n_components = 4)
pls.fit(scale(x_train), y_train)
mean_squared_error(y_test, pls.predict(scale(x_test)))

# add test MSE to matrix
model_stats.loc['PLS', 'Test MSE'] = mean_squared_error(
    y_test, pls.predict(scale(x_test))
    )
# almost identical to PCR but slightly better
# similar value to R


## Single regression tree
# ccp_alpha is the complexity parameter used to determine pruning the tree
# vector of alpha values will be created and used
ccp_alphas = np.array(list(np.arange(0, 0.01, 0.0005)), 
                      dtype = 'float64')

# dataframe to track test MSE for the cross-validation using different alphas
single_tree_stats = pd.DataFrame(columns = ccp_alphas, 
                                 index = list(range(1, 11, 1)))

# iterate over each fold
for i in range(1, k + 1):
    
    # create train sets using all but fold i and test sets using the
    # remaining fold i
    x_train = x[folds != (i - 1)]
    y_train = y[folds != (i - 1)]
    x_test = x[folds == (i - 1)]
    y_test = y[folds == (i - 1)]
    
    # using each alpha    
    for ccp_alpha in ccp_alphas:
        # create tree - i have (arbitrarily) chosen a max depth of 3
        tree = DecisionTreeRegressor(random_state = 35, max_depth = 3,
                                     ccp_alpha = ccp_alpha)
        # fit to training values
        tree.fit(x_train, y_train)
        # create predictions using test set
        tree_predict = tree.predict(x_test)
        # calculate test set MSE
        tree_test_mse = mean_squared_error(y_test, tree_predict)
        # add to dataframe
        single_tree_stats.loc[i, ccp_alpha] = tree_test_mse

# find the mean test set MSE for each alpha
tree_mean = single_tree_stats.apply(np.mean, axis = 0)

# plot
plot_tree_cv_theme = theme(axis_title = element_text(size = 22.5),
                           axis_text = element_text(size = 20))
plot_tree_cv = (ggplot(data = pd.DataFrame(tree_mean), 
                      mapping = aes(x = ccp_alphas, 
                                    y = tree_mean)) +
  geom_point() + # plot points
  geom_line() + # join points with line
  geom_hline(yintercept = min(tree_mean[0:]), linetype = 'dashed',
             colour = 'red') + # add line denoting the lowest test set MSE
  theme_classic() + # white background, no gridlines
  xlab('Alpha') + # change x axis label
  ylab('Test MSE') + # change y axis label
  plot_tree_cv_theme + # change the size of axis titles and axis text
  scale_x_continuous(breaks = np.array(np.arange(0, 0.0125, 0.0025)),
                     labels = np.array(np.arange(0, 0.0125, 0.0025)),
                     limits = np.array([0, 0.01])) +
  scale_y_continuous(breaks = np.array(np.linspace(0.05, 0.11, 5)),
                     labels = np.array(np.linspace(0.05, 0.11, 5)),
                     # using linspace because arange was outputting floats
                     # with a large number of decimal places, linspace stops
                     # this from happening
                     limits = np.array([0.05, 0.11]))
  # change x and y axis labels
  )
plot_tree_cv

# therefore the best tree is unpruned (because lowest test set MSE with alpha
# of 0)

# fit a tree to all training data - again going to use a max depth of 3
# i have tried using everything default, but the tree is massive and illegible
single_tree = DecisionTreeRegressor(random_state = 35, max_depth = 3,
                                    ccp_alpha = 0)
# fit to training values
single_tree.fit(x_train, y_train)
# create predictions using test set
single_tree_predict = single_tree.predict(x_test)
# calculate test set MSE
single_tree_test_mse = mean_squared_error(y_test, single_tree_predict)
single_tree_test_mse
plot_tree(single_tree)

# add test MSE to matrix
model_stats.loc['Single regression tree', 'Test MSE'] = single_tree_test_mse
# worst so far
# was one of the worst in R


## Bagging
# bagging is a special case of a random forest, where the subset of predictors
# used is equal to the number of predictors - max_features = 17 denotes this
bag_model = RandomForestRegressor(max_features = 17, random_state = 35)
# fit to training data
bag_model.fit(x_train, y_train.values.ravel())
# make predictions using test data
bag_predict = bag_model.predict(x_test)
# calculate test set MSE
bag_test_set_mse = mean_squared_error(y_test, bag_predict)

# add test MSE to matrix
model_stats.loc['Bagging', 'Test MSE'] = bag_test_set_mse
# twice as good as the next best
# similar jump in improvemet to R


## Random forest
# growing a random forest is similar, but a lower value of mtry is used
# a value of 5 is used here, the same number i used in R
rf_model = RandomForestRegressor(max_features = 5, random_state = 35)
# fit to training data
rf_model.fit(x_train, y_train.values.ravel())
# make predictions using test data
rf_predict = rf_model.predict(x_test)
# calculate test set MSE
rf_test_set_mse = mean_squared_error(y_test, rf_predict)

# extract importance of each variable
rf_var_importance = pd.DataFrame({'Importance': rf_model.feature_importances_}, 
                                 index = x_train.columns)

# plot
plot_rf_var_importance_theme = theme(axis_title = element_text(size = 20),
                                     axis_text = element_text(size = 17.5))
plot_rf_var_importance = (ggplot(data = rf_var_importance,
                                 mapping = aes(x = rf_var_importance.index,
                                               y = rf_var_importance)) +
  geom_col() + # plot bars
  theme_classic() + # white background, no gridlines
  xlab('Variable') + # change x axis label
  ylab('Importance of Variable') + # change y axis label
  plot_rf_var_importance_theme + # change the size of axis titles and axis text
  coord_flip() # flips to put variables on y axis, improving readability
  )
plot_rf_var_importance


# add test MSE to matrix
model_stats.loc['Random forest', 'Test MSE'] = rf_test_set_mse
# slightly worse than bagging, but still far better than the rest
# random forest was slightly better than bagging in R, but not by much 


## Boosting
# create a dictionary for the different values for the different parameters
parameters = {'Learning rate': [0.001, 0.01, 0.1],
              'Depth': [1, 2, 3, 4, 5],
              'Min. Node Obs.': [5, 10, 15],
              'Bag fraction': [0.65, 0.8, 1]}
# ParameterGrid creates dictionaries for each of the different combinations
# of parameters
parameter_grid = ParameterGrid(parameters)

# dataframe to track test MSE
boost_stats = pd.DataFrame(columns = ['Test MSE']) 

# iterate over each combination of parameters
for i in range(1, len(list(parameter_grid))):
    
    # create boosted model using parameters at i
    boost_model = GradientBoostingRegressor(
        learning_rate = list(parameter_grid)[i]['Learning rate'],
        subsample = list(parameter_grid)[i]['Bag fraction'], # enables
        # stochastic gradient boosting
        min_samples_leaf = list(parameter_grid)[i]['Min. Node Obs.'],
        max_depth = list(parameter_grid)[i]['Depth'],
        random_state = 35
        )
    
    # fit to training data
    boost_model.fit(x_train, y_train.values.ravel())
    # predict using training data
    boost_predict = boost_model.predict(x_test)
    # calculate test set MSE
    boost_test_set_mse = mean_squared_error(y_test, boost_predict)
    # add to dataframe
    boost_stats.loc[i, 'Test MSE'] = boost_test_set_mse

# lowest test set MSE is
min(boost_stats['Test MSE'])
# this was the last model used, which had the following parameters
list(parameter_grid)[134]
# won't be repeating this by using different parameter, but shown it can be
# in R and is possible if a it trickier in python

# add test MSE to matrix
model_stats.loc['Boosting', 'Test MSE'] = min(boost_stats['Test MSE'])
# clearly the best just as in R, very similar value to R
























