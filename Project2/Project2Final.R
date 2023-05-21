# Data science class project, posted with permission of instructor

# import libraries
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(GGally)
library(ggpairs)

# load dataset on home sales prices
data_raw = read_csv('train.csv')

# check dimensions
data_raw |> dim()
data_raw |> dplyr::glimpse()

# extract and rename relevant vars (reference:data_description.txt)
data = data_raw |> 
    select(Price = SalePrice,
           Lot = LotArea,
           Size = GrLivArea,
           Garage = GarageCars,
           Bedroom = BedroomAbvGr,
           Year = YearBuilt,
           Condition = OverallCond)

# histogram showing proportion of `Price` in thousands of USD
ggplot(data, aes(x=Price/1000, y=after_stat(density))) + 
    geom_histogram(bins=30, color="white", fill="blue") +
    scale_x_continuous(labels = dollar_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(title="Price distribution (in $ thousands)", x="Price", y="Density")

# convert categorical vars to factor
data$Garage = factor(data$Garage)
data$Bedroom = factor(data$Bedroom)
data$Condition = factor(data$Condition)

# scatterplot of `Price` in thousands of USD v.s. `Size` 
# to see nature of relationship (linear, positive)
ggplot(data) + geom_point(aes(x=Size, y=Price/1000)) + 
    scale_x_continuous(labels = comma_format()) +
    labs(title="House prices vs size", x="Size (square feet)",
         y = "Price (thousands of dollars)")

# check linearity of relationship within each Garage category
ggplot(data) + geom_point(aes(x=Size, y=Price/1000, color=as.factor(Garage))) +
    facet_grid(Garage ~1.) +
    scale_x_continuous(labels=comma_format()) +
    labs(title = 'House Prices vs. Size',
         x= 'Price (in thousands USD)',
         y = 'Size (in square feet)')

# observe relationships between variables to find continuous var with
# strongest relationship with Price 
ggpairs(data)
# it's Size (.709)

# estimate coefficients of price on size and adjusted R^2
size = lm(data = data, Price ~ Size)
summ <- summary(size)
summ
summ$adj.r.squared

# find strongest linear relationship with categorical vars
ggpairs(data, columns = c('Price', 'Garage', 'Bedroom','Condition'))
# number of garages

# estimate coefficients of price + garage on size and adjusted R^2
size_garage = lm(data = data, Price ~ Size + Garage)
summary(size_garage)

# scatterplot to see whether inclusion of interactions between 
#continuous and categorical variables will improve linear model prediction
data |> 
ggplot(aes(x=Size, y=Price, color=as.factor(Garage))) + 
    geom_point() +
    geom_smooth(method=lm, se=F) +
    scale_x_continuous(labels=comma_format()) +
    scale_y_continuous(labels=dollar_format()) +
    labs(title = 'House Prices vs. Size',
         x= 'Size (in square feet)',
         y = 'Price')

# estimate linear model with Garage as interaction term
sizexgarage <- lm(Price ~ Size*Garage, data=data)
summary(sizexgarage)

# estimate linear model with all variables
all = lm(data = data, Price ~ .)
summary(all)

# new house on market to predict price
new_house = data.frame(Size = 2000,
                       Lot = 10000, 
                       Year = 2015, 
                       Garage = as.factor(0),
                       Bedroom = as.factor(3), 
                       Condition = as.factor(6))

# predict price
price <- predict.lm(all, new_house)
price

# write house price prediction function that takes dataset with same columns
# as data and df with cols Size, Garage, Lot, Year, Bedroom, Condition
house_price = function(Data, House) {
    data = Data %>%
        # select relevant columns to match
        select(Price = SalePrice,
               Lot = LotArea,
               Size = GrLivArea,
               Garage = GarageCars,
               Bedroom = BedroomAbvGr,
               Year = YearBuilt,
               Condition = OverallCond)
    # convert categoricals to factors
    data$Garage = factor(data$Garage)
    data$Bedroom = factor(data$Bedroom)
    data$Condition = factor(data$Condition)
    # linear model
    full = lm(data = data, Price ~ .)
    House$Garage = factor(House$Garage)
    House$Bedroom = factor(House$Bedroom)
    House$Condition = factor(House$Condition)
    price = predict.lm(full, newdata = House)
    return(round(price,0))
}
# test function based on original raw dataset
house_test = data.frame(Size = 1500, Lot = 10000, Year = 1980, 
                        Garage = 2, Bedroom = 3, Condition = 5)
house_price(data_raw, house_test)
