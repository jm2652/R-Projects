# Data science class project, posted with permission of instructor

# Load libraries
library(tidyverse)

# Load the 2019 Gallup World Poll dataset & inspect
happy <- read_csv("2019.csv")
happy |> head(3)

# rename columns
happy = happy %>%
    rename(Rank = 'Overall rank',
           Country = 'Country or region',
           Happiness = 'Score',
           GDP = 'GDP per capita',
           Support = 'Social support',
           Life = 'Healthy life expectancy',
           Freedom = 'Freedom to make life choices',
           Corruption = 'Perceptions of corruption')

# quick summary out of curiosity
happy |> 
    group_by(Corruption, Happiness) |> 
    summarize(AvgCorruption = mean(Corruption),
              AvgHappiness = mean(Happiness))

# histogram of happiness
ggplot(happy, aes(x=Happiness, y=..density..)) +
    geom_histogram(bins=40, 
                   color="white",
                   fill="blue") +
    geom_density(aes(x=Happiness), color="red") + 
    labs(title = "Happiness Density", x = "Happiness Scale 1-7", y = "Density")

# pivot to make boxplot
longhappy <- happy |> 
    pivot_longer(cols=c(GDP, Support, Life, Freedom, Generosity, Corruption), names_to = "Var", values_to= "Val")

ggplot(longhappy) + geom_boxplot(aes(x=Var, y=Val))

# check vars with highest IQR
longhappy |> group_by(Var) |> 
    summarize(IQR=IQR(Val), variance=var(Val)) |> 
    arrange(IQR)

# create function taking numerical vector to find outliers
find_outliers = function(x) {
    quartiles <- quantile(x, probs=c(0.25, 0.75))
    IQR <- diff(quartiles)
    limit = quartiles + c(-1,1)*1.5*IQR
    return(x<limit[1] | x>limit[2]) # return logical vector
}
# test
x = rt(50,1)
find_outliers(x)

# identify outlier countries Corruption & Freedom
happy |> filter(find_outliers(happy |> pull(Corruption))) |> select(Country)
happy |> filter(find_outliers(happy |> pull(Freedom))) |> select(Country)
# alternate way
# happy$Country[find_outliers(happy$Freedom)]

# create function taking numerical vector to find direction of outliers
find_outliers_direction = function(x){
    quartiles <- quantile(x, probs=c(0.25, 0.75))
    IQR <- diff(quartiles)
    limit = quartiles + c(-1,1)*1.5*IQR
    v = rep(0, length(x))
    v[x < limit[1]] = -1
    v[x > limit[2]] = 1
    return(v)
}
# test
x = rt(50,1)
find_outliers_direction(x)

# identify outliers for Generosity & Life
outliergen <- find_outliers_direction(happy |> pull(Generosity))
outlierlife <- find_outliers_direction(happy |> pull(Life))
happy |> mutate(Direction = outliergen) |> filter(outliergen!=0) |> select(Country, Direction) 
happy |> mutate(Direction = outlierlife) |> filter(outlierlife!=0) |> select(Country, Direction) 

# Add variable `Wealth`:
# * 'poor' if country GDP is below or equal to the first quartile
# * 'middle' country GDP is above the first quartile and below
#       or equal to the third quartile
# * 'rich' if the country GDP is above the third quartile
quartile <- quantile(happy$GDP)

happy <- happy |> mutate(Wealth = case_when((GDP <= quartile[2]) ~ 'poor',
                                            (quartile[2] < GDP) & (GDP <= quartile[4]) ~ 'middle',
                                            (GDP > quartile[4]) ~ 'rich'))
happy

# Calculate median and IQR for each level of wealth
happy |> group_by(Wealth) |> 
    summarize(MedHappiness=median(Happiness), IQRHappiness=IQR(Happiness))

# scatterplot of Happiness vs GDP colored by Wealth
ggplot(happy) +
    geom_point(mapping = aes(x=GDP,
                             y=Happiness,
                             color=as.factor(Wealth)),
               alpha=.5, size=3) +
    labs(title = "Happiness vs. GDP",
         x= "Happiness",
         y= "GDP",
         color="Wealth")

# Explore the impact of `Freedom` on `Happiness` for given level of `Wealth`

# Create categorical variable Freedom_cat` with values 'low', 'medium', and 'high
# depending on Freedom quartile
freequartile <- quantile(happy$Freedom)
freequartile
happy <-  happy |> group_by(Freedom) |> mutate(Freedom_cat=case_when(
    (Freedom <= freequartile[2]) ~ 'low',
    (Freedom > freequartile[2]) & (Freedom <= freequartile[4]) ~ 'medium',
    (Freedom > freequartile[4]) ~ 'high'))

# plot histograms for each level of wealth, filled by Freedom level
ggplot(happy) +
    facet_grid(Wealth ~ .) +
    geom_histogram(aes(x=Happiness, 
                       y=after_stat(density), 
                       fill=Freedom_cat),
                   position='identity',
                   color= "white")

# load country "thriving" index timeseries dataset
series <- read_csv("data_series.csv")
series |> head(3)

# find number unique countries
series$Geography[!is.na(series$Geography)] |> 
    unique() |> length()

# number of years of data for each country
years <- series |> group_by(Geography) |>
    select(Geography, Year) |>
    count()
years

# countries with max number of years
years |> filter(n == max(years[2])) |> nrow()

# countries with data for a single year
years |> filter(n == 1) |> select(Geography)

# construct wellbeing index ('WI') as 100 * `Thriving`
#  + 30 * `Struggling` + 5 * `Suffering`; add to dataset
series <- series |> mutate(WI= (Thriving*100 +Struggling*30 + Suffering*5))
series

# create percentage change function between
# last and first index of numeric vector
per_chg = function(x){
    # return percentage change from first numeric vector entry to last
    ((x[1] - x[length(x)]) / (x[length(x)]) * 100)
}
# test with vector
x = c(1,2,1.5,-0.5,-0.4)
per_chg(x)

# calculate the percentage change in `WI` between the first 
# and the last `Year` for each `Geography` in dataset
wi_chg <- series |> group_by(Geography) |> 
    summarize(PercentageChange = per_chg(WI))

# Make line plot of `WI` vs. `Year` for top 5 countries with
# max years in terms of `WI_perc`
range <- years |> filter(n == max(years$n, na.rm=TRUE)) |> 
    pull(Geography)
top5 <- wi_chg |> filter(Geography %in% range) |> 
    slice_max(PercentageChange, n=5) |> pull(Geography)

ggplot(series |> filter(Geography %in% top5)) + 
    geom_line(aes(x=Year, y=WI, color=Geography)) + 
    labs(title= "Wealth Index vs Year, Top 5 Countries",
         y = "Wealth Index")