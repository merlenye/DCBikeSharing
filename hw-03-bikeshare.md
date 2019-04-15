HW 03 - Bike rentals in DC
================
Merle Nye
27/02/2019

### Load packages and data

``` r
library(tidyverse)
```

    ## Warning: package 'tibble' was built under R version 3.5.2

``` r
library(broom)
```

``` r
bikeshare.day <- read_csv("data/bikeshare-day.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

### Question 1

To recode the `season` variable to have meaningful level names, first let's mutate each of the numbers to their correpsonding season name. Then, to recode the variable into a factor, let's use the factor function an establish the order of the factor through a vector that uses spring as the baseline.

``` r
bikeshare.day <- bikeshare.day %>%
  mutate(season = case_when(
    season == 1 ~ "winter", 
    season == 2 ~ "spring", 
    season == 3 ~ "summer", 
    season == 4 ~ "fall"
    ))
bikeshare.day$season <- factor(bikeshare.day$season, c("spring", "summer", "fall", "winter"))
```

### Question 2

To recode the binary variables `holiday` and `workingday` into meaningful variables with factors, we first use the mutate function to change the names for the binary values of 1 and 2 to yes and no. Then we use the factor function to set no as the baseline by putting it in the first index of the vector.

``` r
bikeshare.day <- bikeshare.day %>%
  mutate(workingday = case_when(
    workingday == 1 ~ "yes",
    workingday == 0 ~ "no"
  ))
bikeshare.day$workingday <- factor(bikeshare.day$workingday, c("no", "yes"))
bikeshare.day <- bikeshare.day %>%
  mutate(holiday = case_when(
    holiday == 1 ~ "yes",
    holiday == 0 ~ "no"
  ))
bikeshare.day$holiday <- factor(bikeshare.day$holiday, c("no", "yes"))
```

### Question 3

To recode the `yr` variable, we use the mutate function and provide case\_when to make sure we code for 2011 if the current value is zero and 2012 if the value is 1. We then use the factor function and include 2011 in the first index of the vector to make it the baseline.

``` r
bikeshare.day <- bikeshare.day %>%
   mutate(yr = case_when(
    yr == 1 ~ "2012",
    yr == 0 ~ "2011"
  ))
bikeshare.day$yr <- factor(bikeshare.day$yr, c("2011", "2012"))
```

### Question 4

To recode the `weathersit` variable with meaningful values, we use the mutate and case\_when functions to change the numeric values into their correpsonding weather patterns. We then establish them as a factor using the factor function with clear as the baseline.

``` r
bikeshare.day <- bikeshare.day %>%
  mutate( weathersit = case_when(
    weathersit == 1 ~"clear",
    weathersit == 2 ~"mist",
    weathersit == 3 ~ "light precipitation",
    weathersit == 4 ~ "heavy precipitation"
))
bikeshare.day$weathersit <- factor(bikeshare.day$weathersit, c("clear", "mist", "light precipitation", "heavy precipitation"))
```

### Question 5

To calculate the actual values for the variables listed, I used the mutate function and then multiplied `temp` by 41, `atemp` by 50, `hum` by 100 and `windspeed` by 67. I made new ciolumns to store the raw values of each unit

``` r
bikeshare.day <- bikeshare.day %>%
  mutate(raw_temp = temp * 41)%>%
  mutate(raw_feel = atemp * 50)%>%
  mutate(raw_humidity = hum * 100)%>%
  mutate(raw_windspeed = windspeed * 67)
```

### Question 6

In order to ensure the casual and registered sums added up to the total count, I used the mutate function to add a variable to the data frame of the sum of registered and casual and another variable indicating if the counts were equal for that given row. I think used the summarise function on the data frame and grouped by if the values were equal. I then checked that the sum of all the rows where the counts were equal was the sum of all rows in the original data frame.

``` r
summary <- bikeshare.day%>%
  mutate(both = casual + registered)%>%
  mutate(equality = (both == cnt))
teller <- summary%>%
  group_by(equality)%>%
  summarise(all = n())
teller$all == nrow(bikeshare.day)
```

    ## [1] TRUE

The sum of registered and casual bike rentals added up to the total amount of rows in the original data frame.

### Question 7

To recreate the plot, I used the ggplot function with the point geom. I used the date for my x axis points and the total number of bike rentals (`cnt`) for the y axis.

``` r
ggplot(data = bikeshare.day, aes(x = dteday, y = cnt, color = raw_temp))+
  geom_point(alpha = .7)+
  labs(x = "Date", y = "Bike rentals", title = "Bike rentals in DC, 2011 and 2012", subtitle = "Warmer temperatures associated with more bike rentals", color = "Temperature (C)")
```

![](hw-03-bikeshare_files/figure-markdown_github/recreate_plot-1.png)

This plot clearly shows the times with the highest rate or bike rentals are in the summer months in DC with the highest temperatures. There is a clear positive relationship between time and bike rentals, meaning bike rentals generally go up with time, but the days with higher temperatures clearly have the most bike rentals. There are several outliers in the plot, but these are usually hotter days in colder seasons that yield relatively more bike rentals temporarily.

### Question 8

To create a simple visualization for the relationship between bike rentals and `season`, I used the ggplot bar geom with the stat set to identity.

``` r
ggplot(data = bikeshare.day, aes(x = season, y =cnt))+
  geom_bar(stat = "identity")+
  labs(x = "Season", y = "Bike Rentals", title = "Number of bike rentals by season", subtitle = "Bike sharing more popular in summer and spring")+
  scale_y_continuous(limits = c(0, 1200000))
```

![](hw-03-bikeshare_files/figure-markdown_github/visualizing_rentals_and_season-1.png)

It's clear from this graph that the summer days, on average, have a much higher rate of bike rentals. Spring is the second most popular month for bike rentals. Fall is slightly less popular for renting bikes, and winter has almost half as many rentals as summer.

### Question 9

To fit a linear model to predict total bike rentals from `temp` , lets use the linear model function and use tidy to view the results.

``` r
total_vs_temp <- lm(cnt ~ temp, data = bikeshare.day )
tidy(total_vs_temp)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    1215.      161.      7.54 1.43e-13
    ## 2 temp           6641.      305.     21.8  2.81e-81

When the temperature is 0, the intercept is 1214.642 meaning we will expect 1214.642 bike rentals for days when temperature is 0. The slope is 6640.710 meaning that for every 41 degrees warmer a given day is (to account for the division of the values by 41), we will expect 6640.710 more bike rentals.

### Question 10

Let's fit a linear model predicting bike rentals for a given day based on what the temperature that day feels like.

``` r
total_vs_feel_temp <- lm(cnt ~ atemp, data = bikeshare.day )
tidy(total_vs_feel_temp)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     946.      171.      5.52 4.67e- 8
    ## 2 atemp          7502.      342.     22.0  1.85e-82

When the temperature is 0, the intercept is 945.824 meaning we will expect 945.824 bike rentals for days when temperature is 0. The slope is 7501.834 meaning that for every 50 (to account for divison of the values) degrees warmer a given day is, we will expect 7501.834 more bike rentals. For each degree warmer the day is, there will on average be 150 more bike rentals.

### Question 11

To fit a full model for the variables provided, let's using the linear model predicting total bike rentals for all values for the variables as well as an interaction between temperature and season. To view this model, let's use the tidy function to clearly distinguish the intercept and the slopes for the variables.

``` r
full_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + atemp + windspeed + temp * season, data = bikeshare.day)
tidy(full_model)
```

    ## # A tibble: 15 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                     2114.      296.     7.15   2.08e- 12
    ##  2 seasonsummer                    6086.      613.     9.93   7.65e- 22
    ##  3 seasonfall                      -760.      347.    -2.19   2.89e-  2
    ##  4 seasonwinter                   -1715.      314.    -5.46   6.51e-  8
    ##  5 yr2012                          2052.       56.4   36.4    8.34e-165
    ##  6 holidayyes                      -593.      173.    -3.42   6.56e-  4
    ##  7 workingdayyes                    113.       62.3    1.81   7.00e-  2
    ##  8 weathersitmist                  -762.       61.0  -12.5    1.61e- 32
    ##  9 weathersitlight precipitation  -2599.      173.   -15.0    2.12e- 44
    ## 10 temp                            4794.     1318.     3.64   2.96e-  4
    ## 11 atemp                            -77.2    1421.    -0.0543 9.57e-  1
    ## 12 windspeed                      -2257.      388.    -5.82   8.98e-  9
    ## 13 seasonsummer:temp              -8983.      907.    -9.90   9.30e- 22
    ## 14 seasonfall:temp                 2621.      705.     3.72   2.16e-  4
    ## 15 seasonwinter:temp               1801.      731.     2.46   1.40e-  2

### Question 12

To preform backward selection, we will use the glance function and get the adjusted r squared for the current model and then remove one variable each time and see if that model has a higher r squared than the original.

``` r
glance(full_model)$adj.r.squared
```

    ## [1] 0.8484678

Now let's remove the `season` variable.

``` r
szn_model <- lm(cnt ~ yr + holiday + workingday + weathersit + temp + atemp + windspeed, data = bikeshare.day)
glance(szn_model)$adj.r.squared
```

    ## [1] 0.7470387

Now let's remove the `yr` variable.

``` r
yr_model <- lm(cnt ~ season + holiday + workingday + weathersit + temp + atemp + windspeed + temp * season, data = bikeshare.day)
glance(yr_model)$adj.r.squared
```

    ## [1] 0.5692556

Now let's remove the `holiday` variable.

``` r
holiday_model <- lm(cnt ~ season + yr  + workingday + weathersit + temp + atemp + windspeed + temp * season, data = bikeshare.day)
glance(holiday_model)$adj.r.squared
```

    ## [1] 0.8462038

``` r
workingday_model<- lm(cnt ~ season + yr + holiday + weathersit + temp + atemp + windspeed + temp * season, data = bikeshare.day)
glance(workingday_model)$adj.r.squared
```

    ## [1] 0.847983

Now let's remove the `workingday` variable.

``` r
weather_model <- lm(cnt ~ season + yr + holiday + workingday + temp + atemp + windspeed + temp * season, data = bikeshare.day)
glance(weather_model)$adj.r.squared
```

    ## [1] 0.7787188

Now let's remove the `weathersit` variable.

``` r
temp_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + atemp + windspeed, data = bikeshare.day)
glance(temp_model)$adj.r.squared
```

    ## [1] 0.8126616

Now let's remove the `atemp` variable.

``` r
atemp_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + temp  + windspeed + temp * season, data = bikeshare.day)
glance(atemp_model)$adj.r.squared
```

    ## [1] 0.8486785

Now let's remove the `windspeed` variable.

``` r
wind_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + atemp + temp * season, data = bikeshare.day)
glance(wind_model)$adj.r.squared
```

    ## [1] 0.8415255

The Temperature with the adjusted temperature removed had the highest adjusted r squared value of 0.8486785. Now let's repeat the process to see if removing a variable from the atemp model will yield a higher r squared.

``` r
atemp_szn_model <- lm(cnt ~ yr + holiday + workingday + weathersit + temp  + windspeed, data = bikeshare.day)
glance(atemp_szn_model)$adj.r.squared
```

    ## [1] 0.7458343

``` r
atemp_yr_model <- lm(cnt ~ season + holiday + workingday + weathersit + temp  + windspeed + temp * season, data = bikeshare.day)
glance(atemp_yr_model)$adj.r.squared
```

    ## [1] 0.569446

``` r
atemp_holiday_model <- lm(cnt ~ season + yr + workingday + weathersit + temp  + windspeed + temp * season, data = bikeshare.day)
glance(atemp_holiday_model)$adj.r.squared
```

    ## [1] 0.8464175

``` r
atemp_workingday_model <- lm(cnt ~ season + yr + holiday + weathersit + temp  + windspeed + temp * season, data = bikeshare.day)
glance(atemp_workingday_model)$adj.r.squared
```

    ## [1] 0.8481931

``` r
atemp_weathersit_model <- lm(cnt ~ season + yr + holiday + workingday + temp  + windspeed, data = bikeshare.day)
glance(atemp_weathersit_model)$adj.r.squared
```

    ## [1] 0.7599187

``` r
atemp_temp_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + windspeed , data = bikeshare.day)
glance(atemp_temp_model)$adj.r.squared
```

    ## [1] 0.7508715

``` r
atemp_windspeed_model <- lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + temp * season, data = bikeshare.day)
glance(atemp_windspeed_model)$adj.r.squared
```

    ## [1] 0.8413982

``` r
biggest_r <- glance(atemp_model)$adj.r.squared
```

We can see the model without the adjusted temperature was the most effective model with an adjusted r squared of 0.8486785. Eliminating the remaining variables did nothing to increase the adjusted r squared and therefor offered no improvements, so the best model was the one that did not include normalized feeling temperature.

Now to view the intercept and slopes for all the variables, we will use the tidy function with this model.

``` r
tidy(atemp_model)
```

    ## # A tibble: 14 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                      2110.     282.       7.48 2.25e- 13
    ##  2 seasonsummer                     6087.     612.       9.95 6.33e- 22
    ##  3 seasonfall                       -760.     347.      -2.19 2.88e-  2
    ##  4 seasonwinter                    -1712.     310.      -5.53 4.51e-  8
    ##  5 yr2012                           2052.      56.3     36.4  3.52e-165
    ##  6 holidayyes                       -592.     173.      -3.42 6.51e-  4
    ##  7 workingdayyes                     113.      62.3      1.82 6.96e-  2
    ##  8 weathersitmist                   -762.      61.0    -12.5  1.46e- 32
    ##  9 weathersitlight precipitation   -2599.     173.     -15.0  1.67e- 44
    ## 10 temp                             4727.     461.      10.3  3.81e- 23
    ## 11 windspeed                       -2252.     378.      -5.96 3.91e-  9
    ## 12 seasonsummer:temp               -8984.     906.      -9.92 8.25e- 22
    ## 13 seasonfall:temp                  2621.     705.       3.72 2.15e-  4
    ## 14 seasonwinter:temp                1794.     719.       2.50 1.28e-  2

### Question 13

We can see from the holiday coeffecient that, on average, a day that is considered to be a holiday will see 592.41 less bike rentals than non-holidays. Holidays clearly, on average, lead to less bike rentals.

We can also see that compared to spring, fall days have an average of 760.1 less bike rentals per day and winter days have an average of 1712.3 less bike rentals per day. In summer, there are, on average, 6087.1 more bike rentals per day than in the spring. This makes sense given the earlier graphic comparing bike rentals to temperature. As warmer days have appear to have more bike rental, it makes sense that warmer seasons would appear to also have higher average bike rental rates.

### Question 14

The single biggest factor is the season the day falls under. If a day lands in summertime, bike rentals will be much more in demand. Similarly, warmer temperatures also make for a better day for bike sharing in DC, although the warmer days also tend to land in the summer. There will be more demand for bike rentals if it is a working day and not a holiday. It seems that people are more likely to rent bikes on days were life follows business as usual.
