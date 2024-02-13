---
title: "Homework 8"
author: Carmen Doria
date: "2024-02-13"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
```

## Install `here`
The package `here` is a nice option for keeping directories clear when loading files. I will demonstrate below and let you decide if it is something you want to use.  

```r
#install.packages("here")
```

## Data
For this homework, we will use a data set compiled by the Office of Environment and Heritage in New South Whales, Australia. It contains the enterococci counts in water samples obtained from Sydney beaches as part of the Beachwatch Water Quality Program. Enterococci are bacteria common in the intestines of mammals; they are rarely present in clean water. So, enterococci values are a measurement of pollution. `cfu` stands for colony forming units and measures the number of viable bacteria in a sample [cfu](https://en.wikipedia.org/wiki/Colony-forming_unit).   

This homework loosely follows the tutorial of [R Ladies Sydney](https://rladiessydney.org/). If you get stuck, check it out!  


```r
getwd()
```

```
## [1] "/Users/cmdoria/Desktop/BIS15W2024_cdoria/homework"
```


1. Start by loading the data `sydneybeaches`. Do some exploratory analysis to get an idea of the data structure.

```r
sydney_beaches <- read_csv("../lab9/data/sydneybeaches.csv") %>% clean_names()
```

```
## Rows: 3690 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): Region, Council, Site, Date
## dbl (4): BeachId, Longitude, Latitude, Enterococci (cfu/100ml)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

If you want to try `here`, first notice the output when you load the `here` library. It gives you information on the current working directory. You can then use it to easily and intuitively load files.

```r
#library(here)
```

The quotes show the folder structure from the root directory.

```r
#sydneybeaches <-read_csv(here("homework", "data", "sydneybeaches.csv")) %>% clean_names()
```

2. Are these data "tidy" per the definitions of the tidyverse? How do you know? Are they in wide or long format?

Yes the data is tidy, each column is representing a single variable, there are not various being represented in one. The data is in long format. 

3. We are only interested in the variables site, date, and enterococci_cfu_100ml. Make a new object focused on these variables only. Name the object `sydneybeaches_long`


```r
names(sydney_beaches)
```

```
## [1] "beach_id"              "region"                "council"              
## [4] "site"                  "longitude"             "latitude"             
## [7] "date"                  "enterococci_cfu_100ml"
```


```r
sydneybeaches_long <- sydney_beaches %>% 
  select("site", "date", "enterococci_cfu_100ml")
```


4. Pivot the data such that the dates are column names and each beach only appears once (wide format). Name the object `sydneybeaches_wide`

```r
sydneybeaches_wide <- sydneybeaches_long %>% 
  pivot_wider(names_from = date,
              values_from = enterococci_cfu_100ml)
```


5. Pivot the data back so that the dates are data and not column names.

```r
sydneybeaches_wide %>% 
  pivot_longer(-site,
               names_to = "date",
               values_to = "enterococci_cfu_100ml")
```

```
## # A tibble: 3,784 × 3
##    site           date       enterococci_cfu_100ml
##    <chr>          <chr>                      <dbl>
##  1 Clovelly Beach 02/01/2013                    19
##  2 Clovelly Beach 06/01/2013                     3
##  3 Clovelly Beach 12/01/2013                     2
##  4 Clovelly Beach 18/01/2013                    13
##  5 Clovelly Beach 30/01/2013                     8
##  6 Clovelly Beach 05/02/2013                     7
##  7 Clovelly Beach 11/02/2013                    11
##  8 Clovelly Beach 23/02/2013                    97
##  9 Clovelly Beach 07/03/2013                     3
## 10 Clovelly Beach 25/03/2013                     0
## # ℹ 3,774 more rows
```


6. We haven't dealt much with dates yet, but separate the date into columns day, month, and year. Do this on the `sydneybeaches_long` data.

```r
sydneybeaches_long <- sydneybeaches_long %>% 
  separate(date, into= c("day", "month", "year"), sep = "/")
```


7. What is the average `enterococci_cfu_100ml` by year for each beach. Think about which data you will use- long or wide.

```r
mean_sydney <- sydneybeaches_long %>% 
  group_by(site, year) %>% 
  summarise(mean_entero = mean(enterococci_cfu_100ml, na.rm = T)) %>% 
            arrange(desc(mean_entero))
```

```
## `summarise()` has grouped output by 'site'. You can override using the
## `.groups` argument.
```

8. Make the output from question 7 easier to read by pivoting it to wide format.

```r
mean_sydney %>% 
  pivot_wider(names_from = "year",
              values_from = "mean_entero")
```

```
## # A tibble: 11 × 7
## # Groups:   site [11]
##    site                    `2013` `2018` `2016` `2015` `2014` `2017`
##    <chr>                    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1 Little Bay Beach        122.    59.1    31.2  25.5   19.5   18.2 
##  2 South Maroubra Rockpool  96.4  112.     59.3  47.3   40.6   46.9 
##  3 Malabar Beach           101.    38.0    91.0  66.9   54.5   49.8 
##  4 Bronte Beach             26.8   43.4    61.3  23.6   17.5   16.8 
##  5 Coogee Beach             39.7   21.6    59.5  40.3   52.6   20.7 
##  6 Tamarama Beach           29.7   15.5    50.3  57.0   39.6   20.4 
##  7 Maroubra Beach           47.1    9.21   26.6  14.5    9.23  11.6 
##  8 South Maroubra Beach     39.3   12.5    10.7   8.25  14.9    8.26
##  9 Gordons Bay (East)       24.8   17.6    39.0  36.2   16.7   13.7 
## 10 Bondi Beach              32.2   22.9    19.4  14.3   11.1   13.2 
## 11 Clovelly Beach            9.28  10.6    11.3   8.82  13.8    7.93
```


9. What was the most polluted beach in 2013?
Little Bay Beach was the most polluted. 

```r
mean_sydney %>% 
  pivot_wider(names_from = "year",
              values_from = "mean_entero") %>%
  arrange(desc(2013))
```

```
## # A tibble: 11 × 7
## # Groups:   site [11]
##    site                    `2013` `2018` `2016` `2015` `2014` `2017`
##    <chr>                    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1 Little Bay Beach        122.    59.1    31.2  25.5   19.5   18.2 
##  2 South Maroubra Rockpool  96.4  112.     59.3  47.3   40.6   46.9 
##  3 Malabar Beach           101.    38.0    91.0  66.9   54.5   49.8 
##  4 Bronte Beach             26.8   43.4    61.3  23.6   17.5   16.8 
##  5 Coogee Beach             39.7   21.6    59.5  40.3   52.6   20.7 
##  6 Tamarama Beach           29.7   15.5    50.3  57.0   39.6   20.4 
##  7 Maroubra Beach           47.1    9.21   26.6  14.5    9.23  11.6 
##  8 South Maroubra Beach     39.3   12.5    10.7   8.25  14.9    8.26
##  9 Gordons Bay (East)       24.8   17.6    39.0  36.2   16.7   13.7 
## 10 Bondi Beach              32.2   22.9    19.4  14.3   11.1   13.2 
## 11 Clovelly Beach            9.28  10.6    11.3   8.82  13.8    7.93
```


10. Please complete the class project survey at: [BIS 15L Group Project](https://forms.gle/H2j69Z3ZtbLH3efW6)

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
