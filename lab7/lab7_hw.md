---
title: "Lab 7 Homework"
author: "Carmen Doria"
date: "2024-02-01"
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
library(skimr)
```

For this assignment we are going to work with a large data set from the [United Nations Food and Agriculture Organization](http://www.fao.org/about/en/) on world fisheries. These data are pretty wild, so we need to do some cleaning. First, load the data.  

Load the data `FAO_1950to2012_111914.csv` as a new object titled `fisheries`.

```r
fisheries <- readr::read_csv(file = "data/FAO_1950to2012_111914.csv")
```

```
## Rows: 17692 Columns: 71
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (69): Country, Common name, ISSCAAP taxonomic group, ASFIS species#, ASF...
## dbl  (2): ISSCAAP group#, FAO major fishing area
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

1. Do an exploratory analysis of the data (your choice). What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables?  

```r
dim(fisheries)
```

```
## [1] 17692    71
```

```r
names(fisheries)
```

```
##  [1] "Country"                 "Common name"            
##  [3] "ISSCAAP group#"          "ISSCAAP taxonomic group"
##  [5] "ASFIS species#"          "ASFIS species name"     
##  [7] "FAO major fishing area"  "Measure"                
##  [9] "1950"                    "1951"                   
## [11] "1952"                    "1953"                   
## [13] "1954"                    "1955"                   
## [15] "1956"                    "1957"                   
## [17] "1958"                    "1959"                   
## [19] "1960"                    "1961"                   
## [21] "1962"                    "1963"                   
## [23] "1964"                    "1965"                   
## [25] "1966"                    "1967"                   
## [27] "1968"                    "1969"                   
## [29] "1970"                    "1971"                   
## [31] "1972"                    "1973"                   
## [33] "1974"                    "1975"                   
## [35] "1976"                    "1977"                   
## [37] "1978"                    "1979"                   
## [39] "1980"                    "1981"                   
## [41] "1982"                    "1983"                   
## [43] "1984"                    "1985"                   
## [45] "1986"                    "1987"                   
## [47] "1988"                    "1989"                   
## [49] "1990"                    "1991"                   
## [51] "1992"                    "1993"                   
## [53] "1994"                    "1995"                   
## [55] "1996"                    "1997"                   
## [57] "1998"                    "1999"                   
## [59] "2000"                    "2001"                   
## [61] "2002"                    "2003"                   
## [63] "2004"                    "2005"                   
## [65] "2006"                    "2007"                   
## [67] "2008"                    "2009"                   
## [69] "2010"                    "2011"                   
## [71] "2012"
```

```r
class(fisheries)
```

```
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

```r
any(is.na(fisheries))
```

```
## [1] TRUE
```

2. Use `janitor` to rename the columns and make them easier to use. As part of this cleaning step, change `country`, `isscaap_group_number`, `asfis_species_number`, and `fao_major_fishing_area` to data class factor. 

```r
fisheries <- clean_names(fisheries)
```


```r
fisheries <- fisheries %>% 
  mutate(across(c("country", "isscaap_group_number", "asfis_species_number", "fao_major_fishing_area"), factor))
```

Testing to see if it worked

```r
class(fisheries$country)
```

```
## [1] "factor"
```

We need to deal with the years because they are being treated as characters and start with an X. We also have the problem that the column names that are years actually represent data. We haven't discussed tidy data yet, so here is some help. You should run this ugly chunk to transform the data for the rest of the homework. It will only work if you have used janitor to rename the variables in question 2!  

```r
fisheries_tidy <- fisheries %>% 
  pivot_longer(-c(country,common_name,isscaap_group_number,isscaap_taxonomic_group,asfis_species_number,asfis_species_name,fao_major_fishing_area,measure),
               names_to = "year",
               values_to = "catch",
               values_drop_na = TRUE) %>% 
  mutate(year= as.numeric(str_replace(year, 'x', ''))) %>% 
  mutate(catch= str_replace(catch, c(' F'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('...'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('-'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('0 0'), replacement = ''))

fisheries_tidy$catch <- as.numeric(fisheries_tidy$catch)
```

3. How many countries are represented in the data? Provide a count and list their names.
203 countries are represented in the data. 

```r
fisheries_tidy %>% 
  count(country)
```

```
## # A tibble: 203 × 2
##    country                   n
##    <fct>                 <int>
##  1 "Saint Barth\xe9lemy"     6
##  2 "R\xe9union"            736
##  3 "C\xf4te d'Ivoire"     1859
##  4 "Cura\xe7ao"             18
##  5 "Albania"               934
##  6 "Algeria"              1561
##  7 "American Samoa"        556
##  8 "Angola"               2119
##  9 "Anguilla"              129
## 10 "Antigua and Barbuda"   356
## # ℹ 193 more rows
```

4. Refocus the data only to include country, isscaap_taxonomic_group, asfis_species_name, asfis_species_number, year, catch.

```r
names(fisheries_tidy)
```

```
##  [1] "country"                 "common_name"            
##  [3] "isscaap_group_number"    "isscaap_taxonomic_group"
##  [5] "asfis_species_number"    "asfis_species_name"     
##  [7] "fao_major_fishing_area"  "measure"                
##  [9] "year"                    "catch"
```


```r
fisheries_tidy %>% 
  select(country, isscaap_taxonomic_group, asfis_species_name, asfis_species_number, year, catch)
```

```
## # A tibble: 376,771 × 6
##    country isscaap_taxonomic_group asfis_species_name asfis_species_number  year
##    <fct>   <chr>                   <chr>              <fct>                <dbl>
##  1 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1995
##  2 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1996
##  3 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1997
##  4 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1998
##  5 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1999
##  6 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2000
##  7 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2001
##  8 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2002
##  9 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2003
## 10 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2004
## # ℹ 376,761 more rows
## # ℹ 1 more variable: catch <dbl>
```

5. Based on the asfis_species_number, how many distinct fish species were caught as part of these data?
1551 distinct species were caught. 

```r
fisheries_tidy %>% 
  summarise(n_asfis_species_number = n_distinct(asfis_species_number),
            total = n())
```

```
## # A tibble: 1 × 2
##   n_asfis_species_number  total
##                    <int>  <int>
## 1                   1551 376771
```

6. Which country had the largest overall catch in the year 2000?
China with a catch of 9068. 

```r
fisheries_tidy %>% 
  group_by(country, year) %>%
  summarise(max_catch = max(catch, na.rm = T),
            total = n(), .groups= 'keep') %>%
  filter(year == 2000) %>% 
  arrange(desc(max_catch))
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 1 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 2 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 3 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 4 is invalid UTF-8
```

```
## Warning: There were 1705 warnings in `summarise()`.
## The first warning was:
## ℹ In argument: `max_catch = max(catch, na.rm = T)`.
## ℹ In group 261: `country = "American Samoa"`, `year = 1950`.
## Caused by warning in `max()`:
## ! no non-missing arguments to max; returning -Inf
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1704 remaining warnings.
```

```
## # A tibble: 193 × 4
## # Groups:   country, year [193]
##    country                   year max_catch total
##    <fct>                    <dbl>     <dbl> <int>
##  1 China                     2000      9068    93
##  2 Peru                      2000      5717    54
##  3 Russian Federation        2000      5065   192
##  4 Viet Nam                  2000      4945    13
##  5 Chile                     2000      4299    91
##  6 United States of America  2000      2438   438
##  7 Philippines               2000       999   105
##  8 Japan                     2000       988   241
##  9 Bangladesh                2000       977     4
## 10 Senegal                   2000       970    81
## # ℹ 183 more rows
```

7. Which country caught the most sardines (_Sardina pilchardus_) between the years 1990-2000?
Morocco caught the most sardines. 

```r
fisheries_tidy %>%
  filter(asfis_species_name == "Sardina pilchardus" & between (year, 1990, 2000)) %>% 
  group_by(country, year) %>%
  summarise(max_sardines = max(catch, na.rm = T)) %>% 
  arrange(desc(max_sardines))
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 1 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 2 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 3 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 4 is invalid UTF-8
```

```
## Warning: There were 20 warnings in `summarise()`.
## The first warning was:
## ℹ In argument: `max_sardines = max(catch, na.rm = T)`.
## ℹ In group 2: `country = "Albania"`, `year = 1991`.
## Caused by warning in `max()`:
## ! no non-missing arguments to max; returning -Inf
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 19 remaining warnings.
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 267 × 3
## # Groups:   country [37]
##    country             year max_sardines
##    <fct>              <dbl>        <dbl>
##  1 Morocco             1994          947
##  2 Morocco             1996          925
##  3 Spain               1996          912
##  4 Morocco             2000          859
##  5 Morocco             1990          845
##  6 Morocco             1991          827
##  7 Morocco             1998          723
##  8 Morocco             1993          710
##  9 Russian Federation  1992          627
## 10 Russian Federation  1991          579
## # ℹ 257 more rows
```

8. Which five countries caught the most cephalopods between 2008-2012?
India, China, Italy, Spain, and Algeria. 

```r
fisheries_tidy %>%
  filter(asfis_species_name == "Cephalopoda" & between(year, 2008, 2012)) %>% 
  group_by(country) %>% 
  summarise(max_cephalopods = max(catch, na.rm = T)) %>% 
  arrange(desc(max_cephalopods))
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 1 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 2 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 3 is invalid UTF-8
```

```
## Warning in grepl(",", levels(x), fixed = TRUE): input string 4 is invalid UTF-8
```

```
## Warning: There were 2 warnings in `summarise()`.
## The first warning was:
## ℹ In argument: `max_cephalopods = max(catch, na.rm = T)`.
## ℹ In group 7: `country = "Israel"`.
## Caused by warning in `max()`:
## ! no non-missing arguments to max; returning -Inf
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

```
## # A tibble: 16 × 2
##    country                  max_cephalopods
##    <fct>                              <dbl>
##  1 India                                 94
##  2 China                                 86
##  3 Italy                                 66
##  4 Spain                                 57
##  5 Algeria                               54
##  6 Mauritania                            44
##  7 France                                39
##  8 TimorLeste                            16
##  9 Taiwan Province of China              11
## 10 Mozambique                             8
## 11 Cambodia                               6
## 12 Madagascar                             6
## 13 Croatia                                4
## 14 Viet Nam                               0
## 15 Israel                              -Inf
## 16 Somalia                             -Inf
```

9. Which species had the highest catch total between 2008-2012? (hint: Osteichthyes is not a species)
Marine fishes nei had the highest catch total between 2008 and 2012.

```r
fisheries_tidy %>%
  filter(between(year, 2008, 2012)) %>% 
  group_by(common_name) %>% 
  summarise(max_species_catch = max(catch, na.rm = T)) %>% 
  arrange(desc(max_species_catch))
```

```
## Warning: There were 119 warnings in `summarise()`.
## The first warning was:
## ℹ In argument: `max_species_catch = max(catch, na.rm = T)`.
## ℹ In group 37: `common_name = "Angulate volute"`.
## Caused by warning in `max()`:
## ! no non-missing arguments to max; returning -Inf
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 118 remaining warnings.
```

```
## # A tibble: 1,477 × 2
##    common_name                    max_species_catch
##    <chr>                                      <dbl>
##  1 Marine fishes nei                           9806
##  2 Largehead hairtail                          8221
##  3 Alaska pollock(=Walleye poll.)              8188
##  4 Anchoveta(=Peruvian anchovy)                7981
##  5 Atlantic herring                            7873
##  6 Skipjack tuna                                997
##  7 Blue whiting(=Poutassou)                     996
##  8 Scads nei                                    994
##  9 Yellowstripe scad                            993
## 10 Common squids nei                            991
## # ℹ 1,467 more rows
```

10. Use the data to do at least one analysis of your choice.
What was the mean amount of sardines caught in 1999? The mean sardine catch was 62.73

```r
fisheries_tidy %>% 
  filter(asfis_species_name == "Sardina pilchardus") %>% 
  group_by(asfis_species_name) %>% 
  summarise(mean_sardine_catch = mean(catch, na.rm = T))
```

```
## # A tibble: 1 × 2
##   asfis_species_name mean_sardine_catch
##   <chr>                           <dbl>
## 1 Sardina pilchardus               62.7
```


## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
