---
title: "Lab 4 Homework"
author: Carmen Doria
date: "2024-01-25"
output:
  html_document: 
    theme: spacelab
    keep_md: true
---




```r
#install.packages("palmerpenguins")
```


## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the tidyverse

```r
library(tidyverse)
```

## Data
For the homework, we will use data about vertebrate home range sizes. The data are in the class folder, but the reference is below.  

**Database of vertebrate home range sizes.**  
Reference: Tamburello N, Cote IM, Dulvy NK (2015) Energy and the scaling of animal space use. The American Naturalist 186(2):196-211. http://dx.doi.org/10.1086/682070.  
Data: http://datadryad.org/resource/doi:10.5061/dryad.q5j65/1  

**1. Load the data into a new object called `homerange`.**

```r
homerange <- read_csv("data/Tamburelloetal_HomeRangeDatabase.csv")
```

```
## Rows: 569 Columns: 24
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (16): taxon, common.name, class, order, family, genus, species, primarym...
## dbl  (8): mean.mass.g, log10.mass, mean.hra.m2, log10.hra, dimension, preyma...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

**2. Explore the data. Show the dimensions, column names, classes for each variable, and a statistical summary. Keep these as separate code chunks.**  


```r
dim(homerange)
```

```
## [1] 569  24
```



```r
names(homerange)
```

```
##  [1] "taxon"                      "common.name"               
##  [3] "class"                      "order"                     
##  [5] "family"                     "genus"                     
##  [7] "species"                    "primarymethod"             
##  [9] "N"                          "mean.mass.g"               
## [11] "log10.mass"                 "alternative.mass.reference"
## [13] "mean.hra.m2"                "log10.hra"                 
## [15] "hra.reference"              "realm"                     
## [17] "thermoregulation"           "locomotion"                
## [19] "trophic.guild"              "dimension"                 
## [21] "preymass"                   "log10.preymass"            
## [23] "PPMR"                       "prey.size.reference"
```


```r
summary(homerange)
```

```
##     taxon           common.name           class              order          
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##     family             genus             species          primarymethod     
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##       N              mean.mass.g        log10.mass     
##  Length:569         Min.   :      0   Min.   :-0.6576  
##  Class :character   1st Qu.:     50   1st Qu.: 1.6990  
##  Mode  :character   Median :    330   Median : 2.5185  
##                     Mean   :  34602   Mean   : 2.5947  
##                     3rd Qu.:   2150   3rd Qu.: 3.3324  
##                     Max.   :4000000   Max.   : 6.6021  
##                                                        
##  alternative.mass.reference  mean.hra.m2          log10.hra     
##  Length:569                 Min.   :0.000e+00   Min.   :-1.523  
##  Class :character           1st Qu.:4.500e+03   1st Qu.: 3.653  
##  Mode  :character           Median :3.934e+04   Median : 4.595  
##                             Mean   :2.146e+07   Mean   : 4.709  
##                             3rd Qu.:1.038e+06   3rd Qu.: 6.016  
##                             Max.   :3.551e+09   Max.   : 9.550  
##                                                                 
##  hra.reference         realm           thermoregulation    locomotion       
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  trophic.guild        dimension        preymass         log10.preymass   
##  Length:569         Min.   :2.000   Min.   :     0.67   Min.   :-0.1739  
##  Class :character   1st Qu.:2.000   1st Qu.:    20.02   1st Qu.: 1.3014  
##  Mode  :character   Median :2.000   Median :    53.75   Median : 1.7304  
##                     Mean   :2.218   Mean   :  3989.88   Mean   : 2.0188  
##                     3rd Qu.:2.000   3rd Qu.:   363.35   3rd Qu.: 2.5603  
##                     Max.   :3.000   Max.   :130233.20   Max.   : 5.1147  
##                                     NA's   :502         NA's   :502      
##       PPMR         prey.size.reference
##  Min.   :  0.380   Length:569         
##  1st Qu.:  3.315   Class :character   
##  Median :  7.190   Mode  :character   
##  Mean   : 31.752                      
##  3rd Qu.: 15.966                      
##  Max.   :530.000                      
##  NA's   :502
```



```r
glimpse(homerange)
```

```
## Rows: 569
## Columns: 24
## $ taxon                      <chr> "lake fishes", "river fishes", "river fishe…
## $ common.name                <chr> "american eel", "blacktail redhorse", "cent…
## $ class                      <chr> "actinopterygii", "actinopterygii", "actino…
## $ order                      <chr> "anguilliformes", "cypriniformes", "cyprini…
## $ family                     <chr> "anguillidae", "catostomidae", "cyprinidae"…
## $ genus                      <chr> "anguilla", "moxostoma", "campostoma", "cli…
## $ species                    <chr> "rostrata", "poecilura", "anomalum", "fundu…
## $ primarymethod              <chr> "telemetry", "mark-recapture", "mark-recapt…
## $ N                          <chr> "16", NA, "20", "26", "17", "5", "2", "2", …
## $ mean.mass.g                <dbl> 887.00, 562.00, 34.00, 4.00, 4.00, 3525.00,…
## $ log10.mass                 <dbl> 2.9479236, 2.7497363, 1.5314789, 0.6020600,…
## $ alternative.mass.reference <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ mean.hra.m2                <dbl> 282750.00, 282.10, 116.11, 125.50, 87.10, 3…
## $ log10.hra                  <dbl> 5.4514026, 2.4504031, 2.0648696, 2.0986437,…
## $ hra.reference              <chr> "Minns, C. K. 1995. Allometry of home range…
## $ realm                      <chr> "aquatic", "aquatic", "aquatic", "aquatic",…
## $ thermoregulation           <chr> "ectotherm", "ectotherm", "ectotherm", "ect…
## $ locomotion                 <chr> "swimming", "swimming", "swimming", "swimmi…
## $ trophic.guild              <chr> "carnivore", "carnivore", "carnivore", "car…
## $ dimension                  <dbl> 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3…
## $ preymass                   <dbl> NA, NA, NA, NA, NA, NA, 1.39, NA, NA, NA, N…
## $ log10.preymass             <dbl> NA, NA, NA, NA, NA, NA, 0.1430148, NA, NA, …
## $ PPMR                       <dbl> NA, NA, NA, NA, NA, NA, 530, NA, NA, NA, NA…
## $ prey.size.reference        <chr> NA, NA, NA, NA, NA, NA, "Brose U, et al. 20…
```


```r
class(homerange)
```

```
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```


**3. Change the class of the variables `taxon` and `order` to factors and display their levels.** 

Setting the class of variables to factors 

```r
homerange$taxon <- factor(homerange$taxon)
```


```r
homerange$order <- factor(homerange$order)
```

Displaying levels

```r
levels(homerange$taxon)
```

```
## [1] "birds"         "lake fishes"   "lizards"       "mammals"      
## [5] "marine fishes" "river fishes"  "snakes"        "tortoises"    
## [9] "turtles"
```



```r
levels(homerange$order)
```

```
##  [1] "accipitriformes"       "afrosoricida"          "anguilliformes"       
##  [4] "anseriformes"          "apterygiformes"        "artiodactyla"         
##  [7] "caprimulgiformes"      "carnivora"             "charadriiformes"      
## [10] "columbidormes"         "columbiformes"         "coraciiformes"        
## [13] "cuculiformes"          "cypriniformes"         "dasyuromorpha"        
## [16] "dasyuromorpia"         "didelphimorphia"       "diprodontia"          
## [19] "diprotodontia"         "erinaceomorpha"        "esociformes"          
## [22] "falconiformes"         "gadiformes"            "galliformes"          
## [25] "gruiformes"            "lagomorpha"            "macroscelidea"        
## [28] "monotrematae"          "passeriformes"         "pelecaniformes"       
## [31] "peramelemorphia"       "perciformes"           "perissodactyla"       
## [34] "piciformes"            "pilosa"                "proboscidea"          
## [37] "psittaciformes"        "rheiformes"            "roden"                
## [40] "rodentia"              "salmoniformes"         "scorpaeniformes"      
## [43] "siluriformes"          "soricomorpha"          "squamata"             
## [46] "strigiformes"          "struthioniformes"      "syngnathiformes"      
## [49] "testudines"            "tinamiformes"          "tetraodontiformes\xa0"
```

Checking to see if the code worked

```r
class(homerange$taxon)
```

```
## [1] "factor"
```

```r
class(homerange$order)
```

```
## [1] "factor"
```

**4. What taxa are represented in the `homerange` data frame? Make a new data frame `taxa` that is restricted to taxon, common name, class, order, family, genus, species.** 


```r
taxa <- select(homerange, "taxon", "common.name", "class", "order", "family", "genus", "species")
taxa
```

```
## # A tibble: 569 × 7
##    taxon         common.name             class        order family genus species
##    <fct>         <chr>                   <chr>        <fct> <chr>  <chr> <chr>  
##  1 lake fishes   american eel            actinoptery… angu… angui… angu… rostra…
##  2 river fishes  blacktail redhorse      actinoptery… cypr… catos… moxo… poecil…
##  3 river fishes  central stoneroller     actinoptery… cypr… cypri… camp… anomal…
##  4 river fishes  rosyside dace           actinoptery… cypr… cypri… clin… fundul…
##  5 river fishes  longnose dace           actinoptery… cypr… cypri… rhin… catara…
##  6 river fishes  muskellunge             actinoptery… esoc… esoci… esox  masqui…
##  7 marine fishes pollack                 actinoptery… gadi… gadid… poll… pollac…
##  8 marine fishes saithe                  actinoptery… gadi… gadid… poll… virens 
##  9 marine fishes lined surgeonfish       actinoptery… perc… acant… acan… lineat…
## 10 marine fishes orangespine unicornfish actinoptery… perc… acant… naso  litura…
## # ℹ 559 more rows
```

**5. The variable `taxon` identifies the common name groups of the species represented in `homerange`. Make a table the shows the counts for each of these `taxon`.**  

```r
count(homerange, taxon)
```

```
## # A tibble: 9 × 2
##   taxon             n
##   <fct>         <int>
## 1 birds           140
## 2 lake fishes       9
## 3 lizards          11
## 4 mammals         238
## 5 marine fishes    90
## 6 river fishes     14
## 7 snakes           41
## 8 tortoises        12
## 9 turtles          14
```

**6. The species in `homerange` are also classified into trophic guilds. How many species are represented in each trophic guild.** 
There are 13 species repsresented. 7 species are carnivores, and 6 are herbivores. In the carnivores there are 342 and 227 in the herbivores. 


```r
filtered_species <- homerange %>%
  filter(trophic.guild %in% c("carnivore", "herbivore")) %>%
  count(trophic.guild)
filtered_species
```

```
## # A tibble: 2 × 2
##   trophic.guild     n
##   <chr>         <int>
## 1 carnivore       342
## 2 herbivore       227
```


## Finish 1-6


**7. Make two new data frames, one which is restricted to carnivores and another that is restricted to herbivores.**  

```r
only_carnivores <- filter(homerange, trophic.guild =="carnivore")
only_carnivores
```

```
## # A tibble: 342 × 24
##    taxon        common.name class order family genus species primarymethod N    
##    <fct>        <chr>       <chr> <fct> <chr>  <chr> <chr>   <chr>         <chr>
##  1 lake fishes  american e… acti… angu… angui… angu… rostra… telemetry     16   
##  2 river fishes blacktail … acti… cypr… catos… moxo… poecil… mark-recaptu… <NA> 
##  3 river fishes central st… acti… cypr… cypri… camp… anomal… mark-recaptu… 20   
##  4 river fishes rosyside d… acti… cypr… cypri… clin… fundul… mark-recaptu… 26   
##  5 river fishes longnose d… acti… cypr… cypri… rhin… catara… mark-recaptu… 17   
##  6 river fishes muskellunge acti… esoc… esoci… esox  masqui… telemetry     5    
##  7 marine fish… pollack     acti… gadi… gadid… poll… pollac… telemetry     2    
##  8 marine fish… saithe      acti… gadi… gadid… poll… virens  telemetry     2    
##  9 marine fish… giant trev… acti… perc… caran… cara… ignobi… telemetry     4    
## 10 lake fishes  rock bass   acti… perc… centr… ambl… rupest… mark-recaptu… 16   
## # ℹ 332 more rows
## # ℹ 15 more variables: mean.mass.g <dbl>, log10.mass <dbl>,
## #   alternative.mass.reference <chr>, mean.hra.m2 <dbl>, log10.hra <dbl>,
## #   hra.reference <chr>, realm <chr>, thermoregulation <chr>, locomotion <chr>,
## #   trophic.guild <chr>, dimension <dbl>, preymass <dbl>, log10.preymass <dbl>,
## #   PPMR <dbl>, prey.size.reference <chr>
```


```r
only_herbivores <- filter(homerange, trophic.guild =="herbivore")
only_herbivores
```

```
## # A tibble: 227 × 24
##    taxon        common.name class order family genus species primarymethod N    
##    <fct>        <chr>       <chr> <fct> <chr>  <chr> <chr>   <chr>         <chr>
##  1 marine fish… lined surg… acti… perc… acant… acan… lineat… direct obser… <NA> 
##  2 marine fish… orangespin… acti… perc… acant… naso  litura… telemetry     8    
##  3 marine fish… bluespine … acti… perc… acant… naso  unicor… telemetry     7    
##  4 marine fish… redlip ble… acti… perc… blenn… ophi… atlant… direct obser… 20   
##  5 marine fish… bermuda ch… acti… perc… kypho… kyph… sectat… telemetry     11   
##  6 marine fish… cherubfish  acti… perc… pomac… cent… argi    direct obser… <NA> 
##  7 marine fish… damselfish  acti… perc… pomac… chro… chromis direct obser… <NA> 
##  8 marine fish… twinspot d… acti… perc… pomac… chry… biocel… direct obser… 18   
##  9 marine fish… wards dams… acti… perc… pomac… poma… wardi   direct obser… <NA> 
## 10 marine fish… australian… acti… perc… pomac… steg… apical… direct obser… <NA> 
## # ℹ 217 more rows
## # ℹ 15 more variables: mean.mass.g <dbl>, log10.mass <dbl>,
## #   alternative.mass.reference <chr>, mean.hra.m2 <dbl>, log10.hra <dbl>,
## #   hra.reference <chr>, realm <chr>, thermoregulation <chr>, locomotion <chr>,
## #   trophic.guild <chr>, dimension <dbl>, preymass <dbl>, log10.preymass <dbl>,
## #   PPMR <dbl>, prey.size.reference <chr>
```

**8. Do herbivores or carnivores have, on average, a larger `mean.hra.m2`? Remove any NAs from the data.** 
On average it appears that carnivores have a larger `mean.hra.m2`.


```r
carnivores_mean <- table(only_carnivores$mean.hra.m2)
mean(carnivores_mean, na.rm = T)
```

```
## [1] 1.062112
```


```r
herbivores_mean <- table(only_herbivores$mean.hra.m2)
mean(herbivores_mean, na.rm = T)
```

```
## [1] 1.008889
```

**9. Make a new dataframe `owls` that is limited to the mean mass, log10 mass, family, genus, and species of owls in the database. Which is the smallest owl? What is its common name? Do a little bit of searching online to see what you can learn about this species and provide a link below** 

The smallest owl was the glaucidium	passerinum which is the Eurasian pygmy owl. More info on the Eurasian pygmy owl [here.](https://animaldiversity.org/accounts/Glaucidium_passerinum/)


Make a different dataframe that selects for mean mass, log10 mass, family, genus, and species 

```r
owl_subset <- select(homerange, "mean.mass.g", "log10.mass", "family", "genus", "species")
owl_subset
```

```
## # A tibble: 569 × 5
##    mean.mass.g log10.mass family       genus       species    
##          <dbl>      <dbl> <chr>        <chr>       <chr>      
##  1        887       2.95  anguillidae  anguilla    rostrata   
##  2        562       2.75  catostomidae moxostoma   poecilura  
##  3         34       1.53  cyprinidae   campostoma  anomalum   
##  4          4       0.602 cyprinidae   clinostomus funduloides
##  5          4       0.602 cyprinidae   rhinichthys cataractae 
##  6       3525       3.55  esocidae     esox        masquinongy
##  7        737.      2.87  gadidae      pollachius  pollachius 
##  8        449.      2.65  gadidae      pollachius  virens     
##  9        109.      2.04  acanthuridae acanthurus  lineatus   
## 10        772.      2.89  acanthuridae naso        lituratus  
## # ℹ 559 more rows
```

Now make the owl dataframe

```r
owls <- filter(owl_subset, family == "strigidae")
owls
```

```
## # A tibble: 8 × 5
##   mean.mass.g log10.mass family    genus      species    
##         <dbl>      <dbl> <chr>     <chr>      <chr>      
## 1       119         2.08 strigidae aegolius   funereus   
## 2       252         2.40 strigidae asio       otus       
## 3       156.        2.19 strigidae athene     noctua     
## 4      2191         3.34 strigidae bubo       bubo       
## 5      1510         3.18 strigidae bubo       virginianus
## 6        61.3       1.79 strigidae glaucidium passerinum 
## 7      1920         3.28 strigidae nyctea     scandiaca  
## 8       519         2.72 strigidae strix      aluco
```

find smallest owl

```r
sorted_owls <- arrange(owls, mean.mass.g)
sorted_owls
```

```
## # A tibble: 8 × 5
##   mean.mass.g log10.mass family    genus      species    
##         <dbl>      <dbl> <chr>     <chr>      <chr>      
## 1        61.3       1.79 strigidae glaucidium passerinum 
## 2       119         2.08 strigidae aegolius   funereus   
## 3       156.        2.19 strigidae athene     noctua     
## 4       252         2.40 strigidae asio       otus       
## 5       519         2.72 strigidae strix      aluco      
## 6      1510         3.18 strigidae bubo       virginianus
## 7      1920         3.28 strigidae nyctea     scandiaca  
## 8      2191         3.34 strigidae bubo       bubo
```


**10. As measured by the data, which bird species has the largest homerange? Show all of your work, please. Look this species up online and tell me about it!**. 

The bird with the largest homerange is the carcara cheriway or the Northern crested caracara. 

```r
birds <- filter(homerange, taxon == "birds")
```



```r
bird_subset <- select(birds, genus, species, mean.mass.g, log10.mass, mean.hra.m2, log10.hra)
```


```r
arrange(bird_subset, desc(mean.hra.m2))
```

```
## # A tibble: 140 × 6
##    genus        species      mean.mass.g log10.mass mean.hra.m2 log10.hra
##    <chr>        <chr>              <dbl>      <dbl>       <dbl>     <dbl>
##  1 caracara     cheriway           1125        3.05   241000000      8.38
##  2 circus       pygargus            316.       2.50   200980000      8.30
##  3 falco        peregrinus          782.       2.89   153860000      8.19
##  4 hieraaetus   pennatus            975        2.99   117300000      8.07
##  5 struthio     camelus           88250        4.95    84300000      7.93
##  6 circaetus    gallicus           1699        3.23    78500000      7.89
##  7 streptopelia turtur              140.       2.15    63585000      7.80
##  8 neophron     percnopterus       2203        3.34    63570000      7.80
##  9 buteo        buteo               846        2.93    50240000      7.70
## 10 falco        biarmicus           675        2.83    50000000      7.70
## # ℹ 130 more rows
```

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
