---
title: "FRS Dementia Study"
author: "Pallavi Malladi"
date: "Winter 2019"
output:
  html_document:
    keep_md: yes
    theme: spacelab
---
## Goal: Determine which ethnicity and geographic code have a higher inclincation for developing dementia. Is there any correlation between the ethnicity and geography?

## Load the tidyverse

```r
library(tidyverse)
```


```r
dementia_data <- readr::read_csv("Recorded_Dementia_Diagnoses_by_Ethnicity.csv")
```

```
## Parsed with column specification:
## cols(
##   ach_date = col_character(),
##   COMMISSIONER_ORGANISATION_CODE = col_character(),
##   GEOGRAPHY_CODE = col_character(),
##   CCG_NAME = col_character(),
##   Measure = col_character(),
##   Value = col_double()
## )
```


```r
names(dementia_data)
```

```
## [1] "ach_date"                       "COMMISSIONER_ORGANISATION_CODE"
## [3] "GEOGRAPHY_CODE"                 "CCG_NAME"                      
## [5] "Measure"                        "Value"
```

Rename some of the variables. Notice that I am replacing the old `dementia_data` data.

```r
dementia_data <- 
  dementia_data %>% 
  dplyr::rename(
          Recorded_Date = 'ach_date',
          Organization_Code  = 'COMMISSIONER_ORGANISATION_CODE',
          Clinical_Commisioning_Groups  = 'CCG_NAME',
          Ethnicity  = 'Measure',
          Value   = 'Value'
          )
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Ethnicity
##    <chr>         <chr>            <chr>          <chr>            <chr>    
##  1 30Apr2018     00C              E38000042      NHS Darlington … ASIAN_OR…
##  2 30Apr2018     00C              E38000042      NHS Darlington … BLACK_OR…
##  3 30Apr2018     00C              E38000042      NHS Darlington … MIXED_OR…
##  4 30Apr2018     00C              E38000042      NHS Darlington … NOT_DEFI…
##  5 30Apr2018     00C              E38000042      NHS Darlington … OTHER_ET…
##  6 30Apr2018     00C              E38000042      NHS Darlington … WHITE    
##  7 30Apr2018     00D              E38000047      NHS Durham Dale… ASIAN_OR…
##  8 30Apr2018     00D              E38000047      NHS Durham Dale… BLACK_OR…
##  9 30Apr2018     00D              E38000047      NHS Durham Dale… MIXED_OR…
## 10 30Apr2018     00D              E38000047      NHS Durham Dale… NOT_DEFI…
## # … with 11,690 more rows, and 1 more variable: Value <dbl>
```

## Explore the data using the method that you prefer. Below, I am using a new package called `skimr`. If you want to use this, make sure that it is installed.

```r
##install.packages("skimr")
##installs a new package (not the tidyverse!)
```


```r
library("skimr")
```

```
## 
## Attaching package: 'skimr'
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```


```r
dementia_data %>% 
  skimr::skim()
```

```
## Skim summary statistics
##  n obs: 11700 
##  n variables: 6 
## 
## ── Variable type:character ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
##                      variable missing complete     n min max empty
##  Clinical_Commisioning_Groups       0    11700 11700  12  57     0
##                     Ethnicity       0    11700 11700   5  46     0
##                GEOGRAPHY_CODE       0    11700 11700   9   9     0
##             Organization_Code       0    11700 11700   3   3     0
##                 Recorded_Date       0    11700 11700   9   9     0
##  n_unique
##       195
##         6
##       195
##       195
##        10
## 
## ── Variable type:numeric ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
##  variable missing complete     n   mean     sd p0 p25 p50 p75 p100
##     Value       0    11700 11700 392.65 806.17  0   1  13 434 7377
##      hist
##  ▇▁▁▁▁▁▁▁
```

```r
##skimr is like another summary function
```

## 2. Run the code below. Are there any NA's in the data? Does this seem likely?

```r
dementia_data %>% 
  summarize(number_nas= sum(is.na(dementia_data)))
```

```
## # A tibble: 1 x 1
##   number_nas
##        <int>
## 1          0
```

```r
## Technically, there are no NA's, but it is possible that there is another value such as -999 that represents a null value
```

##Produce a summary that shows the number of observations by ethnicity.

```r
names(dementia_data) ##I'm using this to get a general idea of the column headers so I know which notation to use within my code
```

```
## [1] "Recorded_Date"                "Organization_Code"           
## [3] "GEOGRAPHY_CODE"               "Clinical_Commisioning_Groups"
## [5] "Ethnicity"                    "Value"
```


```r
dementia_data %>% 
  group_by(Ethnicity) %>% ##groups by taxonomic order
  summarize(n())## n represents the number of observations; each order has a certain number of observations
```

```
## # A tibble: 6 x 2
##   Ethnicity                                      `n()`
##   <chr>                                          <int>
## 1 ASIAN_OR_ASIAN_BRITISH                          1950
## 2 BLACK_OR_AFRICAN_OR_CARIBBEAN_OR_BLACK_BRITISH  1950
## 3 MIXED_OR_MULTIPLE_ETHNIC_GROUPS                 1950
## 4 NOT_DEFINED                                     1950
## 5 OTHER_ETHNIC_GROUP                              1950
## 6 WHITE                                           1950
```

## 5. Mammals have a range of life histories, including lifespan. Produce a summary of lifespan in years by order. Be sure to include the minimum, maximum, mean, standard deviation, and total n.

```r
dementia_data <-dementia_data %>%
  ## MAKE CODE TO REMOVE THE NOT_DEFINED ETHNICITY SINCE IT IS NOT TELLING US RELEVANT INFORMATION
  arrange(desc(Value))
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Ethnicity
##    <chr>         <chr>            <chr>          <chr>            <chr>    
##  1 30Sep2018     99P              E38000129      NHS Northern, E… NOT_DEFI…
##  2 30Sep2018     11J              E38000045      NHS Dorset CCG   NOT_DEFI…
##  3 30Nov2018     11J              E38000045      NHS Dorset CCG   NOT_DEFI…
##  4 31Dec2018     11J              E38000045      NHS Dorset CCG   NOT_DEFI…
##  5 30Nov2018     99P              E38000129      NHS Northern, E… NOT_DEFI…
##  6 31Oct2018     11J              E38000045      NHS Dorset CCG   NOT_DEFI…
##  7 31Jan2019     11J              E38000045      NHS Dorset CCG   NOT_DEFI…
##  8 31Oct2018     99P              E38000129      NHS Northern, E… NOT_DEFI…
##  9 31Jan2019     99P              E38000129      NHS Northern, E… NOT_DEFI…
## 10 31Dec2018     99P              E38000129      NHS Northern, E… NOT_DEFI…
## # … with 11,690 more rows, and 1 more variable: Value <dbl>
```

## Proboscidea has longest mean gestation; "The Proboscidea (from the Greek προβοσκίς and the Latin proboscis) are a taxonomic order of afrotherian mammals containing one living family (Elephantidae) and several extinct families"


##Below are the plots comparing rates of dementia diagnosis per month for 6 races.

```r
dementia_data %>% 
  ggplot(aes(x=Ethnicity, y=Value, fill=Ethnicity))+
  geom_bar(stat="identity")
```

![](Dementia_Study_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
dementia_data %>% 
  ggplot(aes(x=Value))+
  geom_density(fill="steelblue", alpha=0.4)+
  geom_histogram(fill="royal blue")+
  facet_wrap(~Clinical_Conditioning_Groups)+
  facet_grid(~Ethnicity)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Dementia_Study_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## 8 minute presentation- 2 minutes for questions so 6 minutes.

Introduction: We chose to analyze data regarding dementia in England. We got our data from NHS Digital, which publishes the recorded dementia diagnoses each month with the latest from January 2019. This data was collected because not everyone gets a formal diagnosis for dementia. The data compares people who were thought to have dementia and those who were actually diagnosed. These people were all 65 and older.

Some Key facts include:
464,413 people had a diagnosis of demetia as of January 31st 2019.
Camden CCG has the highest estimated dementia dianosis rate, at 90.60%
Kernow CCG has the lowest estimated dementia diagnosis rate, at 52.10%
