---
title: "FRS Dementia Study"
author: "Pallavi Malladi and Tiffany Sin"
date: "Winter 2019"
output:
  html_document:
    keep_md: yes
    theme: spacelab
---
## Goal: Determine which Race and geographic code have a higher inclincation for developing dementia. Is there any correlation between the Race and geography?

## The data is collected between April 2018 and January 2019. Data was collected at the end of each month, and recorded values represent the number of people who showed signs/symptoms of Dementia at various clinics throughout England.

## Load the tidyverse

```r
library(tidyverse)
```


```r
dementia_data <- readr::read_csv("Recorded_Dementia_Diagnoses_by_Race.csv")
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
          Race  = 'Measure',
          Dementia_Rate_Indicator   = 'Value'
          )
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Race 
##    <chr>         <chr>            <chr>          <chr>            <chr>
##  1 30Apr2018     00C              E38000042      NHS Darlington … ASIA…
##  2 30Apr2018     00C              E38000042      NHS Darlington … BLAC…
##  3 30Apr2018     00C              E38000042      NHS Darlington … MIXE…
##  4 30Apr2018     00C              E38000042      NHS Darlington … NOT_…
##  5 30Apr2018     00C              E38000042      NHS Darlington … OTHE…
##  6 30Apr2018     00C              E38000042      NHS Darlington … WHITE
##  7 30Apr2018     00D              E38000047      NHS Durham Dale… ASIA…
##  8 30Apr2018     00D              E38000047      NHS Durham Dale… BLAC…
##  9 30Apr2018     00D              E38000047      NHS Durham Dale… MIXE…
## 10 30Apr2018     00D              E38000047      NHS Durham Dale… NOT_…
## # … with 11,690 more rows, and 1 more variable:
## #   Dementia_Rate_Indicator <dbl>
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
## Warning: package 'skimr' was built under R version 3.5.2
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
## ── Variable type:character ──────────────────────────────────────────
##                      variable missing complete     n min max empty
##  Clinical_Commisioning_Groups       0    11700 11700  12  57     0
##                GEOGRAPHY_CODE       0    11700 11700   9   9     0
##             Organization_Code       0    11700 11700   3   3     0
##                          Race       0    11700 11700   5  46     0
##                 Recorded_Date       0    11700 11700   9   9     0
##  n_unique
##       195
##       195
##       195
##         6
##        10
## 
## ── Variable type:numeric ────────────────────────────────────────────
##                 variable missing complete     n   mean     sd p0 p25 p50
##  Dementia_Rate_Indicator       0    11700 11700 392.65 806.17  0   1  13
##  p75 p100     hist
##  434 7377 ▇▁▁▁▁▁▁▁
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

##Produce a summary that shows the number of observations by Race.

```r
names(dementia_data) ##I'm using this to get a general idea of the column headers so I know which notation to use within my code
```

```
## [1] "Recorded_Date"                "Organization_Code"           
## [3] "GEOGRAPHY_CODE"               "Clinical_Commisioning_Groups"
## [5] "Race"                         "Dementia_Rate_Indicator"
```


```r
dementia_data %>% 
  group_by(Race) %>% ##groups by taxonomic order
  summarize(n())## n represents the number of observations; each order has a certain number of observations
```

```
## # A tibble: 6 x 2
##   Race                                           `n()`
##   <chr>                                          <int>
## 1 ASIAN_OR_ASIAN_BRITISH                          1950
## 2 BLACK_OR_AFRICAN_OR_CARIBBEAN_OR_BLACK_BRITISH  1950
## 3 MIXED_OR_MULTIPLE_ETHNIC_GROUPS                 1950
## 4 NOT_DEFINED                                     1950
## 5 OTHER_ETHNIC_GROUP                              1950
## 6 WHITE                                           1950
```

```r
## From this line of code, we see that each Race has 1950 observations
```

## Mutate the Dementia_data set to not include the "NOT_DEFINED" since it is not contributing to our results dealing with specific Races.

```r
dementia_data<- dementia_data %>% 
  filter(Race!= "NOT_DEFINED")
dementia_data
```

```
## # A tibble: 9,750 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Race 
##    <chr>         <chr>            <chr>          <chr>            <chr>
##  1 30Apr2018     00C              E38000042      NHS Darlington … ASIA…
##  2 30Apr2018     00C              E38000042      NHS Darlington … BLAC…
##  3 30Apr2018     00C              E38000042      NHS Darlington … MIXE…
##  4 30Apr2018     00C              E38000042      NHS Darlington … OTHE…
##  5 30Apr2018     00C              E38000042      NHS Darlington … WHITE
##  6 30Apr2018     00D              E38000047      NHS Durham Dale… ASIA…
##  7 30Apr2018     00D              E38000047      NHS Durham Dale… BLAC…
##  8 30Apr2018     00D              E38000047      NHS Durham Dale… MIXE…
##  9 30Apr2018     00D              E38000047      NHS Durham Dale… OTHE…
## 10 30Apr2018     00D              E38000047      NHS Durham Dale… WHITE
## # … with 9,740 more rows, and 1 more variable:
## #   Dementia_Rate_Indicator <dbl>
```


## 5. Mammals have a range of life histories, including lifespan. Produce a summary of lifespan in years by order. Be sure to include the minimum, maximum, mean, standard deviation, and total n.

```r
dementia_data <-dementia_data %>%
  ## MAKE CODE TO REMOVE THE NOT_DEFINED Race SINCE IT IS NOT TELLING US RELEVANT INFORMATION
  arrange(desc(Dementia_Rate_Indicator))
dementia_data
```

```
## # A tibble: 9,750 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Race 
##    <chr>         <chr>            <chr>          <chr>            <chr>
##  1 31Jan2019     15F              E38000225      NHS Leeds CCG    WHITE
##  2 31Dec2018     15F              E38000225      NHS Leeds CCG    WHITE
##  3 30Nov2018     15F              E38000225      NHS Leeds CCG    WHITE
##  4 31Oct2018     15F              E38000225      NHS Leeds CCG    WHITE
##  5 30Sep2018     15F              E38000225      NHS Leeds CCG    WHITE
##  6 31Aug2018     15F              E38000225      NHS Leeds CCG    WHITE
##  7 31Jul2018     15F              E38000225      NHS Leeds CCG    WHITE
##  8 30Jun2018     15F              E38000225      NHS Leeds CCG    WHITE
##  9 31May2018     15F              E38000225      NHS Leeds CCG    WHITE
## 10 30Apr2018     15F              E38000225      NHS Leeds CCG    WHITE
## # … with 9,740 more rows, and 1 more variable:
## #   Dementia_Rate_Indicator <dbl>
```

## Making a plot to visually show the difference between. Plot is shown for each month and shows barplot comparing Races showing Dementia

```r
dementia_data %>% 
  ggplot(aes(x=Race, y=Dementia_Rate_Indicator, fill=Race))+
  geom_bar(stat="identity")
```

![](Dementia_Study_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
#general_data<- dementia_data %>% 
 # filter(Organization_Code, GEOGRAPHY_CODE,Clinical_Commisioning_Groups, Race, Dementia_Rate_Indicator)
```

## Making a plot to compare Ethncity and Dementia Diagnosis indicator by region to see if there is a correlation between Dementia Diagnosis and the geograohical region.

```r
dementia_data %>% 
  ggplot(aes(x=Clinical_Commisioning_Groups, y=Dementia_Rate_Indicator, fill=Race))+
  geom_bar(stat="identity")+
  facet_wrap(~Clinical_Commisioning_Groups)
```

![](Dementia_Study_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## Within each ethnicity-> find where their highest Dementia rate indicator is

```r
highest_dementia_rate<-dementia_data %>% 
  group_by(Race) %>% 
  select(-Recorded_Date, -Organization_Code, -GEOGRAPHY_CODE)
  ##arrange(desc(Dementia_Rate_Indicator))
highest_dementia_rate
```

```
## # A tibble: 9,750 x 3
## # Groups:   Race [5]
##    Clinical_Commisioning_Groups Race  Dementia_Rate_Indicator
##    <chr>                        <chr>                   <dbl>
##  1 NHS Leeds CCG                WHITE                    3117
##  2 NHS Leeds CCG                WHITE                    3095
##  3 NHS Leeds CCG                WHITE                    3077
##  4 NHS Leeds CCG                WHITE                    3027
##  5 NHS Leeds CCG                WHITE                    2942
##  6 NHS Leeds CCG                WHITE                    2787
##  7 NHS Leeds CCG                WHITE                    2755
##  8 NHS Leeds CCG                WHITE                    2695
##  9 NHS Leeds CCG                WHITE                    2638
## 10 NHS Leeds CCG                WHITE                    2585
## # … with 9,740 more rows
```


```r
lowest_dementia_rate<- dementia_data %>% 
  select(-Recorded_Date, -Organization_Code, -GEOGRAPHY_CODE) %>% 
  group_by(Race) %>% 
  arrange(desc(Dementia_Rate_Indicator))
lowest_dementia_rate  
```

```
## # A tibble: 9,750 x 3
## # Groups:   Race [5]
##    Clinical_Commisioning_Groups Race  Dementia_Rate_Indicator
##    <chr>                        <chr>                   <dbl>
##  1 NHS Leeds CCG                WHITE                    3117
##  2 NHS Leeds CCG                WHITE                    3095
##  3 NHS Leeds CCG                WHITE                    3077
##  4 NHS Leeds CCG                WHITE                    3027
##  5 NHS Leeds CCG                WHITE                    2942
##  6 NHS Leeds CCG                WHITE                    2787
##  7 NHS Leeds CCG                WHITE                    2755
##  8 NHS Leeds CCG                WHITE                    2695
##  9 NHS Leeds CCG                WHITE                    2638
## 10 NHS Leeds CCG                WHITE                    2585
## # … with 9,740 more rows
```
##Question: What race has the highest dementia rate? Given the data from April 2018-January 2019, which CCG clinic has the highest dementia rate for each race?

## Introduction: For our presentation, we wanted to focus our attention on data in the health field. We chose to analyze data regarding dementia in England and we got our data from NHS Digital, which publishes the recorded dementia diagnoses each month with the latest from January 2019. This data was collected because not everyone gets a formal diagnosis for dementia. The data compares people who were thought to have dementia and those who were acutally diagnosed. These people were all 65 and older.

## Conclusion: There seems to be a significantly higher amount of Whites coming into the clinics. This may indicate a social difference between White and other Races. For example, other Races may have cultural backgrounds that stigmatizes mental health or going to hospitals regularly for chekcups. We were not sue how to work around this significantly higher data, but it does bring about an important topic for discussion regarding mental health, social taboos, financial burders, and personal care.

##Some key facts include:
  464,413 people had a diagnosis of demenia as of January 31st 2019.
  Camden CCG has the highest estimated dementia diagnosis rate, at 90.60%.
  Kernow CCG has the lowest estimated dementia diagnosis rate, at 52.10%
