---
title: "FRS Dementia Study"
author: "Pallavi Malladi and Tiffany Sin"
date: "Winter 2019"
output:
  html_document:
    keep_md: yes
    theme: spacelab
---

## Introduction: For our presentation, we wanted to focus our attention on data in the health field. We chose to analyze data regarding dementia in England and we got our data from NHS Digital, which publishes the recorded dementia diagnoses each month with the latest from January 2019. This data was collected because not everyone gets a formal diagnosis for dementia. The data compares people who were thought to have dementia and those who were acutally diagnosed. These people were all 65 and older.

## Goal: Determine which Race and geographic code have a higher inclincation for developing dementia. Is there any correlation between the Race and geography?

## The data is collected between April 2018 and January 2019. Data was collected at the end of each month, and recorded values represent the number of people who showed signs/symptoms of Dementia at various clinics throughout England.

## Load the tidyverse

```r
library(tidyverse)
```


```r
dementia_data <- readr::read_csv("~/Desktop/FRS_417/pallavimalladi/Dementia_Study/Recorded_Dementia_Diagnoses_by_Race.csv")
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

## Produce a summary that shows the number of observations by Race.

```r
names(dementia_data) ##I'm using this to get a general idea of the column headers so I know which notation to use within my code
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

## Rename the data values for Race so that the names are not so long (long names may cause problems in later lines of code, especially when trying to make plots)

```r
dementia_data<- dementia_data %>% 
  separate(Race, into= c("Race","Race2","Race3","Race4","Race5","Race6","Race7", "Race8" ), sep = "_") %>% 
  select(-Race2, -Race3,-Race4,-Race5,-Race6, -Race7,-Race8)
```

```
## Warning: Expected 8 pieces. Missing pieces filled with `NA` in 9750 rows
## [1, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 21, 22, 23,
## 24, ...].
```

```r
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Race 
##    <chr>         <chr>            <chr>          <chr>            <chr>
##  1 30Apr2018     00C              E38000042      NHS Darlington … ASIAN
##  2 30Apr2018     00C              E38000042      NHS Darlington … BLACK
##  3 30Apr2018     00C              E38000042      NHS Darlington … MIXED
##  4 30Apr2018     00C              E38000042      NHS Darlington … NOT  
##  5 30Apr2018     00C              E38000042      NHS Darlington … OTHER
##  6 30Apr2018     00C              E38000042      NHS Darlington … WHITE
##  7 30Apr2018     00D              E38000047      NHS Durham Dale… ASIAN
##  8 30Apr2018     00D              E38000047      NHS Durham Dale… BLACK
##  9 30Apr2018     00D              E38000047      NHS Durham Dale… MIXED
## 10 30Apr2018     00D              E38000047      NHS Durham Dale… NOT  
## # … with 11,690 more rows, and 1 more variable:
## #   Dementia_Rate_Indicator <dbl>
```

```r
##Please not that "NOT" means not defined
```

## Mutate the Dementia_data set to not include the "NOT" (NOT DEFINED) since it is not contributing to our results dealing with specific Races.

```r
dementia_data<- dementia_data %>% 
  filter(Race!= "NOT")
dementia_data
```

```
## # A tibble: 9,750 x 6
##    Recorded_Date Organization_Co… GEOGRAPHY_CODE Clinical_Commis… Race 
##    <chr>         <chr>            <chr>          <chr>            <chr>
##  1 30Apr2018     00C              E38000042      NHS Darlington … ASIAN
##  2 30Apr2018     00C              E38000042      NHS Darlington … BLACK
##  3 30Apr2018     00C              E38000042      NHS Darlington … MIXED
##  4 30Apr2018     00C              E38000042      NHS Darlington … OTHER
##  5 30Apr2018     00C              E38000042      NHS Darlington … WHITE
##  6 30Apr2018     00D              E38000047      NHS Durham Dale… ASIAN
##  7 30Apr2018     00D              E38000047      NHS Durham Dale… BLACK
##  8 30Apr2018     00D              E38000047      NHS Durham Dale… MIXED
##  9 30Apr2018     00D              E38000047      NHS Durham Dale… OTHER
## 10 30Apr2018     00D              E38000047      NHS Durham Dale… WHITE
## # … with 9,740 more rows, and 1 more variable:
## #   Dementia_Rate_Indicator <dbl>
```

## Rearrange the data table to show decreasing order of Dementia Indicator. This will be a good indicator as to the highest race coming into clinics with Dementia diagnoses

```r
dementia_data <-dementia_data %>%
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

##Find the total number of people coming into the NIH clinics for each Race between April 2018 to January 2019

```r
dementia_data %>% 
  group_by(Race) %>% 
  summarize(total= sum(Dementia_Rate_Indicator)) %>% 
  arrange(desc(total))
```

```
## # A tibble: 5 x 2
##   Race    total
##   <chr>   <dbl>
## 1 WHITE 1159685
## 2 ASIAN  112824
## 3 OTHER   21478
## 4 MIXED   20533
## 5 BLACK     336
```

```r
## total represents number of people who have come into clinics within each race
##Notice how from this data, the White and Asian Races have the highest Dementia Diagnoses, while Black data is closest to 0. This should be very apparent in our upcoming plots.
```

## Making a plot to visually show the difference between. Plot is shown for each month and shows barplot comparing Races showing Dementia

```r
dementia_data %>% 
  ggplot(aes(x=Race, y=Dementia_Rate_Indicator, fill=Race))+
  geom_bar(stat="identity")+
  theme(plot.title = element_text(size = rel(1.95)))+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1))
```

![](Dementia_Study_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Find the Clincal Commisioning Group where the Dementia_Rate_Indicator is highest for each Race. Use this information to find geographical location where dementia is prevalent for that specific Race.

```r
dementia_data %>% 
  group_by(Clinical_Commisioning_Groups) %>% 
  summarize(total= sum(Dementia_Rate_Indicator)) %>% 
  arrange(desc(total)) 
```

```
## # A tibble: 195 x 2
##    Clinical_Commisioning_Groups            total
##    <chr>                                   <dbl>
##  1 NHS Leeds CCG                           30913
##  2 NHS Birmingham and Solihull CCG         30070
##  3 NHS Mid Essex CCG                       22063
##  4 NHS Bedfordshire CCG                    21770
##  5 NHS Leicester City CCG                  20156
##  6 NHS Nene CCG                            19672
##  7 NHS West Leicestershire CCG             19443
##  8 NHS Sheffield CCG                       18965
##  9 NHS East and North Hertfordshire CCG    18891
## 10 NHS Cambridgeshire and Peterborough CCG 18449
## # … with 185 more rows
```

```r
## total represents number of people who have come into each specific clinic
```

## Making the plot showing the basic race data for the top 5 clinical commisioning groups with the most amount of clinic visits

```r
CCG_data <- dementia_data %>% 
  select (Clinical_Commisioning_Groups, Race, Dementia_Rate_Indicator) %>% 
  filter(Clinical_Commisioning_Groups=="NHS Leeds CCG"| Clinical_Commisioning_Groups=="NHS Birmingham and Solihull CCG"| Clinical_Commisioning_Groups=="NHS Mid Essex CCG"| Clinical_Commisioning_Groups=="NHS Bedfordshire CCG" | Clinical_Commisioning_Groups=="NHS Leicester City CCG")
CCG_data
```

```
## # A tibble: 250 x 3
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
## # … with 240 more rows
```


```r
CCG_data %>% 
  ggplot(aes(x=Race, y= Dementia_Rate_Indicator, fill=Race))+
  geom_bar(stat="identity") +
  facet_wrap(~Clinical_Commisioning_Groups)+
  theme(plot.title = element_text(size = rel(1.95)))+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1))
```

![](Dementia_Study_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
general_data<- dementia_data %>% 
  select(Organization_Code, GEOGRAPHY_CODE,Clinical_Commisioning_Groups, Race, Dementia_Rate_Indicator)
general_data
```

```
## # A tibble: 9,750 x 5
##    Organization_Code GEOGRAPHY_CODE Clinical_Commis… Race  Dementia_Rate_I…
##    <chr>             <chr>          <chr>            <chr>            <dbl>
##  1 15F               E38000225      NHS Leeds CCG    WHITE             3117
##  2 15F               E38000225      NHS Leeds CCG    WHITE             3095
##  3 15F               E38000225      NHS Leeds CCG    WHITE             3077
##  4 15F               E38000225      NHS Leeds CCG    WHITE             3027
##  5 15F               E38000225      NHS Leeds CCG    WHITE             2942
##  6 15F               E38000225      NHS Leeds CCG    WHITE             2787
##  7 15F               E38000225      NHS Leeds CCG    WHITE             2755
##  8 15F               E38000225      NHS Leeds CCG    WHITE             2695
##  9 15F               E38000225      NHS Leeds CCG    WHITE             2638
## 10 15F               E38000225      NHS Leeds CCG    WHITE             2585
## # … with 9,740 more rows
```

## Conclusion: There seems to be a significantly higher amount of Whites coming into the clinics. This may indicate a social difference between White and other Races. For example, other Races may have cultural backgrounds that stigmatizes mental health or going to hospitals regularly for chekcups. We were not sue how to work around this significantly higher data, but it does bring about an important topic for discussion regarding mental health, social taboos, financial burders, and personal care.

## Facts from the study: 464,413 people had a diagnosis of demenia as of January 31st 2019. Camden CCG has the highest estimated dementia diagnosis rate, at 90.60%. Kernow CCG has the lowest estimated dementia diagnosis rate, at 52.10%
