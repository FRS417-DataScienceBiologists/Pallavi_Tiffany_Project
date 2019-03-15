---
title: "FRS Dementia Study"
author: "Pallavi Malladi and Tiffany Sin"
date: "Winter 2019"
output:
  html_document:
    keep_md: yes
    theme: spacelab
---
## Introduction
For our presentation, we wanted to focus our attention on data in the health field. We chose to analyze data regarding dementia in England and we got our data from NHS Digital, which publishes the recorded dementia diagnoses each month with the latest from January 2019. This data was collected because not everyone gets a formal diagnosis for dementia. The data compares people who were thought to have dementia and those who were acutally diagnosed. These people were all 65 and older.

## Goal
- Determine which Race and geographic code have a higher inclincation for developing dementia.  
- Is there any correlation between the Race and geography?  

## Data
- The data is collected between April 2018 and January 2019.  
- Data was collected at the end of each month, and recorded values represent the number of people who showed signs/symptoms of Dementia at various clinics throughout England.

## Load the tidyverse

```r
library(tidyverse)
```

## Import the data

```r
dementia_data <- 
  readr::read_csv("Recorded_Dementia_Diagnoses_by_Race.csv")
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

## 
###1. What are the column names? 

```r
names(dementia_data)
```

```
## [1] "ach_date"                       "COMMISSIONER_ORGANISATION_CODE"
## [3] "GEOGRAPHY_CODE"                 "CCG_NAME"                      
## [5] "Measure"                        "Value"
```

## 
###Run the code below. Are there any NA's in the data? Does this seem likely?

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
####There are no NA's, but it is possible that there is another value such as -999 that represents a null value

## 
###2. Produce a summary that shows the number of observations by Race.

```r
glimpse(dementia_data) 
```

```
## Observations: 11,700
## Variables: 6
## $ ach_date                       <chr> "30Apr2018", "30Apr2018", "30Apr2…
## $ COMMISSIONER_ORGANISATION_CODE <chr> "00C", "00C", "00C", "00C", "00C"…
## $ GEOGRAPHY_CODE                 <chr> "E38000042", "E38000042", "E38000…
## $ CCG_NAME                       <chr> "NHS Darlington CCG", "NHS Darlin…
## $ Measure                        <chr> "ASIAN_OR_ASIAN_BRITISH", "BLACK_…
## $ Value                          <dbl> 4, 0, 1, 466, 2, 584, 3, 0, 0, 14…
```
####general idea of the column headers so I know which notation to use within my code


## 
###3. Rename some of the variables. Notice that I am replacing the old `dementia_data` data. {.smaller}

```r
dementia_data <- 
  dementia_data %>% 
  dplyr::rename(
          Recorded_Date = 'ach_date',
          Clinical_Commisioning_Groups  = 'CCG_NAME',
          Race  = 'Measure',
          Dementia_Rate_Indicator   = 'Value',
          Organization_Code  = 'COMMISSIONER_ORGANISATION_CODE'
          )
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Code GEOGRAPHY_CODE Clinical_Commisioning_Groups                   Race                                           Dementia_Rate_Indicator
##    <chr>         <chr>             <chr>          <chr>                                          <chr>                                                            <dbl>
##  1 30Apr2018     00C               E38000042      NHS Darlington CCG                             ASIAN_OR_ASIAN_BRITISH                                               4
##  2 30Apr2018     00C               E38000042      NHS Darlington CCG                             BLACK_OR_AFRICAN_OR_CARIBBEAN_OR_BLACK_BRITISH                       0
##  3 30Apr2018     00C               E38000042      NHS Darlington CCG                             MIXED_OR_MULTIPLE_ETHNIC_GROUPS                                      1
##  4 30Apr2018     00C               E38000042      NHS Darlington CCG                             NOT_DEFINED                                                        466
##  5 30Apr2018     00C               E38000042      NHS Darlington CCG                             OTHER_ETHNIC_GROUP                                                   2
##  6 30Apr2018     00C               E38000042      NHS Darlington CCG                             WHITE                                                              584
##  7 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG ASIAN_OR_ASIAN_BRITISH                                               3
##  8 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG BLACK_OR_AFRICAN_OR_CARIBBEAN_OR_BLACK_BRITISH                       0
##  9 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG MIXED_OR_MULTIPLE_ETHNIC_GROUPS                                      0
## 10 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG NOT_DEFINED                                                       1405
## # … with 11,690 more rows
```

## 
###4. Show the number of observations for each race

```r
dementia_data %>% 
  group_by(Race) %>% 
  summarize(n())
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
## n represents the number of observations
###each order has a certain number of observations
### We see that each Race has 1950 observations

##
###5. Rename the data values for Race so that the names are not so long (long names may cause problems in later lines of code, especially when trying to make plots)

```r
dementia_data<- dementia_data %>% 
  separate(Race, into= c("Race","Race2","Race3","Race4","Race5","Race6","Race7", "Race8" ), sep = "_") %>% 
  select(-Race2, -Race3,-Race4,-Race5,-Race6, -Race7,-Race8)
```

```
## Warning: Expected 8 pieces. Missing pieces filled with `NA` in 9750 rows [1, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 21, 22, 23, 24, ...].
```

```r
dementia_data
```

```
## # A tibble: 11,700 x 6
##    Recorded_Date Organization_Code GEOGRAPHY_CODE Clinical_Commisioning_Groups                   Race  Dementia_Rate_Indicator
##    <chr>         <chr>             <chr>          <chr>                                          <chr>                   <dbl>
##  1 30Apr2018     00C               E38000042      NHS Darlington CCG                             ASIAN                       4
##  2 30Apr2018     00C               E38000042      NHS Darlington CCG                             BLACK                       0
##  3 30Apr2018     00C               E38000042      NHS Darlington CCG                             MIXED                       1
##  4 30Apr2018     00C               E38000042      NHS Darlington CCG                             NOT                       466
##  5 30Apr2018     00C               E38000042      NHS Darlington CCG                             OTHER                       2
##  6 30Apr2018     00C               E38000042      NHS Darlington CCG                             WHITE                     584
##  7 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG ASIAN                       3
##  8 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG BLACK                       0
##  9 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG MIXED                       0
## 10 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG NOT                      1405
## # … with 11,690 more rows
```
#### note that "NOT" means not defined

## 
###6. Mutate the Dementia_data set to not include the "NOT" (NOT DEFINED) since it is not contributing to our results dealing with specific Races.

```r
dementia_data<- dementia_data %>% 
  filter(Race!= "NOT")
dementia_data
```

```
## # A tibble: 9,750 x 6
##    Recorded_Date Organization_Code GEOGRAPHY_CODE Clinical_Commisioning_Groups                   Race  Dementia_Rate_Indicator
##    <chr>         <chr>             <chr>          <chr>                                          <chr>                   <dbl>
##  1 30Apr2018     00C               E38000042      NHS Darlington CCG                             ASIAN                       4
##  2 30Apr2018     00C               E38000042      NHS Darlington CCG                             BLACK                       0
##  3 30Apr2018     00C               E38000042      NHS Darlington CCG                             MIXED                       1
##  4 30Apr2018     00C               E38000042      NHS Darlington CCG                             OTHER                       2
##  5 30Apr2018     00C               E38000042      NHS Darlington CCG                             WHITE                     584
##  6 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG ASIAN                       3
##  7 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG BLACK                       0
##  8 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG MIXED                       0
##  9 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG OTHER                       2
## 10 30Apr2018     00D               E38000047      NHS Durham Dales, Easington and Sedgefield CCG WHITE                    1630
## # … with 9,740 more rows
```

## 
###7. Rearrange the data table to show decreasing order of Dementia Indicator. This will be a good indicator as to the highest race coming into clinics with Dementia diagnoses

```r
dementia_data <-dementia_data %>%
  arrange(desc(Dementia_Rate_Indicator))
dementia_data
```

```
## # A tibble: 9,750 x 6
##    Recorded_Date Organization_Code GEOGRAPHY_CODE Clinical_Commisioning_Groups Race  Dementia_Rate_Indicator
##    <chr>         <chr>             <chr>          <chr>                        <chr>                   <dbl>
##  1 31Jan2019     15F               E38000225      NHS Leeds CCG                WHITE                    3117
##  2 31Dec2018     15F               E38000225      NHS Leeds CCG                WHITE                    3095
##  3 30Nov2018     15F               E38000225      NHS Leeds CCG                WHITE                    3077
##  4 31Oct2018     15F               E38000225      NHS Leeds CCG                WHITE                    3027
##  5 30Sep2018     15F               E38000225      NHS Leeds CCG                WHITE                    2942
##  6 31Aug2018     15F               E38000225      NHS Leeds CCG                WHITE                    2787
##  7 31Jul2018     15F               E38000225      NHS Leeds CCG                WHITE                    2755
##  8 30Jun2018     15F               E38000225      NHS Leeds CCG                WHITE                    2695
##  9 31May2018     15F               E38000225      NHS Leeds CCG                WHITE                    2638
## 10 30Apr2018     15F               E38000225      NHS Leeds CCG                WHITE                    2585
## # … with 9,740 more rows
```

## 
###8. Find the total number of people coming into the NIH clinics for each Race between April 2018 to January 2019

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
#### total represents number of people who have come into clinics within each race

##Observation
-Notice how from this data, the White and Asian Races have the highest Dementia Diagnoses, while Black data is closest to 0. This should be very apparent in our upcoming plots.

##
### 9. Making a plot to visually show the difference between. Plot is shown for each month and shows barplot comparing Races showing Dementia

```r
dementia_data %>% 
  ggplot(aes(x=Race, y=Dementia_Rate_Indicator, fill=Race))+
  geom_bar(stat="identity")+
  theme(plot.title = element_text(size = rel(1.95)))+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1))
```

![](Dementia_Study_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## 
###10.Find the Clincal Commisioning Group where the Dementia_Rate_Indicator is highest for each Race. 

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
#### total represents number of people who have come into each specific clinic

## 
###11. Finding top 5 clinical commisioning groups with the most amount of clinic visits and putting into new data frame

```r
CCG_data <- dementia_data %>% 
  select (Clinical_Commisioning_Groups, Race, Dementia_Rate_Indicator) %>% 
  filter(Clinical_Commisioning_Groups=="NHS Leeds CCG"|
           Clinical_Commisioning_Groups=="NHS Birmingham and Solihull CCG"| 
           Clinical_Commisioning_Groups=="NHS Mid Essex CCG"| 
           Clinical_Commisioning_Groups=="NHS Bedfordshire CCG" | 
           Clinical_Commisioning_Groups=="NHS Leicester City CCG")
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

## 
###12 .Make a plot showing the breakdown for Race by the Top 5 Clinical Commisioning groups

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

## Conclusion
### There seems to be a significantly higher amount of Whites coming into the clinics. This may indicate a social difference between White and other Races. For example, other Races may have cultural backgrounds that stigmatizes mental health or going to hospitals regularly for chekcups. We were not sue how to work around this significantly higher data, but it does bring about an important topic for discussion regarding mental health, social taboos, financial burders, and personal care.

## Facts from the study
###464,413 people had a diagnosis of demenia as of January 31st 2019. Camden CCG has the highest estimated dementia diagnosis rate, at 90.60%. Kernow CCG has the lowest estimated dementia diagnosis rate, at 52.10%
