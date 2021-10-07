Assignment 2
================
Caroline He
10/6/2021

Library packages

``` r
library(data.table)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(dtplyr)
```

# Data Wrangling

#### Download and read in the data

``` r
if (!file.exists("chs_individual.csv")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv",
              destfile = "chs_individual.csv", 
              method="libcurl", 
              timeout = 60
              )
}
individual <- data.table::fread("chs_individual.csv")

if (!file.exists("chs_regional.csv")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv",
              destfile = "chs_regional.csv", 
              method="libcurl", 
              timeout = 60
              )
}
regional <- data.table::fread("chs_regional.csv")
```

#### Merge the data

``` r
ind_reg <- merge(
  x = individual,
  y = regional,
  by.x = "townname",
  by.y = "townname",
  all.x = TRUE,
  all.y = FALSE
)
```

#### Check for the Duplicate

After merging the data, make sure you don’t have any duplicates by
counting the number of rows. Make sure it matches. In the case of
missing values, impute data using the average within the variables
“male” and “hispanic.”

``` r
dim(ind_reg)
```

    ## [1] 1200   49

``` r
summary(is.na(ind_reg))
```

    ##   townname          sid             male            race        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##   hispanic         agepft          height          weight       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1111      FALSE:1111      FALSE:1111     
    ##                  TRUE :89        TRUE :89        TRUE :89       
    ##     bmi            asthma        active_asthma   father_asthma  
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1111      FALSE:1169      FALSE:1200      FALSE:1094     
    ##  TRUE :89        TRUE :31                        TRUE :106      
    ##  mother_asthma     wheeze         hayfever        allergy       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1144      FALSE:1129      FALSE:1082      FALSE:1137     
    ##  TRUE :56        TRUE :71        TRUE :118       TRUE :63       
    ##  educ_parent       smoke            pets          gasstove      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1136      FALSE:1160      FALSE:1200      FALSE:1167     
    ##  TRUE :64        TRUE :40                        TRUE :33       
    ##     fev             fvc             mmef         pm25_mass      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1105      FALSE:1103      FALSE:1094      FALSE:1200     
    ##  TRUE :95        TRUE :97        TRUE :106                      
    ##   pm25_so4        pm25_no3        pm25_nh4        pm25_oc       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##   pm25_ec         pm25_om         pm10_oc         pm10_ec       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##   pm10_tc          formic          acetic           hcl         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##     hno3           o3_max          o3106           o3_24        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##     no2             pm10          no_24hr         pm2_5_fr      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1100      FALSE:900      
    ##                                  TRUE :100       TRUE :300      
    ##    iacid           oacid         total_acids        lon         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##                                                                 
    ##     lat         
    ##  Mode :logical  
    ##  FALSE:1200     
    ## 

``` r
# Group the data by male and Hispanic characteristics
male_hispanic <- ind_reg[male == 1 & hispanic == 1]
```

``` r
#replacing NAs by means
ind_reg[is.na(agepft), agepft := mean(male_hispanic$agepft, na.rm = TRUE)]
ind_reg[is.na(height), height := mean(male_hispanic$height, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(height), `:=`(height,
    ## mean(male_hispanic$height, : 138.598394 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 7 named
    ## 'height')

``` r
ind_reg[is.na(weight), weight := mean(male_hispanic$weight, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(weight), `:=`(weight,
    ## mean(male_hispanic$weight, : 82.767068 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 8 named
    ## 'weight')

``` r
ind_reg[is.na(bmi), bmi := mean(male_hispanic$bmi, na.rm = TRUE)]
ind_reg[is.na(asthma), asthma := mean(male_hispanic$asthma, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(asthma), `:=`(asthma,
    ## mean(male_hispanic$asthma, : 0.160156 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 10 named
    ## 'asthma')

``` r
ind_reg[is.na(father_asthma), father_asthma := mean(male_hispanic$father_asthma, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(father_asthma), `:=`(father_asthma, :
    ## 0.084034 (type 'double') at RHS position 1 truncated (precision lost) when
    ## assigning to type 'integer' (column 12 named 'father_asthma')

``` r
ind_reg[is.na(mother_asthma), mother_asthma := mean(male_hispanic$mother_asthma, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(mother_asthma), `:=`(mother_asthma, :
    ## 0.106719 (type 'double') at RHS position 1 truncated (precision lost) when
    ## assigning to type 'integer' (column 13 named 'mother_asthma')

``` r
ind_reg[is.na(wheeze), wheeze := mean(male_hispanic$wheeze, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(wheeze), `:=`(wheeze,
    ## mean(male_hispanic$wheeze, : 0.353414 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 14 named
    ## 'wheeze')

``` r
ind_reg[is.na(hayfever), hayfever := mean(male_hispanic$hayfever, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(hayfever), `:=`(hayfever,
    ## mean(male_hispanic$hayfever, : 0.174468 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 15 named
    ## 'hayfever')

``` r
ind_reg[is.na(allergy), allergy := mean(male_hispanic$allergy, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(allergy), `:=`(allergy,
    ## mean(male_hispanic$allergy, : 0.254032 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 16 named
    ## 'allergy')

``` r
ind_reg[is.na(educ_parent), educ_parent := mean(male_hispanic$educ_parent, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(educ_parent), `:=`(educ_parent, :
    ## 2.423868 (type 'double') at RHS position 1 truncated (precision lost) when
    ## assigning to type 'integer' (column 17 named 'educ_parent')

``` r
ind_reg[is.na(smoke), smoke := mean(male_hispanic$smoke, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(smoke), `:=`(smoke,
    ## mean(male_hispanic$smoke, : 0.150198 (type 'double') at RHS position 1 truncated
    ## (precision lost) when assigning to type 'integer' (column 18 named 'smoke')

``` r
ind_reg[is.na(gasstove), gasstove := mean(male_hispanic$gasstove, na.rm = TRUE)]
```

    ## Warning in `[.data.table`(ind_reg, is.na(gasstove), `:=`(gasstove,
    ## mean(male_hispanic$gasstove, : 0.815686 (type 'double') at RHS position 1
    ## truncated (precision lost) when assigning to type 'integer' (column 20 named
    ## 'gasstove')

``` r
ind_reg[is.na(fev), fev := mean(male_hispanic$fev, na.rm = TRUE)]
ind_reg[is.na(fvc), fvc := mean(male_hispanic$fvc, na.rm = TRUE)]
ind_reg[is.na(mmef), mmef := mean(male_hispanic$mmef, na.rm = TRUE)]
ind_reg[is.na(no_24hr), no_24hr := mean(male_hispanic$no_24hr, na.rm = TRUE)]
ind_reg[is.na(pm2_5_fr), pm2_5_fr := mean(male_hispanic$pm2_5_fr, na.rm = TRUE)]
summary(is.na(ind_reg))
```

    ##   townname          sid             male            race        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##   hispanic         agepft          height          weight       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##     bmi            asthma        active_asthma   father_asthma  
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##  mother_asthma     wheeze         hayfever        allergy       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##  educ_parent       smoke            pets          gasstove      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##     fev             fvc             mmef         pm25_mass      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##   pm25_so4        pm25_no3        pm25_nh4        pm25_oc       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##   pm25_ec         pm25_om         pm10_oc         pm10_ec       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##   pm10_tc          formic          acetic           hcl         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##     hno3           o3_max          o3106           o3_24        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##     no2             pm10          no_24hr         pm2_5_fr      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##    iacid           oacid         total_acids        lon         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:1200      FALSE:1200      FALSE:1200      FALSE:1200     
    ##     lat         
    ##  Mode :logical  
    ##  FALSE:1200

``` r
suppressWarnings(warning("ind_reg"))
```

All NAs were replaced by means

#### Create a new categorical variable named “obesity\_level” and a summary table

Create a new categorical variable named “obesity\_level” using the BMI
measurement (underweight BMI&lt;14; normal BMI 14-22; overweight BMI
22-24; obese BMI&gt;24). To make sure the variable is rightly coded,
create a summary table that contains the minimum BMI, maximum BMI, and
the total number of observations per category.

``` r
#create a new variable
ind_reg[, obesity_level := 
          fifelse(bmi < 14, "underweight",
                  fifelse(bmi >= 14 & bmi < 22, "normal",
                          fifelse(bmi >= 22 & bmi < 24, "overweight",
                                  fifelse(bmi >= 24, "obese", "none"))))]
```

``` r
#create a summary table
summary_obesity <- ind_reg[,.(
  max_bmi = max(bmi),
  min_bmi = min(bmi),
  total = length(bmi)
), by = obesity_level]
knitr::kable(summary_obesity)
```

| obesity\_level | max\_bmi | min\_bmi | total |
|:---------------|---------:|---------:|------:|
| normal         | 21.96387 | 14.00380 |   975 |
| overweight     | 23.99650 | 22.02353 |    87 |
| obese          | 41.26613 | 24.00647 |   103 |
| underweight    | 13.98601 | 11.29640 |    35 |

#### Create categorical variable “smoke\_gas\_exposure”

Create another categorical variable named “smoke\_gas\_exposure” that
summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have
four categories in total.

``` r
ind_reg[, smoke_gas_exposure := 
          fifelse(smoke == 0 & gasstove == 0, "no_expo",
                  fifelse(smoke == 1 & gasstove == 0, "smoke_expo",
                          fifelse(smoke == 0 & gasstove == 1, "gas_expo",
                                  fifelse(smoke == 1 & gasstove == 1, "smole_gas_expo","none"))))]
```

#### Creating a summary table of FEV

Create four summary tables showing the average (or proportion, if
binary) and sd of “Forced expiatory volume in 1 second (ml)” and asthma
indicator by town, sex, obesity level, and “smoke\_gas\_exposure.”

``` r
#by town name
summary_fev <- ind_reg[,.(
  avg_fev = mean(fev),
  sd_fev = sd(fev),
  avg_asthma = mean(asthma),
  sd_asthma = sd(asthma)
), by = townname]
knitr::kable(summary_fev)
```

| townname      | avg\_fev |  sd\_fev | avg\_asthma | sd\_asthma |
|:--------------|---------:|---------:|------------:|-----------:|
| Alpine        | 2090.576 | 290.6566 |        0.11 |  0.3144660 |
| Atascadero    | 2081.418 | 323.5128 |        0.25 |  0.4351941 |
| Lake Elsinore | 2047.030 | 303.7138 |        0.12 |  0.3265986 |
| Lake Gregory  | 2094.811 | 318.6348 |        0.15 |  0.3588703 |
| Lancaster     | 2016.676 | 318.3450 |        0.16 |  0.3684529 |
| Lompoc        | 2045.610 | 351.1411 |        0.11 |  0.3144660 |
| Long Beach    | 1993.442 | 320.6366 |        0.13 |  0.3379977 |
| Mira Loma     | 1994.214 | 326.3779 |        0.15 |  0.3588703 |
| Riverside     | 1998.277 | 278.9353 |        0.11 |  0.3144660 |
| San Dimas     | 2029.655 | 318.9705 |        0.17 |  0.3775252 |
| Santa Maria   | 2033.301 | 312.6835 |        0.13 |  0.3379977 |
| Upland        | 2034.723 | 343.4018 |        0.12 |  0.3265986 |

``` r
#by gender
summary_fev <- ind_reg[,.(
  avg_fev = mean(fev),
  sd_fev = sd(fev),
  avg_asthma = mean(asthma),
  sd_asthma = sd(asthma)
), by = male]
knitr::kable(summary_fev)
```

| male | avg\_fev |  sd\_fev | avg\_asthma | sd\_asthma |
|-----:|---------:|---------:|------------:|-----------:|
|    0 | 1973.900 | 315.3421 |   0.1180328 |  0.3229117 |
|    1 | 2104.906 | 307.5156 |   0.1677966 |  0.3740027 |

``` r
#by obesity level
summary_fev <- ind_reg[,.(
  avg_fev = mean(fev),
  sd_fev = sd(fev),
  avg_asthma = mean(asthma),
  sd_asthma = sd(asthma)
), by = obesity_level]
knitr::kable(summary_fev)
```

| obesity\_level | avg\_fev |  sd\_fev | avg\_asthma | sd\_asthma |
|:---------------|---------:|---------:|------------:|-----------:|
| normal         | 2009.639 | 296.4532 |   0.1364103 |  0.3433998 |
| overweight     | 2224.322 | 317.4261 |   0.1609195 |  0.3695869 |
| obese          | 2267.848 | 324.2386 |   0.2038835 |  0.4048535 |
| underweight    | 1699.185 | 304.5785 |   0.0857143 |  0.2840286 |

``` r
#by smoke gas exposure
summary_fev <- ind_reg[,.(
  avg_fev = mean(fev),
  sd_fev = sd(fev),
  avg_asthma = mean(asthma),
  sd_asthma = sd(asthma)
), by = smoke_gas_exposure]
knitr::kable(summary_fev)
```

| smoke\_gas\_exposure | avg\_fev |  sd\_fev | avg\_asthma | sd\_asthma |
|:---------------------|---------:|---------:|------------:|-----------:|
| no\_expo             | 2047.460 | 332.8773 |   0.1365462 |  0.3440592 |
| smoke\_expo          | 2092.556 | 297.2326 |   0.1538462 |  0.3655178 |
| gas\_expo            | 2034.331 | 318.1718 |   0.1471748 |  0.3545131 |
| smole\_gas\_expo     | 2029.273 | 299.6817 |   0.1258278 |  0.3327589 |
