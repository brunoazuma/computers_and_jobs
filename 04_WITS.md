Computers and jobs - WITS
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
exploratory analysis and preparation of data from the World Integrated
Trade Solution (WITS), developed by the World Bank, in collaboration
with the United Nations Conference on Trade and Development (UNCTAD) and
in consultation with organizations such as International Trade Center,
United Nations Statistical Division (UNSD) and the World Trade
Organization (WTO). The database is available for simple exploration at
<https://wits.worldbank.org/Default.aspx?lang=en> and for advanced
queries via API.

``` r
# install.packages("httr2")
library("httr2")
# install.packages('arrow')
library(arrow, warn.conflicts = FALSE)
# install.packages("tidyverse")
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::duration() masks arrow::duration()
    ## ✖ dplyr::filter()       masks stats::filter()
    ## ✖ dplyr::lag()          masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# install.packages("XML")
# library(XML)
# # install.packages("xml2")
library(xml2)
```

    ## 
    ## Attaching package: 'xml2'
    ## 
    ## The following object is masked from 'package:httr2':
    ## 
    ##     url_parse

``` r
# install.packages("rvest")
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
# install.packages("janitor")
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

# Countries

The countries available as reporters or partners aren’t actually
contries, but also can represent groups of countries. To standartize my
future queries, first I get the data of all the countries available and
keep only the single countries records.

``` r
url = "http://wits.worldbank.org/API/V1/wits/datasource/trn/country/ALL"

countries_doc <- url %>% read_xml
```

``` r
country_nodes <- countries_doc %>% xml_find_all("//wits:country")
```

``` r
countries <- data.frame(
  countrycode=country_nodes %>% xml_attr("countrycode"),
  iso3Code = country_nodes %>% xml_find_first(".//ancestor::wits:iso3Code") %>% xml_text,
  name = country_nodes %>% xml_find_first(".//ancestor::wits:name") %>% xml_text,
  isReporter=country_nodes %>% xml_attr("isreporter"),
  isPartner=country_nodes %>% xml_attr("ispartner"),
  isGroup=country_nodes %>% xml_attr("isgroup"),
  groupType=country_nodes %>% xml_attr("grouptype")
) %>%
  as_tibble %>%
  mutate (
    isReporter=if_else(isReporter=='1', TRUE, FALSE),
    isPartner=if_else(isPartner=='1', TRUE, FALSE),
    isGroup=if_else(isGroup=='Yes', TRUE, FALSE)
  )

countries %>% head
```

    ## # A tibble: 6 × 7
    ##   countrycode iso3Code name               isReporter isPartner isGroup groupType
    ##   <chr>       <chr>    <chr>              <lgl>      <lgl>     <lgl>   <chr>    
    ## 1 A01         A01      Selected Developi… FALSE      TRUE      TRUE    Benefici…
    ## 2 A02         A02      Caribbean Economi… FALSE      TRUE      TRUE    Benefici…
    ## 3 A03         A03      Caribbean Economi… FALSE      TRUE      TRUE    Benefici…
    ## 4 A04         A04      Preferential tari… FALSE      TRUE      TRUE    Benefici…
    ## 5 A05         A05      Norway tariff for… FALSE      TRUE      TRUE    Benefici…
    ## 6 A07         A07      Preferential tari… FALSE      TRUE      TRUE    Benefici…

``` r
countries_no_groups <- countries %>% filter(isGroup==FALSE)
countries_no_groups %>% head
```

    ## # A tibble: 6 × 7
    ##   countrycode iso3Code name        isReporter isPartner isGroup groupType
    ##   <chr>       <chr>    <chr>       <lgl>      <lgl>     <lgl>   <chr>    
    ## 1 533         ABW      Aruba       TRUE       TRUE      FALSE   N/A      
    ## 2 004         AFG      Afghanistan TRUE       TRUE      FALSE   N/A      
    ## 3 024         AGO      Angola      TRUE       FALSE     FALSE   N/A      
    ## 4 660         AIA      Anguila     TRUE       FALSE     FALSE   N/A      
    ## 5 008         ALB      Albania     TRUE       TRUE      FALSE   N/A      
    ## 6 020         AND      Andorra     FALSE      TRUE      FALSE   N/A

For some reason, European Union isn’t classified as a group, so I
manually remove this record from the countries dataset.

``` r
countries_no_groups <- countries_no_groups %>% filter(iso3Code != 'EUN')
```

# Exports data

Now, the exports data by HS 6 digit product isn’t available through the
API, so I’ll have to scrap it from html. The exports data is available
for all countries - one year - one product code level. As the preferred
code for personal computers (847130) isn’t available, I’ll get the data
on all the 4 digit group.

``` r
years <- seq(2006, 2021)
products <- c(847110, 847120, 847191, 847192, 847193, 847199)

base_url <- "https://wits.worldbank.org/trade/comtrade/en/country/ALL/year/${year}/tradeflow/Exports/partner/WLD/product/${product}"
trades <- NULL

for (product in products) {
  for (year in years) {
    
    print(str_interp("Downloading ${year} trades data for ${product} product..."))
    trades_ <- read_html(str_interp(base_url))%>%
      html_node("table#dataCatalogMetadata") %>%
      html_table %>%
      row_to_names(row_number = 1)
    
    
    if (is.null(trades)) {
      trades <- trades_
    } else {
      trades <- bind_rows(trades, trades_)
    }
  }
}
```

    ## [1] "Downloading 2006 trades data for 847110 product..."
    ## [1] "Downloading 2007 trades data for 847110 product..."
    ## [1] "Downloading 2008 trades data for 847110 product..."
    ## [1] "Downloading 2009 trades data for 847110 product..."
    ## [1] "Downloading 2010 trades data for 847110 product..."
    ## [1] "Downloading 2011 trades data for 847110 product..."
    ## [1] "Downloading 2012 trades data for 847110 product..."
    ## [1] "Downloading 2013 trades data for 847110 product..."
    ## [1] "Downloading 2014 trades data for 847110 product..."
    ## [1] "Downloading 2015 trades data for 847110 product..."
    ## [1] "Downloading 2016 trades data for 847110 product..."
    ## [1] "Downloading 2017 trades data for 847110 product..."
    ## [1] "Downloading 2018 trades data for 847110 product..."
    ## [1] "Downloading 2019 trades data for 847110 product..."
    ## [1] "Downloading 2020 trades data for 847110 product..."
    ## [1] "Downloading 2021 trades data for 847110 product..."
    ## [1] "Downloading 2006 trades data for 847120 product..."
    ## [1] "Downloading 2007 trades data for 847120 product..."
    ## [1] "Downloading 2008 trades data for 847120 product..."
    ## [1] "Downloading 2009 trades data for 847120 product..."
    ## [1] "Downloading 2010 trades data for 847120 product..."
    ## [1] "Downloading 2011 trades data for 847120 product..."
    ## [1] "Downloading 2012 trades data for 847120 product..."
    ## [1] "Downloading 2013 trades data for 847120 product..."
    ## [1] "Downloading 2014 trades data for 847120 product..."
    ## [1] "Downloading 2015 trades data for 847120 product..."
    ## [1] "Downloading 2016 trades data for 847120 product..."
    ## [1] "Downloading 2017 trades data for 847120 product..."
    ## [1] "Downloading 2018 trades data for 847120 product..."
    ## [1] "Downloading 2019 trades data for 847120 product..."
    ## [1] "Downloading 2020 trades data for 847120 product..."
    ## [1] "Downloading 2021 trades data for 847120 product..."
    ## [1] "Downloading 2006 trades data for 847191 product..."
    ## [1] "Downloading 2007 trades data for 847191 product..."
    ## [1] "Downloading 2008 trades data for 847191 product..."
    ## [1] "Downloading 2009 trades data for 847191 product..."
    ## [1] "Downloading 2010 trades data for 847191 product..."
    ## [1] "Downloading 2011 trades data for 847191 product..."
    ## [1] "Downloading 2012 trades data for 847191 product..."
    ## [1] "Downloading 2013 trades data for 847191 product..."
    ## [1] "Downloading 2014 trades data for 847191 product..."
    ## [1] "Downloading 2015 trades data for 847191 product..."
    ## [1] "Downloading 2016 trades data for 847191 product..."
    ## [1] "Downloading 2017 trades data for 847191 product..."
    ## [1] "Downloading 2018 trades data for 847191 product..."
    ## [1] "Downloading 2019 trades data for 847191 product..."
    ## [1] "Downloading 2020 trades data for 847191 product..."
    ## [1] "Downloading 2021 trades data for 847191 product..."
    ## [1] "Downloading 2006 trades data for 847192 product..."
    ## [1] "Downloading 2007 trades data for 847192 product..."
    ## [1] "Downloading 2008 trades data for 847192 product..."
    ## [1] "Downloading 2009 trades data for 847192 product..."
    ## [1] "Downloading 2010 trades data for 847192 product..."
    ## [1] "Downloading 2011 trades data for 847192 product..."
    ## [1] "Downloading 2012 trades data for 847192 product..."
    ## [1] "Downloading 2013 trades data for 847192 product..."
    ## [1] "Downloading 2014 trades data for 847192 product..."
    ## [1] "Downloading 2015 trades data for 847192 product..."
    ## [1] "Downloading 2016 trades data for 847192 product..."
    ## [1] "Downloading 2017 trades data for 847192 product..."
    ## [1] "Downloading 2018 trades data for 847192 product..."
    ## [1] "Downloading 2019 trades data for 847192 product..."
    ## [1] "Downloading 2020 trades data for 847192 product..."
    ## [1] "Downloading 2021 trades data for 847192 product..."
    ## [1] "Downloading 2006 trades data for 847193 product..."
    ## [1] "Downloading 2007 trades data for 847193 product..."
    ## [1] "Downloading 2008 trades data for 847193 product..."
    ## [1] "Downloading 2009 trades data for 847193 product..."
    ## [1] "Downloading 2010 trades data for 847193 product..."
    ## [1] "Downloading 2011 trades data for 847193 product..."
    ## [1] "Downloading 2012 trades data for 847193 product..."
    ## [1] "Downloading 2013 trades data for 847193 product..."
    ## [1] "Downloading 2014 trades data for 847193 product..."
    ## [1] "Downloading 2015 trades data for 847193 product..."
    ## [1] "Downloading 2016 trades data for 847193 product..."
    ## [1] "Downloading 2017 trades data for 847193 product..."
    ## [1] "Downloading 2018 trades data for 847193 product..."
    ## [1] "Downloading 2019 trades data for 847193 product..."
    ## [1] "Downloading 2020 trades data for 847193 product..."
    ## [1] "Downloading 2021 trades data for 847193 product..."
    ## [1] "Downloading 2006 trades data for 847199 product..."
    ## [1] "Downloading 2007 trades data for 847199 product..."
    ## [1] "Downloading 2008 trades data for 847199 product..."
    ## [1] "Downloading 2009 trades data for 847199 product..."
    ## [1] "Downloading 2010 trades data for 847199 product..."
    ## [1] "Downloading 2011 trades data for 847199 product..."
    ## [1] "Downloading 2012 trades data for 847199 product..."
    ## [1] "Downloading 2013 trades data for 847199 product..."
    ## [1] "Downloading 2014 trades data for 847199 product..."
    ## [1] "Downloading 2015 trades data for 847199 product..."
    ## [1] "Downloading 2016 trades data for 847199 product..."
    ## [1] "Downloading 2017 trades data for 847199 product..."
    ## [1] "Downloading 2018 trades data for 847199 product..."
    ## [1] "Downloading 2019 trades data for 847199 product..."
    ## [1] "Downloading 2020 trades data for 847199 product..."
    ## [1] "Downloading 2021 trades data for 847199 product..."

Now, I filter the data to single countries only.

``` r
trades_no_groups <- trades %>%
  filter(Reporter %in% countries_no_groups$name)
trades_no_groups %>% head
```

    ## # A tibble: 6 × 9
    ##   Reporter       TradeFlow ProductCode `Product Description`       Year  Partner
    ##   <chr>          <chr>     <chr>       <chr>                       <chr> <chr>  
    ## 1 Mexico         Export    847110      Data processing machines; … 2006  World  
    ## 2 United States  Export    847110      Data processing machines; … 2006  World  
    ## 3 United Kingdom Export    847110      Data processing machines; … 2006  World  
    ## 4 Austria        Export    847110      Data processing machines; … 2006  World  
    ## 5 Thailand       Export    847110      Data processing machines; … 2006  World  
    ## 6 Canada         Export    847110      Data processing machines; … 2006  World  
    ## # ℹ 3 more variables: `Trade Value 1000USD` <chr>, Quantity <chr>,
    ## #   `Quantity Unit` <chr>

Now, I transform the year, trade value and quantity columns in
numerical.

``` r
trades_no_groups <- trades_no_groups %>%
  mutate(
    Year=as.integer(Year),
    `Trade Value 1000USD`=as.double(gsub(",","",`Trade Value 1000USD`)),
    Quantity=as.integer(gsub(",","",Quantity))
  )
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `Quantity = as.integer(gsub(",", "", Quantity))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion to integer range

``` r
trades_no_groups %>% head
```

    ## # A tibble: 6 × 9
    ##   Reporter       TradeFlow ProductCode `Product Description`        Year Partner
    ##   <chr>          <chr>     <chr>       <chr>                       <int> <chr>  
    ## 1 Mexico         Export    847110      Data processing machines; …  2006 World  
    ## 2 United States  Export    847110      Data processing machines; …  2006 World  
    ## 3 United Kingdom Export    847110      Data processing machines; …  2006 World  
    ## 4 Austria        Export    847110      Data processing machines; …  2006 World  
    ## 5 Thailand       Export    847110      Data processing machines; …  2006 World  
    ## 6 Canada         Export    847110      Data processing machines; …  2006 World  
    ## # ℹ 3 more variables: `Trade Value 1000USD` <dbl>, Quantity <int>,
    ## #   `Quantity Unit` <chr>

And summarise the data by country and year.

``` r
trades_no_groups <- trades_no_groups %>%
  group_by(Reporter, Year) %>%
  summarise(
    `Trade Value 1000USD` = sum(`Trade Value 1000USD`),
    Quantity=sum(Quantity)
  ) %>%
  arrange(
    Reporter,
    Year
  )
```

    ## `summarise()` has grouped output by 'Reporter'. You can override using the
    ## `.groups` argument.

Now, create a combination of all countries and years to hold the final
dataset.

``` r
final_trades <- crossing(countries_no_groups$name, years) %>%
  rename(
    "reporter" = "countries_no_groups$name",
    "year" = "years"
  )
final_trades
```

    ## # A tibble: 3,152 × 2
    ##    reporter     year
    ##    <chr>       <int>
    ##  1 Afghanistan  2006
    ##  2 Afghanistan  2007
    ##  3 Afghanistan  2008
    ##  4 Afghanistan  2009
    ##  5 Afghanistan  2010
    ##  6 Afghanistan  2011
    ##  7 Afghanistan  2012
    ##  8 Afghanistan  2013
    ##  9 Afghanistan  2014
    ## 10 Afghanistan  2015
    ## # ℹ 3,142 more rows

Filter the final dataset so that it contains only the countries that
appear at least one time on the trades dataset.

``` r
final_trades <- final_trades %>%
  filter(reporter %in% trades_no_groups$Reporter)
final_trades
```

    ## # A tibble: 2,784 × 2
    ##    reporter  year
    ##    <chr>    <int>
    ##  1 Albania   2006
    ##  2 Albania   2007
    ##  3 Albania   2008
    ##  4 Albania   2009
    ##  5 Albania   2010
    ##  6 Albania   2011
    ##  7 Albania   2012
    ##  8 Albania   2013
    ##  9 Albania   2014
    ## 10 Albania   2015
    ## # ℹ 2,774 more rows

And bring the data from trades dataset into the final dataset.

``` r
final_trades<- final_trades %>%
  left_join(trades_no_groups, by=join_by(reporter==Reporter, year==Year))
final_trades
```

    ## # A tibble: 2,784 × 4
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <int>                 <dbl>    <int>
    ##  1 Albania   2006                  274.      527
    ##  2 Albania   2007                  317.       NA
    ##  3 Albania   2008                 1003.       NA
    ##  4 Albania   2009                  616.    11974
    ##  5 Albania   2010                 1047.     4047
    ##  6 Albania   2011                  697.     5896
    ##  7 Albania   2012                  825.    12284
    ##  8 Albania   2013                 1150.     5924
    ##  9 Albania   2014                  153.     1273
    ## 10 Albania   2015                  280.     4773
    ## # ℹ 2,774 more rows

``` r
na_qts <- final_trades %>%
  filter(is.na(Quantity)) %>%
  select(reporter)
```

``` r
final_trades <- final_trades %>%
  filter(!reporter %in% na_qts$reporter)
final_trades
```

    ## # A tibble: 272 × 4
    ##    reporter   year `Trade Value 1000USD` Quantity
    ##    <chr>     <int>                 <dbl>    <int>
    ##  1 Argentina  2006                13878.    85676
    ##  2 Argentina  2007                20000     83043
    ##  3 Argentina  2008                14504.   100520
    ##  4 Argentina  2009                18224.    44723
    ##  5 Argentina  2010                 9932.    56892
    ##  6 Argentina  2011                13856.    76958
    ##  7 Argentina  2012                 8990.    45540
    ##  8 Argentina  2013                32700.    56247
    ##  9 Argentina  2014                82803.   112749
    ## 10 Argentina  2015                19010.    77484
    ## # ℹ 262 more rows

# Missings validations

As the final dataset is just a part of all the available dataset (due to
the exclusion of missings), it’s worth doing some evaluations about the
dataset to be sure it’s still good enough.

First, let’s check how much of the trade value by year it contains.

``` r
total_trade_value <- trades_no_groups %>%
  group_by(Year) %>%
  summarise(
    `Total Trade Value 1000USD`=sum(`Trade Value 1000USD`)
  )
```

``` r
final_trade_value <- final_trades %>%
  group_by(year) %>%
  summarise(
    `Total Trade Value 1000USD`=sum(`Trade Value 1000USD`)
  )
```

``` r
final_trade_value %>%
  left_join(total_trade_value, by=join_by(year==Year)) %>%
  mutate(
    share=`Total Trade Value 1000USD.x`/`Total Trade Value 1000USD.y`
  )
```

    ## # A tibble: 16 × 4
    ##     year `Total Trade Value 1000USD.x` `Total Trade Value 1000USD.y`  share
    ##    <int>                         <dbl>                         <dbl>  <dbl>
    ##  1  2006                     21026355.                    231072139. 0.0910
    ##  2  2007                     14661869.                    246227764. 0.0595
    ##  3  2008                     13608238.                    253311857. 0.0537
    ##  4  2009                      9623765.                    219395438. 0.0439
    ##  5  2010                      9843504.                    275205599. 0.0358
    ##  6  2011                     10199204.                    295505231. 0.0345
    ##  7  2012                     12145049.                    351368004. 0.0346
    ##  8  2013                     12352072.                    337485955. 0.0366
    ##  9  2014                     12577569.                    345090941. 0.0364
    ## 10  2015                     11159806.                    306936257. 0.0364
    ## 11  2016                     10703943.                    288780109. 0.0371
    ## 12  2017                      6723179.                    232504176. 0.0289
    ## 13  2018                      7463330.                    253623908. 0.0294
    ## 14  2019                      7913666.                    242400457. 0.0326
    ## 15  2020                      7460319.                    266334322. 0.0280
    ## 16  2021                      8604344.                    313112543. 0.0275

There was a considerable loss. The dataset contains less then 5% of the
global trade value, so it is better to make use of another approach for
the missing values.

The second approach will be: a) exclude all countries with non reported
quantity in the first year of the sample; b) exclude all countries with
less than half of the periods with reported quantities;and c) fill the
non reported values with the previous period value.

# Filling missings with previous period

First, I rebuild the dataset with non reported quantities.

``` r
final_trades <- crossing(countries_no_groups$name, years) %>%
  rename(
    "reporter" = "countries_no_groups$name",
    "year" = "years"
  )  %>%
  filter(reporter %in% trades_no_groups$Reporter) %>%
  left_join(trades_no_groups, by=join_by(reporter==Reporter, year==Year))
final_trades
```

    ## # A tibble: 2,784 × 4
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <int>                 <dbl>    <int>
    ##  1 Albania   2006                  274.      527
    ##  2 Albania   2007                  317.       NA
    ##  3 Albania   2008                 1003.       NA
    ##  4 Albania   2009                  616.    11974
    ##  5 Albania   2010                 1047.     4047
    ##  6 Albania   2011                  697.     5896
    ##  7 Albania   2012                  825.    12284
    ##  8 Albania   2013                 1150.     5924
    ##  9 Albania   2014                  153.     1273
    ## 10 Albania   2015                  280.     4773
    ## # ℹ 2,774 more rows

Now, I exclude the countries with non reported quantity on the first
year of the sample.

``` r
na_first_year <- final_trades %>%
  filter(is.na(Quantity) & year==years[1]) %>%
  select(reporter) %>%
  unique

final_trades <- final_trades %>%
  filter(!reporter %in% na_first_year$reporter)
final_trades
```

    ## # A tibble: 2,032 × 4
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <int>                 <dbl>    <int>
    ##  1 Albania   2006                  274.      527
    ##  2 Albania   2007                  317.       NA
    ##  3 Albania   2008                 1003.       NA
    ##  4 Albania   2009                  616.    11974
    ##  5 Albania   2010                 1047.     4047
    ##  6 Albania   2011                  697.     5896
    ##  7 Albania   2012                  825.    12284
    ##  8 Albania   2013                 1150.     5924
    ##  9 Albania   2014                  153.     1273
    ## 10 Albania   2015                  280.     4773
    ## # ℹ 2,022 more rows

Now, I remove the countries with less than half of the periods with
reported quantities.

``` r
less_than_half <- final_trades %>%
  mutate(
    qt_reported = !is.na(Quantity)
  ) %>%
  summarise(
    sum_qt_reported = sum(qt_reported),
    .by = reporter
  ) %>%
  filter(
    sum_qt_reported < length(years)/2
  ) %>%
  select(reporter) %>%
  unique

final_trades <- final_trades %>%
  filter(!reporter %in% less_than_half$reporter)
final_trades
```

    ## # A tibble: 1,136 × 4
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <int>                 <dbl>    <int>
    ##  1 Albania   2006                  274.      527
    ##  2 Albania   2007                  317.       NA
    ##  3 Albania   2008                 1003.       NA
    ##  4 Albania   2009                  616.    11974
    ##  5 Albania   2010                 1047.     4047
    ##  6 Albania   2011                  697.     5896
    ##  7 Albania   2012                  825.    12284
    ##  8 Albania   2013                 1150.     5924
    ##  9 Albania   2014                  153.     1273
    ## 10 Albania   2015                  280.     4773
    ## # ℹ 1,126 more rows

Now, I fill the non reported quantities with the previous values.

``` r
while (final_trades %>% filter(is.na(Quantity)) %>% nrow > 0) {
  final_trades <- final_trades %>%
    group_by(reporter) %>%
    mutate(
      Quantity=ifelse(!is.na(Quantity), Quantity, lag(Quantity))
    )
}

final_trades
```

    ## # A tibble: 1,136 × 4
    ## # Groups:   reporter [71]
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <int>                 <dbl>    <int>
    ##  1 Albania   2006                  274.      527
    ##  2 Albania   2007                  317.      527
    ##  3 Albania   2008                 1003.      527
    ##  4 Albania   2009                  616.    11974
    ##  5 Albania   2010                 1047.     4047
    ##  6 Albania   2011                  697.     5896
    ##  7 Albania   2012                  825.    12284
    ##  8 Albania   2013                 1150.     5924
    ##  9 Albania   2014                  153.     1273
    ## 10 Albania   2015                  280.     4773
    ## # ℹ 1,126 more rows

# Final validations

First, let’s check how much of the trade value by year it contains.

``` r
total_trade_value <- trades_no_groups %>%
  group_by(Year) %>%
  summarise(
    `Total Trade Value 1000USD`=sum(`Trade Value 1000USD`, na.rm = TRUE)
  )
```

``` r
final_trade_value <- final_trades %>%
  group_by(year) %>%
  summarise(
    `Total Trade Value 1000USD`=sum(`Trade Value 1000USD`, na.rm = TRUE)
  )
```

``` r
final_trade_value %>%
  left_join(total_trade_value, by=join_by(year==Year)) %>%
  mutate(
    share=`Total Trade Value 1000USD.x`/`Total Trade Value 1000USD.y`
  )
```

    ## # A tibble: 16 × 4
    ##     year `Total Trade Value 1000USD.x` `Total Trade Value 1000USD.y` share
    ##    <int>                         <dbl>                         <dbl> <dbl>
    ##  1  2006                    206243628.                    231072139. 0.893
    ##  2  2007                    218502574.                    246227764. 0.887
    ##  3  2008                    227704946.                    253311857. 0.899
    ##  4  2009                    199452483.                    219395438. 0.909
    ##  5  2010                    253020386.                    275205599. 0.919
    ##  6  2011                    274349247.                    295505231. 0.928
    ##  7  2012                    314301993.                    351368004. 0.895
    ##  8  2013                    306169127.                    337485955. 0.907
    ##  9  2014                    312239558.                    345090941. 0.905
    ## 10  2015                    274528642.                    306936257. 0.894
    ## 11  2016                    258561156.                    288780109. 0.895
    ## 12  2017                    218145612.                    232504176. 0.938
    ## 13  2018                    242444596.                    253623908. 0.956
    ## 14  2019                    230540888.                    242400457. 0.951
    ## 15  2020                    252038202.                    266334322. 0.946
    ## 16  2021                    297255913.                    313112543. 0.949

Now the final dataset contain a fair amount of the global trade value,
with more than 88% of the global share, for each year.

Now, I’ll check the top 5 countries for each year and check how many of
them are still present on the final dataset.

``` r
top_5 <- trades_no_groups %>%
  group_by(Year) %>%
  slice_max(order_by = `Trade Value 1000USD`, n=5) %>%
  ungroup
top_5
```

    ## # A tibble: 80 × 4
    ##    Reporter        Year `Trade Value 1000USD`   Quantity
    ##    <chr>          <int>                 <dbl>      <int>
    ##  1 China           2006             93017370. 1426071823
    ##  2 United States   2006             26584860.   58968739
    ##  3 Malaysia        2006             16321713.  459586190
    ##  4 Singapore       2006             11563986.  155813240
    ##  5 United Kingdom  2006             11320869.  145767083
    ##  6 China           2007            112243867. 1365466000
    ##  7 United States   2007             25346767.   45175820
    ##  8 Malaysia        2007             15639459.   83563074
    ##  9 Thailand        2007             12574957.         NA
    ## 10 Singapore       2007             10056333.  110625500
    ## # ℹ 70 more rows

``` r
top_5 %>% select(Reporter) %>% unique
```

    ## # A tibble: 9 × 1
    ##   Reporter        
    ##   <chr>           
    ## 1 China           
    ## 2 United States   
    ## 3 Malaysia        
    ## 4 Singapore       
    ## 5 United Kingdom  
    ## 6 Thailand        
    ## 7 Hong Kong, China
    ## 8 Mexico          
    ## 9 Czech Republic

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter)
```

    ## # A tibble: 10 × 4
    ##    Reporter  Year `Trade Value 1000USD`  Quantity
    ##    <chr>    <int>                 <dbl>     <int>
    ##  1 Thailand  2007             12574957.        NA
    ##  2 Thailand  2008             13397055.        NA
    ##  3 Thailand  2009             11281266. 386165530
    ##  4 Thailand  2010             13025752.        NA
    ##  5 Thailand  2011             11284266.        NA
    ##  6 Thailand  2012             15214795.        NA
    ##  7 Thailand  2013             13443937.        NA
    ##  8 Thailand  2014             13928100.        NA
    ##  9 Thailand  2015             13639466.        NA
    ## 10 Thailand  2016             12576596.        NA

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter) %>% select(Reporter) %>% unique
```

    ## # A tibble: 1 × 1
    ##   Reporter
    ##   <chr>   
    ## 1 Thailand

The final dataset misses only Thailand, because it doesn’t report
quantities for most of the years in sample. As so, the final dataset
seems to be good enough for my purposes.

To avoid the influence of domestic demand on exports, I’ll follow
Boustan, Choi e Clingingsmith (2022) and keep only the countries that
figured on the top 3.

``` r
top_3 <- trades_no_groups %>%
  group_by(Year) %>%
  slice_max(order_by = `Trade Value 1000USD`, n=3) %>%
  ungroup %>%
  select(Reporter) %>%
  unique
top_3
```

    ## # A tibble: 6 × 1
    ##   Reporter        
    ##   <chr>           
    ## 1 China           
    ## 2 United States   
    ## 3 Malaysia        
    ## 4 Thailand        
    ## 5 Mexico          
    ## 6 Hong Kong, China

``` r
final_trades <- final_trades %>%
  filter(reporter %in% top_3$Reporter)

final_trades
```

    ## # A tibble: 80 × 4
    ## # Groups:   reporter [5]
    ##    reporter  year `Trade Value 1000USD`   Quantity
    ##    <chr>    <int>                 <dbl>      <int>
    ##  1 China     2006             93017370. 1426071823
    ##  2 China     2007            112243867. 1365466000
    ##  3 China     2008            122727667. 1359163400
    ##  4 China     2009            111890627. 1250304300
    ##  5 China     2010            148802627. 1577944300
    ##  6 China     2011            160121815. 1748174800
    ##  7 China     2012            186014976. 1834627000
    ##  8 China     2013            183000233. 1871969800
    ##  9 China     2014            182688761. 1920601100
    ## 10 China     2015            153213658. 1717687200
    ## # ℹ 70 more rows

Finally, I save the final table as parquet.

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs/")
datadir <- "data/WITS/"
filename <- "WITS_trades"

write_parquet(final_trades, str_interp("${datadir}${filename}.parquet"))
```

<div id="refs" class="references csl-bib-body">

<div id="ref-nberw30400" class="csl-entry">

BOUSTAN, L. P.; CHOI, J.; CLINGINGSMITH, D. **Automation After the
Assembly Line: Computerized Machine Tools, Employment and Productivity
in the United States**. ago. 2022. Disponível em:
\<<https://www.nber.org/papers/w30400>\>.

</div>

</div>
