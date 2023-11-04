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
years <- c(2008, 2009, 2010)
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

    ## [1] "Downloading 2008 trades data for 847110 product..."
    ## [1] "Downloading 2009 trades data for 847110 product..."
    ## [1] "Downloading 2010 trades data for 847110 product..."
    ## [1] "Downloading 2008 trades data for 847120 product..."
    ## [1] "Downloading 2009 trades data for 847120 product..."
    ## [1] "Downloading 2010 trades data for 847120 product..."
    ## [1] "Downloading 2008 trades data for 847191 product..."
    ## [1] "Downloading 2009 trades data for 847191 product..."
    ## [1] "Downloading 2010 trades data for 847191 product..."
    ## [1] "Downloading 2008 trades data for 847192 product..."
    ## [1] "Downloading 2009 trades data for 847192 product..."
    ## [1] "Downloading 2010 trades data for 847192 product..."
    ## [1] "Downloading 2008 trades data for 847193 product..."
    ## [1] "Downloading 2009 trades data for 847193 product..."
    ## [1] "Downloading 2010 trades data for 847193 product..."
    ## [1] "Downloading 2008 trades data for 847199 product..."
    ## [1] "Downloading 2009 trades data for 847199 product..."
    ## [1] "Downloading 2010 trades data for 847199 product..."

Now, I filter the data to single countries only.

``` r
trades_no_groups <- trades %>%
  filter(Reporter %in% countries_no_groups$name)
trades_no_groups %>% head
```

    ## # A tibble: 6 × 9
    ##   Reporter  TradeFlow ProductCode `Product Description`            Year  Partner
    ##   <chr>     <chr>     <chr>       <chr>                            <chr> <chr>  
    ## 1 Indonesia Export    847110      Data processing machines; analo… 2008  World  
    ## 2 Ukraine   Export    847110      Data processing machines; analo… 2008  World  
    ## 3 Albania   Export    847110      Data processing machines; analo… 2008  World  
    ## 4 Cameroon  Export    847110      Data processing machines; analo… 2008  World  
    ## 5 Malaysia  Export    847110      Data processing machines; analo… 2008  World  
    ## 6 Botswana  Export    847110      Data processing machines; analo… 2008  World  
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
    ##   Reporter  TradeFlow ProductCode `Product Description`             Year Partner
    ##   <chr>     <chr>     <chr>       <chr>                            <int> <chr>  
    ## 1 Indonesia Export    847110      Data processing machines; analo…  2008 World  
    ## 2 Ukraine   Export    847110      Data processing machines; analo…  2008 World  
    ## 3 Albania   Export    847110      Data processing machines; analo…  2008 World  
    ## 4 Cameroon  Export    847110      Data processing machines; analo…  2008 World  
    ## 5 Malaysia  Export    847110      Data processing machines; analo…  2008 World  
    ## 6 Botswana  Export    847110      Data processing machines; analo…  2008 World  
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

    ## # A tibble: 591 × 2
    ##    reporter     year
    ##    <chr>       <dbl>
    ##  1 Afghanistan  2008
    ##  2 Afghanistan  2009
    ##  3 Afghanistan  2010
    ##  4 Albania      2008
    ##  5 Albania      2009
    ##  6 Albania      2010
    ##  7 Algeria      2008
    ##  8 Algeria      2009
    ##  9 Algeria      2010
    ## 10 Andorra      2008
    ## # ℹ 581 more rows

Filter the final dataset so that it contains only the countries that
appear at least one time on the trades dataset.

``` r
final_trades <- final_trades %>%
  filter(reporter %in% trades_no_groups$Reporter)
final_trades
```

    ## # A tibble: 474 × 2
    ##    reporter  year
    ##    <chr>    <dbl>
    ##  1 Albania   2008
    ##  2 Albania   2009
    ##  3 Albania   2010
    ##  4 Algeria   2008
    ##  5 Algeria   2009
    ##  6 Algeria   2010
    ##  7 Andorra   2008
    ##  8 Andorra   2009
    ##  9 Andorra   2010
    ## 10 Anguila   2008
    ## # ℹ 464 more rows

And bring the data from trades dataset into the final dataset.

``` r
final_trades<- final_trades %>%
  left_join(trades_no_groups, by=join_by(reporter==Reporter, year==Year))
final_trades
```

    ## # A tibble: 474 × 4
    ##    reporter  year `Trade Value 1000USD` Quantity
    ##    <chr>    <dbl>                 <dbl>    <int>
    ##  1 Albania   2008                1003.        NA
    ##  2 Albania   2009                 616.     11974
    ##  3 Albania   2010                1047.      4047
    ##  4 Algeria   2008                 103.        22
    ##  5 Algeria   2009                  74         44
    ##  6 Algeria   2010                  23.0       18
    ##  7 Andorra   2008                 539         NA
    ##  8 Andorra   2009                 253.        NA
    ##  9 Andorra   2010                 405.        NA
    ## 10 Anguila   2008                  24.6       NA
    ## # ℹ 464 more rows

``` r
na_qts <- final_trades %>% filter(is.na(Quantity)) %>% select(reporter)
```

``` r
final_trades <- final_trades %>%
  filter(!reporter %in% na_qts$reporter)
final_trades
```

    ## # A tibble: 147 × 4
    ##    reporter   year `Trade Value 1000USD` Quantity
    ##    <chr>     <dbl>                 <dbl>    <int>
    ##  1 Algeria    2008                 103.        22
    ##  2 Algeria    2009                  74         44
    ##  3 Algeria    2010                  23.0       18
    ##  4 Argentina  2008               14504.    100520
    ##  5 Argentina  2009               18224.     44723
    ##  6 Argentina  2010                9932.     56892
    ##  7 Armenia    2008                 103.       466
    ##  8 Armenia    2009                 218.       151
    ##  9 Armenia    2010                 205.       532
    ## 10 Australia  2008              403742.   3083715
    ## # ℹ 137 more rows

# Final validations

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

    ## # A tibble: 3 × 4
    ##    year `Total Trade Value 1000USD.x` `Total Trade Value 1000USD.y` share
    ##   <dbl>                         <dbl>                         <dbl> <dbl>
    ## 1  2008                    166661585.                    253311857. 0.658
    ## 2  2009                    149598590.                    219395438. 0.682
    ## 3  2010                    196680985.                    275205599. 0.715

Although there was a considerable loss, the dataset still contains the
majority of the world trade for each year, so I think we can go with
that.

Now, I’ll check the top 5 countries for each year and check how many of
them are still present on the final dataset.

``` r
top_5 <- trades_no_groups %>%
  group_by(Year) %>%
  slice_max(order_by = `Trade Value 1000USD`, n=5) %>%
  ungroup
top_5
```

    ## # A tibble: 15 × 4
    ##    Reporter          Year `Trade Value 1000USD`   Quantity
    ##    <chr>            <int>                 <dbl>      <int>
    ##  1 China             2008            122727667. 1359163400
    ##  2 United States     2008             24722279.         NA
    ##  3 Malaysia          2008             14183845.   84677851
    ##  4 Thailand          2008             13397055.         NA
    ##  5 Hong Kong, China  2008             10994109.  391873536
    ##  6 China             2009            111890627. 1250304300
    ##  7 United States     2009             21113125.   47708550
    ##  8 Thailand          2009             11281266.  386165530
    ##  9 Hong Kong, China  2009             10231178.  366034777
    ## 10 Malaysia          2009             10161061.         NA
    ## 11 China             2010            148802627. 1577944300
    ## 12 United States     2010             25195434.   65013100
    ## 13 Mexico            2010             13731085.   24580970
    ## 14 Hong Kong, China  2010             13619564.  373483729
    ## 15 Thailand          2010             13025752.         NA

``` r
top_5 %>% select(Reporter) %>% unique
```

    ## # A tibble: 6 × 1
    ##   Reporter        
    ##   <chr>           
    ## 1 China           
    ## 2 United States   
    ## 3 Malaysia        
    ## 4 Thailand        
    ## 5 Hong Kong, China
    ## 6 Mexico

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter)
```

    ## # A tibble: 8 × 4
    ##   Reporter       Year `Trade Value 1000USD`  Quantity
    ##   <chr>         <int>                 <dbl>     <int>
    ## 1 United States  2008             24722279.        NA
    ## 2 Malaysia       2008             14183845.  84677851
    ## 3 Thailand       2008             13397055.        NA
    ## 4 United States  2009             21113125.  47708550
    ## 5 Thailand       2009             11281266. 386165530
    ## 6 Malaysia       2009             10161061.        NA
    ## 7 United States  2010             25195434.  65013100
    ## 8 Thailand       2010             13025752.        NA

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter) %>% select(Reporter) %>% unique
```

    ## # A tibble: 3 × 1
    ##   Reporter     
    ##   <chr>        
    ## 1 United States
    ## 2 Malaysia     
    ## 3 Thailand

That seems to be a bigger problem. The final dataset excludes 3 of 6 big
players on the subject. Yet, the amount represented by all of them seem
to be less than the top player, China, which is still present on the
final dataset.

For now, I’ll carry on the analysis with the approach of dropping all
countries with some non reported quantity. If it become necessary, I’ll
return to this dataset and search for a good missing values filling
approach.

Finally, I save the final table as parquet.

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs/")
datadir <- "data/WITS/"
filename <- "WITS_trades"

write_parquet(final_trades, str_interp("${datadir}${filename}.parquet"))
```
