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

countries %>% head()
```

<div class="kable-table">

| countrycode | iso3Code | name                                               | isReporter | isPartner | isGroup | groupType     |
|:------------|:---------|:---------------------------------------------------|:-----------|:----------|:--------|:--------------|
| A01         | A01      | Selected Developing Countries for Switzerland      | FALSE      | TRUE      | TRUE    | Beneficiaries |
| A02         | A02      | Caribbean Economic Recovery Act: USA 2007          | FALSE      | TRUE      | TRUE    | Beneficiaries |
| A03         | A03      | Caribbean Economic Recovery Act: USA 2009          | FALSE      | TRUE      | TRUE    | Beneficiaries |
| A04         | A04      | Preferential tariff for selected african countries | FALSE      | TRUE      | TRUE    | Beneficiaries |
| A05         | A05      | Norway tariff for lower income GSP countries       | FALSE      | TRUE      | TRUE    | Beneficiaries |
| A07         | A07      | Preferential tariff for COMESA FTA                 | FALSE      | TRUE      | TRUE    | Beneficiaries |

</div>

``` r
countries_no_groups <- countries %>% filter(isGroup==FALSE)
countries_no_groups %>% head()
```

<div class="kable-table">

| countrycode | iso3Code | name        | isReporter | isPartner | isGroup | groupType |
|:------------|:---------|:------------|:-----------|:----------|:--------|:----------|
| 533         | ABW      | Aruba       | TRUE       | TRUE      | FALSE   | N/A       |
| 004         | AFG      | Afghanistan | TRUE       | TRUE      | FALSE   | N/A       |
| 024         | AGO      | Angola      | TRUE       | FALSE     | FALSE   | N/A       |
| 660         | AIA      | Anguila     | TRUE       | FALSE     | FALSE   | N/A       |
| 008         | ALB      | Albania     | TRUE       | TRUE      | FALSE   | N/A       |
| 020         | AND      | Andorra     | FALSE      | TRUE      | FALSE   | N/A       |

</div>

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
trades_no_groups %>% head()
```

<div class="kable-table">

| Reporter       | TradeFlow | ProductCode | Product Description                                         | Year | Partner | Trade Value 1000USD | Quantity  | Quantity Unit |
|:---------------|:----------|:------------|:------------------------------------------------------------|:-----|:--------|:--------------------|:----------|:--------------|
| Mexico         | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 869,047.20          | 1,876,590 | Item          |
| United States  | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 563,928.96          | 141,739   | Item          |
| United Kingdom | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 196,055.63          | 530,903   | Item          |
| Austria        | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 104,268.97          | 7,749     | Item          |
| Thailand       | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 14,965.43           | 29,768    | Item          |
| Canada         | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   | 12,694.80           | 6,715     | Item          |

</div>

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
trades_no_groups %>% head()
```

<div class="kable-table">

| Reporter       | TradeFlow | ProductCode | Product Description                                         | Year | Partner | Trade Value 1000USD | Quantity | Quantity Unit |
|:---------------|:----------|:------------|:------------------------------------------------------------|-----:|:--------|--------------------:|---------:|:--------------|
| Mexico         | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |           869047.20 |  1876590 | Item          |
| United States  | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |           563928.96 |   141739 | Item          |
| United Kingdom | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |           196055.63 |   530903 | Item          |
| Austria        | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |           104268.97 |     7749 | Item          |
| Thailand       | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |            14965.43 |    29768 | Item          |
| Canada         | Export    | 847110      | Data processing machines; analogue or hybrid automatic type | 2006 | World   |            12694.80 |     6715 | Item          |

</div>

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
final_trades %>% head()
```

<div class="kable-table">

| reporter    | year |
|:------------|-----:|
| Afghanistan | 2006 |
| Afghanistan | 2007 |
| Afghanistan | 2008 |
| Afghanistan | 2009 |
| Afghanistan | 2010 |
| Afghanistan | 2011 |

</div>

Filter the final dataset so that it contains only the countries that
appear at least one time on the trades dataset.

``` r
final_trades <- final_trades %>%
  filter(reporter %in% trades_no_groups$Reporter)
final_trades %>% head()
```

<div class="kable-table">

| reporter | year |
|:---------|-----:|
| Albania  | 2006 |
| Albania  | 2007 |
| Albania  | 2008 |
| Albania  | 2009 |
| Albania  | 2010 |
| Albania  | 2011 |

</div>

And bring the data from trades dataset into the final dataset.

``` r
final_trades<- final_trades %>%
  left_join(trades_no_groups, by=join_by(reporter==Reporter, year==Year))
final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD | Quantity |
|:---------|-----:|--------------------:|---------:|
| Albania  | 2006 |              274.44 |      527 |
| Albania  | 2007 |              316.90 |       NA |
| Albania  | 2008 |             1002.59 |       NA |
| Albania  | 2009 |              615.91 |    11974 |
| Albania  | 2010 |             1046.96 |     4047 |
| Albania  | 2011 |              696.83 |     5896 |

</div>

``` r
na_qts <- final_trades %>%
  filter(is.na(Quantity)) %>%
  select(reporter)
```

``` r
final_trades <- final_trades %>%
  filter(!reporter %in% na_qts$reporter)
final_trades %>% head()
```

<div class="kable-table">

| reporter  | year | Trade Value 1000USD | Quantity |
|:----------|-----:|--------------------:|---------:|
| Argentina | 2006 |            13877.53 |    85676 |
| Argentina | 2007 |            20000.00 |    83043 |
| Argentina | 2008 |            14504.21 |   100520 |
| Argentina | 2009 |            18224.47 |    44723 |
| Argentina | 2010 |             9932.13 |    56892 |
| Argentina | 2011 |            13855.60 |    76958 |

</div>

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

<div class="kable-table">

| year | Total Trade Value 1000USD.x | Total Trade Value 1000USD.y |     share |
|-----:|----------------------------:|----------------------------:|----------:|
| 2006 |                    21026355 |                   231072139 | 0.0909948 |
| 2007 |                    14661869 |                   246227764 | 0.0595460 |
| 2008 |                    13608238 |                   253311857 | 0.0537213 |
| 2009 |                     9623765 |                   219395438 | 0.0438649 |
| 2010 |                     9843504 |                   275205599 | 0.0357678 |
| 2011 |                    10199204 |                   295505231 | 0.0345145 |
| 2012 |                    12145049 |                   351368004 | 0.0345650 |
| 2013 |                    12352072 |                   337485955 | 0.0366003 |
| 2014 |                    12577569 |                   345090941 | 0.0364471 |
| 2015 |                    11159806 |                   306936257 | 0.0363587 |
| 2016 |                    10703943 |                   288780109 | 0.0370661 |
| 2017 |                     6723179 |                   232504176 | 0.0289164 |
| 2018 |                     7463330 |                   253623908 | 0.0294268 |
| 2019 |                     7913666 |                   242400457 | 0.0326471 |
| 2020 |                     7460319 |                   266334417 | 0.0280111 |
| 2021 |                     8604344 |                   313112543 | 0.0274800 |

</div>

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
final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD | Quantity |
|:---------|-----:|--------------------:|---------:|
| Albania  | 2006 |              274.44 |      527 |
| Albania  | 2007 |              316.90 |       NA |
| Albania  | 2008 |             1002.59 |       NA |
| Albania  | 2009 |              615.91 |    11974 |
| Albania  | 2010 |             1046.96 |     4047 |
| Albania  | 2011 |              696.83 |     5896 |

</div>

Now, I exclude the countries with non reported quantity on the first
year of the sample.

``` r
na_first_year <- final_trades %>%
  filter(is.na(Quantity) & year==years[1]) %>%
  select(reporter) %>%
  unique()

final_trades <- final_trades %>%
  filter(!reporter %in% na_first_year$reporter)
final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD | Quantity |
|:---------|-----:|--------------------:|---------:|
| Albania  | 2006 |              274.44 |      527 |
| Albania  | 2007 |              316.90 |       NA |
| Albania  | 2008 |             1002.59 |       NA |
| Albania  | 2009 |              615.91 |    11974 |
| Albania  | 2010 |             1046.96 |     4047 |
| Albania  | 2011 |              696.83 |     5896 |

</div>

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
  unique()

final_trades <- final_trades %>%
  filter(!reporter %in% less_than_half$reporter)
final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD | Quantity |
|:---------|-----:|--------------------:|---------:|
| Albania  | 2006 |              274.44 |      527 |
| Albania  | 2007 |              316.90 |       NA |
| Albania  | 2008 |             1002.59 |       NA |
| Albania  | 2009 |              615.91 |    11974 |
| Albania  | 2010 |             1046.96 |     4047 |
| Albania  | 2011 |              696.83 |     5896 |

</div>

Now, I fill the non reported quantities with the previous values.

``` r
while (final_trades %>% filter(is.na(Quantity)) %>% nrow > 0) {
  final_trades <- final_trades %>%
    group_by(reporter) %>%
    mutate(
      Quantity=ifelse(!is.na(Quantity), Quantity, lag(Quantity))
    )
}

final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD | Quantity |
|:---------|-----:|--------------------:|---------:|
| Albania  | 2006 |              274.44 |      527 |
| Albania  | 2007 |              316.90 |      527 |
| Albania  | 2008 |             1002.59 |      527 |
| Albania  | 2009 |              615.91 |    11974 |
| Albania  | 2010 |             1046.96 |     4047 |
| Albania  | 2011 |              696.83 |     5896 |

</div>

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

<div class="kable-table">

| year | Total Trade Value 1000USD.x | Total Trade Value 1000USD.y |     share |
|-----:|----------------------------:|----------------------------:|----------:|
| 2006 |                   206243628 |                   231072139 | 0.8925508 |
| 2007 |                   218502574 |                   246227764 | 0.8874002 |
| 2008 |                   227704946 |                   253311857 | 0.8989115 |
| 2009 |                   199452483 |                   219395438 | 0.9091004 |
| 2010 |                   253020386 |                   275205599 | 0.9193868 |
| 2011 |                   274349247 |                   295505231 | 0.9284074 |
| 2012 |                   314301993 |                   351368004 | 0.8945094 |
| 2013 |                   306169127 |                   337485955 | 0.9072055 |
| 2014 |                   312239558 |                   345090941 | 0.9048037 |
| 2015 |                   274528642 |                   306936257 | 0.8944158 |
| 2016 |                   258561156 |                   288780109 | 0.8953565 |
| 2017 |                   218145612 |                   232504176 | 0.9382438 |
| 2018 |                   242444596 |                   253623908 | 0.9559217 |
| 2019 |                   230540888 |                   242400457 | 0.9510745 |
| 2020 |                   252038296 |                   266334417 | 0.9463227 |
| 2021 |                   297255913 |                   313112543 | 0.9493581 |

</div>

Now the final dataset contain a fair amount of the global trade value,
with more than 88% of the global share, for each year.

Now, I’ll check the top 5 countries for each year and check how many of
them are still present on the final dataset.

``` r
top_5 <- trades_no_groups %>%
  group_by(Year) %>%
  slice_max(order_by = `Trade Value 1000USD`, n=5) %>%
  ungroup ()
top_5 %>% head(20)
```

<div class="kable-table">

| Reporter         | Year | Trade Value 1000USD |   Quantity |
|:-----------------|-----:|--------------------:|-----------:|
| China            | 2006 |            93017370 | 1426071823 |
| United States    | 2006 |            26584860 |   58968739 |
| Malaysia         | 2006 |            16321713 |  459586190 |
| Singapore        | 2006 |            11563986 |  155813240 |
| United Kingdom   | 2006 |            11320869 |  145767083 |
| China            | 2007 |           112243867 | 1365466000 |
| United States    | 2007 |            25346767 |   45175820 |
| Malaysia         | 2007 |            15639459 |   83563074 |
| Thailand         | 2007 |            12574957 |         NA |
| Singapore        | 2007 |            10056333 |  110625500 |
| China            | 2008 |           122727667 | 1359163400 |
| United States    | 2008 |            24722279 |         NA |
| Malaysia         | 2008 |            14183845 |   84677851 |
| Thailand         | 2008 |            13397055 |         NA |
| Hong Kong, China | 2008 |            10994109 |  391873536 |
| China            | 2009 |           111890627 | 1250304300 |
| United States    | 2009 |            21113125 |   47708550 |
| Thailand         | 2009 |            11281266 |  386165530 |
| Hong Kong, China | 2009 |            10231178 |  366034777 |
| Malaysia         | 2009 |            10161061 |         NA |

</div>

``` r
top_5 %>% select(Reporter) %>% unique()
```

<div class="kable-table">

| Reporter         |
|:-----------------|
| China            |
| United States    |
| Malaysia         |
| Singapore        |
| United Kingdom   |
| Thailand         |
| Hong Kong, China |
| Mexico           |
| Czech Republic   |

</div>

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter)
```

<div class="kable-table">

| Reporter | Year | Trade Value 1000USD |  Quantity |
|:---------|-----:|--------------------:|----------:|
| Thailand | 2007 |            12574957 |        NA |
| Thailand | 2008 |            13397055 |        NA |
| Thailand | 2009 |            11281266 | 386165530 |
| Thailand | 2010 |            13025752 |        NA |
| Thailand | 2011 |            11284266 |        NA |
| Thailand | 2012 |            15214795 |        NA |
| Thailand | 2013 |            13443937 |        NA |
| Thailand | 2014 |            13928100 |        NA |
| Thailand | 2015 |            13639466 |        NA |
| Thailand | 2016 |            12576596 |        NA |

</div>

``` r
top_5 %>% filter(!Reporter %in% final_trades$reporter) %>% select(Reporter) %>% unique()
```

<div class="kable-table">

| Reporter |
|:---------|
| Thailand |

</div>

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

<div class="kable-table">

| Reporter         |
|:-----------------|
| China            |
| United States    |
| Malaysia         |
| Thailand         |
| Mexico           |
| Hong Kong, China |

</div>

``` r
final_trades <- final_trades %>%
  filter(reporter %in% top_3$Reporter)

final_trades %>% head()
```

<div class="kable-table">

| reporter | year | Trade Value 1000USD |   Quantity |
|:---------|-----:|--------------------:|-----------:|
| China    | 2006 |            93017370 | 1426071823 |
| China    | 2007 |           112243867 | 1365466000 |
| China    | 2008 |           122727667 | 1359163400 |
| China    | 2009 |           111890627 | 1250304300 |
| China    | 2010 |           148802627 | 1577944300 |
| China    | 2011 |           160121815 | 1748174800 |

</div>

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
