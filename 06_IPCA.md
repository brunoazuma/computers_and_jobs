Computers and jobs - IPCA
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
exploratory analysis and preparation of data from the Índice Nacional de
Preços ao Consumidor Amplo (IPCA), producted by Instituto Brasileiro de
Geografia e Estatística (IBGE), as made available by [Base dos
Dados](https://basedosdados.org/).

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs")
# install.packages(\"dotenv\")
library("dotenv")
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

First, I need to setup the GCP project data for using the `basedosdados`
package to get the data.

``` r
# Defina o seu projeto no Google Cloud
load_dot_env()
project_id <- Sys.getenv("GCP_PROJECT_ID")
basedosdados::set_billing_id(project_id)
```

    ## Project keys set successfully

``` r
bigrquery::bq_auth(email = Sys.getenv("GCP_EMAIL"))
```

``` r
peek <- basedosdados::read_sql("SELECT * FROM `basedosdados.br_ibge_ipca.mes_brasil` LIMIT 1")
peek %>% dplyr::glimpse()
```

    ## Rows: 1
    ## Columns: 8
    ## $ ano                 <int64> 1997
    ## $ mes                 <int64> 1
    ## $ indice              <dbl> 1379.33
    ## $ variacao_mensal     <dbl> 1.18
    ## $ variacao_trimestral <dbl> 1.98
    ## $ variacao_semestral  <dbl> 2.89
    ## $ variacao_anual      <dbl> 1.18
    ## $ variacao_doze_meses <dbl> 9.39

Now, I download the IPCA data from basedosdados.

``` r
sql <- "
SELECT
  ipca.ano,
  ipca.variacao_anual
FROM
  basedosdados.br_ibge_ipca.mes_brasil AS ipca
WHERE
  ipca.ano > 2006
  AND
  ipca.ano < 2022
  AND
  ipca.mes = 12
ORDER BY
  ipca.ano;
"

ipca <- basedosdados::read_sql(sql)
ipca
```

<div class="kable-table">

|  ano | variacao_anual |
|-----:|---------------:|
| 2007 |           4.46 |
| 2008 |           5.90 |
| 2009 |           4.31 |
| 2010 |           5.91 |
| 2011 |           6.50 |
| 2012 |           5.84 |
| 2013 |           5.91 |
| 2014 |           6.41 |
| 2015 |          10.67 |
| 2016 |           6.29 |
| 2017 |           2.95 |
| 2018 |           3.75 |
| 2019 |           4.31 |
| 2020 |           4.52 |
| 2021 |          10.06 |

</div>

After that, I create a multiplier column to get the prices into 2021
level.

``` r
ipca <- ipca %>%
  arrange(desc(ano)) %>%
  mutate(
    multiplier_2021=cumprod(1+variacao_anual/100),
    ano=ano-1
  ) %>%
  select(ano, multiplier_2021) %>%
  add_row(ano=as.integer(2021), multiplier_2021=1, .before=1)
ipca
```

<div class="kable-table">

|  ano | multiplier_2021 |
|-----:|----------------:|
| 2021 |        1.000000 |
| 2020 |        1.100600 |
| 2019 |        1.150347 |
| 2018 |        1.199927 |
| 2017 |        1.244924 |
| 2016 |        1.281650 |
| 2015 |        1.362265 |
| 2014 |        1.507619 |
| 2013 |        1.604257 |
| 2012 |        1.699069 |
| 2011 |        1.798295 |
| 2010 |        1.915184 |
| 2009 |        2.028371 |
| 2008 |        2.115794 |
| 2007 |        2.240626 |
| 2006 |        2.340558 |

</div>

``` r
data_dir <- "data/"
file <- str_interp("${data_dir}IPCA.parquet")

write_parquet(ipca, file)
```
