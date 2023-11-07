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

    ## # A tibble: 15 × 2
    ##        ano variacao_anual
    ##    <int64>          <dbl>
    ##  1    2007           4.46
    ##  2    2008           5.9 
    ##  3    2009           4.31
    ##  4    2010           5.91
    ##  5    2011           6.5 
    ##  6    2012           5.84
    ##  7    2013           5.91
    ##  8    2014           6.41
    ##  9    2015          10.7 
    ## 10    2016           6.29
    ## 11    2017           2.95
    ## 12    2018           3.75
    ## 13    2019           4.31
    ## 14    2020           4.52
    ## 15    2021          10.1

After that, I create a multiplier column to get the prices into 2021
level.

``` r
ipca <- ipca %>%
  arrange(desc(ano)) %>%
  mutate(
    multiplier_2021=cumprod(1+variacao_anual/100),
    ano=ano-1
  ) %>%
  select(ano, multiplier_2021)
ipca
```

    ## # A tibble: 15 × 2
    ##        ano multiplier_2021
    ##    <int64>           <dbl>
    ##  1    2020            1.10
    ##  2    2019            1.15
    ##  3    2018            1.20
    ##  4    2017            1.24
    ##  5    2016            1.28
    ##  6    2015            1.36
    ##  7    2014            1.51
    ##  8    2013            1.60
    ##  9    2012            1.70
    ## 10    2011            1.80
    ## 11    2010            1.92
    ## 12    2009            2.03
    ## 13    2008            2.12
    ## 14    2007            2.24
    ## 15    2006            2.34

``` r
data_dir <- "data/"
file <- str_interp("${data_dir}IPCA.parquet")

write_parquet(ipca, file)
```
