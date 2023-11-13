Computers and jobs - RC share, export share and expose to computers
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
preparation and merge of data from the Relação Anual de Informações
Sociais (RAIS) and World Integrated Trade Solution (WITS) and the
generation of the Exposure to computers measure, inspired by the
Boustan, Choi e Clingingsmith (2022) exposure to CNC measure.

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

``` r
library("duckdb")
```

    ## Carregando pacotes exigidos: DBI

# Setting up file paths and DuckDB

As the RAIS data is still big, it’s still useful to use `dbplyr` and
`duckdb` to handle the loading of the files for me.

First, I’ll setup the connection.

``` r
base_datadir <- Sys.getenv("DATA_DIR")
temp_db_dir <- Sys.getenv("DUCK_DB_DIR")
temp_db_mem <- Sys.getenv("DUCK_DB_MEM")

con <- dbConnect(duckdb(), dbdir = temp_db_dir)
dbExecute(conn = con, str_interp("PRAGMA memory_limit='${temp_db_mem}'"))
```

    ## [1] 0

# Setting up tables

Now, I’ll setup the RAIS dataset and read only the first rows just to
see columns.

``` r
rais_filepath <- str_interp("${base_datadir}RAIS_AGG.parquet")

rais <- tbl(con, rais_filepath)

rais %>%
  head() %>%
  collect()
```

<div class="kable-table">

|  ano | sigla_uf | secao | descricao_secao                                            | group | grau_instrucao_apos_2005 | sexo | raca_cor | idade | valor_remuneracao_media | valor_remuneracao_media_sm | mean_wage | job_number |
|-----:|:---------|:------|:-----------------------------------------------------------|:------|:-------------------------|:-----|:---------|------:|------------------------:|---------------------------:|----------:|-----------:|
| 2006 | AL       | C     | Indústrias de transformação                                | RC    | 4                        | 2    | 2        |    47 |                399.5825 |                   1.177500 |  5.313897 |          4 |
| 2006 | AL       | C     | Indústrias de transformação                                | RM    | 2                        | 1    | 2        |    34 |                565.4407 |                   1.698872 |  7.519583 |        195 |
| 2006 | AL       | C     | Indústrias de transformação                                | RC    | 7                        | 2    | 6        |    34 |                619.7200 |                   1.830000 |  8.241423 |          1 |
| 2006 | AL       | C     | Indústrias de transformação                                | NRC   | 5                        | 1    | 8        |    36 |                624.2250 |                   1.846667 |  9.345891 |          6 |
| 2006 | AL       | G     | Comércio; reparação de veículos automotores e motocicletas | RC    | 5                        | 1    | 8        |    19 |                471.6527 |                   1.371569 |  6.295499 |         51 |
| 2006 | AL       | I     | Alojamento e alimentação                                   | NRM   | 5                        | 1    | 8        |    26 |                424.0461 |                   1.255714 |  5.756866 |         49 |

</div>

And the same for the WITS dataset.

``` r
file <- str_interp('${base_datadir}/WITS/WITS_trades.parquet')
wits <- tbl(con, file)

wits %>%
  head() %>%
  collect()
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

# RC share

Now, I’ll prepare the instructions for the $RC_{share}$ component of the
measure. I’ll not collect the results, because I’ll collect as the final
step, in addition to the $Export_{share}$.

``` r
rc_share <- rais %>%
  filter(ano==2006 & !is.na(group)) %>%
  mutate(
    rc_job_number=as.integer(group=="RC")*job_number
  ) %>%
  group_by(secao) %>%
  summarise(
    rc_job_number=sum(rc_job_number),
    job_number=sum(job_number)
  ) %>%
  mutate(
    rc_share=rc_job_number/job_number
  ) %>%
  ungroup()
```

# Export share

Following Boustan, Choi e Clingingsmith (2022), I kept only the
countries that figured as top 3 world exporters of HS 8471 Products
(Automatic data processing machines and units thereof, magnetic or
optical readers, machines for transcribing data onto data media in coded
form and machines for processing such data, not elsewhere specified or
included). That was made to minimize the chances of brazilian domestic
demand for computer be endogenous with the export component of the
exposure to computers measure.

``` r
export_share <- wits %>%
  group_by(year) %>%
  summarise(
    quantity=sum(Quantity)
  ) %>%
  ungroup() %>%
  mutate(
    x_share = quantity/first(quantity)
  )
```

# Merging the components and creating the exposure measure

Now, I merge both of the components to the RAIS dataset and create the
exposure to computers measure.

``` r
rais <- rais %>%
  left_join(
    rc_share %>% select(secao, rc_share),
    by=join_by(secao==secao)
  ) %>%
  left_join(
    export_share %>% select(year, x_share),
    by=join_by(ano==year)) %>%
  mutate(
    exposure=rc_share*x_share
  )
```

# Collecting and saving to disk

Finally, I process the data and save to disk

``` r
tictoc::tic()
final_db <- rais %>%
  collect()
```

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

``` r
tictoc::toc()
```

    ## 151.82 sec elapsed

``` r
final_db %>% head()
```

<div class="kable-table">

|  ano | sigla_uf | secao | descricao_secao                                            | group | grau_instrucao_apos_2005 | sexo | raca_cor | idade | valor_remuneracao_media | valor_remuneracao_media_sm | mean_wage | job_number |  rc_share | x_share |  exposure |
|-----:|:---------|:------|:-----------------------------------------------------------|:------|:-------------------------|:-----|:---------|------:|------------------------:|---------------------------:|----------:|-----------:|----------:|--------:|----------:|
| 2006 | AL       | C     | Indústrias de transformação                                | RC    | 4                        | 2    | 2        |    47 |                399.5825 |                   1.177500 |  5.313897 |          4 | 0.1461642 |       1 | 0.1461642 |
| 2006 | AL       | C     | Indústrias de transformação                                | RM    | 2                        | 1    | 2        |    34 |                565.4407 |                   1.698872 |  7.519583 |        195 | 0.1461642 |       1 | 0.1461642 |
| 2006 | AL       | C     | Indústrias de transformação                                | RC    | 7                        | 2    | 6        |    34 |                619.7200 |                   1.830000 |  8.241423 |          1 | 0.1461642 |       1 | 0.1461642 |
| 2006 | AL       | C     | Indústrias de transformação                                | NRC   | 5                        | 1    | 8        |    36 |                624.2250 |                   1.846667 |  9.345891 |          6 | 0.1461642 |       1 | 0.1461642 |
| 2006 | AL       | G     | Comércio; reparação de veículos automotores e motocicletas | RC    | 5                        | 1    | 8        |    19 |                471.6527 |                   1.371569 |  6.295499 |         51 | 0.5886394 |       1 | 0.5886394 |
| 2006 | AL       | I     | Alojamento e alimentação                                   | NRM   | 5                        | 1    | 8        |    26 |                424.0461 |                   1.255714 |  5.756866 |         49 | 0.1671943 |       1 | 0.1671943 |

</div>

``` r
dbDisconnect(con, shutdown=TRUE)
write_parquet(final_db, str_interp("${base_datadir}/Final_db.parquet"))
```

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-nberw30400" class="csl-entry">

BOUSTAN, L. P.; CHOI, J.; CLINGINGSMITH, D. **Automation After the
Assembly Line: Computerized Machine Tools, Employment and Productivity
in the United States**. ago. 2022. Disponível em:
\<<https://www.nber.org/papers/w30400>\>.

</div>

</div>
