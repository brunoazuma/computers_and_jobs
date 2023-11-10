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

    ## # A tibble: 6 × 13
    ##     ano sigla_uf secao descricao_secao        group grau_instrucao_apos_…¹ sexo 
    ##   <dbl> <chr>    <chr> <chr>                  <chr> <chr>                  <chr>
    ## 1  2006 AC       F     Construção             RC    7                      1    
    ## 2  2006 AC       G     Comércio; reparação d… RC    4                      2    
    ## 3  2006 AC       G     Comércio; reparação d… RM    5                      1    
    ## 4  2006 AC       G     Comércio; reparação d… RC    7                      1    
    ## 5  2006 AC       G     Comércio; reparação d… RC    6                      1    
    ## 6  2006 AC       H     Transporte, armazenag… NRC   7                      1    
    ## # ℹ abbreviated name: ¹​grau_instrucao_apos_2005
    ## # ℹ 6 more variables: raca_cor <chr>, idade <dbl>,
    ## #   valor_remuneracao_media <dbl>, valor_remuneracao_media_sm <dbl>,
    ## #   mean_wage <dbl>, job_number <dbl>

And the same for the WITS dataset.

``` r
file <- str_interp('${base_datadir}/WITS/WITS_trades.parquet')
wits <- tbl(con, file)

wits %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 4
    ##   reporter  year `Trade Value 1000USD`   Quantity
    ##   <chr>    <int>                 <dbl>      <int>
    ## 1 China     2006             93017370. 1426071823
    ## 2 China     2007            112243867. 1365466000
    ## 3 China     2008            122727667. 1359163400
    ## 4 China     2009            111890627. 1250304300
    ## 5 China     2010            148802627. 1577944300
    ## 6 China     2011            160121815. 1748174800

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

    ## 143.49 sec elapsed

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
