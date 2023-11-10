Computers and jobs - Merging of RAIS, CNAE and CBO
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
preparation and merge of data from the Relação Anual de Informações
Sociais (RAIS), Classificação Nacional das Atividades Econômicas (CNAE),
Cadastro Brasileiro de Ocupações (CBO) and Índice Nacional de Preços ao
Consumidor Amplo (IPCA).

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs")
# install.packages(\"dotenv\")
library("dotenv")
```

    ## Warning in readLines(file): linha final incompleta encontrada em '.env'

``` r
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

``` r
project_id <- 
base_datadir <- Sys.getenv("DATA_DIR")
rais_datadir <- str_interp("${base_datadir}RAIS/")
```

As the RAIS data is too big to fit in memory at one time and is
partitioned by UF and year, I’ll use `dbplyr` and `duckdb` to handle the
loading of the files for me.

First, I’ll setup the connection.

``` r
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
rais_filepath <- str_interp("'${rais_datadir}RAIS_*_*.parquet'")

rais <- tbl(con, rais_filepath)

rais %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 11
    ##     ano sigla_uf cnae_2 cbo_2002 grau_instrucao_apos_2005 sexo  raca_cor idade
    ##   <dbl> <chr>    <chr>  <chr>    <chr>                    <chr> <chr>    <dbl>
    ## 1  2006 AC       47547  142320   7                        2     2           44
    ## 2  2006 AC       10112  782810   4                        1     8           40
    ## 3  2006 AC       52401  783205   8                        1     2           23
    ## 4  2006 AC       10112  848520   3                        1     2           28
    ## 5  2006 AC       41204  519930   7                        1     9           25
    ## 6  2006 AC       84116  332205   7                        2     8           30
    ## # ℹ 3 more variables: valor_remuneracao_media_sm <dbl>,
    ## #   valor_remuneracao_media <dbl>, quantidade_horas_contratadas <dbl>

And the same for CBO, CNAE and IPCA datasets.

``` r
cbo <- tbl(con, str_interp("'${base_datadir}CBO.parquet'"))

cbo %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 3
    ##   cbo_2002 descricao                     group
    ##   <chr>    <chr>                         <chr>
    ## 1 111105   Senador                       NRC  
    ## 2 111110   Deputado federal              NRC  
    ## 3 111115   Deputado estadual e distrital NRC  
    ## 4 111120   Vereador                      NRC  
    ## 5 111205   Presidente da república       NRC  
    ## 6 111210   Vice-presidente da república  NRC

``` r
cnae <- tbl(con, str_interp("'${base_datadir}CNAE.parquet'"))
cnae %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 8
    ##   cnae_2 descricao         grupo descricao_grupo divisao descricao_divisao secao
    ##   <chr>  <chr>             <chr> <chr>           <chr>   <chr>             <chr>
    ## 1 01156  Cultivo de soja   01.1  Produção de la… 01      Agricultura, pec… A    
    ## 2 01113  Cultivo de cerea… 01.1  Produção de la… 01      Agricultura, pec… A    
    ## 3 01121  Cultivo de algod… 01.1  Produção de la… 01      Agricultura, pec… A    
    ## 4 01148  Cultivo de fumo   01.1  Produção de la… 01      Agricultura, pec… A    
    ## 5 01130  Cultivo de cana-… 01.1  Produção de la… 01      Agricultura, pec… A    
    ## 6 01199  Cultivo de plant… 01.1  Produção de la… 01      Agricultura, pec… A    
    ## # ℹ 1 more variable: descricao_secao <chr>

``` r
ipca <- tbl(con, str_interp("'${base_datadir}IPCA.parquet'"))
ipca %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 2
    ##     ano multiplier_2021
    ##   <dbl>           <dbl>
    ## 1  2020            1.10
    ## 2  2019            1.15
    ## 3  2018            1.20
    ## 4  2017            1.24
    ## 5  2016            1.28
    ## 6  2015            1.36

# Merging the datasets

And merge the CBO and CNAE datasets.

``` r
rais <- tbl(con, rais_filepath) %>%
  left_join(
    cbo %>% select(cbo_2002, group),
    by=join_by(cbo_2002==cbo_2002)
  ) %>%
  left_join(
    cnae %>% select(cnae_2, secao, descricao_secao),
    by=join_by(cnae_2==cnae_2)
  ) %>%
  left_join(
    ipca,
    by=join_by(ano==ano)
  )

rais %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 15
    ##     ano sigla_uf cnae_2 cbo_2002 grau_instrucao_apos_2005 sexo  raca_cor idade
    ##   <dbl> <chr>    <chr>  <chr>    <chr>                    <chr> <chr>    <dbl>
    ## 1  2006 AC       10112  782810   4                        1     8           40
    ## 2  2006 AC       10112  848520   3                        1     2           28
    ## 3  2006 AC       41204  519930   7                        1     9           25
    ## 4  2006 AC       84116  332205   7                        2     8           30
    ## 5  2006 AC       16102  715135   3                        1     8           30
    ## 6  2006 AC       16218  862120   2                        1     8           27
    ## # ℹ 7 more variables: valor_remuneracao_media_sm <dbl>,
    ## #   valor_remuneracao_media <dbl>, quantidade_horas_contratadas <dbl>,
    ## #   group <chr>, secao <chr>, descricao_secao <chr>, multiplier_2021 <dbl>

# Correcting the wage values

Now, I correct the wage values to 2021 prices using IPCA and convert to
hourly wages (so that the different contract hours are take into
account).

``` r
rais <- rais %>%
  mutate(
    mean_wage = valor_remuneracao_media*multiplier_2021,
    mean_wage = mean_wage/(quantidade_horas_contratadas*4)
  )

rais %>%
  head() %>%
  collect()
```

    ## # A tibble: 6 × 16
    ##     ano sigla_uf cnae_2 cbo_2002 grau_instrucao_apos_2005 sexo  raca_cor idade
    ##   <dbl> <chr>    <chr>  <chr>    <chr>                    <chr> <chr>    <dbl>
    ## 1  2006 AC       10112  782810   4                        1     8           40
    ## 2  2006 AC       10112  848520   3                        1     2           28
    ## 3  2006 AC       41204  519930   7                        1     9           25
    ## 4  2006 AC       84116  332205   7                        2     8           30
    ## 5  2006 AC       16102  715135   3                        1     8           30
    ## 6  2006 AC       16218  862120   2                        1     8           27
    ## # ℹ 8 more variables: valor_remuneracao_media_sm <dbl>,
    ## #   valor_remuneracao_media <dbl>, quantidade_horas_contratadas <dbl>,
    ## #   group <chr>, secao <chr>, descricao_secao <chr>, multiplier_2021 <dbl>,
    ## #   mean_wage <dbl>

# Aggregation the database

Finally, I aggregate the data by the fixed effects and possible
cofactors.

``` r
rais <- rais %>%
      group_by(
        ano,
        sigla_uf,
        secao,
        descricao_secao,
        group,
        grau_instrucao_apos_2005,
        sexo,
        raca_cor,
        idade
      ) %>%
      summarise(
        valor_remuneracao_media=mean(valor_remuneracao_media),
        valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
        mean_wage=mean(mean_wage),
        job_number=n()
      ) %>%
      ungroup()
```

``` r
tictoc::tic()
rais_agg <- rais %>%
  collect()
```

    ## `summarise()` has grouped output by "ano", "sigla_uf", "secao",
    ## "descricao_secao", "group", "grau_instrucao_apos_2005", "sexo", and "raca_cor".
    ## You can override using the `.groups` argument.

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

``` r
tictoc::toc()
```

    ## 1850.75 sec elapsed

# Saving the results to disk

``` r
write_parquet(rais_agg, str_interp("${base_datadir}/RAIS_AGG.parquet"))
```
