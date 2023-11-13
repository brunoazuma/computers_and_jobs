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
load_dot_env()
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

<div class="kable-table">

|  ano | sigla_uf | cnae_2 | cbo_2002 | grau_instrucao_apos_2005 | sexo | raca_cor | idade | valor_remuneracao_media_sm | valor_remuneracao_media | quantidade_horas_contratadas |
|-----:|:---------|:-------|:---------|:-------------------------|:-----|:---------|------:|---------------------------:|------------------------:|-----------------------------:|
| 2006 | AC       | 47547  | 142320   | 7                        | 2    | 2        |    44 |                      14.80 |                 5183.51 |                           44 |
| 2006 | AC       | 10112  | 782810   | 4                        | 1    | 8        |    40 |                       1.55 |                  527.36 |                           44 |
| 2006 | AC       | 52401  | 783205   | 8                        | 1    | 2        |    23 |                       2.55 |                  883.48 |                           36 |
| 2006 | AC       | 10112  | 848520   | 3                        | 1    | 2        |    28 |                       1.57 |                  551.18 |                           44 |
| 2006 | AC       | 41204  | 519930   | 7                        | 1    | 9        |    25 |                       1.27 |                  423.57 |                           44 |
| 2006 | AC       | 84116  | 332205   | 7                        | 2    | 8        |    30 |                       2.74 |                  939.49 |                           44 |

</div>

And the same for CBO, CNAE and IPCA datasets.

``` r
cbo <- tbl(con, str_interp("'${base_datadir}CBO.parquet'"))

cbo %>%
  head() %>%
  collect()
```

<div class="kable-table">

| cbo_2002 | descricao                     | group |
|:---------|:------------------------------|:------|
| 111105   | Senador                       | NRC   |
| 111110   | Deputado federal              | NRC   |
| 111115   | Deputado estadual e distrital | NRC   |
| 111120   | Vereador                      | NRC   |
| 111205   | Presidente da república       | NRC   |
| 111210   | Vice-presidente da república  | NRC   |

</div>

``` r
cnae <- tbl(con, str_interp("'${base_datadir}CNAE.parquet'"))
cnae %>%
  head() %>%
  collect()
```

<div class="kable-table">

| cnae_2 | descricao                                                                | grupo | descricao_grupo                  | divisao | descricao_divisao                             | secao | descricao_secao                                                |
|:-------|:-------------------------------------------------------------------------|:------|:---------------------------------|:--------|:----------------------------------------------|:------|:---------------------------------------------------------------|
| 01156  | Cultivo de soja                                                          | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |
| 01113  | Cultivo de cereais                                                       | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |
| 01121  | Cultivo de algodão herbáceo e de outras fibras de lavoura temporária     | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |
| 01148  | Cultivo de fumo                                                          | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |
| 01130  | Cultivo de cana-de-açúcar                                                | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |
| 01199  | Cultivo de plantas de lavoura temporária não especificadas anteriormente | 01.1  | Produção de lavouras temporárias | 01      | Agricultura, pecuária e serviços relacionados | A     | Agricultura, pecuária, produção florestal, pesca e aqüicultura |

</div>

``` r
ipca <- tbl(con, str_interp("'${base_datadir}IPCA.parquet'"))
ipca %>%
  head() %>%
  collect()
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

</div>

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

<div class="kable-table">

|  ano | sigla_uf | cnae_2 | cbo_2002 | grau_instrucao_apos_2005 | sexo | raca_cor | idade | valor_remuneracao_media_sm | valor_remuneracao_media | quantidade_horas_contratadas | group | secao | descricao_secao                                   | multiplier_2021 |
|-----:|:---------|:-------|:---------|:-------------------------|:-----|:---------|------:|---------------------------:|------------------------:|-----------------------------:|:------|:------|:--------------------------------------------------|----------------:|
| 2006 | AC       | 10112  | 782810   | 4                        | 1    | 8        |    40 |                       1.55 |                  527.36 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |
| 2006 | AC       | 10112  | 848520   | 3                        | 1    | 2        |    28 |                       1.57 |                  551.18 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |
| 2006 | AC       | 41204  | 519930   | 7                        | 1    | 9        |    25 |                       1.27 |                  423.57 |                           44 | NRM   | F     | Construção                                        |        2.340558 |
| 2006 | AC       | 84116  | 332205   | 7                        | 2    | 8        |    30 |                       2.74 |                  939.49 |                           44 | RC    | O     | Administração pública, defesa e seguridade social |        2.340558 |
| 2006 | AC       | 16102  | 715135   | 3                        | 1    | 8        |    30 |                       3.32 |                 1166.61 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |
| 2006 | AC       | 16218  | 862120   | 2                        | 1    | 8        |    27 |                       1.36 |                  479.32 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |

</div>

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

<div class="kable-table">

|  ano | sigla_uf | cnae_2 | cbo_2002 | grau_instrucao_apos_2005 | sexo | raca_cor | idade | valor_remuneracao_media_sm | valor_remuneracao_media | quantidade_horas_contratadas | group | secao | descricao_secao                                   | multiplier_2021 | mean_wage |
|-----:|:---------|:-------|:---------|:-------------------------|:-----|:---------|------:|---------------------------:|------------------------:|-----------------------------:|:------|:------|:--------------------------------------------------|----------------:|----------:|
| 2006 | AC       | 10112  | 782810   | 4                        | 1    | 8        |    40 |                       1.55 |                  527.36 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |  7.013162 |
| 2006 | AC       | 10112  | 848520   | 3                        | 1    | 2        |    28 |                       1.57 |                  551.18 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |  7.329936 |
| 2006 | AC       | 41204  | 519930   | 7                        | 1    | 9        |    25 |                       1.27 |                  423.57 |                           44 | NRM   | F     | Construção                                        |        2.340558 |  5.632898 |
| 2006 | AC       | 84116  | 332205   | 7                        | 2    | 8        |    30 |                       2.74 |                  939.49 |                           44 | RC    | O     | Administração pública, defesa e seguridade social |        2.340558 | 12.493924 |
| 2006 | AC       | 16102  | 715135   | 3                        | 1    | 8        |    30 |                       3.32 |                 1166.61 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 | 15.514308 |
| 2006 | AC       | 16218  | 862120   | 2                        | 1    | 8        |    27 |                       1.36 |                  479.32 |                           44 | RM    | C     | Indústrias de transformação                       |        2.340558 |  6.374296 |

</div>

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

    ## 1820.92 sec elapsed

``` r
rais_agg %>% head()
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

# Saving the results to disk

``` r
write_parquet(rais_agg, str_interp("${base_datadir}/RAIS_AGG.parquet"))
```
