Computers and jobs - CNAE
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
exploratory analysis and preparation of data from the Classificação
Nacional das Atividades Econômicas (CNAE), managed by Instituto
Brasileiro de Geografia e Estatística (IBGE), as made available by [Base
dos Dados](https://basedosdados.org/).

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
peek <- basedosdados::read_sql("SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cnae_2` LIMIT 1")
peek %>% dplyr::glimpse()
```

    ## Rows: 1
    ## Columns: 8
    ## $ cnae_2            <chr> "01156"
    ## $ descricao         <chr> "Cultivo de soja"
    ## $ grupo             <chr> "01.1"
    ## $ descricao_grupo   <chr> "Produção de lavouras temporárias"
    ## $ divisao           <chr> "01"
    ## $ descricao_divisao <chr> "Agricultura, pecuária e serviços relacionados"
    ## $ secao             <chr> "A"
    ## $ descricao_secao   <chr> "Agricultura, pecuária, produção florestal, pesca e …

Now, I download the CNAE data from basedosdados.

``` r
sql <- "
SELECT
  cnae.cnae_2,
  cnae.descricao,
  cnae.grupo,
  cnae.descricao_grupo,
  cnae.divisao,
  cnae.descricao_divisao,
  cnae.secao,
  cnae.descricao_secao
FROM
  basedosdados.br_bd_diretorios_brasil.cnae_2 AS cnae
"

cnae <- basedosdados::read_sql(sql)
cnae %>% head()
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
data_dir <- "data/CNAE/"
file <- str_interp("${data_dir}CNAE.parquet")

write_parquet(cnae, file)
```
