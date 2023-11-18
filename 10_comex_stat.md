# Computers and jobs - Imports data
Bruno Azuma Balzano

In order to have some variable about the usage of computers for
professional reasons, I’ll get data about imports of HS 8471 products. I
do that because specific data on computer usage is not available
annually, so I use import data on the

# Setup

As always, I’ll start by setting up the libraries and configurations
needed to run the entire notebook.

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs")
library("dotenv")
library("arrow")
```


    Attaching package: 'arrow'

    The following object is masked from 'package:utils':

        timestamp

``` r
library("tidyverse")
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ✔ purrr     1.0.1     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("basedosdados")
library("bigrquery")
```

First, I need to setup the GCP project data for using the
\`basedosdados\` package to get the data.

``` r
load_dot_env()

project_id <- Sys.getenv("GCP_PROJECT_ID")

set_billing_id(project_id)
```

    Project keys set successfully

``` r
bq_auth(email = Sys.getenv("GCP_EMAIL"))
```

``` r
peek <- basedosdados::read_sql("SELECT * FROM `basedosdados.br_me_comex_stat.municipio_importacao` LIMIT 1")
peek %>% dplyr::glimpse()
```

    Rows: 1
    Columns: 8
    $ ano             <int64> 2014
    $ mes             <int64> 4
    $ id_sh4          <chr> "806"
    $ id_pais         <chr> "13"
    $ sigla_uf        <chr> "RO"
    $ id_municipio    <chr> "1100205"
    $ peso_liquido_kg <int64> 25000
    $ valor_fob_dolar <int64> 44098

# Download from BigQuery

Now, I download the Comex Stat data from basedosdados.

``` r
sql <- "
SELECT
  imp.ano,
  imp.sigla_uf,
  imp.id_sh4,
  SUM(imp.peso_liquido_kg) AS peso_liquido_kg,
  SUM(imp.valor_fob_dolar) AS valor_fob_dolar
FROM
  `basedosdados.br_me_comex_stat.municipio_importacao` AS imp
WHERE
  imp.ano > 2005
  AND
  imp.ano < 2022
  AND
  imp.id_sh4 = '8471'
GROUP BY
  imp.ano,
  imp.sigla_uf,
  imp.id_sh4;
"

imports <- basedosdados::read_sql(sql)
imports %>% head()
```

|  ano | sigla_uf | id_sh4 | peso_liquido_kg | valor_fob_dolar |
|-----:|:---------|:-------|----------------:|----------------:|
| 2010 | CE       | 8471   |          119546 |         3676120 |
| 2010 | SE       | 8471   |             217 |           94670 |
| 2010 | GO       | 8471   |          228131 |        33778062 |
| 2010 | PA       | 8471   |            3538 |          317603 |
| 2021 | SC       | 8471   |         5108098 |       105706528 |
| 2021 | AM       | 8471   |          216646 |        35346702 |

# Saving to parquet

As nothing seems to be wrong with the dataset, I save it as a parquet
file.

``` r
data_dir <- "data/"
file <- str_interp("${data_dir}Imports.parquet")

write_parquet(imports, file)
```
