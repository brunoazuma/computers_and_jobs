Computers and jobs - CBO
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
exploratory analysis and preparation of data from the Cadastro
Brasileiro de Ocupações (CBO) of the brazilian federal Ministry of Labor
and Employment, as made available by [Base dos
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

Now, I download the CBO data from basedosdados.

``` r
sql <- "
SELECT
  cbo.cbo_2002,
  cbo.descricao
FROM
  basedosdados.br_bd_diretorios_brasil.cbo_2002 AS cbo
"

cbo <- basedosdados::read_sql(sql)
cbo %>% head()
```

<div class="kable-table">

| cbo_2002 | descricao                      |
|:---------|:-------------------------------|
| 010105   | Oficial general da aeronáutica |
| 010110   | Oficial general do exército    |
| 010115   | Oficial general da marinha     |
| 010205   | Oficial da aeronáutica         |
| 010210   | Oficial do exército            |
| 010215   | Oficial da marinha             |

</div>

Now, I attribute the discrete classification based on Adamczyk, Ehrl e
Monasterio (2022).

``` r
cbo <- cbo %>%
  mutate(group=case_when(
    str_detect(cbo_2002, '^1') ~ 'NRC',
    str_detect(cbo_2002, '^2') ~ 'NRC',
    str_detect(cbo_2002, '^30') ~ 'NRC',
    str_detect(cbo_2002, '^31') ~ 'NRC',
    str_detect(cbo_2002, '^32') ~ 'NRC',
    str_detect(cbo_2002, '^33') ~ 'RC',
    str_detect(cbo_2002, '^35') ~ 'RC',
    str_detect(cbo_2002, '^39') ~ 'RC',
    str_detect(cbo_2002, '^4') ~ 'RC',
    str_detect(cbo_2002, '^52') ~ 'RC',
    str_detect(cbo_2002, '^34') ~ 'NRM',
    str_detect(cbo_2002, '^37') ~ 'NRM',
    str_detect(cbo_2002, '^51') ~ 'NRM',
    str_detect(cbo_2002, '^61') ~ 'NRM',
    str_detect(cbo_2002, '^62') ~ 'RM',
    str_detect(cbo_2002, '^63') ~ 'RM',
    str_detect(cbo_2002, '^64') ~ 'RM',
    str_detect(cbo_2002, '^7') ~ 'RM',
    str_detect(cbo_2002, '^8') ~ 'RM',
    str_detect(cbo_2002, '^9') ~ 'RM',
  ))

cbo %>% head()
```

<div class="kable-table">

| cbo_2002 | descricao                      | group |
|:---------|:-------------------------------|:------|
| 010105   | Oficial general da aeronáutica | NA    |
| 010110   | Oficial general do exército    | NA    |
| 010115   | Oficial general da marinha     | NA    |
| 010205   | Oficial da aeronáutica         | NA    |
| 010210   | Oficial do exército            | NA    |
| 010215   | Oficial da marinha             | NA    |

</div>

Finally, I exclude the military occupations from the table, following
Adamczyk, Ehrl e Monasterio (2022), and save the table in a parquet
file.

``` r
cbo <- cbo %>% filter(!is.na(group))
cbo %>% head()
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
data_dir <- "data/CBO/"
file <- str_interp("${data_dir}CBO.parquet")

write_parquet(cbo, file)
```

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-adamczyk2022" class="csl-entry">

ADAMCZYK, W.; EHRL, P.; MONASTERIO, L. **[Skills and employment
transitions in Brazil](https://doi.org/10.54394/ZWJU1062)**. \[s.l.\]
ILO, 2022. v. 65

</div>

</div>
