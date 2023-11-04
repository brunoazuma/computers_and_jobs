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
cbo
```

    ## # A tibble: 2,684 × 2
    ##    cbo_2002 descricao                     
    ##    <chr>    <chr>                         
    ##  1 010105   Oficial general da aeronáutica
    ##  2 010110   Oficial general do exército   
    ##  3 010115   Oficial general da marinha    
    ##  4 010205   Oficial da aeronáutica        
    ##  5 010210   Oficial do exército           
    ##  6 010215   Oficial da marinha            
    ##  7 010305   Praça da aeronáutica          
    ##  8 010310   Praça do exército             
    ##  9 010315   Praça da marinha              
    ## 10 020105   Coronel da polícia militar    
    ## # ℹ 2,674 more rows

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

cbo
```

    ## # A tibble: 2,684 × 3
    ##    cbo_2002 descricao                      group
    ##    <chr>    <chr>                          <chr>
    ##  1 010105   Oficial general da aeronáutica <NA> 
    ##  2 010110   Oficial general do exército    <NA> 
    ##  3 010115   Oficial general da marinha     <NA> 
    ##  4 010205   Oficial da aeronáutica         <NA> 
    ##  5 010210   Oficial do exército            <NA> 
    ##  6 010215   Oficial da marinha             <NA> 
    ##  7 010305   Praça da aeronáutica           <NA> 
    ##  8 010310   Praça do exército              <NA> 
    ##  9 010315   Praça da marinha               <NA> 
    ## 10 020105   Coronel da polícia militar     <NA> 
    ## # ℹ 2,674 more rows

Finally, I exclude the military occupations from the table, following
Adamczyk, Ehrl e Monasterio (2022), and save the table in a parquet
file.

``` r
cbo <- cbo %>% filter(!is.na(group))
```

``` r
data_dir <- "data/CBO/"
file <- str_interp("${data_dir}CBO.parquet")

write_parquet(cbo, file)
```

<div id="refs" class="references csl-bib-body">

<div id="ref-adamczyk2022" class="csl-entry">

ADAMCZYK, W.; EHRL, P.; MONASTERIO, L. **[Skills and employment
transitions in Brazil](https://doi.org/10.54394/ZWJU1062)**. \[s.l.\]
ILO, 2022. v. 65

</div>

</div>
