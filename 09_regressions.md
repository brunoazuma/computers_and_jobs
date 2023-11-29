Computers and jobs - Regressions
================

- [1 Initial setup](#1-initial-setup)
- [2 Effectiveness of the task-based model for the Brazilian formal
  labor
  market](#2-effectiveness-of-the-task-based-model-for-the-brazilian-formal-labor-market)
  - [2.1 Only skill groups as explainable
    variables](#21-only-skill-groups-as-explainable-variables)
  - [2.2 Skill groups and industry sectors as explainable
    variables](#22-skill-groups-and-industry-sectors-as-explainable-variables)
  - [2.3 Results of the skill groups
    specifications](#23-results-of-the-skill-groups-specifications)
  - [2.4 Industry sectors as the only explainable
    variables](#24-industry-sectors-as-the-only-explainable-variables)
  - [2.5 Results of the industry sector
    specifications](#25-results-of-the-industry-sector-specifications)
- [3 Relation of computer adoption and the labor
  market](#3-relation-of-computer-adoption-and-the-labor-market)
  - [3.1 First specification - reproduction of Boustan, Choi e
    Clingingsmith
    (2022)](#31-first-specification---reproduction-of-nberw30400)
  - [3.2 Second specification - adding occupation
    classification](#32-second-specification---adding-occupation-classification)
  - [3.3 Third specification - Interacting group with
    exposure](#33-third-specification---interacting-group-with-exposure)
- [4 References](#4-references)

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
first set of regressions developed for the Undergrad thesis. As always,
the first part is for the setup of the libraries and paths.

# 1 Initial setup

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

``` r
if(!require(modelsummary)){install.packages('modelsummary');require(modelsummary)}
```

    ## Carregando pacotes exigidos: modelsummary

``` r
library("gtsummary")
if(!require(plm)){install.packages("plm");require(plm)}
```

    ## Carregando pacotes exigidos: plm
    ## 
    ## Attaching package: 'plm'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lag, lead

``` r
datadir <- Sys.getenv("DATA_DIR")
file <- str_interp('${datadir}Final_db.parquet')
```

At first, I’m not using the additional covariates present in the
database (gender, race, age and school level) as they don’t exactly fit
with the TWFE approach documented previously. So, I’ll use DuckDB to
load only a subset of the columns and execute an initial grouping.

``` r
temp_db_dir <- Sys.getenv("DUCK_DB_DIR")
temp_db_mem <- Sys.getenv("DUCK_DB_MEM")

con <- dbConnect(duckdb(), dbdir = temp_db_dir)
dbExecute(conn = con, str_interp("PRAGMA memory_limit='${temp_db_mem}'"))
```

    ## [1] 0

``` r
tictoc::tic()

data <- tbl(con, file) %>%
  filter(
    !is.na(secao) & !is.na(ano) & !is.na(group)
  ) %>%
  filter (
    ano < 2020
  ) %>%
  group_by(
    descricao_secao,
    ano,
    group
  ) %>%
  summarise(
    exposure=mean(exposure),
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  ungroup %>%
  mutate(
    log_wage = log(mean_wage + 1)
  ) %>%
  collect()
```

    ## `summarise()` has grouped output by "descricao_secao" and "ano". You can
    ## override using the `.groups` argument.

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

``` r
dbDisconnect(con, shutdown=TRUE)

data <- data %>%
  rename(
    secao = descricao_secao
  ) %>%
  mutate(
    secao = as.factor(secao),
    group = as.factor(group) %>% relevel('NRM'),
    ano = as.factor(ano),
    id = factor(paste(secao, group, sep="-"))
  )

tictoc::toc()
```

    ## 5.05 sec elapsed

``` r
data %>% head()
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">

secao

</th>
<th style="text-align:left;">

ano

</th>
<th style="text-align:left;">

group

</th>
<th style="text-align:right;">

exposure

</th>
<th style="text-align:right;">

valor_remuneracao_media

</th>
<th style="text-align:right;">

valor_remuneracao_media_sm

</th>
<th style="text-align:right;">

mean_wage

</th>
<th style="text-align:right;">

job_number

</th>
<th style="text-align:right;">

log_wage

</th>
<th style="text-align:left;">

id

</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

Transporte, armazenagem e correio

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.2379079

</td>
<td style="text-align:right;">

1260.7154

</td>
<td style="text-align:right;">

2.309238

</td>
<td style="text-align:right;">

13.85570

</td>
<td style="text-align:right;">

684236

</td>
<td style="text-align:right;">

2.698384

</td>
<td style="text-align:left;">

Transporte, armazenagem e correio-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRC

</td>
<td style="text-align:right;">

0.6200034

</td>
<td style="text-align:right;">

1946.4987

</td>
<td style="text-align:right;">

3.569966

</td>
<td style="text-align:right;">

20.75955

</td>
<td style="text-align:right;">

1273735

</td>
<td style="text-align:right;">

3.080053

</td>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas-NRC

</td>
</tr>
<tr>
<td style="text-align:left;">

Construção

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.0815139

</td>
<td style="text-align:right;">

903.6245

</td>
<td style="text-align:right;">

1.653258

</td>
<td style="text-align:right;">

9.51419

</td>
<td style="text-align:right;">

280251

</td>
<td style="text-align:right;">

2.352726

</td>
<td style="text-align:left;">

Construção-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Educação

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRC

</td>
<td style="text-align:right;">

0.3277561

</td>
<td style="text-align:right;">

2593.7109

</td>
<td style="text-align:right;">

4.758843

</td>
<td style="text-align:right;">

79.90194

</td>
<td style="text-align:right;">

1118786

</td>
<td style="text-align:right;">

4.393238

</td>
<td style="text-align:left;">

Educação-NRC

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades financeiras, de seguros e serviços relacionados

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.6921101

</td>
<td style="text-align:right;">

2922.6557

</td>
<td style="text-align:right;">

5.363682

</td>
<td style="text-align:right;">

37.25879

</td>
<td style="text-align:right;">

713651

</td>
<td style="text-align:right;">

3.644373

</td>
<td style="text-align:left;">

Atividades financeiras, de seguros e serviços relacionados-RC

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades imobiliárias

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.5546143

</td>
<td style="text-align:right;">

1117.9290

</td>
<td style="text-align:right;">

2.047064

</td>
<td style="text-align:right;">

11.79532

</td>
<td style="text-align:right;">

90300

</td>
<td style="text-align:right;">

2.549079

</td>
<td style="text-align:left;">

Atividades imobiliárias-RC

</td>
</tr>
</tbody>
</table>

</div>

Also, in order to support some of the models, I need to add the share of
employment relationships of each group in respect of industry-year.

``` r
data2 <- data %>%
  group_by(ano) %>%
  mutate(
    job_share = case_when(
      ano == 2006 ~ job_number/ sum(job_number),
      .default = 0
    )
  ) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    job_share = order_by(ano, cumsum(job_share)),
    log_job = log(job_number)
  ) %>%
  ungroup()

data2 %>% head()
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">

secao

</th>
<th style="text-align:left;">

ano

</th>
<th style="text-align:left;">

group

</th>
<th style="text-align:right;">

exposure

</th>
<th style="text-align:right;">

valor_remuneracao_media

</th>
<th style="text-align:right;">

valor_remuneracao_media_sm

</th>
<th style="text-align:right;">

mean_wage

</th>
<th style="text-align:right;">

job_number

</th>
<th style="text-align:right;">

log_wage

</th>
<th style="text-align:left;">

id

</th>
<th style="text-align:right;">

job_share

</th>
<th style="text-align:right;">

log_job

</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

Transporte, armazenagem e correio

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.2379079

</td>
<td style="text-align:right;">

1260.7154

</td>
<td style="text-align:right;">

2.309238

</td>
<td style="text-align:right;">

13.85570

</td>
<td style="text-align:right;">

684236

</td>
<td style="text-align:right;">

2.698384

</td>
<td style="text-align:left;">

Transporte, armazenagem e correio-NRM

</td>
<td style="text-align:right;">

0.0094560

</td>
<td style="text-align:right;">

13.43606

</td>
</tr>
<tr>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRC

</td>
<td style="text-align:right;">

0.6200034

</td>
<td style="text-align:right;">

1946.4987

</td>
<td style="text-align:right;">

3.569966

</td>
<td style="text-align:right;">

20.75955

</td>
<td style="text-align:right;">

1273735

</td>
<td style="text-align:right;">

3.080053

</td>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas-NRC

</td>
<td style="text-align:right;">

0.0174193

</td>
<td style="text-align:right;">

14.05746

</td>
</tr>
<tr>
<td style="text-align:left;">

Construção

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.0815139

</td>
<td style="text-align:right;">

903.6245

</td>
<td style="text-align:right;">

1.653258

</td>
<td style="text-align:right;">

9.51419

</td>
<td style="text-align:right;">

280251

</td>
<td style="text-align:right;">

2.352726

</td>
<td style="text-align:left;">

Construção-NRM

</td>
<td style="text-align:right;">

0.0029003

</td>
<td style="text-align:right;">

12.54344

</td>
</tr>
<tr>
<td style="text-align:left;">

Educação

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

NRC

</td>
<td style="text-align:right;">

0.3277561

</td>
<td style="text-align:right;">

2593.7109

</td>
<td style="text-align:right;">

4.758843

</td>
<td style="text-align:right;">

79.90194

</td>
<td style="text-align:right;">

1118786

</td>
<td style="text-align:right;">

4.393238

</td>
<td style="text-align:left;">

Educação-NRC

</td>
<td style="text-align:right;">

0.0173323

</td>
<td style="text-align:right;">

13.92775

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades financeiras, de seguros e serviços relacionados

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.6921101

</td>
<td style="text-align:right;">

2922.6557

</td>
<td style="text-align:right;">

5.363682

</td>
<td style="text-align:right;">

37.25879

</td>
<td style="text-align:right;">

713651

</td>
<td style="text-align:right;">

3.644373

</td>
<td style="text-align:left;">

Atividades financeiras, de seguros e serviços relacionados-RC

</td>
<td style="text-align:right;">

0.0117054

</td>
<td style="text-align:right;">

13.47815

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades imobiliárias

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.5546143

</td>
<td style="text-align:right;">

1117.9290

</td>
<td style="text-align:right;">

2.047064

</td>
<td style="text-align:right;">

11.79532

</td>
<td style="text-align:right;">

90300

</td>
<td style="text-align:right;">

2.549079

</td>
<td style="text-align:left;">

Atividades imobiliárias-RC

</td>
<td style="text-align:right;">

0.0010424

</td>
<td style="text-align:right;">

11.41089

</td>
</tr>
</tbody>
</table>

</div>

# 2 Effectiveness of the task-based model for the Brazilian formal labor market

Lastly, although initially as fixed effects to evaluate the influence of
exposure to computers on the mean_wage inequality, it could be useful to
evaluate the influence of the skill composition of the occupations with
the mean_wage inequality. It is useful as allows an evaluation of the
capability of task model itself to explain (at least, part of) the
mean_wage inequality on the formal jobs in Brazil.

To do that, I start from the following model:

$$\log(y_{j, c, t}) = \beta Exposure_{j, t} + \alpha_j + \lambda_c + \theta_t + \varepsilon_{j, c, t},$$

where $Exposure_{j, t}$ is the industry-year measure of exposure to
computers, $\alpha_j$ is an industry fixed effect, $\lambda_c$ is a
skill classification fixed effect, and $\theta_t$ is a period fixed
effect. Thus, $\beta$ will identify the effects of the changes in
exposure to computers within each outcome. The outcome here can be
either the log wage, where $y_j, c, t$ stands for the mean wage for
occupations in the industry $j$ and category $c$ on year $t$, or the log
of the number of employment relationships, where $y_j, c, t$ stands for
the sum of employment relationships of occupations in the industry $j$
and category $c$ on year $t$.

Then, I estimate three derived models based on the One-Way Fixed Effects
Model described in Baltagi (2021, p. 15–19) on the time variable and
include the skill groups of the occupations as explainable variables.
I’ll evaluate one specification with the skill groups as the only
explainable variable, and another specification with industry sector as
explainable variables, to check for omitted variable bias.

## 2.1 Only skill groups as explainable variables

So, this model becomes:

$$\left[\log(y_{j, c, t}) - \overline{\log(y_{t})}\right] = (\lambda_c - \overline{\lambda}) + (\varepsilon_{j, c, t} - \overline{\varepsilon_{t}})$$

where $\overline{\log(y_{t})}$ and $\overline{\lambda}$ are the time
demeaned versions of the outcome variables and skill classification
group dummies.

``` r
fe_reg_w_1 <- plm(log_wage ~ group,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))

fe_reg_j_1 <- plm(log_job ~ group,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 2.2 Skill groups and industry sectors as explainable variables

This one becomes:

$$\left[\log(y_{j, c, t}) - \overline{\log(y_{t})}\right] = (\lambda_c - \overline{\lambda}) + (\alpha_j - \overline{\alpha}) + (\varepsilon_{j, c, t} - \overline{\varepsilon_{t}})$$

where $\overline{\log(y_{t})}$, $\overline{\lambda}$, and
$\overline{\alpha}$ are the time demeaned versions of the outcome
variables, skill classification group dummies, and industry sector
dummies.

``` r
fe_reg_w_2 <- plm(log_wage ~ group + secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))

fe_reg_j_2 <- plm(log_job ~ group + secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 2.3 Results of the skill groups specifications

``` r
tb_fe_reg_w_1 <- gtsummary_table(
  fe_reg_w_1,
  include=c('group'),
  label=list(
    'group'='Skill group'
  ),
  robust='HC1'
)
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(include)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(include))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
tb_fe_reg_j_1 <- gtsummary_table(
  fe_reg_j_1,
  include=c('group'),
  label=list(
    'group'='Skill group'
  ),
  robust='HC1'
)

tb_fe_reg_w_2 <- gtsummary_table(
  fe_reg_w_2,
  include=c('group'),
  label=list(
    'group'='Skill group'
  ),
  robust='HC1'
)
tb_fe_reg_j_2 <- gtsummary_table(
  fe_reg_j_2,
  include=c('group'),
  label=list(
    'group'='Skill group'
  ),
  robust='HC1'
)

tbl_merge(
  list(tb_fe_reg_w_1, tb_fe_reg_w_2, tb_fe_reg_j_1, tb_fe_reg_j_2),
  c('**log(wage)**', '**log(wage)**', '**log(job)**', '**log(job)**')
)
```

<div id="ngbcmeovxq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ngbcmeovxq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ngbcmeovxq thead, #ngbcmeovxq tbody, #ngbcmeovxq tfoot, #ngbcmeovxq tr, #ngbcmeovxq td, #ngbcmeovxq th {
  border-style: none;
}
&#10;#ngbcmeovxq p {
  margin: 0;
  padding: 0;
}
&#10;#ngbcmeovxq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ngbcmeovxq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ngbcmeovxq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ngbcmeovxq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ngbcmeovxq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ngbcmeovxq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ngbcmeovxq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ngbcmeovxq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ngbcmeovxq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ngbcmeovxq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ngbcmeovxq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ngbcmeovxq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ngbcmeovxq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ngbcmeovxq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ngbcmeovxq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ngbcmeovxq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ngbcmeovxq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ngbcmeovxq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ngbcmeovxq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ngbcmeovxq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ngbcmeovxq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ngbcmeovxq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ngbcmeovxq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ngbcmeovxq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ngbcmeovxq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ngbcmeovxq .gt_left {
  text-align: left;
}
&#10;#ngbcmeovxq .gt_center {
  text-align: center;
}
&#10;#ngbcmeovxq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ngbcmeovxq .gt_font_normal {
  font-weight: normal;
}
&#10;#ngbcmeovxq .gt_font_bold {
  font-weight: bold;
}
&#10;#ngbcmeovxq .gt_font_italic {
  font-style: italic;
}
&#10;#ngbcmeovxq .gt_super {
  font-size: 65%;
}
&#10;#ngbcmeovxq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ngbcmeovxq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ngbcmeovxq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ngbcmeovxq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ngbcmeovxq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ngbcmeovxq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ngbcmeovxq .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(wage)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wage)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(job)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(job)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Skill group</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center"></td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center"></td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRM</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">—</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRC</td>
<td headers="estimate_1" class="gt_row gt_center">1.0368*** (0.113)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.0368*** (0.048)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.1265 (0.657)</td>
<td headers="p.value_3" class="gt_row gt_center">0.8</td>
<td headers="estimate_4" class="gt_row gt_center">0.1265 (0.264)</td>
<td headers="p.value_4" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RC</td>
<td headers="estimate_1" class="gt_row gt_center">0.3985*** (0.103)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.3985*** (0.041)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.6507 (0.665)</td>
<td headers="p.value_3" class="gt_row gt_center">0.3</td>
<td headers="estimate_4" class="gt_row gt_center">0.6507** (0.230)</td>
<td headers="p.value_4" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RM</td>
<td headers="estimate_1" class="gt_row gt_center">0.1472 (0.095)</td>
<td headers="p.value_1" class="gt_row gt_center">0.12</td>
<td headers="estimate_2" class="gt_row gt_center">0.1472*** (0.041)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.2705 (0.667)</td>
<td headers="p.value_3" class="gt_row gt_center">0.7</td>
<td headers="estimate_4" class="gt_row gt_center">0.2705 (0.319)</td>
<td headers="p.value_4" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.527</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.918</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.012</td>
<td headers="p.value_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.827</td>
<td headers="p.value_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">0.521</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">0.916</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">-0.001</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">0.822</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">1,176</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">1,176</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">1,176</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">1,176</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

Now, more interesting results. Although the main focus of the thesis was
to investigate the relation of computers and wage polarization, the
approach would only be valid if the task model itself could be
considered able to explain the wage inequality in the Brazilian formal
market.

And that’s what we can conclude from the models above. Even without the
industry sector fixed effects, the model get a $R^2\approx0.525$ and a
$\bar{R^2}\approx0.519$, which means that the main composition of the
tasks of the occupations alone can explain more than half of the
variability of the wages in the formal market. Of course, this results
can be biased by omitted variables, like school level or signalization
of diplomas, but the results are way better than the expected. With the
inclusion of industry sector fixed effects, the $R^2$ and $\bar{R^2}$
change to $0.916$ and $0.913$, respectively, but the increase is mixed
with the explanation capability of the industry sectors themselves,
which is not the focus of this thesis.

Now, the most important results: all of the variables are extremely
significant and the inclusion of the industry sector effects didn’t
change the coefficients, which is a good indicative of non existence of
omitted variable bias. Again, further specifications could be thought to
investigate this bias, but that would need additional theoretical
investigation to compose the model.

The interpretation of the coefficients in this case, thankfully, is very
simple: a worker with an occupation composed mostly of tasks belonging
to the RM group is expected to receive, on average, $14,7\%$ more for
his hourly wage than a worker with an occupation composed mostly of
tasks belonging to the NRM group. For the NRC and RC groups, the premium
is $103,1\%$ and $39,2\%$, respectively. However, if we consider the
order of the groups to be, from lowest to highest, { NRM, RM, RC, NRC },
there is no wage ***polarization*** effect. Although the task model
exhibits an excellent power of explanation of the Brazilian formal labor
market, the values encountered for the coefficients indicates a monotone
change of the wages.

## 2.4 Industry sectors as the only explainable variables

Just to dig a little bit more about omitted variable bias in the
industry sector fixed effects, I’ll regress wages on industry sector
only and compare with the fixed effects in the previous model with
industry sector and skill groups. So, this model becomes:

$$\left[\log(y_{j, c, t}) - \overline{\log(y_{t})}\right] = (\alpha_j - \overline{\alpha}) + (\varepsilon_{j, c, t} - \overline{\varepsilon_{t}})$$

where $\overline{\log(y_{t})}$ and $\overline{\alpha}$ are the time
demeaned versions of the outcome variables and industry sector dummies.

``` r
fe_reg_w_3 <- plm(log_wage ~ secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))

fe_reg_j_3 <- plm(log_job ~ secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 2.5 Results of the industry sector specifications

``` r
tb2_fe_reg_w_2 <- gtsummary_table(
  fe_reg_w_2,
  include=c('secao'),
  label=list(
    'secao'='CNAE industry sector'
  ),
  robust='HC1'
)
tb_fe_reg_w_3 <- gtsummary_table(
  fe_reg_w_3,
  include=c('secao'),
  label=list(
    'secao'='CNAE industry sector'
  ),
  robust='HC1'
)
tb2_fe_reg_j_2 <- gtsummary_table(
  fe_reg_j_2,
  include=c('secao'),
  label=list(
    'secao'='CNAE industry sector'
  ),
  robust='HC1'
)
tb_fe_reg_j_3 <- gtsummary_table(
  fe_reg_j_3,
  include=c('secao'),
  label=list(
    'secao'='CNAE industry sector'
  ),
  robust='HC1'
)
tbl_merge(
  list(tb2_fe_reg_w_2, tb_fe_reg_w_3, tb2_fe_reg_j_2, tb_fe_reg_j_3),
  c('**log(wage)**', '**log(wage)**', '**log(job)**', '**log(job)**')
)
```

<div id="qeuskezmbx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qeuskezmbx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qeuskezmbx thead, #qeuskezmbx tbody, #qeuskezmbx tfoot, #qeuskezmbx tr, #qeuskezmbx td, #qeuskezmbx th {
  border-style: none;
}
&#10;#qeuskezmbx p {
  margin: 0;
  padding: 0;
}
&#10;#qeuskezmbx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qeuskezmbx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#qeuskezmbx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#qeuskezmbx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#qeuskezmbx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#qeuskezmbx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qeuskezmbx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qeuskezmbx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#qeuskezmbx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qeuskezmbx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#qeuskezmbx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#qeuskezmbx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qeuskezmbx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qeuskezmbx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#qeuskezmbx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeuskezmbx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#qeuskezmbx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qeuskezmbx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qeuskezmbx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeuskezmbx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qeuskezmbx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeuskezmbx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qeuskezmbx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeuskezmbx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qeuskezmbx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeuskezmbx .gt_left {
  text-align: left;
}
&#10;#qeuskezmbx .gt_center {
  text-align: center;
}
&#10;#qeuskezmbx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qeuskezmbx .gt_font_normal {
  font-weight: normal;
}
&#10;#qeuskezmbx .gt_font_bold {
  font-weight: bold;
}
&#10;#qeuskezmbx .gt_font_italic {
  font-style: italic;
}
&#10;#qeuskezmbx .gt_super {
  font-size: 65%;
}
&#10;#qeuskezmbx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qeuskezmbx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qeuskezmbx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qeuskezmbx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qeuskezmbx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qeuskezmbx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qeuskezmbx .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(wage)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wage)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(job)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(job)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">CNAE industry sector</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center"></td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center"></td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Administração pública, defesa e seguridade social</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">—</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Agricultura, pecuária, produção florestal, pesca e aqüicultura</td>
<td headers="estimate_1" class="gt_row gt_center">-0.3656*** (0.064)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.3656 (0.256)</td>
<td headers="p.value_2" class="gt_row gt_center">0.2</td>
<td headers="estimate_3" class="gt_row gt_center">-2.0903** (0.798)</td>
<td headers="p.value_3" class="gt_row gt_center">0.009</td>
<td headers="estimate_4" class="gt_row gt_center">-2.0903** (0.798)</td>
<td headers="p.value_4" class="gt_row gt_center">0.009</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Alojamento e alimentação</td>
<td headers="estimate_1" class="gt_row gt_center">-0.5785*** (0.102)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.5785** (0.207)</td>
<td headers="p.value_2" class="gt_row gt_center">0.005</td>
<td headers="estimate_3" class="gt_row gt_center">-1.5313* (0.671)</td>
<td headers="p.value_3" class="gt_row gt_center">0.023</td>
<td headers="estimate_4" class="gt_row gt_center">-1.5313* (0.639)</td>
<td headers="p.value_4" class="gt_row gt_center">0.017</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Artes, cultura, esporte e recreação</td>
<td headers="estimate_1" class="gt_row gt_center">-0.2582** (0.086)</td>
<td headers="p.value_1" class="gt_row gt_center">0.003</td>
<td headers="estimate_2" class="gt_row gt_center">-0.2582 (0.227)</td>
<td headers="p.value_2" class="gt_row gt_center">0.3</td>
<td headers="estimate_3" class="gt_row gt_center">-3.2638*** (0.489)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-3.2638*** (0.480)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades administrativas e serviços complementares</td>
<td headers="estimate_1" class="gt_row gt_center">-0.3831*** (0.065)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.3831 (0.242)</td>
<td headers="p.value_2" class="gt_row gt_center">0.11</td>
<td headers="estimate_3" class="gt_row gt_center">-0.3698 (0.565)</td>
<td headers="p.value_3" class="gt_row gt_center">0.5</td>
<td headers="estimate_4" class="gt_row gt_center">-0.3698 (0.578)</td>
<td headers="p.value_4" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades financeiras, de seguros e serviços relacionados</td>
<td headers="estimate_1" class="gt_row gt_center">0.2592 (0.150)</td>
<td headers="p.value_1" class="gt_row gt_center">0.085</td>
<td headers="estimate_2" class="gt_row gt_center">0.2592 (0.355)</td>
<td headers="p.value_2" class="gt_row gt_center">0.5</td>
<td headers="estimate_3" class="gt_row gt_center">-3.0181*** (0.882)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-3.0181** (0.946)</td>
<td headers="p.value_4" class="gt_row gt_center">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades imobiliárias</td>
<td headers="estimate_1" class="gt_row gt_center">-0.3316*** (0.066)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.3316 (0.260)</td>
<td headers="p.value_2" class="gt_row gt_center">0.2</td>
<td headers="estimate_3" class="gt_row gt_center">-3.9409*** (0.440)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-3.9409*** (0.491)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades profissionais, científicas e técnicas</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0792 (0.076)</td>
<td headers="p.value_1" class="gt_row gt_center">0.3</td>
<td headers="estimate_2" class="gt_row gt_center">-0.0792 (0.297)</td>
<td headers="p.value_2" class="gt_row gt_center">0.8</td>
<td headers="estimate_3" class="gt_row gt_center">-1.9080*** (0.466)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-1.9080*** (0.531)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Comércio; reparação de veículos automotores e motocicletas</td>
<td headers="estimate_1" class="gt_row gt_center">-0.4707*** (0.074)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.4707* (0.229)</td>
<td headers="p.value_2" class="gt_row gt_center">0.040</td>
<td headers="estimate_3" class="gt_row gt_center">0.2371 (0.483)</td>
<td headers="p.value_3" class="gt_row gt_center">0.6</td>
<td headers="estimate_4" class="gt_row gt_center">0.2371 (0.564)</td>
<td headers="p.value_4" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Construção</td>
<td headers="estimate_1" class="gt_row gt_center">-0.2066*** (0.057)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.2066 (0.269)</td>
<td headers="p.value_2" class="gt_row gt_center">0.4</td>
<td headers="estimate_3" class="gt_row gt_center">-1.2120 (0.679)</td>
<td headers="p.value_3" class="gt_row gt_center">0.075</td>
<td headers="estimate_4" class="gt_row gt_center">-1.2120 (0.691)</td>
<td headers="p.value_4" class="gt_row gt_center">0.080</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Educação</td>
<td headers="estimate_1" class="gt_row gt_center">0.1874 (0.134)</td>
<td headers="p.value_1" class="gt_row gt_center">0.2</td>
<td headers="estimate_2" class="gt_row gt_center">0.1874 (0.364)</td>
<td headers="p.value_2" class="gt_row gt_center">0.6</td>
<td headers="estimate_3" class="gt_row gt_center">-1.7442* (0.713)</td>
<td headers="p.value_3" class="gt_row gt_center">0.015</td>
<td headers="estimate_4" class="gt_row gt_center">-1.7442* (0.723)</td>
<td headers="p.value_4" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Eletricidade e gás</td>
<td headers="estimate_1" class="gt_row gt_center">0.7208*** (0.102)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.7208** (0.262)</td>
<td headers="p.value_2" class="gt_row gt_center">0.006</td>
<td headers="estimate_3" class="gt_row gt_center">-4.2553*** (0.582)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-4.2553*** (0.616)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    ��gua, esgoto, atividades de gestão de resíduos e descontaminação</td>
<td headers="estimate_1" class="gt_row gt_center">0.0743 (0.101)</td>
<td headers="p.value_1" class="gt_row gt_center">0.5</td>
<td headers="estimate_2" class="gt_row gt_center">0.0743 (0.317)</td>
<td headers="p.value_2" class="gt_row gt_center">0.8</td>
<td headers="estimate_3" class="gt_row gt_center">-3.0543*** (0.526)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-3.0543*** (0.498)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Indústrias de transformação</td>
<td headers="estimate_1" class="gt_row gt_center">-0.1696** (0.055)</td>
<td headers="p.value_1" class="gt_row gt_center">0.002</td>
<td headers="estimate_2" class="gt_row gt_center">-0.1696 (0.271)</td>
<td headers="p.value_2" class="gt_row gt_center">0.5</td>
<td headers="estimate_3" class="gt_row gt_center">-0.2372 (0.624)</td>
<td headers="p.value_3" class="gt_row gt_center">0.7</td>
<td headers="estimate_4" class="gt_row gt_center">-0.2372 (0.660)</td>
<td headers="p.value_4" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Indústrias extrativas</td>
<td headers="estimate_1" class="gt_row gt_center">0.4979*** (0.079)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.4979 (0.307)</td>
<td headers="p.value_2" class="gt_row gt_center">0.11</td>
<td headers="estimate_3" class="gt_row gt_center">-3.6409*** (0.582)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-3.6409*** (0.598)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Informação e comunicação</td>
<td headers="estimate_1" class="gt_row gt_center">0.0297 (0.074)</td>
<td headers="p.value_1" class="gt_row gt_center">0.7</td>
<td headers="estimate_2" class="gt_row gt_center">0.0297 (0.250)</td>
<td headers="p.value_2" class="gt_row gt_center">>0.9</td>
<td headers="estimate_3" class="gt_row gt_center">-2.1590*** (0.526)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-2.1590*** (0.558)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Organismos internacionais e outras instituições extraterritoriais</td>
<td headers="estimate_1" class="gt_row gt_center">0.3386*** (0.070)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.3386 (0.273)</td>
<td headers="p.value_2" class="gt_row gt_center">0.2</td>
<td headers="estimate_3" class="gt_row gt_center">-7.1360*** (0.397)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-7.1360*** (0.419)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outras atividades de serviços</td>
<td headers="estimate_1" class="gt_row gt_center">-0.2250*** (0.065)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.2250 (0.282)</td>
<td headers="p.value_2" class="gt_row gt_center">0.4</td>
<td headers="estimate_3" class="gt_row gt_center">-1.6148*** (0.431)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-1.6148*** (0.437)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Saúde humana e serviços sociais</td>
<td headers="estimate_1" class="gt_row gt_center">-0.2189*** (0.059)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.2189 (0.249)</td>
<td headers="p.value_2" class="gt_row gt_center">0.4</td>
<td headers="estimate_3" class="gt_row gt_center">-1.5339* (0.669)</td>
<td headers="p.value_3" class="gt_row gt_center">0.022</td>
<td headers="estimate_4" class="gt_row gt_center">-1.5339* (0.670)</td>
<td headers="p.value_4" class="gt_row gt_center">0.022</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Serviços domésticos</td>
<td headers="estimate_1" class="gt_row gt_center">-0.6639*** (0.117)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">-0.6639*** (0.197)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">-7.3112*** (0.682)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td>
<td headers="estimate_4" class="gt_row gt_center">-7.3112*** (0.643)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Transporte, armazenagem e correio</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0712 (0.066)</td>
<td headers="p.value_1" class="gt_row gt_center">0.3</td>
<td headers="estimate_2" class="gt_row gt_center">-0.0712 (0.258)</td>
<td headers="p.value_2" class="gt_row gt_center">0.8</td>
<td headers="estimate_3" class="gt_row gt_center">-1.1739* (0.567)</td>
<td headers="p.value_3" class="gt_row gt_center">0.039</td>
<td headers="estimate_4" class="gt_row gt_center">-1.1739* (0.586)</td>
<td headers="p.value_4" class="gt_row gt_center">0.045</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.918</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.391</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.827</td>
<td headers="p.value_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.815</td>
<td headers="p.value_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">0.916</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">0.374</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">0.822</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">0.809</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">1,176</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">1,176</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">1,176</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">1,176</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

The main interest in this results is the possibility of bias in the
industry sector coefficients, which didn’t appear, as all the
coefficients kept the same with and without the skill group variable.

# 3 Relation of computer adoption and the labor market

Here, I also start from the following model:

$$\log(y_{j, c, t}) = \beta Exposure_{j, t} + \alpha_j + \lambda_c + \theta_t + \varepsilon_{j, c, t},$$

where $Exposure_{j, t}$ is the industry-year measure of exposure to
computers, $\alpha_j$ is an industry fixed effect, $\lambda_c$ is a
skill classification fixed effect, and $\theta_t$ is a period fixed
effect. Thus, $\beta$ will identify the effects of the changes in
exposure to computers within each outcome. The outcome here can be
either the log wage, where $y_j, c, t$ stands for the mean wage for
occupations in the industry $j$ and category $c$ on year $t$, or the log
of the number of employment relationships, where $y_j, c, t$ stands for
the sum of employment relationships of occupations in the industry $j$
and category $c$ on year $t$.

Then, I estimate three derived models based on the Two Way Fixed Effects
Model described in Baltagi (2021). I’ll detail each model right before
its estimation.

## 3.1 First specification - reproduction of Boustan, Choi e Clingingsmith (2022)

First, I’ll try to reproduce the same analysis of Boustan, Choi e
Clingingsmith (2022), without considering the predominant task set of
the occupations. For that, the first thing I need to do is aggregate the
data.

``` r
nber30400 <- data%>%
  filter(!is.na(secao) & !is.na(ano)) %>%
  group_by(secao, ano) %>%
  summarise(
    mean_wage = mean(mean_wage),
    exposure = mean(exposure),
    job_number = sum(job_number)
  ) %>%
  mutate(
    log_wage = log(mean_wage + 1),
    log_job = log(job_number)
  ) %>%
  ungroup() %>%
  group_by(ano) %>%
  mutate(
    job_share = case_when(
      ano == 2006 ~ job_number/ sum(job_number),
      .default = 0
    )
  ) %>%
  ungroup() %>%
  group_by(secao) %>%
  mutate(
    job_share = cumsum(job_share)
  ) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'secao'. You can override using the
    ## `.groups` argument.

``` r
nber30400 %>% head()
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">

secao

</th>
<th style="text-align:left;">

ano

</th>
<th style="text-align:right;">

mean_wage

</th>
<th style="text-align:right;">

exposure

</th>
<th style="text-align:right;">

job_number

</th>
<th style="text-align:right;">

log_wage

</th>
<th style="text-align:right;">

log_job

</th>
<th style="text-align:right;">

job_share

</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2006

</td>
<td style="text-align:right;">

17.92330

</td>
<td style="text-align:right;">

0.3948245

</td>
<td style="text-align:right;">

7991148

</td>
<td style="text-align:right;">

2.940394

</td>
<td style="text-align:right;">

15.89385

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2007

</td>
<td style="text-align:right;">

18.52453

</td>
<td style="text-align:right;">

0.3351270

</td>
<td style="text-align:right;">

8544899

</td>
<td style="text-align:right;">

2.971672

</td>
<td style="text-align:right;">

15.96085

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2008

</td>
<td style="text-align:right;">

18.65942

</td>
<td style="text-align:right;">

0.3235541

</td>
<td style="text-align:right;">

9016991

</td>
<td style="text-align:right;">

2.978557

</td>
<td style="text-align:right;">

16.01462

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2009

</td>
<td style="text-align:right;">

19.40677

</td>
<td style="text-align:right;">

0.3013252

</td>
<td style="text-align:right;">

9351978

</td>
<td style="text-align:right;">

3.015867

</td>
<td style="text-align:right;">

16.05110

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2010

</td>
<td style="text-align:right;">

19.94080

</td>
<td style="text-align:right;">

0.4009316

</td>
<td style="text-align:right;">

9881696

</td>
<td style="text-align:right;">

3.041700

</td>
<td style="text-align:right;">

16.10619

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
<tr>
<td style="text-align:left;">

Administração pública, defesa e seguridade social

</td>
<td style="text-align:left;">

2011

</td>
<td style="text-align:right;">

21.08816

</td>
<td style="text-align:right;">

0.4158616

</td>
<td style="text-align:right;">

9955126

</td>
<td style="text-align:right;">

3.095042

</td>
<td style="text-align:right;">

16.11360

</td>
<td style="text-align:right;">

0.1601092

</td>
</tr>
</tbody>
</table>

</div>

Now, I regress exposure on log_remuneracao_media with plm. This package
provides a series of estimators for panel data, including the Two-Way
Fixed Effects described in Baltagi (2021, p. 47–49), which is used here.
So, the estimated model has the following form:

$$\widetilde{\log(y)}_{j,t} = \beta\widetilde{Exp}_{j,t} + \widetilde{\varepsilon}_{j,t}$$

where $\widetilde{\log(y)}_{j,t}$ and $\widetilde{Exp}_{j,t}$ are the
double demeaned versions of the outcome variables (log wages or log of
employment relationships) and exposure for computers measure. Also, as
this model doesn’t consider the skill groups of occupations, the outcome
is indexed only by industry-year.

``` r
twfe_reg_w <- plm(log_wage ~ exposure,
             data = nber30400,
             model = 'within',
             effect = 'twoways',
             index = c('secao','ano'))
```

``` r
twfe_reg_j <- plm(log_job ~ exposure,
             data = nber30400,
             model = 'within',
             effect = 'twoways',
             index = c('secao','ano'))
```

``` r
tb_twfe_reg_w <- gtsummary_table(
  twfe_reg_w,
  include = c('exposure'),
  label = list('exposure'='Exposure to computers'),
  robust = 'HC1'
)

tb_twfe_reg_j <- gtsummary_table(
  twfe_reg_j,
  include=c('exposure'),
  label = list('exposure'='Exposure to computers'),
  robust = 'HC1'
)

tbl_merge(
  list(tb_twfe_reg_w, tb_twfe_reg_j),
  c('**log(wages)**', '**log(jobs)**')
)
```

<div id="mpwebkfbvx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mpwebkfbvx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mpwebkfbvx thead, #mpwebkfbvx tbody, #mpwebkfbvx tfoot, #mpwebkfbvx tr, #mpwebkfbvx td, #mpwebkfbvx th {
  border-style: none;
}
&#10;#mpwebkfbvx p {
  margin: 0;
  padding: 0;
}
&#10;#mpwebkfbvx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mpwebkfbvx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#mpwebkfbvx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#mpwebkfbvx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#mpwebkfbvx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#mpwebkfbvx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mpwebkfbvx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mpwebkfbvx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#mpwebkfbvx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mpwebkfbvx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#mpwebkfbvx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#mpwebkfbvx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mpwebkfbvx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mpwebkfbvx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#mpwebkfbvx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mpwebkfbvx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#mpwebkfbvx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mpwebkfbvx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mpwebkfbvx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mpwebkfbvx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mpwebkfbvx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mpwebkfbvx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mpwebkfbvx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mpwebkfbvx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mpwebkfbvx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mpwebkfbvx .gt_left {
  text-align: left;
}
&#10;#mpwebkfbvx .gt_center {
  text-align: center;
}
&#10;#mpwebkfbvx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mpwebkfbvx .gt_font_normal {
  font-weight: normal;
}
&#10;#mpwebkfbvx .gt_font_bold {
  font-weight: bold;
}
&#10;#mpwebkfbvx .gt_font_italic {
  font-style: italic;
}
&#10;#mpwebkfbvx .gt_super {
  font-size: 65%;
}
&#10;#mpwebkfbvx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mpwebkfbvx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mpwebkfbvx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mpwebkfbvx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mpwebkfbvx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mpwebkfbvx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mpwebkfbvx .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(wages)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wages)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(jobs)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(jobs)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0117 (0.109)</td>
<td headers="p.value_1" class="gt_row gt_center">>0.9</td>
<td headers="estimate_2" class="gt_row gt_center">-0.8675* (0.347)</td>
<td headers="p.value_2" class="gt_row gt_center">0.013</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.000</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.042</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">-0.131</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">-0.083</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">294</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">294</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

The results show that the `Exposure to computers` measure alone does not
explain any variation in the log mean_wage of the brazilian formal
market, both by the non significant result for the variable itself and
by the $R^2$ for the within estimator.

## 3.2 Second specification - adding occupation classification

As a second step, I’ll modify the model to incorporate the task
compositions of the occupations through the discrete classification of
the predominant skill group of each occupation. With the data aggregated
by industry sector, year and task group, I regress the log_wages on the
exposure measure.In this case, the estimated model is:

$$\widetilde{\log(y)}_{j, c, t} = \gamma\widetilde{Exp}_{j,t} + \widetilde{\varepsilon}_{j, c, t}$$

where, again, $\widetilde{\log(y)}_{j, c, t}$ and
$\widetilde{Exp}_{j,t}$ are the double demeaned versions of the outcome
variables (log wages or log of employment relationships) and exposure
for computers measure.

``` r
twfe_reg_w_2 <- plm(log_wage ~ exposure,
             data = data2,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
twfe_reg_j_2 <- plm(log_job ~ exposure,
             data = data2,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
tb_twfe_reg_w_2 <- gtsummary_table(
  twfe_reg_w_2,
  include = c('exposure'),
  label = list('exposure'='Exposure to computers'),
  robust = 'HC1'
)

tb_twfe_reg_j_2 <- gtsummary_table(
  twfe_reg_j_2,
  include = c('exposure'),
  label = list('exposure'='Exposure to computers'),
  robust='HC1'
)

tbl_merge(
  list(tb_twfe_reg_w_2, tb_twfe_reg_j_2),
  c('**log(wages)**', '**log(jobs)**')
)
```

<div id="qbpxiuqqgg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qbpxiuqqgg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qbpxiuqqgg thead, #qbpxiuqqgg tbody, #qbpxiuqqgg tfoot, #qbpxiuqqgg tr, #qbpxiuqqgg td, #qbpxiuqqgg th {
  border-style: none;
}
&#10;#qbpxiuqqgg p {
  margin: 0;
  padding: 0;
}
&#10;#qbpxiuqqgg .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qbpxiuqqgg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#qbpxiuqqgg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#qbpxiuqqgg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#qbpxiuqqgg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#qbpxiuqqgg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qbpxiuqqgg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qbpxiuqqgg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#qbpxiuqqgg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qbpxiuqqgg .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#qbpxiuqqgg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#qbpxiuqqgg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qbpxiuqqgg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qbpxiuqqgg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#qbpxiuqqgg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qbpxiuqqgg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#qbpxiuqqgg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qbpxiuqqgg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qbpxiuqqgg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qbpxiuqqgg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qbpxiuqqgg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qbpxiuqqgg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qbpxiuqqgg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qbpxiuqqgg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qbpxiuqqgg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qbpxiuqqgg .gt_left {
  text-align: left;
}
&#10;#qbpxiuqqgg .gt_center {
  text-align: center;
}
&#10;#qbpxiuqqgg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qbpxiuqqgg .gt_font_normal {
  font-weight: normal;
}
&#10;#qbpxiuqqgg .gt_font_bold {
  font-weight: bold;
}
&#10;#qbpxiuqqgg .gt_font_italic {
  font-style: italic;
}
&#10;#qbpxiuqqgg .gt_super {
  font-size: 65%;
}
&#10;#qbpxiuqqgg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qbpxiuqqgg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qbpxiuqqgg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qbpxiuqqgg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qbpxiuqqgg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qbpxiuqqgg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qbpxiuqqgg .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(wages)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wages)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(jobs)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(jobs)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0015 (0.064)</td>
<td headers="p.value_1" class="gt_row gt_center">>0.9</td>
<td headers="estimate_2" class="gt_row gt_center">-0.5762** (0.205)</td>
<td headers="p.value_2" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.000</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.016</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">-0.090</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">-0.072</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">1,176</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">1,176</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

Again, the results show that the `Exposure to computers` measure alone
does not explain any variation in the log mean_wage of the brazilian
formal market, both by the non significant result for the variable
itself and by the $R^2$ for the within estimator. As the result of both
these specifications indicate a non significant value for the
coefficient, at least it seems to be consistent.

## 3.3 Third specification - Interacting group with exposure

Two last test using the Baltagi (2021) TWFE will be made by interacting
the predominant skill group of the occupations with the exposure to
computers measure, so we can investigate if the exposure to computer has
heterogeneous effects through the different skill groups. To prevent the
correlation between the exposure to computers measure and the RC
occupations of the first period, used to construct the measure, we
exclude the first period data in this evaluation. So, the two following
models are estimated here:

$$\widetilde{\log(y)}_{j, c, t} = \delta\widetilde{Exp\times\lambda}_{j,c,t} + \widetilde{\varepsilon}_{j, c, t}$$

and

$$\widetilde{\log(y)}_{j, c, t} = \rho\widetilde{Exp}_{j,t} + \delta\widetilde{Exp\times\lambda}_{j,c,t} + \widetilde{\varepsilon}_{j, c, t}$$

where, again, $\widetilde{\log(y)}_{j, c, t}$ and
$\widetilde{Exp}_{j,t}$ are the double demeaned versions of the outcome
variables (log wages or log of employment relationships) and exposure
for computers measure, and $\delta\widetilde{Exp\times\lambda}_{j,c,t}$
is the double demeaned version of the multiplication of exposure for
computers measure and skill group classification vector.

``` r
twfe_reg_w_3 <- plm(log_wage ~ exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
twfe_reg_j_3 <- plm(log_job ~ exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
twfe_reg_w_4 <- plm(log_wage ~ exposure + exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
twfe_reg_j_4 <- plm(log_job ~ exposure + exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
tb_twfe_reg_w_3 <- gtsummary_table(
  twfe_reg_w_3,
  include = c('exposure:group'),
  label = list(
    'exposure:group'='Exposure to computers x Skill type'
    ),
  robust = 'HC1'
)

tb_twfe_reg_w_4 <- gtsummary_table(
  twfe_reg_w_4,
  include = c('exposure', 'exposure:group'),
  label = list(
    'exposure:group'='Exposure to computers x Skill type'
    ),
  robust = 'HC1'
)

tb_twfe_reg_j_3 <- gtsummary_table(
  twfe_reg_j_3,
  include = c('exposure:group'),
  label = list(
    'exposure:group'='Exposure to computers x Skill type'
    ),
  robust = 'HC1'
)

tb_twfe_reg_j_4 <- gtsummary_table(
  twfe_reg_j_4,
  include = c('exposure', 'exposure:group'),
  label = list(
    'exposure:group'='Exposure to computers x Skill type'
    ),
  robust = 'HC1'
)

tbl_merge(
  list(tb_twfe_reg_w_4, tb_twfe_reg_w_3, tb_twfe_reg_j_4, tb_twfe_reg_j_3),
  c('**log(wage)**', '**log(wage)**', '**log(job)**', '**log(job)**')
)
```

<div id="hqgoalxynu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hqgoalxynu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hqgoalxynu thead, #hqgoalxynu tbody, #hqgoalxynu tfoot, #hqgoalxynu tr, #hqgoalxynu td, #hqgoalxynu th {
  border-style: none;
}
&#10;#hqgoalxynu p {
  margin: 0;
  padding: 0;
}
&#10;#hqgoalxynu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hqgoalxynu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#hqgoalxynu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#hqgoalxynu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#hqgoalxynu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#hqgoalxynu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hqgoalxynu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hqgoalxynu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#hqgoalxynu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hqgoalxynu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#hqgoalxynu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#hqgoalxynu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hqgoalxynu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hqgoalxynu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#hqgoalxynu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hqgoalxynu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#hqgoalxynu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hqgoalxynu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hqgoalxynu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hqgoalxynu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hqgoalxynu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hqgoalxynu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hqgoalxynu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hqgoalxynu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#hqgoalxynu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hqgoalxynu .gt_left {
  text-align: left;
}
&#10;#hqgoalxynu .gt_center {
  text-align: center;
}
&#10;#hqgoalxynu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hqgoalxynu .gt_font_normal {
  font-weight: normal;
}
&#10;#hqgoalxynu .gt_font_bold {
  font-weight: bold;
}
&#10;#hqgoalxynu .gt_font_italic {
  font-style: italic;
}
&#10;#hqgoalxynu .gt_super {
  font-size: 65%;
}
&#10;#hqgoalxynu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hqgoalxynu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hqgoalxynu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hqgoalxynu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hqgoalxynu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hqgoalxynu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hqgoalxynu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(wage)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wage)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="&lt;strong&gt;log(job)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(job)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">exposure</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0748 (0.079)</td>
<td headers="p.value_1" class="gt_row gt_center">0.3</td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">-0.5641* (0.224)</td>
<td headers="p.value_3" class="gt_row gt_center">0.012</td>
<td headers="estimate_4" class="gt_row gt_center"></td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers x Skill type</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center"></td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center"></td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * NRC</td>
<td headers="estimate_1" class="gt_row gt_center">0.0771 (0.087)</td>
<td headers="p.value_1" class="gt_row gt_center">0.4</td>
<td headers="estimate_2" class="gt_row gt_center">0.0023 (0.080)</td>
<td headers="p.value_2" class="gt_row gt_center">>0.9</td>
<td headers="estimate_3" class="gt_row gt_center">-0.4390* (0.203)</td>
<td headers="p.value_3" class="gt_row gt_center">0.031</td>
<td headers="estimate_4" class="gt_row gt_center">-1.0031*** (0.207)</td>
<td headers="p.value_4" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RC</td>
<td headers="estimate_1" class="gt_row gt_center">0.1573 (0.083)</td>
<td headers="p.value_1" class="gt_row gt_center">0.057</td>
<td headers="estimate_2" class="gt_row gt_center">0.0825 (0.075)</td>
<td headers="p.value_2" class="gt_row gt_center">0.3</td>
<td headers="estimate_3" class="gt_row gt_center">-0.0443 (0.196)</td>
<td headers="p.value_3" class="gt_row gt_center">0.8</td>
<td headers="estimate_4" class="gt_row gt_center">-0.6084** (0.200)</td>
<td headers="p.value_4" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RM</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0321 (0.079)</td>
<td headers="p.value_1" class="gt_row gt_center">0.7</td>
<td headers="estimate_2" class="gt_row gt_center">-0.1070 (0.080)</td>
<td headers="p.value_2" class="gt_row gt_center">0.2</td>
<td headers="estimate_3" class="gt_row gt_center">0.5519* (0.223)</td>
<td headers="p.value_3" class="gt_row gt_center">0.014</td>
<td headers="estimate_4" class="gt_row gt_center">-0.0122 (0.214)</td>
<td headers="p.value_4" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * NRM</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">-0.0748 (0.079)</td>
<td headers="p.value_2" class="gt_row gt_center">0.3</td>
<td headers="estimate_3" class="gt_row gt_center"></td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">-0.5641* (0.224)</td>
<td headers="p.value_4" class="gt_row gt_center">0.012</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.008</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.008</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.045</td>
<td headers="p.value_3" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.045</td>
<td headers="p.value_4" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">-0.092</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">-0.092</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">-0.050</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">-0.050</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">1,092</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">1,092</td>
<td headers="p.value_2" class="gt_row gt_center"></td>
<td headers="estimate_3" class="gt_row gt_center">1,092</td>
<td headers="p.value_3" class="gt_row gt_center"></td>
<td headers="estimate_4" class="gt_row gt_center">1,092</td>
<td headers="p.value_4" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="9"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

Now, some interesting results. On **(1)**, with just the interactions as
explainable variables, none of the coefficients are significant at 5%
level, but the *p-value* of the interaction between the measure and the
RC occupations is $\approx0.054$, which seems good enough to deserve
some consideration. But, contrary to my hypothesis, in this case the
exposure to computers would be responsible to an increase in the mean
wage of this group of occupations. As the measure itself is not so clear
to explain, the better approach of interpretation I can think is to
describe what would happen in the wage based on the means and standard
errors of both of the variables (explained and explainable). I’ll not
evolve on this now, as I didn’t do any descriptive statistics work, but
I’ll return on this later.

On **(2)**, the things change a little. As the NRM interaction is
dropped, all of the other coefficients get bigger absolute values. But
the variance of the estimators don’t change too much, which causes the
*p-value* of all of the coefficients to decrease. So, in this
specification, we get the NRC and RC to be significant at 5% level, but
it seems more like a mechanical (and accidental) p-hack. Before finish
this evaluation, I’ll try a third variation here, setting the NRC
interaction (the closest to 0) to be dropped.

``` r
data3 <- data2 %>%
  mutate(
    group = as.factor(group) %>% relevel('NRC'),
  ) %>%
  filter(ano != '2006')

twfe_reg_w_5 <- plm(log_wage ~ exposure + exposure:group,
             data = data3,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))

twfe_reg_j_5 <- plm(log_job ~ exposure + exposure:group,
             data = data3,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
tb_twfe_reg_w_5 <- gtsummary_table(
  twfe_reg_w_5,
  include=c('exposure', 'exposure:group'),
  label = list('exposure'='exposure',
               'exposure:group'='Exposure to computers x Skill type'),
  robust = 'HC1'
)

tb_twfe_reg_j_5 <- gtsummary_table(
  twfe_reg_j_5,
  include=c('exposure', 'exposure:group'),
  label = list('exposure'='exposure',
               'exposure:group'='Exposure to computers x Skill type'),
  robust = 'HC1'
)

tbl_merge(
  list(tb_twfe_reg_w_5, tb_twfe_reg_j_5),
  c('**log(wage)**', '**log(job)**')
)
```

<div id="mhlxzanays" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mhlxzanays table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mhlxzanays thead, #mhlxzanays tbody, #mhlxzanays tfoot, #mhlxzanays tr, #mhlxzanays td, #mhlxzanays th {
  border-style: none;
}
&#10;#mhlxzanays p {
  margin: 0;
  padding: 0;
}
&#10;#mhlxzanays .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mhlxzanays .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#mhlxzanays .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#mhlxzanays .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#mhlxzanays .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#mhlxzanays .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mhlxzanays .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mhlxzanays .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#mhlxzanays .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mhlxzanays .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#mhlxzanays .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#mhlxzanays .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mhlxzanays .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mhlxzanays .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#mhlxzanays .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mhlxzanays .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#mhlxzanays .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mhlxzanays .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mhlxzanays .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mhlxzanays .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mhlxzanays .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mhlxzanays .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mhlxzanays .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mhlxzanays .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mhlxzanays .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mhlxzanays .gt_left {
  text-align: left;
}
&#10;#mhlxzanays .gt_center {
  text-align: center;
}
&#10;#mhlxzanays .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mhlxzanays .gt_font_normal {
  font-weight: normal;
}
&#10;#mhlxzanays .gt_font_bold {
  font-weight: bold;
}
&#10;#mhlxzanays .gt_font_italic {
  font-style: italic;
}
&#10;#mhlxzanays .gt_super {
  font-size: 65%;
}
&#10;#mhlxzanays .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mhlxzanays .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mhlxzanays .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mhlxzanays .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mhlxzanays .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mhlxzanays .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mhlxzanays .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(wage)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(wage)</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;log(job)&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>log(job)</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Coef. (SE)&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;"><strong>Coef. (SE)</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1,2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">exposure</td>
<td headers="estimate_1" class="gt_row gt_center">0.0023 (0.080)</td>
<td headers="p.value_1" class="gt_row gt_center">>0.9</td>
<td headers="estimate_2" class="gt_row gt_center">-1.0031*** (0.207)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers x Skill type</td>
<td headers="estimate_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * NRM</td>
<td headers="estimate_1" class="gt_row gt_center">-0.0771 (0.087)</td>
<td headers="p.value_1" class="gt_row gt_center">0.4</td>
<td headers="estimate_2" class="gt_row gt_center">0.4390* (0.203)</td>
<td headers="p.value_2" class="gt_row gt_center">0.031</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RC</td>
<td headers="estimate_1" class="gt_row gt_center">0.0802 (0.082)</td>
<td headers="p.value_1" class="gt_row gt_center">0.3</td>
<td headers="estimate_2" class="gt_row gt_center">0.3948* (0.163)</td>
<td headers="p.value_2" class="gt_row gt_center">0.015</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RM</td>
<td headers="estimate_1" class="gt_row gt_center">-0.1092 (0.078)</td>
<td headers="p.value_1" class="gt_row gt_center">0.2</td>
<td headers="estimate_2" class="gt_row gt_center">0.9910*** (0.194)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.008</td>
<td headers="p.value_1" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td>
<td headers="estimate_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.045</td>
<td headers="p.value_2" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate_1" class="gt_row gt_center">-0.092</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">-0.050</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate_1" class="gt_row gt_center">1,092</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="estimate_2" class="gt_row gt_center">1,092</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> SE = Standard error</td>
    </tr>
  </tfoot>
</table>
</div>

As expected, in **(3)** the significance of the RC interaction doesn’t
keep significant (by far, with a $0.342$ *p-value*) with the change of
group to be the ground level. Now, the RM and NRM interactions (which
have signs opposed to NRC interaction) show a 5% level significance. But
the results from **(2)** and **(3)** doesn’t appear to be robust enough
to be reported, so I intend to keep just the **(1)** specification on
the final version of the thesis.

# 4 References

<div id="refs" class="references csl-bib-body">

<div id="ref-baltagi2021" class="csl-entry">

BALTAGI, B. H. **[Econometric Analysis of Panel
Data](https://doi.org/10.1007/978-3-030-53953-5)**. \[s.l.\] Springer
International Publishing, 2021.

</div>

<div id="ref-nberw30400" class="csl-entry">

BOUSTAN, L. P.; CHOI, J.; CLINGINGSMITH, D. **Automation After the
Assembly Line: Computerized Machine Tools, Employment and Productivity
in the United States**. ago. 2022. Disponível em:
\<<https://www.nber.org/papers/w30400>\>.

</div>

</div>
