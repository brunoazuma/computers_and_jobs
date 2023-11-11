Computers and jobs - Regressions
================

- [1 Initial setup](#1-initial-setup)
- [2 First specification - reproduction of Boustan, Choi e Clingingsmith
  (2022)](#2-first-specification---reproduction-of-nberw30400)
- [3 Second specification - adding occupation
  classification](#3-second-specification---adding-occupation-classification)
- [4 Third specification - Interacting group with
  exposure](#4-third-specification---interacting-group-with-exposure)
- [5 Using group as an explainable
  variable](#5-using-group-as-an-explainable-variable)
  - [5.1 Only skill groups as explainable
    variables](#51-only-skill-groups-as-explainable-variables)
  - [5.2 Skill groups and industry sectors as explainable
    variables](#52-skill-groups-and-industry-sectors-as-explainable-variables)
  - [5.3 Results of the skill groups
    specifications](#53-results-of-the-skill-groups-specifications)
  - [5.4 Industry sectors as the only explainable
    variables](#54-industry-sectors-as-the-only-explainable-variables)
  - [5.5 Results of the industry sector
    specifications](#55-results-of-the-industry-sector-specifications)
- [6 References](#6-references)

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

    ## 3.91 sec elapsed

``` r
knitr::kable(data %>% head())
```

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

Atividades administrativas e serviços complementares

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1081246

</td>
<td style="text-align:right;">

1387.846

</td>
<td style="text-align:right;">

1.254437

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

3416762

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades administrativas e serviços complementares-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Educação

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1009501

</td>
<td style="text-align:right;">

1583.108

</td>
<td style="text-align:right;">

1.431735

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

297674

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Educação-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.1659635

</td>
<td style="text-align:right;">

2310.278

</td>
<td style="text-align:right;">

2.092872

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

914336

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas-RC

</td>
</tr>
<tr>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1909634

</td>
<td style="text-align:right;">

1377.084

</td>
<td style="text-align:right;">

1.244584

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

1243422

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Informação e comunicação

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1351196

</td>
<td style="text-align:right;">

1987.541

</td>
<td style="text-align:right;">

1.799336

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

102422

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Informação e comunicação-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

RM

</td>
<td style="text-align:right;">

0.1659635

</td>
<td style="text-align:right;">

1952.168

</td>
<td style="text-align:right;">

1.767445

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

300809

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas-RM

</td>
</tr>
</tbody>
</table>

# 2 First specification - reproduction of Boustan, Choi e Clingingsmith (2022)

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
    exposure = mean(exposure)
  ) %>%
  mutate(
    log_wage = log(mean_wage + 1)
  ) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'secao'. You can override using the
    ## `.groups` argument.

``` r
knitr::kable(nber30400 %>% head())
```

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

log_wage

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

2.940394

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

2.971672

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

2.978557

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

3.015867

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

3.041700

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

3.095042

</td>
</tr>
</tbody>
</table>

Now, I regress exposure on log_remuneracao_media with plm. This package
provides a series of estimators for panel data, including the Two-Way
Fixed Effects described in Baltagi (2021, p. 47–49), which is used here.

``` r
twfe_reg <- plm(log_wage ~ exposure,
             data = nber30400,
             model = 'within',
             effect = 'twoways',
             index = c('secao','ano'))
```

``` r
default_table(
  twfe_reg,
  c('exposure'='Exposure to computers'),
  robust='stata'
)
```

|                       |                 $1$ |
|-----------------------|--------------------:|
| Exposure to computers |       0.024 (0.079) |
|                       | (p $\approx$ 0.764) |
| Num.Obs.              |                 315 |
| R2                    |               0.000 |
| R2 Adj.               |              -0.125 |
| Std.Errors            |                 HC1 |

The results show that the `Exposure to computers` measure alone does not
explain any variation in the log mean_wage of the brazilian formal
market, both by the non significant result for the variable itself and
by the $R^2$ for the within estimator.

# 3 Second specification - adding occupation classification

As a second step, I’ll modify the model to incorporate the task
compositions of the occupations through the discrete classification of
the predominant skill group of each occupation.

So, the data will now be aggregated by industry sector, year and task
group. With that new aggregation, in order to be able to use the Baltagi
(2021) TWFE, I have to create a new composite factor of industry-group
to serve as my “individual” observation identification.

``` r
data2 <- data

knitr::kable(data2 %>% head())
```

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

Atividades administrativas e serviços complementares

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1081246

</td>
<td style="text-align:right;">

1387.846

</td>
<td style="text-align:right;">

1.254437

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

3416762

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades administrativas e serviços complementares-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Educação

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1009501

</td>
<td style="text-align:right;">

1583.108

</td>
<td style="text-align:right;">

1.431735

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

297674

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Educação-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

RC

</td>
<td style="text-align:right;">

0.1659635

</td>
<td style="text-align:right;">

2310.278

</td>
<td style="text-align:right;">

2.092872

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

914336

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas-RC

</td>
</tr>
<tr>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1909634

</td>
<td style="text-align:right;">

1377.084

</td>
<td style="text-align:right;">

1.244584

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

1243422

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Comércio; reparação de veículos automotores e motocicletas-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Informação e comunicação

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

NRM

</td>
<td style="text-align:right;">

0.1351196

</td>
<td style="text-align:right;">

1987.541

</td>
<td style="text-align:right;">

1.799336

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

102422

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Informação e comunicação-NRM

</td>
</tr>
<tr>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas

</td>
<td style="text-align:left;">

2021

</td>
<td style="text-align:left;">

RM

</td>
<td style="text-align:right;">

0.1659635

</td>
<td style="text-align:right;">

1952.168

</td>
<td style="text-align:right;">

1.767445

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:right;">

300809

</td>
<td style="text-align:right;">

NA

</td>
<td style="text-align:left;">

Atividades profissionais, científicas e técnicas-RM

</td>
</tr>
</tbody>
</table>

With the data in the new shape, I regress the log_wages on the exposure
measure.

``` r
twfe_reg_2 <- plm(log_wage ~ exposure,
             data = data2,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
default_table(
  twfe_reg_2,
  c('exposure'='Exposure to computers'),
  robust='stata'
)
```

|                       |                 $1$ |
|-----------------------|--------------------:|
| Exposure to computers |       0.022 (0.049) |
|                       | (p $\approx$ 0.649) |
| Num.Obs.              |                1260 |
| R2                    |               0.000 |
| R2 Adj.               |              -0.084 |
| Std.Errors            |                 HC1 |

Again, the results show that the `Exposure to computers` measure alone
does not explain any variation in the log mean_wage of the brazilian
formal market, both by the non significant result for the variable
itself and by the $R^2$ for the within estimator. As the result of both
these specifications indicate a non significant value for the
coefficient, at least it seems to be consistent.

# 4 Third specification - Interacting group with exposure

Two last test using the Baltagi (2021) TWFE will be made by interacting
the predominant skill group of the occupations with the exposure to
computers measure, so we can investigate if the exposure to computer has
heterogeneous effects through the different skill groups. To prevent the
correlation between the exposure to computers measure and the RC
occupations of the first period, used to construct the measure, we
exclude the first period data in this evaluation.

``` r
twfe_reg_3 <- plm(log_wage ~ exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
twfe_reg_4 <- plm(log_wage ~ exposure + exposure:group,
             data = data2 %>% filter(ano != '2006'),
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
third_coefs <- c(
  'exposure'='Exposure to computers',
  'exposure:groupNRM'='Exposure to computers $\\times$ NRM',
  'exposure:groupNRC'='Exposure to computers $\\times$ NRC',
  'exposure:groupRC'='Exposure to computers $\\times$ RC',
  'exposure:groupRM'='Exposure to computers $\\times$ RM'
)

default_table(
  list(twfe_reg_3, twfe_reg_4),
  third_coefs,
  robust='stata'
)
```

|                                    |                 $1$ |                 $2$ |
|------------------------------------|--------------------:|--------------------:|
| Exposure to computers $\times$ NRM |      -0.080 (0.065) |                     |
|                                    | (p $\approx$ 0.216) |                     |
| Exposure to computers $\times$ NRC |       0.059 (0.065) |     0.139\* (0.069) |
|                                    | (p $\approx$ 0.359) | (p $\approx$ 0.043) |
| Exposure to computers $\times$ RC  |      0.124+ (0.065) |   0.204\*\* (0.069) |
|                                    | (p $\approx$ 0.054) | (p $\approx$ 0.003) |
| Exposure to computers $\times$ RM  |      -0.104 (0.065) |      -0.025 (0.069) |
|                                    | (p $\approx$ 0.106) | (p $\approx$ 0.721) |
| Exposure to computers              |                     |      -0.080 (0.065) |
|                                    |                     | (p $\approx$ 0.216) |
| Num.Obs.                           |                1176 |                1176 |
| R2                                 |               0.014 |               0.014 |
| R2 Adj.                            |              -0.078 |              -0.078 |
| Std.Errors                         |                 HC1 |                 HC1 |

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

twfe_reg_5 <- plm(log_wage ~ exposure + exposure:group,
             data = data3,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
default_table(
  list(twfe_reg_3, twfe_reg_4, twfe_reg_5),
  third_coefs,
  robust='stata'
)
```

|                                    |                 $1$ |                 $2$ |                 $3$ |
|------------------------------------|--------------------:|--------------------:|--------------------:|
| Exposure to computers $\times$ NRM |      -0.080 (0.065) |                     |    -0.139\* (0.069) |
|                                    | (p $\approx$ 0.216) |                     | (p $\approx$ 0.043) |
| Exposure to computers $\times$ NRC |       0.059 (0.065) |     0.139\* (0.069) |                     |
|                                    | (p $\approx$ 0.359) | (p $\approx$ 0.043) |                     |
| Exposure to computers $\times$ RC  |      0.124+ (0.065) |   0.204\*\* (0.069) |       0.065 (0.069) |
|                                    | (p $\approx$ 0.054) | (p $\approx$ 0.003) | (p $\approx$ 0.342) |
| Exposure to computers $\times$ RM  |      -0.104 (0.065) |      -0.025 (0.069) |    -0.164\* (0.069) |
|                                    | (p $\approx$ 0.106) | (p $\approx$ 0.721) | (p $\approx$ 0.017) |
| Exposure to computers              |                     |      -0.080 (0.065) |       0.059 (0.065) |
|                                    |                     | (p $\approx$ 0.216) | (p $\approx$ 0.359) |
| Num.Obs.                           |                1176 |                1176 |                1176 |
| R2                                 |               0.014 |               0.014 |               0.014 |
| R2 Adj.                            |              -0.078 |              -0.078 |              -0.078 |
| Std.Errors                         |                 HC1 |                 HC1 |                 HC1 |

As expected, in **(3)** the significance of the RC interaction doesn’t
keep significant (by far, with a $0.342$ *p-value*) with the change of
group to be the ground level. Now, the RM and NRM interactions (which
have signs opposed to NRC interaction) show a 5% level significance. But
the results from **(2)** and **(3)** doesn’t appear to be robust enough
to be reported, so I intend to keep just the **(1)** specification on
the final version of the thesis.

# 5 Using group as an explainable variable

Lastly, although initially as fixed effects to evaluate the influence of
exposure to computers on the mean_wage inequality, it could be useful to
evaluate the influence of the skill composition of the occupations with
the mean_wage inequality. It is useful as allows an evaluation of the
capability of task model itself to explain (at least, part of) the
mean_wage inequality on the formal jobs in Brazil.

To do that, I’ll use a One-Way Fixed Effect model (Baltagi, 2021, p.
15–19) on the time variable and include the skill groups of the
occupations as explainable variables. I’ll evaluate one specification
with the skill groups as the only explainable variable, and another
specification with industry sector as explainable variables, to check
for omitted variable bias.

## 5.1 Only skill groups as explainable variables

``` r
fe_reg1 <- plm(log_wage ~ group,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 5.2 Skill groups and industry sectors as explainable variables

``` r
fe_reg2 <- plm(log_wage ~ group + secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 5.3 Results of the skill groups specifications

``` r
library(tibble)
rows <- tribble(~term,          ~`(1)`,  ~`(2)`,
                'Industry FE',  '', 'X',
                'Year FE',  'X', 'X')
attr(rows, 'position') <- c(7, 8)

default_table(
  list(fe_reg1, fe_reg2),
  c(
    'groupNRC'='NRC',
    'groupRC'='RC',
    'groupRM'='RM',
    'groupNRM'='NRM'
  ),
  robust='stata',
  coef_omit='.*secao*.',
  add_rows=rows
)
```

|             |                   $1$ |                   $2$ |
|-------------|----------------------:|----------------------:|
| NRC         |   1.031\*\*\* (0.030) |   1.031\*\*\* (0.013) |
|             | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| RC          |   0.392\*\*\* (0.030) |   0.392\*\*\* (0.013) |
|             | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| RM          |   0.147\*\*\* (0.030) |   0.147\*\*\* (0.013) |
|             | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| Industry FE |                       |                     X |
| Year FE     |                     X |                     X |
| Num.Obs.    |                  1260 |                  1260 |
| R2          |                 0.525 |                 0.916 |
| R2 Adj.     |                 0.519 |                 0.913 |
| Std.Errors  |                   HC1 |                   HC1 |

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

## 5.4 Industry sectors as the only explainable variables

Just to dig a little bit more about omitted variable bias in the
industry sector fixed effects, I’ll regress wages on industry sector
only and compare with the fixed effects in the previous model with
industry sector and skill groups.

``` r
fe_reg3 <- plm(log_wage ~ secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

## 5.5 Results of the industry sector specifications

``` r
default_table(
  list(fe_reg2, fe_reg3),
  c(
    'groupNRC'='NRC',
    'groupRC'='RC',
    'groupRM'='RM',
    'groupNRM'='NRM'
  ),
  robust='stata'
)
```

|                                                                        |                   $1$ |                   $2$ |
|------------------------------------------------------------------------|----------------------:|----------------------:|
| NRC                                                                    |   1.031\*\*\* (0.013) |                       |
|                                                                        | (p $\approx$ \<0.001) |                       |
| RC                                                                     |   0.392\*\*\* (0.013) |                       |
|                                                                        | (p $\approx$ \<0.001) |                       |
| RM                                                                     |   0.147\*\*\* (0.013) |                       |
|                                                                        | (p $\approx$ \<0.001) |                       |
| secaoAgricultura, pecuária, produção florestal, pesca e aqüicultura    |  -0.366\*\*\* (0.029) |  -0.366\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoAlojamento e alimentação                                          |  -0.580\*\*\* (0.029) |  -0.580\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoArtes, cultura, esporte e recreação                               |  -0.257\*\*\* (0.029) |    -0.257\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.001) |
| secaoAtividades administrativas e serviços complementares              |  -0.383\*\*\* (0.029) |  -0.383\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoAtividades financeiras, de seguros e serviços relacionados        |   0.253\*\*\* (0.029) |     0.253\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.001) |
| secaoAtividades imobiliárias                                           |  -0.337\*\*\* (0.029) |  -0.337\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoAtividades profissionais, científicas e técnicas                  |    -0.080\*\* (0.029) |        -0.080 (0.079) |
|                                                                        |   (p $\approx$ 0.007) |   (p $\approx$ 0.312) |
| secaoComércio; reparação de veículos automotores e motocicletas        |  -0.472\*\*\* (0.029) |  -0.472\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoConstrução                                                        |  -0.215\*\*\* (0.029) |    -0.215\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.007) |
| secaoEducação                                                          |   0.180\*\*\* (0.029) |       0.180\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.022) |
| secaoEletricidade e gás                                                |   0.711\*\*\* (0.029) |   0.711\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secao��gua, esgoto, atividades de gestão de resíduos e descontaminação |       0.075\* (0.029) |         0.075 (0.079) |
|                                                                        |   (p $\approx$ 0.010) |   (p $\approx$ 0.339) |
| secaoIndústrias de transformação                                       |  -0.174\*\*\* (0.029) |      -0.174\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.028) |
| secaoIndústrias extrativas                                             |   0.492\*\*\* (0.029) |   0.492\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoInformação e comunicação                                          |         0.022 (0.029) |         0.022 (0.079) |
|                                                                        |   (p $\approx$ 0.449) |   (p $\approx$ 0.778) |
| secaoOrganismos internacionais e outras instituições extraterritoriais |   0.354\*\*\* (0.029) |   0.354\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoOutras atividades de serviços                                     |  -0.222\*\*\* (0.029) |    -0.222\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.005) |
| secaoSaúde humana e serviços sociais                                   |  -0.217\*\*\* (0.029) |    -0.217\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) |   (p $\approx$ 0.006) |
| secaoServiços domésticos                                               |  -0.656\*\*\* (0.029) |  -0.656\*\*\* (0.079) |
|                                                                        | (p $\approx$ \<0.001) | (p $\approx$ \<0.001) |
| secaoTransporte, armazenagem e correio                                 |    -0.076\*\* (0.029) |        -0.076 (0.079) |
|                                                                        |   (p $\approx$ 0.009) |   (p $\approx$ 0.332) |
| Num.Obs.                                                               |                  1260 |                  1260 |
| R2                                                                     |                 0.916 |                 0.390 |
| R2 Adj.                                                                |                 0.913 |                 0.373 |
| Std.Errors                                                             |                   HC1 |                   HC1 |

The main interest in this results is the possibility of bias in the
industry sector coefficients, which didn’t appear, as all the
coefficients kept the same with and without the skill group variable.

# 6 References

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
