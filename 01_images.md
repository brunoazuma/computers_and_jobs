Images
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
development and saving of the images used in the undergrad thesis. Some
images will be developed on my own, some will be replications of cited
papers. The notebook will follow the structure of the undergrad thesis.

``` r
library("dotenv")
library("scales")
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::col_factor() masks scales::col_factor()
    ## ✖ purrr::discard()    masks scales::discard()
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("duckdb")
```

    ## Carregando pacotes exigidos: DBI

``` r
library("gtsummary")
```

    ## #BlackLivesMatter

``` r
library("labelled")
library("ggthemes")
library("knitr")
```

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs")
load_dot_env()
base_dir <- "D:/OneDrive/R Workspace/computers_and_jobs"
fig_dir <- str_interp("${base_dir}/fig")
```

# Introduction

The first image used in the introduction section is a reproduction of
Autor (2019).

**Changes in Occupational Employment Shares, 1970-2016**

*Working Age Adults (Percent Change Over Decade)*

``` r
# Figure 3
plot_occ_group_pct_changes(df,subgroup='overall',subgroup_title='Working Age Adults',
                                        intervals=c('1970-1980','1980-1990','1990-2000','2000-2016'))
```

![](01_images_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Descriptive statistics

## RAIS and skill classification

First, I’ll describe the outcome variables (mean wages and number of
jobs) in general and segmented by skill type of the occupation, both for
the first period and on average for all periods.

``` r
datadir <- Sys.getenv("DATA_DIR")
file <- str_interp('${datadir}Final_db.parquet')
temp_db_dir <- Sys.getenv("DUCK_DB_DIR")
temp_db_mem <- Sys.getenv("DUCK_DB_MEM")

con <- dbConnect(duckdb(), dbdir = temp_db_dir)
dbExecute(conn = con, str_interp("PRAGMA memory_limit='${temp_db_mem}'"))
```

    ## [1] 0

``` r
tictoc::tic()

data <- tbl(con, file) %>%
  filter(ano < 2020) %>%
  group_by(
    ano,
    group
  ) %>%
  summarise(
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  ungroup() %>%
  mutate(
    log_wage = log(mean_wage + 1)
  ) %>%
  collect()
```

    ## `summarise()` has grouped output by "ano". You can override using the `.groups`
    ## argument.

    ## Warning: Missing values are always removed in SQL aggregation functions.
    ## Use `na.rm = TRUE` to silence this warning
    ## This warning is displayed once every 8 hours.

``` r
data <- data %>%
  mutate(
    group = as.factor(group) %>% relevel('NRM'),
    ano = as.factor(ano)
  )

tictoc::toc()
```

    ## 2.86 sec elapsed

``` r
data %>% head()
```

<div class="kable-table">

| ano  | group | valor_remuneracao_media | valor_remuneracao_media_sm | mean_wage | job_number | log_wage |
|:-----|:------|------------------------:|---------------------------:|----------:|-----------:|---------:|
| 2012 | NRC   |                3055.424 |                   4.904207 |  38.30066 |   12048770 | 3.671241 |
| 2014 | NRC   |                3551.861 |                   4.897767 |  38.67342 |   12885043 | 3.680681 |
| 2015 | RM    |                1663.083 |                   2.102697 |  13.69034 |   22064091 | 2.687190 |
| 2015 | RC    |                2021.336 |                   2.557159 |  18.20880 |   24062206 | 2.955369 |
| 2016 | NRC   |                4131.794 |                   4.687130 |  38.30202 |   12508961 | 3.671276 |
| 2017 | NRC   |                4376.438 |                   4.662584 |  39.51470 |   12622474 | 3.701665 |

</div>

``` r
data %>%
  filter(ano==2006) %>%
  group_by(
    group
  ) %>%
  summarise(
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  mutate(
    job_share=job_number/sum(job_number)
  ) %>%
  ungroup() %>%
  mutate(
    mean_wage=label_dollar(prefix = 'R$ ')(mean_wage),
    job_number=label_number(big.mark = ',')(job_number),
    job_share=label_percent(accuracy=.01)(job_share)
  ) %>%
  select(group, mean_wage, job_number, job_share) %>%
  kable(
    col.names = c(
      'Skill group',
      'Mean hourly wage',
      'Number of emp. relationships',
      'Share of emp. relationships'
    ),
    caption = "Summary of wages and employment relationships for skill groups, 2006",
  align = "llrr"
  )
```

| Skill group | Mean hourly wage | Number of emp. relationships | Share of emp. relationships |
|:------------|:-----------------|-----------------------------:|----------------------------:|
| NRM         | R\$ 9.30         |                    7,735,410 |                      15.26% |
| NRC         | R\$ 31.95        |                    7,995,661 |                      15.77% |
| RC          | R\$ 16.19        |                   16,154,345 |                      31.86% |
| RM          | R\$ 11.04        |                   18,025,205 |                      35.55% |
| NA          | R\$ 31.09        |                      790,399 |                       1.56% |

Summary of wages and employment relationships for skill groups, 2006

``` r
data %>%
  filter(ano==2006) %>%
  summarise(
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  mutate(
    group='All groups',
    job_share=job_number/sum(job_number),
    mean_wage=label_dollar(prefix = 'R$ ')(mean_wage),
    job_number=label_number(big.mark = ',')(job_number),
    job_share=label_percent(accuracy=.01)(job_share)
  ) %>%
  select(group, mean_wage, job_number, job_share) %>%
  kable(
    col.names = c(
      'Skill group',
      'Mean hourly wage',
      'Number of emp. relationships',
      'Share of emp. relationships'
    ),
    caption = "Summary of wages and employment relationships, 2006",
  align = "llrr"
  )
```

| Skill group | Mean hourly wage | Number of emp. relationships | Share of emp. relationships |
|:------------|:-----------------|-----------------------------:|----------------------------:|
| All groups  | R\$ 19.91        |                   50,701,020 |                     100.00% |

Summary of wages and employment relationships, 2006

``` r
data %>%
  filter(ano==2019) %>%
  group_by(
    group
  ) %>%
  summarise(
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  mutate(
    job_share=job_number/sum(job_number)
  ) %>%
  ungroup() %>%
  mutate(
    mean_wage=label_dollar(prefix = 'R$ ')(mean_wage),
    job_number=label_number(big.mark=',')(job_number),
    job_share=label_percent(accuracy=.01)(job_share)
  ) %>%
  select(group, mean_wage, job_number, job_share) %>%
  kable(
    col.names = c(
      'Skill group',
      'Mean hourly wage',
      'Number of emp. relationships',
      'Share of emp. relationships'
    ),
    caption = "Summary of wages and employment relationships for skill groups, 2019",
  align = "llrr"
  )
```

| Skill group | Mean hourly wage | Number of emp. relationships | Share of emp. relationships |
|:------------|:-----------------|-----------------------------:|----------------------------:|
| NRM         | R\$ 11.65        |                   11,774,724 |                      17.66% |
| NRC         | R\$ 35.95        |                   13,219,736 |                      19.83% |
| RC          | R\$ 17.32        |                   22,764,576 |                      34.15% |
| RM          | R\$ 13.82        |                   18,334,787 |                      27.50% |
| NA          | R\$ 54.78        |                      573,594 |                       0.86% |

Summary of wages and employment relationships for skill groups, 2019

``` r
data %>%
  filter(ano==2019) %>%
  summarise(
    valor_remuneracao_media=mean(valor_remuneracao_media),
    valor_remuneracao_media_sm=mean(valor_remuneracao_media_sm),
    mean_wage=mean(mean_wage),
    job_number=sum(job_number)
  ) %>%
  mutate(
    group='All groups',
    job_share=job_number/sum(job_number),
    mean_wage=label_dollar(prefix = 'R$ ')(mean_wage),
    job_number=label_number(big.mark = ',')(job_number),
    job_share=label_percent(accuracy=.01)(job_share)
  ) %>%
  select(group, mean_wage, job_number, job_share) %>%
  kable(
    col.names = c(
      'Skill group',
      'Mean hourly wage',
      'Number of emp. relationships',
      'Share of emp. relationships'
    ),
    caption = "Summary of wages and employment relationships, 2019",
  align = "llrr"
  )
```

| Skill group | Mean hourly wage | Number of emp. relationships | Share of emp. relationships |
|:------------|:-----------------|-----------------------------:|----------------------------:|
| All groups  | R\$ 26.70        |                   66,667,417 |                     100.00% |

Summary of wages and employment relationships, 2019

``` r
chart1data <- data %>%
  filter(!is.na(group)) %>%
  select(ano, mean_wage, job_number) %>%
  group_by(ano) %>%
  summarise_all(mean) %>%
  mutate(
    mean_wage_index = mean_wage/first(mean_wage),
    job_number_index = job_number/first(job_number)
  ) %>%
  ungroup()

chart1data
```

<div class="kable-table">

| ano  | mean_wage | job_number | mean_wage_index | job_number_index |
|:-----|----------:|-----------:|----------------:|-----------------:|
| 2006 |  17.11949 |   12477655 |        1.000000 |         1.000000 |
| 2007 |  17.43213 |   13475527 |        1.018262 |         1.079973 |
| 2008 |  18.02922 |   14734277 |        1.053140 |         1.180853 |
| 2009 |  19.14512 |   15080324 |        1.118323 |         1.208586 |
| 2010 |  19.18109 |   16516893 |        1.120424 |         1.323718 |
| 2011 |  19.85267 |   17565949 |        1.159653 |         1.407793 |
| 2012 |  20.37739 |   18147939 |        1.190303 |         1.454435 |
| 2013 |  20.86985 |   18645047 |        1.219070 |         1.494275 |
| 2014 |  20.76853 |   18814077 |        1.213151 |         1.507821 |
| 2015 |  20.23677 |   17839239 |        1.182089 |         1.429695 |
| 2016 |  20.50479 |   16568559 |        1.197745 |         1.327858 |
| 2017 |  21.15457 |   16202126 |        1.235701 |         1.298491 |
| 2018 |  21.37083 |   16358961 |        1.248333 |         1.311060 |
| 2019 |  19.68642 |   16523456 |        1.149942 |         1.324244 |

</div>

``` r
fig41<- chart1data %>%
  ggplot( aes(x=ano, y=mean_wage_index, group=1)) +
    geom_line() +
    # ggtitle("Mean wages") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Mean hourly wages (2006=1)") +
    xlab(NULL)

ggsave(str_interp("${fig_dir}/fig-4-1.png"),width=8,height=5)

fig41
```

![](01_images_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
fig42 <- chart1data %>%
  ggplot( aes(x=ano, y=job_number_index, group=1)) +
    geom_line() +
    # ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Number of employment relationships (2006=1)") +
    xlab(NULL)

ggsave(str_interp("${fig_dir}/fig-4-2.png"),width=8,height=5)

fig42
```

![](01_images_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
chart2data <- data %>%
  select(group, ano, mean_wage, job_number) %>%
  group_by(group, ano) %>%
  summarise_all(mean) %>%
  mutate(
    mean_wage_index = mean_wage/first(mean_wage),
    job_number_index = job_number/first(job_number)
  ) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(
    mean_wage_2006 = sum(mean_wage*as.integer(ano==2006))
  ) %>%
  ungroup() %>%
  arrange(mean_wage_2006, ano) %>%
  mutate(
    order=seq(1, n())
  )

chart2data
```

<div class="kable-table">

| group | ano  | mean_wage | job_number | mean_wage_index | job_number_index | mean_wage_2006 | order |
|:------|:-----|----------:|-----------:|----------------:|-----------------:|---------------:|------:|
| NRM   | 2006 |  9.302970 |    7735410 |       1.0000000 |        1.0000000 |        9.30297 |     1 |
| NRM   | 2007 |  9.531405 |    8229576 |       1.0245551 |        1.0638836 |        9.30297 |     2 |
| NRM   | 2008 |  9.725977 |    9500534 |       1.0454701 |        1.2281875 |        9.30297 |     3 |
| NRM   | 2009 | 10.228528 |    9833760 |       1.0994907 |        1.2712655 |        9.30297 |     4 |
| NRM   | 2010 | 10.416894 |   10636678 |       1.1197386 |        1.3750632 |        9.30297 |     5 |
| NRM   | 2011 | 10.703247 |   11421806 |       1.1505194 |        1.4765612 |        9.30297 |     6 |
| NRM   | 2012 | 11.198855 |   11962233 |       1.2037935 |        1.5464252 |        9.30297 |     7 |
| NRM   | 2013 | 11.540082 |   12480746 |       1.2404729 |        1.6134563 |        9.30297 |     8 |
| NRM   | 2014 | 11.575727 |   12826292 |       1.2443045 |        1.6581270 |        9.30297 |     9 |
| NRM   | 2015 | 11.250450 |   12445052 |       1.2093396 |        1.6088419 |        9.30297 |    10 |
| NRM   | 2016 | 11.587752 |   11702065 |       1.2455971 |        1.5127918 |        9.30297 |    11 |
| NRM   | 2017 | 11.995247 |   11532051 |       1.2893998 |        1.4908132 |        9.30297 |    12 |
| NRM   | 2018 | 12.096800 |   11680372 |       1.3003160 |        1.5099874 |        9.30297 |    13 |
| NRM   | 2019 | 11.649699 |   11774724 |       1.2522560 |        1.5221849 |        9.30297 |    14 |
| RM    | 2006 | 11.039408 |   18025205 |       1.0000000 |        1.0000000 |       11.03941 |    15 |
| RM    | 2007 | 11.253693 |   19576914 |       1.0194109 |        1.0860855 |       11.03941 |    16 |
| RM    | 2008 | 12.079557 |   20894541 |       1.0942214 |        1.1591847 |       11.03941 |    17 |
| RM    | 2009 | 12.754076 |   20878207 |       1.1553225 |        1.1582785 |       11.03941 |    18 |
| RM    | 2010 | 12.789535 |   23070562 |       1.1585345 |        1.2799057 |       11.03941 |    19 |
| RM    | 2011 | 12.889281 |   24417277 |       1.1675699 |        1.3546185 |       11.03941 |    20 |
| RM    | 2012 | 13.527032 |   24783889 |       1.2253404 |        1.3749574 |       11.03941 |    21 |
| RM    | 2013 | 14.059050 |   24934981 |       1.2735330 |        1.3833397 |       11.03941 |    22 |
| RM    | 2014 | 14.032017 |   24430294 |       1.2710842 |        1.3553407 |       11.03941 |    23 |
| RM    | 2015 | 13.690340 |   22064091 |       1.2401335 |        1.2240688 |       11.03941 |    24 |
| RM    | 2016 | 13.805582 |   19477034 |       1.2505727 |        1.0805444 |       11.03941 |    25 |
| RM    | 2017 | 14.345242 |   18389124 |       1.2994576 |        1.0201895 |       11.03941 |    26 |
| RM    | 2018 | 14.920426 |   18313791 |       1.3515604 |        1.0160101 |       11.03941 |    27 |
| RM    | 2019 | 13.824701 |   18334787 |       1.2523046 |        1.0171750 |       11.03941 |    28 |
| RC    | 2006 | 16.189075 |   16154345 |       1.0000000 |        1.0000000 |       16.18908 |    29 |
| RC    | 2007 | 16.503412 |   17534239 |       1.0194166 |        1.0854194 |       16.18908 |    30 |
| RC    | 2008 | 16.743275 |   19260879 |       1.0342330 |        1.1923033 |       16.18908 |    31 |
| RC    | 2009 | 17.633542 |   19775623 |       1.0892248 |        1.2241674 |       16.18908 |    32 |
| RC    | 2010 | 17.340545 |   21672574 |       1.0711264 |        1.3415941 |       16.18908 |    33 |
| RC    | 2011 | 17.964392 |   22974952 |       1.1096614 |        1.4222150 |       16.18908 |    34 |
| RC    | 2012 | 18.482994 |   23796865 |       1.1416954 |        1.4730938 |       16.18908 |    35 |
| RC    | 2013 | 18.854056 |   24663608 |       1.1646160 |        1.5267476 |       16.18908 |    36 |
| RC    | 2014 | 18.792941 |   25114677 |       1.1608409 |        1.5546701 |       16.18908 |    37 |
| RC    | 2015 | 18.208802 |   24062206 |       1.1247586 |        1.4895191 |       16.18908 |    38 |
| RC    | 2016 | 18.323793 |   22586175 |       1.1318616 |        1.3981486 |       16.18908 |    39 |
| RC    | 2017 | 18.763105 |   22264853 |       1.1589979 |        1.3782579 |       16.18908 |    40 |
| RC    | 2018 | 18.582734 |   22523182 |       1.1478564 |        1.3942492 |       16.18908 |    41 |
| RC    | 2019 | 17.323905 |   22764576 |       1.0700985 |        1.4091921 |       16.18908 |    42 |
| NA    | 2006 | 31.088394 |     790399 |       1.0000000 |        1.0000000 |       31.08839 |    43 |
| NA    | 2007 | 32.521603 |     747021 |       1.0461011 |        0.9451189 |       31.08839 |    44 |
| NA    | 2008 | 42.607172 |     769313 |       1.3705170 |        0.9733223 |       31.08839 |    45 |
| NA    | 2009 | 45.233888 |     805601 |       1.4550089 |        1.0192333 |       31.08839 |    46 |
| NA    | 2010 | 27.943220 |     679731 |       0.8988313 |        0.8599846 |       31.08839 |    47 |
| NA    | 2011 | 56.887366 |     707328 |       1.8298586 |        0.8948999 |       31.08839 |    48 |
| NA    | 2012 | 33.482319 |     734728 |       1.0770038 |        0.9295660 |       31.08839 |    49 |
| NA    | 2013 | 35.006697 |     820323 |       1.1260375 |        1.0378594 |       31.08839 |    50 |
| NA    | 2014 | 38.742100 |     850973 |       1.2461917 |        1.0766372 |       31.08839 |    51 |
| NA    | 2015 | 38.925686 |     818147 |       1.2520970 |        1.0351063 |       31.08839 |    52 |
| NA    | 2016 | 39.426647 |     870363 |       1.2682111 |        1.1011692 |       31.08839 |    53 |
| NA    | 2017 | 40.303385 |     847380 |       1.2964126 |        1.0720914 |       31.08839 |    54 |
| NA    | 2018 | 45.751534 |     778847 |       1.4716596 |        0.9853846 |       31.08839 |    55 |
| NA    | 2019 | 54.777778 |     573594 |       1.7620009 |        0.7257018 |       31.08839 |    56 |
| NRC   | 2006 | 31.946513 |    7995661 |       1.0000000 |        1.0000000 |       31.94651 |    57 |
| NRC   | 2007 | 32.440004 |    8561379 |       1.0154474 |        1.0707531 |       31.94651 |    58 |
| NRC   | 2008 | 33.568055 |    9281152 |       1.0507580 |        1.1607736 |       31.94651 |    59 |
| NRC   | 2009 | 35.964326 |    9833705 |       1.1257669 |        1.2298802 |       31.94651 |    60 |
| NRC   | 2010 | 36.177404 |   10687757 |       1.1324367 |        1.3366946 |       31.94651 |    61 |
| NRC   | 2011 | 37.853744 |   11449762 |       1.1849101 |        1.4319969 |       31.94651 |    62 |
| NRC   | 2012 | 38.300665 |   12048770 |       1.1988997 |        1.5069136 |       31.94651 |    63 |
| NRC   | 2013 | 39.026219 |   12500852 |       1.2216112 |        1.5634545 |       31.94651 |    64 |
| NRC   | 2014 | 38.673423 |   12885043 |       1.2105679 |        1.6115044 |       31.94651 |    65 |
| NRC   | 2015 | 37.797469 |   12785606 |       1.1831485 |        1.5990680 |       31.94651 |    66 |
| NRC   | 2016 | 38.302025 |   12508961 |       1.1989423 |        1.5644687 |       31.94651 |    67 |
| NRC   | 2017 | 39.514705 |   12622474 |       1.2369020 |        1.5786655 |       31.94651 |    68 |
| NRC   | 2018 | 39.883379 |   12918500 |       1.2484423 |        1.6156888 |       31.94651 |    69 |
| NRC   | 2019 | 35.947379 |   13219736 |       1.1252364 |        1.6533637 |       31.94651 |    70 |

</div>

``` r
fig43 <- chart2data %>%
  filter(!is.na(group)) %>%
  ggplot( aes(x=ano, y=mean_wage_index, group=group, color=reorder(group, order))) +
    geom_line(size=1) +
    scale_color_manual(values=c('#d7191c','#2c7bb6','#4de678','#fdae61'))+
    # ggtitle("Mean wages by skill group") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
                                                guides(fill=FALSE) +
    ylab("Mean hourly wages (2006=1)") +
    xlab(NULL)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
ggsave(str_interp("${fig_dir}/fig-4-3.png"),width=8,height=5)

fig43
```

![](01_images_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
fig44 <- chart2data %>%
  filter(!is.na(group)) %>%
  ggplot(
    aes(x=ano, y=job_number_index, group=group, color=reorder(group, order))
  ) +
    geom_line(size=1) +
    scale_color_manual(values=c('#d7191c','#2c7bb6','#4de678','#fdae61'))+
    # scale_color_viridis(discrete = TRUE) +
    # ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
      axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
      legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Number of employment relationships (2006=1)") +
    xlab(NULL)


ggsave(str_interp("${fig_dir}/fig-4-4.png"),width=8,height=5)

fig44
```

![](01_images_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
chart3data <- data %>%
  filter(ano %in% cbind(seq(2006, 2018, 3), 2019)) %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(
    delta_job_number = case_when(
      ano == 2019 ~ job_number/first(job_number) - 1,
      .default = job_number/lag(job_number) - 1
    ),
    interval = case_when(
      ano == 2019 ~ paste(first(ano), ano, sep = '-'),
      .default = paste(lag(ano), ano, sep = '-')
    ),
    mean_wage_2006 = sum(mean_wage*as.integer(ano==2006))
  ) %>%
  ungroup() %>%
  filter(ano != 2006) %>%
  arrange(mean_wage_2006, ano) %>%
  select(interval, group, delta_job_number, mean_wage_2006) %>%
  mutate(
    order=seq(1, n())
  )

chart3data
```

<div class="kable-table">

| interval  | group | delta_job_number | mean_wage_2006 | order |
|:----------|:------|-----------------:|---------------:|------:|
| 2006-2009 | NRM   |        0.2712655 |        9.30297 |     1 |
| 2009-2012 | NRM   |        0.2164455 |        9.30297 |     2 |
| 2012-2015 | NRM   |        0.0403619 |        9.30297 |     3 |
| 2015-2018 | NRM   |       -0.0614445 |        9.30297 |     4 |
| 2006-2019 | NRM   |        0.5221849 |        9.30297 |     5 |
| 2006-2009 | RM    |        0.1582785 |       11.03941 |     6 |
| 2009-2012 | RM    |        0.1870698 |       11.03941 |     7 |
| 2012-2015 | RM    |       -0.1097406 |       11.03941 |     8 |
| 2015-2018 | RM    |       -0.1699730 |       11.03941 |     9 |
| 2006-2019 | RM    |        0.0171750 |       11.03941 |    10 |
| 2006-2009 | RC    |        0.2241674 |       16.18908 |    11 |
| 2009-2012 | RC    |        0.2033434 |       16.18908 |    12 |
| 2012-2015 | RC    |        0.0111503 |       16.18908 |    13 |
| 2015-2018 | RC    |       -0.0639602 |       16.18908 |    14 |
| 2006-2019 | RC    |        0.4091921 |       16.18908 |    15 |
| 2006-2009 | NRC   |        0.2298802 |       31.94651 |    16 |
| 2009-2012 | NRC   |        0.2252523 |       31.94651 |    17 |
| 2012-2015 | NRC   |        0.0611545 |       31.94651 |    18 |
| 2015-2018 | NRC   |        0.0103940 |       31.94651 |    19 |
| 2006-2019 | NRC   |        0.6533637 |       31.94651 |    20 |

</div>

``` r
fig45 <- chart3data %>% ggplot(aes(x=reorder(group, order),y=delta_job_number,fill=group,alpha=interval))+
                                            geom_bar(stat='identity',position='dodge')+
                                            scale_alpha_discrete(range=seq(.4,1,.2),labels=paste(unique(chart3data$interval),' '))+theme_stata()+
                                            labs(x='',y='Change (% pts)')+
                                            scale_fill_manual(values=c('#d7191c','#fdae61','#4de678','#2c7bb6'))+
                                            theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
                                                axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
                                                legend.title=element_blank(),legend.background = element_rect(colour='white'))+
                                                guides(fill=FALSE)
```

    ## Warning: Using alpha for a discrete variable is not advised.

``` r
ggsave(str_interp("${fig_dir}/fig-4-5.png"),width=8,height=5)

fig45
```

![](01_images_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Export data

``` r
file <- str_interp('${datadir}/WITS/WITS_trades.parquet')
wits <- tbl(con, file) %>%
  filter(year < 2020) %>%
  collect()

dbDisconnect(con, shutdown=TRUE)

wits %>%
  head()
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

``` r
fig46 <- wits %>%
  mutate (
    trade_value = `Trade Value 1000USD`/10^6
  ) %>%
  ggplot(
    aes(x=year, y=trade_value, group=reporter, color=reporter)
  ) +
    geom_line(size=1) +
    scale_color_manual(values=c('#d7191c','#2c7bb6','#4de678','#fdae61', '#0ddead'))+
    scale_y_continuous(trans='log10') +
    scale_x_continuous(breaks = seq(2006,2019)) +
    # ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
      axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
      legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Trade Value (billions of US dollars)") +
    xlab(NULL)


ggsave(str_interp("${fig_dir}/fig-4-6.png"),width=8,height=5)

fig46
```

![](01_images_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
fig47 <- wits %>%
  mutate (
    quantity = Quantity/10^6
  ) %>%
  ggplot(
    aes(x=year, y=quantity, group=reporter, color=reporter)
  ) +
    geom_line(size=1) +
    scale_color_manual(values=c('#d7191c','#2c7bb6','#4de678','#fdae61', '#0ddead'))+
    scale_y_continuous(trans='log10') +
    scale_x_continuous(breaks = seq(2006,2019)) +
    # ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
      axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
      legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Quantity (millions of items)") +
    xlab(NULL)


ggsave(str_interp("${fig_dir}/fig-4-7.png"),width=8,height=5)

fig47
```

![](01_images_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-autor2019" class="csl-entry">

AUTOR, D. H. [Work of the Past, Work of the
Future](https://doi.org/10.1257/pandp.20191110). **AEA Papers and
Proceedings**, v. 109, p. 1–32, maio 2019.

</div>

</div>
