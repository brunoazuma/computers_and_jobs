Images
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
development and saving of the images used in the undergrad thesis. Some
images will be developed on my own, some will be replications of cited
papers. The notebook will follow the structure of the undergrad thesis.

``` r
library("dotenv")
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("duckdb")
```

    ## Carregando pacotes exigidos: DBI

``` r
library("gtsummary")
library("labelled")
library("ggthemes")
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

    ## 2.91 sec elapsed

``` r
data %>% head()
```

    ## # A tibble: 6 × 7
    ##   ano   group valor_remuneracao_me…¹ valor_remuneracao_me…² mean_wage job_number
    ##   <fct> <fct>                  <dbl>                  <dbl>     <dbl>      <dbl>
    ## 1 2007  NRC                    2012.                   5.41      32.4    8561379
    ## 2 2010  NRM                     868.                   1.69      10.4   10636678
    ## 3 2013  NRC                    3328.                   4.90      39.0   12500852
    ## 4 2015  <NA>                   3968.                   5.03      38.9     818147
    ## 5 2018  <NA>                   4949.                   5.18      45.8     778847
    ## 6 2010  <NA>                   2104.                   4.12      27.9     679731
    ## # ℹ abbreviated names: ¹​valor_remuneracao_media, ²​valor_remuneracao_media_sm
    ## # ℹ 1 more variable: log_wage <dbl>

``` r
data %>%
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
  select(group, mean_wage, job_number, job_share) %>%
  knitr::kable()
```

| group | mean_wage | job_number | job_share |
|:------|----------:|-----------:|----------:|
| NRM   |  10.91455 |  153761299 | 0.1659425 |
| NRC   |  36.81395 |  159299358 | 0.1719193 |
| RC    |  17.83618 |  305148754 | 0.3293231 |
| RM    |  13.21500 |  297590697 | 0.3211663 |
| NA    |  40.19270 |   10793748 | 0.0116488 |

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

    ## # A tibble: 14 × 5
    ##    ano   mean_wage job_number mean_wage_index job_number_index
    ##    <fct>     <dbl>      <dbl>           <dbl>            <dbl>
    ##  1 2006       17.1  12477655.            1                1   
    ##  2 2007       17.4  13475527             1.02             1.08
    ##  3 2008       18.0  14734276.            1.05             1.18
    ##  4 2009       19.1  15080324.            1.12             1.21
    ##  5 2010       19.2  16516893.            1.12             1.32
    ##  6 2011       19.9  17565949.            1.16             1.41
    ##  7 2012       20.4  18147939.            1.19             1.45
    ##  8 2013       20.9  18645047.            1.22             1.49
    ##  9 2014       20.8  18814076.            1.21             1.51
    ## 10 2015       20.2  17839239.            1.18             1.43
    ## 11 2016       20.5  16568559.            1.20             1.33
    ## 12 2017       21.2  16202126.            1.24             1.30
    ## 13 2018       21.4  16358961.            1.25             1.31
    ## 14 2019       19.7  16523456.            1.15             1.32

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

![](01_images_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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

![](01_images_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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

    ## # A tibble: 70 × 8
    ##    group ano   mean_wage job_number mean_wage_index job_number_index
    ##    <fct> <fct>     <dbl>      <dbl>           <dbl>            <dbl>
    ##  1 NRM   2006       9.30    7735410            1                1   
    ##  2 NRM   2007       9.53    8229576            1.02             1.06
    ##  3 NRM   2008       9.73    9500534            1.05             1.23
    ##  4 NRM   2009      10.2     9833760            1.10             1.27
    ##  5 NRM   2010      10.4    10636678            1.12             1.38
    ##  6 NRM   2011      10.7    11421806            1.15             1.48
    ##  7 NRM   2012      11.2    11962233            1.20             1.55
    ##  8 NRM   2013      11.5    12480746            1.24             1.61
    ##  9 NRM   2014      11.6    12826292            1.24             1.66
    ## 10 NRM   2015      11.3    12445052            1.21             1.61
    ## # ℹ 60 more rows
    ## # ℹ 2 more variables: mean_wage_2006 <dbl>, order <int>

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

![](01_images_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

![](01_images_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

    ## # A tibble: 20 × 5
    ##    interval  group delta_job_number mean_wage_2006 order
    ##    <chr>     <fct>            <dbl>          <dbl> <int>
    ##  1 2006-2009 NRM             0.271            9.30     1
    ##  2 2009-2012 NRM             0.216            9.30     2
    ##  3 2012-2015 NRM             0.0404           9.30     3
    ##  4 2015-2018 NRM            -0.0614           9.30     4
    ##  5 2006-2019 NRM             0.522            9.30     5
    ##  6 2006-2009 RM              0.158           11.0      6
    ##  7 2009-2012 RM              0.187           11.0      7
    ##  8 2012-2015 RM             -0.110           11.0      8
    ##  9 2015-2018 RM             -0.170           11.0      9
    ## 10 2006-2019 RM              0.0172          11.0     10
    ## 11 2006-2009 RC              0.224           16.2     11
    ## 12 2009-2012 RC              0.203           16.2     12
    ## 13 2012-2015 RC              0.0112          16.2     13
    ## 14 2015-2018 RC             -0.0640          16.2     14
    ## 15 2006-2019 RC              0.409           16.2     15
    ## 16 2006-2009 NRC             0.230           31.9     16
    ## 17 2009-2012 NRC             0.225           31.9     17
    ## 18 2012-2015 NRC             0.0612          31.9     18
    ## 19 2015-2018 NRC             0.0104          31.9     19
    ## 20 2006-2019 NRC             0.653           31.9     20

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

![](01_images_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

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

    ## # A tibble: 6 × 4
    ##   reporter  year `Trade Value 1000USD`   Quantity
    ##   <chr>    <int>                 <dbl>      <int>
    ## 1 China     2006             93017370. 1426071823
    ## 2 China     2007            112243867. 1365466000
    ## 3 China     2008            122727667. 1359163400
    ## 4 China     2009            111890627. 1250304300
    ## 5 China     2010            148802627. 1577944300
    ## 6 China     2011            160121815. 1748174800

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

![](01_images_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

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

![](01_images_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-autor2019" class="csl-entry">

AUTOR, D. H. [Work of the Past, Work of the
Future](https://doi.org/10.1257/pandp.20191110). **AEA Papers and
Proceedings**, v. 109, p. 1–32, maio 2019.

</div>

</div>
