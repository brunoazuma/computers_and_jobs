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
dbDisconnect(con, shutdown=TRUE)

data <- data %>%
  mutate(
    group = as.factor(group) %>% relevel('NRM'),
    ano = as.factor(ano)
  )

tictoc::toc()
```

    ## 3.04 sec elapsed

``` r
data %>% head()
```

    ## # A tibble: 6 × 7
    ##   ano   group valor_remuneracao_me…¹ valor_remuneracao_me…² mean_wage job_number
    ##   <fct> <fct>                  <dbl>                  <dbl>     <dbl>      <dbl>
    ## 1 2014  RM                     1547.                   2.13      14.0   24430294
    ## 2 2014  RC                     1890.                   2.60      18.8   25114677
    ## 3 2015  NRC                    3850.                   4.88      37.8   12785606
    ## 4 2016  RM                     1782.                   2.02      13.8   19477034
    ## 5 2016  RC                     2161.                   2.45      18.3   22586175
    ## 6 2017  RC                     2272.                   2.42      18.8   22264853
    ## # ℹ abbreviated names: ¹​valor_remuneracao_media, ²​valor_remuneracao_media_sm
    ## # ℹ 1 more variable: log_wage <dbl>

Now, I’ll add labels to the columns to improve future display.

``` r
var_label(data) <- list(
  ano = "Year",
  group = "Skill classification",
  mean_wage = "Mean hourly wages (at 2021 prices)",
  job_number = "Number of employment relationships",
  log_wage = "Log of mean hourly wages (at 2021 prices)"
)
```

``` r
chart1data <- data %>%
  filter(ano!=2020 & ano!=2021) %>%
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
chart1data %>%
  ggplot( aes(x=ano, y=mean_wage_index, group=1)) +
    geom_line() +
    # scale_color_viridis(discrete = TRUE) +
    ggtitle("Mean wages") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Mean hourly wages (2006=1)") +
    xlab(NULL)
```

![](01_images_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
chart1data %>%
  ggplot( aes(x=ano, y=job_number_index, group=1)) +
    geom_line() +
    # scale_color_viridis(discrete = TRUE) +
    ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
    guides(fill=FALSE) +
    ylab("Number of employment relationships (2006=1)") +
    xlab(NULL)
```

![](01_images_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
chart2data <- data %>%
  filter(ano!=2020 & ano!=2021) %>%
  select(group, ano, mean_wage, job_number) %>%
  group_by(group, ano) %>%
  summarise_all(mean) %>%
  mutate(
    mean_wage_index = mean_wage/first(mean_wage),
    job_number_index = job_number/first(job_number)
  ) %>%
  ungroup()

chart2data
```

    ## # A tibble: 70 × 6
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

``` r
chart2data %>%
  filter(!is.na(group)) %>%
  ggplot( aes(x=ano, y=mean_wage_index, group=group, color=group)) +
    geom_line() +
    # scale_color_viridis(discrete = TRUE) +
    ggtitle("Mean wages by skill group") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
                                                guides(fill=FALSE) +
    ylab("Mean hourly wages (2006=1)") +
    xlab(NULL)
```

![](01_images_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
chart2data %>%
  filter(!is.na(group)) %>%
  ggplot( aes(x=ano, y=job_number_index, group=group, color=group)) +
    geom_line() +
    # scale_color_viridis(discrete = TRUE) +
    ggtitle("Employment relationships") +
        theme_stata() +
    theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
          axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
          legend.title=element_blank(),legend.background = element_rect(colour='white'))+
                                                guides(fill=FALSE) +
    ylab("Number of employment relationships (2006=1)") +
    xlab(NULL)
```

![](01_images_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
chart3data <- data %>%
  filter(ano %in% seq(2006, 2018, 2)) %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(
    delta_job_number = job_number/lag(job_number) - 1,
    interval = paste(lag(ano), ano, sep = '-'),
    mean_wage_2006 = sum(mean_wage*as.integer(ano==2006))
  ) %>%
  ungroup() %>%
  filter(ano != 2006) %>%
  select(interval, group, delta_job_number, mean_wage_2006) %>%
  arrange(mean_wage_2006, interval) %>%
  mutate(
    order=seq(1, 24)
  )

chart3data
```

    ## # A tibble: 24 × 5
    ##    interval  group delta_job_number mean_wage_2006 order
    ##    <chr>     <fct>            <dbl>          <dbl> <int>
    ##  1 2006-2008 NRM            0.228             9.30     1
    ##  2 2008-2010 NRM            0.120             9.30     2
    ##  3 2010-2012 NRM            0.125             9.30     3
    ##  4 2012-2014 NRM            0.0722            9.30     4
    ##  5 2014-2016 NRM           -0.0877            9.30     5
    ##  6 2016-2018 NRM           -0.00185           9.30     6
    ##  7 2006-2008 RM             0.159            11.0      7
    ##  8 2008-2010 RM             0.104            11.0      8
    ##  9 2010-2012 RM             0.0743           11.0      9
    ## 10 2012-2014 RM            -0.0143           11.0     10
    ## # ℹ 14 more rows

``` r
chart3data %>% ggplot(aes(x=reorder(group, order),y=delta_job_number,fill=group,alpha=interval))+
                                            geom_bar(stat='identity',position='dodge')+
                                            scale_alpha_discrete(range=seq(.5,1,.1),labels=paste(unique(chart3data$interval),' '))+theme_stata()+
                                            labs(x='',y='Change (% pts)')+
                                            scale_fill_manual(values=c('#d7191c','#fdae61','#4de678','#2c7bb6'))+
                                            theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
                                                axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
                                                legend.title=element_blank(),legend.background = element_rect(colour='white'))+
                                                guides(fill=FALSE)
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](01_images_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-autor2019" class="csl-entry">

AUTOR, D. H. [Work of the Past, Work of the
Future](https://doi.org/10.1257/pandp.20191110). **AEA Papers and
Proceedings**, v. 109, p. 1–32, maio 2019.

</div>

</div>
