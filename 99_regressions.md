Computers and jobs - Regressions
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

``` r
setwd("D:/OneDrive/R Workspace/computers_and_jobs")
# install.packages(\"dotenv\")
library("dotenv")
# install.packages('arrow')
library(arrow, warn.conflicts = FALSE)
# install.packages("basedosdados")
library("basedosdados")
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
if(!require(gtsummary)){install.packages('gtsummary');require(gtsummary)}
```

    ## Carregando pacotes exigidos: gtsummary

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
datadir <- 'data/'
file <- str_interp('${datadir}RAIS_AGG.parquet')
```

``` r
rais <- read_parquet(file)
```

``` r
file <- str_interp('${datadir}/WITS/WITS_trades.parquet')
wits <- read_parquet(file)
```

``` r
export_share <- wits %>%
  group_by(year) %>%
  summarise(
    quantity=sum(Quantity)
  ) %>%
  ungroup %>%
  mutate(
    x_share = quantity/first(quantity)
  )
```

``` r
rc_share <- rais %>%
  filter(ano==2006 & !is.na(group)) %>%
  mutate(
    rc_job_number=(group=="RC")*job_number
  ) %>%
  group_by(secao) %>%
  summarise(
    rc_job_number=sum(rc_job_number),
    job_number=sum(job_number),
    rc_share=rc_job_number/job_number
  ) %>%
  ungroup()

rc_share
```

    ## # A tibble: 21 × 4
    ##    secao rc_job_number job_number rc_share
    ##    <chr>         <dbl>      <dbl>    <dbl>
    ##  1 A            101175    2855877   0.0354
    ##  2 B             31705     231261   0.137 
    ##  3 C           1317618    9014641   0.146 
    ##  4 D             32369     125342   0.258 
    ##  5 E             50313     328794   0.153 
    ##  6 F            226052    2920930   0.0774
    ##  7 G           5635236    9573324   0.589 
    ##  8 H            501628    2220842   0.226 
    ##  9 I            299356    1790468   0.167 
    ## 10 J            384519     923209   0.417 
    ## # ℹ 11 more rows

``` r
rais <- rais %>%
  left_join(
    rc_share %>% select(secao, rc_share),
    by=join_by(secao==secao)
  ) %>%
  left_join(
    export_share %>% select(year, x_share),
    by=join_by(ano==year)) %>%
  mutate(
    exposure=rc_share*x_share
  )
gc()
```

    ##             used   (Mb) gc trigger    (Mb)   max used    (Mb)
    ## Ncells   1784506   95.4    3521861   188.1    2095607   112.0
    ## Vcells 663190697 5059.8 1837710942 14020.7 1612217828 12300.3

``` r
rais
```

    ## # A tibble: 41,261,463 × 16
    ##      ano sigla_uf secao descricao_secao       group grau_instrucao_apos_…¹ sexo 
    ##    <dbl> <chr>    <chr> <chr>                 <chr> <chr>                  <chr>
    ##  1  2006 AM       C     Indústrias de transf… RC    7                      1    
    ##  2  2006 AM       C     Indústrias de transf… RM    7                      1    
    ##  3  2006 AM       G     Comércio; reparação … RC    6                      2    
    ##  4  2006 AM       F     Construção            RM    2                      1    
    ##  5  2006 AM       M     Atividades profissio… NRM   6                      2    
    ##  6  2006 AM       G     Comércio; reparação … RC    7                      2    
    ##  7  2006 AM       P     Educação              NRC   8                      1    
    ##  8  2006 AM       H     Transporte, armazena… RC    7                      1    
    ##  9  2006 AM       H     Transporte, armazena… RM    6                      1    
    ## 10  2006 AM       C     Indústrias de transf… RM    7                      2    
    ## # ℹ 41,261,453 more rows
    ## # ℹ abbreviated name: ¹​grau_instrucao_apos_2005
    ## # ℹ 9 more variables: raca_cor <chr>, idade <dbl>,
    ## #   valor_remuneracao_media <dbl>, valor_remuneracao_media_sm <dbl>,
    ## #   mean_wage <dbl>, job_number <dbl>, rc_share <dbl>, x_share <dbl>,
    ## #   exposure <dbl>

``` r
rais <- rais %>%
  mutate(
    log_remuneracao_media = log(valor_remuneracao_media + 1),
    sigla_uf = as.factor(sigla_uf),
    cd_secao = secao,
    secao = as.factor(descricao_secao),
    group = as.factor(group),
    grau_instrucao_apos_2005 = as.factor(grau_instrucao_apos_2005),
    sexo = as.factor(sexo),
    raca_cor = as.factor(raca_cor),
    ano = as.factor(ano)
  )
gc()
```

    ##             used   (Mb) gc trigger    (Mb)   max used    (Mb)
    ## Ncells   1786157   95.4    3521861   188.1    2095607   112.0
    ## Vcells 601302447 4587.6 1837710942 14020.7 1793296247 13681.8

``` r
rais
```

    ## # A tibble: 41,261,463 × 18
    ##    ano   sigla_uf secao       descricao_secao group grau_instrucao_apos_…¹ sexo 
    ##    <fct> <fct>    <fct>       <chr>           <fct> <fct>                  <fct>
    ##  1 2006  AM       Indústrias… Indústrias de … RC    7                      1    
    ##  2 2006  AM       Indústrias… Indústrias de … RM    7                      1    
    ##  3 2006  AM       Comércio; … Comércio; repa… RC    6                      2    
    ##  4 2006  AM       Construção  Construção      RM    2                      1    
    ##  5 2006  AM       Atividades… Atividades pro… NRM   6                      2    
    ##  6 2006  AM       Comércio; … Comércio; repa… RC    7                      2    
    ##  7 2006  AM       Educação    Educação        NRC   8                      1    
    ##  8 2006  AM       Transporte… Transporte, ar… RC    7                      1    
    ##  9 2006  AM       Transporte… Transporte, ar… RM    6                      1    
    ## 10 2006  AM       Indústrias… Indústrias de … RM    7                      2    
    ## # ℹ 41,261,453 more rows
    ## # ℹ abbreviated name: ¹​grau_instrucao_apos_2005
    ## # ℹ 11 more variables: raca_cor <fct>, idade <dbl>,
    ## #   valor_remuneracao_media <dbl>, valor_remuneracao_media_sm <dbl>,
    ## #   mean_wage <dbl>, job_number <dbl>, rc_share <dbl>, x_share <dbl>,
    ## #   exposure <dbl>, log_remuneracao_media <dbl>, cd_secao <chr>

``` r
tbl_padrao <- function(model, include, label, robust=NULL) {
  tb <- tbl_regression(
    model,
    include = include,
    label = label,
    intercept=TRUE,
    estimate_fun = function(x) style_number(x, digits = 4)
    ) 
  if(!is.null(robust)) {
    tb <- tbl_regression(
      model,
      include = include,
      label = label,
      intercept=TRUE,
      estimate_fun = function(x) style_number(x, digits = 4),
      tidy_fun = partial(tidy_robust, vcov = robust)
      )
  }
  
    tb <- tb %>%
    bold_labels() %>%
    add_significance_stars(
      hide_se = TRUE,
      hide_p=FALSE,
      pattern = '{estimate}{stars} \n({std.error})'
    ) %>%
    # modify_header(
    #   estimate ~ '**Coef. \n(EP)**',
    #   p.value ~ 'p-valor'
    #   ) %>%
    # modify_footnote(estimate ~ "EP = Erro padrão", abbreviation = TRUE) %>%
    add_glance_table(
      include=c(
        'r.squared',
        'adj.r.squared',
        'nobs'
      )
    )

  return(tb)
}
```

# First specification - reproduction of (**nber30400?**)

Primeiro, tentarei reproduzir a mesma análise de (**nber30400?**), sem
levar em conta o tipo de tarefa predominante por ocupação. Para isso,
preciso primeiro agregar os dados.

``` r
nber30400 <- rais %>%
  group_by(secao, ano) %>%
  summarise(
    valor_remuneracao_media = mean(valor_remuneracao_media),
    exposure = mean(exposure)
  ) %>%
  mutate(
    log_remuneracao_media = log(valor_remuneracao_media)
  ) %>%
  ungroup() %>%
  filter(!is.na(secao) & !is.na(ano))
```

    ## `summarise()` has grouped output by 'secao'. You can override using the
    ## `.groups` argument.

``` r
nber30400
```

    ## # A tibble: 336 × 5
    ##    secao             ano   valor_remuneracao_me…¹ exposure log_remuneracao_media
    ##    <fct>             <fct>                  <dbl>    <dbl>                 <dbl>
    ##  1 Administração pú… 2006                   1118.    0.395                  7.02
    ##  2 Administração pú… 2007                   1211.    0.335                  7.10
    ##  3 Administração pú… 2008                   1320.    0.324                  7.19
    ##  4 Administração pú… 2009                   1425.    0.301                  7.26
    ##  5 Administração pú… 2010                   1529.    0.401                  7.33
    ##  6 Administração pú… 2011                   1699.    0.416                  7.44
    ##  7 Administração pú… 2012                   1877.    0.435                  7.54
    ##  8 Administração pú… 2013                   2002.    0.430                  7.60
    ##  9 Administração pú… 2014                   2180.    0.441                  7.69
    ## 10 Administração pú… 2015                   2379.    0.394                  7.77
    ## # ℹ 326 more rows
    ## # ℹ abbreviated name: ¹​valor_remuneracao_media

Now, I regress exposure on log_remuneracao_media with plm.

``` r
twfe_reg <- plm(log_remuneracao_media ~ exposure,
             data = nber30400,
             model = 'within',
             effect = 'twoways',
             index = c('secao','ano'))
```

``` r
tb1 <- tbl_padrao(
  twfe_reg,
  c('exposure'),
  list(exposure='Exposure to computers'),
  robust=TRUE
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

    ## tidy_robust(): Robust estimation with
    ## `parameters::model_parameters(model = x, ci = 0.95, vcov = TRUE)`

``` r
tb1
```

<div id="oljvzgvmxw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#oljvzgvmxw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#oljvzgvmxw thead, #oljvzgvmxw tbody, #oljvzgvmxw tfoot, #oljvzgvmxw tr, #oljvzgvmxw td, #oljvzgvmxw th {
  border-style: none;
}
&#10;#oljvzgvmxw p {
  margin: 0;
  padding: 0;
}
&#10;#oljvzgvmxw .gt_table {
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
&#10;#oljvzgvmxw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#oljvzgvmxw .gt_title {
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
&#10;#oljvzgvmxw .gt_subtitle {
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
&#10;#oljvzgvmxw .gt_heading {
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
&#10;#oljvzgvmxw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_col_headings {
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
&#10;#oljvzgvmxw .gt_col_heading {
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
&#10;#oljvzgvmxw .gt_column_spanner_outer {
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
&#10;#oljvzgvmxw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#oljvzgvmxw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#oljvzgvmxw .gt_column_spanner {
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
&#10;#oljvzgvmxw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#oljvzgvmxw .gt_group_heading {
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
&#10;#oljvzgvmxw .gt_empty_group_heading {
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
&#10;#oljvzgvmxw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#oljvzgvmxw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#oljvzgvmxw .gt_row {
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
&#10;#oljvzgvmxw .gt_stub {
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
&#10;#oljvzgvmxw .gt_stub_row_group {
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
&#10;#oljvzgvmxw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#oljvzgvmxw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#oljvzgvmxw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oljvzgvmxw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#oljvzgvmxw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oljvzgvmxw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#oljvzgvmxw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oljvzgvmxw .gt_footnotes {
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
&#10;#oljvzgvmxw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oljvzgvmxw .gt_sourcenotes {
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
&#10;#oljvzgvmxw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oljvzgvmxw .gt_left {
  text-align: left;
}
&#10;#oljvzgvmxw .gt_center {
  text-align: center;
}
&#10;#oljvzgvmxw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#oljvzgvmxw .gt_font_normal {
  font-weight: normal;
}
&#10;#oljvzgvmxw .gt_font_bold {
  font-weight: bold;
}
&#10;#oljvzgvmxw .gt_font_italic {
  font-style: italic;
}
&#10;#oljvzgvmxw .gt_super {
  font-size: 65%;
}
&#10;#oljvzgvmxw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#oljvzgvmxw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#oljvzgvmxw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#oljvzgvmxw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#oljvzgvmxw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#oljvzgvmxw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#oljvzgvmxw .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Beta</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers</td>
<td headers="estimate" class="gt_row gt_center">-0.0312 
(0.076)</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.001</td>
<td headers="p.value" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate" class="gt_row gt_center">-0.120</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate" class="gt_row gt_center">336</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
  </tfoot>
</table>
</div>

The results show that the `Exposure to computers` measure alone does not
explain any variation in the log wage of the brazilian formal market,
both by the non significant result for the variable itself and by the
$R^2$ regression.

# Second specification - adding occupation classification

``` r
data2 <- rais %>%
  filter(
    !is.na(secao) & !is.na(ano) & !is.na(group)
  ) %>%
  group_by(
    secao,
    ano,
    group
  ) %>%
  summarise(
    wage=mean(valor_remuneracao_media),
    exposure=mean(exposure)
  ) %>%
  ungroup %>%
  mutate(
    log_wage = log(wage),
    id = factor(paste(secao, group, sep="-"))
  )
```

    ## `summarise()` has grouped output by 'secao', 'ano'. You can override using the
    ## `.groups` argument.

``` r
data2
```

    ## # A tibble: 1,344 × 7
    ##    secao                               ano   group  wage exposure log_wage id   
    ##    <fct>                               <fct> <fct> <dbl>    <dbl>    <dbl> <fct>
    ##  1 Administração pública, defesa e se… 2006  NRC   1554.    0.395     7.35 Admi…
    ##  2 Administração pública, defesa e se… 2006  NRM    720.    0.395     6.58 Admi…
    ##  3 Administração pública, defesa e se… 2006  RC    1157.    0.395     7.05 Admi…
    ##  4 Administração pública, defesa e se… 2006  RM     733.    0.395     6.60 Admi…
    ##  5 Administração pública, defesa e se… 2007  NRC   1686.    0.335     7.43 Admi…
    ##  6 Administração pública, defesa e se… 2007  NRM    795.    0.335     6.68 Admi…
    ##  7 Administração pública, defesa e se… 2007  RC    1224.    0.335     7.11 Admi…
    ##  8 Administração pública, defesa e se… 2007  RM     799.    0.335     6.68 Admi…
    ##  9 Administração pública, defesa e se… 2008  NRC   1746.    0.324     7.47 Admi…
    ## 10 Administração pública, defesa e se… 2008  NRM    839.    0.324     6.73 Admi…
    ## # ℹ 1,334 more rows

``` r
twfe_reg_2 <- plm(log_wage ~ exposure,
             data = data2,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
tb2 <- tbl_padrao(
  twfe_reg_2,
  c('exposure'),
  list(exposure='Exposure to computers'),
  robust=TRUE
  )
```

    ## tidy_robust(): Robust estimation with
    ## `parameters::model_parameters(model = x, ci = 0.95, vcov = TRUE)`

``` r
tb2
```

<div id="ekyqhnyfef" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ekyqhnyfef table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ekyqhnyfef thead, #ekyqhnyfef tbody, #ekyqhnyfef tfoot, #ekyqhnyfef tr, #ekyqhnyfef td, #ekyqhnyfef th {
  border-style: none;
}
&#10;#ekyqhnyfef p {
  margin: 0;
  padding: 0;
}
&#10;#ekyqhnyfef .gt_table {
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
&#10;#ekyqhnyfef .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ekyqhnyfef .gt_title {
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
&#10;#ekyqhnyfef .gt_subtitle {
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
&#10;#ekyqhnyfef .gt_heading {
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
&#10;#ekyqhnyfef .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_col_headings {
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
&#10;#ekyqhnyfef .gt_col_heading {
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
&#10;#ekyqhnyfef .gt_column_spanner_outer {
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
&#10;#ekyqhnyfef .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ekyqhnyfef .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ekyqhnyfef .gt_column_spanner {
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
&#10;#ekyqhnyfef .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ekyqhnyfef .gt_group_heading {
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
&#10;#ekyqhnyfef .gt_empty_group_heading {
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
&#10;#ekyqhnyfef .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ekyqhnyfef .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ekyqhnyfef .gt_row {
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
&#10;#ekyqhnyfef .gt_stub {
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
&#10;#ekyqhnyfef .gt_stub_row_group {
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
&#10;#ekyqhnyfef .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ekyqhnyfef .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ekyqhnyfef .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ekyqhnyfef .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ekyqhnyfef .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ekyqhnyfef .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ekyqhnyfef .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ekyqhnyfef .gt_footnotes {
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
&#10;#ekyqhnyfef .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ekyqhnyfef .gt_sourcenotes {
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
&#10;#ekyqhnyfef .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ekyqhnyfef .gt_left {
  text-align: left;
}
&#10;#ekyqhnyfef .gt_center {
  text-align: center;
}
&#10;#ekyqhnyfef .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ekyqhnyfef .gt_font_normal {
  font-weight: normal;
}
&#10;#ekyqhnyfef .gt_font_bold {
  font-weight: bold;
}
&#10;#ekyqhnyfef .gt_font_italic {
  font-style: italic;
}
&#10;#ekyqhnyfef .gt_super {
  font-size: 65%;
}
&#10;#ekyqhnyfef .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ekyqhnyfef .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ekyqhnyfef .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ekyqhnyfef .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ekyqhnyfef .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ekyqhnyfef .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ekyqhnyfef .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Beta</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers</td>
<td headers="estimate" class="gt_row gt_center">-0.0022 
(0.041)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.000</td>
<td headers="p.value" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate" class="gt_row gt_center">-0.080</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate" class="gt_row gt_center">1,344</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
  </tfoot>
</table>
</div>

# Interacting group with exposure

``` r
twfe_reg_3 <- plm(log_wage ~ exposure:group,
             data = data2,
             model = 'within',
             effect = 'twoways',
             index = c('id','ano'))
```

``` r
tb3 <- tbl_padrao(
  twfe_reg_3,
  c('exposure:group'),
  list(`exposure:group`='Exposure to computers'),
  robust=TRUE
  )
```

    ## tidy_robust(): Robust estimation with
    ## `parameters::model_parameters(model = x, ci = 0.95, vcov = TRUE)`

``` r
tb3
```

<div id="prvtoswqnl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#prvtoswqnl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#prvtoswqnl thead, #prvtoswqnl tbody, #prvtoswqnl tfoot, #prvtoswqnl tr, #prvtoswqnl td, #prvtoswqnl th {
  border-style: none;
}
&#10;#prvtoswqnl p {
  margin: 0;
  padding: 0;
}
&#10;#prvtoswqnl .gt_table {
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
&#10;#prvtoswqnl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#prvtoswqnl .gt_title {
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
&#10;#prvtoswqnl .gt_subtitle {
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
&#10;#prvtoswqnl .gt_heading {
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
&#10;#prvtoswqnl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_col_headings {
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
&#10;#prvtoswqnl .gt_col_heading {
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
&#10;#prvtoswqnl .gt_column_spanner_outer {
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
&#10;#prvtoswqnl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#prvtoswqnl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#prvtoswqnl .gt_column_spanner {
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
&#10;#prvtoswqnl .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#prvtoswqnl .gt_group_heading {
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
&#10;#prvtoswqnl .gt_empty_group_heading {
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
&#10;#prvtoswqnl .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#prvtoswqnl .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#prvtoswqnl .gt_row {
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
&#10;#prvtoswqnl .gt_stub {
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
&#10;#prvtoswqnl .gt_stub_row_group {
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
&#10;#prvtoswqnl .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#prvtoswqnl .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#prvtoswqnl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#prvtoswqnl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#prvtoswqnl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#prvtoswqnl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#prvtoswqnl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#prvtoswqnl .gt_footnotes {
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
&#10;#prvtoswqnl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#prvtoswqnl .gt_sourcenotes {
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
&#10;#prvtoswqnl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#prvtoswqnl .gt_left {
  text-align: left;
}
&#10;#prvtoswqnl .gt_center {
  text-align: center;
}
&#10;#prvtoswqnl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#prvtoswqnl .gt_font_normal {
  font-weight: normal;
}
&#10;#prvtoswqnl .gt_font_bold {
  font-weight: bold;
}
&#10;#prvtoswqnl .gt_font_italic {
  font-style: italic;
}
&#10;#prvtoswqnl .gt_super {
  font-size: 65%;
}
&#10;#prvtoswqnl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#prvtoswqnl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#prvtoswqnl .gt_indent_1 {
  text-indent: 5px;
}
&#10;#prvtoswqnl .gt_indent_2 {
  text-indent: 10px;
}
&#10;#prvtoswqnl .gt_indent_3 {
  text-indent: 15px;
}
&#10;#prvtoswqnl .gt_indent_4 {
  text-indent: 20px;
}
&#10;#prvtoswqnl .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Beta</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Exposure to computers</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * NRC</td>
<td headers="estimate" class="gt_row gt_center">-0.0158 
(0.054)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * NRM</td>
<td headers="estimate" class="gt_row gt_center">-0.0484 
(0.054)</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RC</td>
<td headers="estimate" class="gt_row gt_center">0.0671 
(0.054)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    exposure * RM</td>
<td headers="estimate" class="gt_row gt_center">-0.0119 
(0.054)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.004</td>
<td headers="p.value" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate" class="gt_row gt_center">-0.078</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate" class="gt_row gt_center">1,344</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
  </tfoot>
</table>
</div>

# Using goup as an explainable variable

## only group as explainable variables

``` r
twfe_reg_4 <- plm(log_wage ~ group,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

``` r
tb4 <- tbl_padrao(
  twfe_reg_4,
  c('group'),
  list(
    group='Discrete classification of occupation'
  ),
  robust=TRUE
  )
```

    ## tidy_robust(): Robust estimation with
    ## `parameters::model_parameters(model = x, ci = 0.95, vcov = TRUE)`

``` r
tb4
```

<div id="dqtfbwlykr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dqtfbwlykr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dqtfbwlykr thead, #dqtfbwlykr tbody, #dqtfbwlykr tfoot, #dqtfbwlykr tr, #dqtfbwlykr td, #dqtfbwlykr th {
  border-style: none;
}
&#10;#dqtfbwlykr p {
  margin: 0;
  padding: 0;
}
&#10;#dqtfbwlykr .gt_table {
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
&#10;#dqtfbwlykr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dqtfbwlykr .gt_title {
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
&#10;#dqtfbwlykr .gt_subtitle {
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
&#10;#dqtfbwlykr .gt_heading {
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
&#10;#dqtfbwlykr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_col_headings {
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
&#10;#dqtfbwlykr .gt_col_heading {
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
&#10;#dqtfbwlykr .gt_column_spanner_outer {
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
&#10;#dqtfbwlykr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dqtfbwlykr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dqtfbwlykr .gt_column_spanner {
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
&#10;#dqtfbwlykr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dqtfbwlykr .gt_group_heading {
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
&#10;#dqtfbwlykr .gt_empty_group_heading {
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
&#10;#dqtfbwlykr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dqtfbwlykr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dqtfbwlykr .gt_row {
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
&#10;#dqtfbwlykr .gt_stub {
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
&#10;#dqtfbwlykr .gt_stub_row_group {
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
&#10;#dqtfbwlykr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dqtfbwlykr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dqtfbwlykr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dqtfbwlykr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dqtfbwlykr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dqtfbwlykr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dqtfbwlykr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dqtfbwlykr .gt_footnotes {
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
&#10;#dqtfbwlykr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dqtfbwlykr .gt_sourcenotes {
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
&#10;#dqtfbwlykr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dqtfbwlykr .gt_left {
  text-align: left;
}
&#10;#dqtfbwlykr .gt_center {
  text-align: center;
}
&#10;#dqtfbwlykr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dqtfbwlykr .gt_font_normal {
  font-weight: normal;
}
&#10;#dqtfbwlykr .gt_font_bold {
  font-weight: bold;
}
&#10;#dqtfbwlykr .gt_font_italic {
  font-style: italic;
}
&#10;#dqtfbwlykr .gt_super {
  font-size: 65%;
}
&#10;#dqtfbwlykr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dqtfbwlykr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dqtfbwlykr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dqtfbwlykr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dqtfbwlykr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dqtfbwlykr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dqtfbwlykr .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Beta</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Discrete classification of occupation</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRC</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRM</td>
<td headers="estimate" class="gt_row gt_center">-1.0113*** 
(0.029)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RC</td>
<td headers="estimate" class="gt_row gt_center">-0.6195*** 
(0.029)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RM</td>
<td headers="estimate" class="gt_row gt_center">-0.8270*** 
(0.029)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.512</td>
<td headers="p.value" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate" class="gt_row gt_center">0.505</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate" class="gt_row gt_center">1,344</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
  </tfoot>
</table>
</div>

## group and secao as explainable variables

``` r
twfe_reg_5 <- plm(log_wage ~ group + secao,
             data = data2,
             model = 'within',
             effect = 'time',
             index = c('id', 'ano'))
```

``` r
tb5 <- tbl_padrao(
  twfe_reg_5,
  c('group', 'secao'),
  list(
    group='Discrete classification of occupation',
    secao='CNAE seção'
  ),
  robust=TRUE
  )
```

    ## tidy_robust(): Robust estimation with
    ## `parameters::model_parameters(model = x, ci = 0.95, vcov = TRUE)`

``` r
tb5
```

<div id="atfxjrhdda" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#atfxjrhdda table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#atfxjrhdda thead, #atfxjrhdda tbody, #atfxjrhdda tfoot, #atfxjrhdda tr, #atfxjrhdda td, #atfxjrhdda th {
  border-style: none;
}
&#10;#atfxjrhdda p {
  margin: 0;
  padding: 0;
}
&#10;#atfxjrhdda .gt_table {
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
&#10;#atfxjrhdda .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#atfxjrhdda .gt_title {
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
&#10;#atfxjrhdda .gt_subtitle {
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
&#10;#atfxjrhdda .gt_heading {
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
&#10;#atfxjrhdda .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_col_headings {
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
&#10;#atfxjrhdda .gt_col_heading {
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
&#10;#atfxjrhdda .gt_column_spanner_outer {
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
&#10;#atfxjrhdda .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#atfxjrhdda .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#atfxjrhdda .gt_column_spanner {
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
&#10;#atfxjrhdda .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#atfxjrhdda .gt_group_heading {
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
&#10;#atfxjrhdda .gt_empty_group_heading {
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
&#10;#atfxjrhdda .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#atfxjrhdda .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#atfxjrhdda .gt_row {
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
&#10;#atfxjrhdda .gt_stub {
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
&#10;#atfxjrhdda .gt_stub_row_group {
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
&#10;#atfxjrhdda .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#atfxjrhdda .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#atfxjrhdda .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atfxjrhdda .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#atfxjrhdda .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atfxjrhdda .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#atfxjrhdda .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atfxjrhdda .gt_footnotes {
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
&#10;#atfxjrhdda .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atfxjrhdda .gt_sourcenotes {
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
&#10;#atfxjrhdda .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atfxjrhdda .gt_left {
  text-align: left;
}
&#10;#atfxjrhdda .gt_center {
  text-align: center;
}
&#10;#atfxjrhdda .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#atfxjrhdda .gt_font_normal {
  font-weight: normal;
}
&#10;#atfxjrhdda .gt_font_bold {
  font-weight: bold;
}
&#10;#atfxjrhdda .gt_font_italic {
  font-style: italic;
}
&#10;#atfxjrhdda .gt_super {
  font-size: 65%;
}
&#10;#atfxjrhdda .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#atfxjrhdda .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#atfxjrhdda .gt_indent_1 {
  text-indent: 5px;
}
&#10;#atfxjrhdda .gt_indent_2 {
  text-indent: 10px;
}
&#10;#atfxjrhdda .gt_indent_3 {
  text-indent: 15px;
}
&#10;#atfxjrhdda .gt_indent_4 {
  text-indent: 20px;
}
&#10;#atfxjrhdda .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Beta</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Discrete classification of occupation</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRC</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    NRM</td>
<td headers="estimate" class="gt_row gt_center">-1.0113*** 
(0.011)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RC</td>
<td headers="estimate" class="gt_row gt_center">-0.6195*** 
(0.011)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    RM</td>
<td headers="estimate" class="gt_row gt_center">-0.8270*** 
(0.011)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">CNAE seção</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Administração pública, defesa e seguridade social</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Agricultura, pecuária, produção florestal, pesca e aqüicultura</td>
<td headers="estimate" class="gt_row gt_center">-0.2106*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Alojamento e alimentação</td>
<td headers="estimate" class="gt_row gt_center">-0.4534*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Artes, cultura, esporte e recreação</td>
<td headers="estimate" class="gt_row gt_center">-0.2579*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades administrativas e serviços complementares</td>
<td headers="estimate" class="gt_row gt_center">-0.2529*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades financeiras, de seguros e serviços relacionados</td>
<td headers="estimate" class="gt_row gt_center">0.3582*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades imobiliárias</td>
<td headers="estimate" class="gt_row gt_center">-0.1848*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Atividades profissionais, científicas e técnicas</td>
<td headers="estimate" class="gt_row gt_center">0.0435 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">0.095</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Comércio; reparação de veículos automotores e motocicletas</td>
<td headers="estimate" class="gt_row gt_center">-0.3156*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Construção</td>
<td headers="estimate" class="gt_row gt_center">-0.0679** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">0.009</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Educação</td>
<td headers="estimate" class="gt_row gt_center">-0.0364 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Eletricidade e gás</td>
<td headers="estimate" class="gt_row gt_center">0.8366*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    ��gua, esgoto, atividades de gestão de resíduos e descontaminação</td>
<td headers="estimate" class="gt_row gt_center">0.2285*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Indústrias de transformação</td>
<td headers="estimate" class="gt_row gt_center">-0.0019 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Indústrias extrativas</td>
<td headers="estimate" class="gt_row gt_center">0.6321*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Informação e comunicação</td>
<td headers="estimate" class="gt_row gt_center">0.0689** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">0.008</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Organismos internacionais e outras instituições extraterritoriais</td>
<td headers="estimate" class="gt_row gt_center">0.5144*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outras atividades de serviços</td>
<td headers="estimate" class="gt_row gt_center">-0.2258*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Saúde humana e serviços sociais</td>
<td headers="estimate" class="gt_row gt_center">-0.1724*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Serviços domésticos</td>
<td headers="estimate" class="gt_row gt_center">-0.5522*** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Transporte, armazenagem e correio</td>
<td headers="estimate" class="gt_row gt_center">0.0827** 
(0.026)</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">R²</td>
<td headers="estimate" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;">0.926</td>
<td headers="p.value" class="gt_row gt_center" style="border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3;"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Adjusted R²</td>
<td headers="estimate" class="gt_row gt_center">0.924</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">No. Obs.</td>
<td headers="estimate" class="gt_row gt_center">1,344</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</td>
    </tr>
  </tfoot>
</table>
</div>
