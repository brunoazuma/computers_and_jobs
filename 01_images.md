Images
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the
development and saving of the images used in the undergrad thesis. Some
images will be developed on my own, some will be replications of cited
papers. The notebook will follow the structure of the undergrad thesis.

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

![](01_images_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-autor2019" class="csl-entry">

AUTOR, D. H. [Work of the Past, Work of the
Future](https://doi.org/10.1257/pandp.20191110). **AEA Papers and
Proceedings**, v. 109, p. 1–32, maio 2019.

</div>

</div>
