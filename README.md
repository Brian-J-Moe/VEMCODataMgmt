
# VEMCODataMgmt

<!-- badges: start -->
<!-- badges: end -->

VEMCODataMgmt is designed for management and data consolidation of Florida Fish and Wildlife, Charlotte Harbor Field Laboratory passive acoustic data. Function included in this package clean and merge data from the FWC Charlotte Harbor array and the FACT and iTAG networks.  


## Installation

You can install the development version of VEMCODataMgmt like so:

``` r
pak::pak("Brian-J-Moe/VEMCODataMgmt")
```

When merging new FACT data, ensure the column names have not changed from the default. To view the default column names run:

```r
?VEMCODataMgmt::process_fact_workflow
```

