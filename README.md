# bevLCA-UCSB
R code and associated data to calculate environmental impacts of beverages sold through UC Santa Barbara Dining. 

## Kyle need to do

I like imp_is_allscen.pdf, but it need a "net total" indicator for each panel -- could I add the total as a diamond or dot at x = 1.5 for each?

## Overview

### Three environmental impacts are estimated:

* Climate impact (g CO2 eq)
* Plastic pollution (g plastic entering the environment)
* Blue water use (liter blue water)

### Calculations and results

Impacts of the different scenarios are calculated in the script **1 impacts.R**. 
There are two main type of data input:

* Beverage flow, in liters of beverage per year; reported by type of beverage, type of container, 
  * also reports beverage container volume
* Impacts of different types of beverages and containers, per liter of beverage (as-consumed) 

Note: container volume and beverage volume as-drank is different for "fountain"-type drink concentrates, which are diluted before being served and consumed. See descriptions of data inputs below for more description.

### data organization

* data is stored as csv when needed, and table inputs are also supplied as csv
  * csv files that are directly in */data* folder are inputs
  * csv files in */data/data_gen* are intermediate, tidy objects, which are not meant meant for consumption
  * csv files in */data/data_tbls* are tables that are meant for publication, and/or to be supplied as underlying data for a figure
* figures are in */figs*
  * Figures used in manuscript and SI are named with "FIG" followed by number



## naming approach 
Objects are typically named to indicate the variables included, as follows:
 # "d" = distribution [vending, dining(retail), water station]
 # "b" = beverage type
 # "s" = SSB_status [yes, no]
 # "c" = container type
 # "i" = "item"; refers to whether an impact is due to the beverage (liquid without container), or from the container
 
Example: A chart to show the amount of beverages consumed for the baseline scenario, by beverage type (e.g. bars along the x-axis), and the distribution channel (different color stacks, which make up each bar); It would have the code "_db_", to denote "distribution" and "beverage type".

# Source (input) data

## */data/*

### Data from campus purchases

- bev_ucsb_18_19.csv; various units; Beverage sales (volume) data; each row represents a purchase of beverage (or beverage concentrate) by UCSB dining; includes 
  - liter_cont; container volume; [liter / yr]
  - liter_drink; beverage volume, as drank; [liter / yr]
  - distrib; whether the beverage was "purchased"" through UCSB Dining (including in cafeterias and from stores), or whether it as purchased through a vending machine; [factor]
  - sug_add, sug_other, sug_unspec, sug_total; [g sugar / liter of container volume]; sugar content of purchased substane; sug_agg = added sugar, sug_other = sugar not added (i.e. "natural" sugar), sug_unspec = unspecified sugar (don't know whether it is added or natural sugar), sug_total = sum of added, other, and unspecified sugar; [g sugar / liter of originally packaged beverage product]
  - mix_r; mixing ratio for beverags that require dilution (e.g. fountain bag-in-box containers); the mixing ratio is equal to zero for all beverages that do not need dilution (i.e. bought by UC Dining in a ready-to-drink format); in our data, all the beverage products in bag-in-box containers required dilution, and thus have a non-zero mixing ratio (although this does not have to be the case; bag-in-box is relatively common for large-volume fresh coffee purchases, and bag-in-box alternative to plastic bottles are also being developed) [volume of dilution water / volume as originally packaged]

- flow_dbsc_scen0.csv; various uniss; Beverage sales (volume) data, summarized by distribution channel (distrib), bev_type, SSB_status, cont_type
  - vol; volume of beverage, as drank by the consumer; [L]
  - vol_cont; volume of beverage container, as delivered to a dining service provider (e.g. UCSB); [L]
  - mix_r_avg; the added water mixing ratio for "bag-in-box" containers (BinB); mixing ratio = 0 indicates no water is added before consumption; only beverages that are served in a fountain-dispenser type of setting would have a mixing ratio > 0; in our data set, all bag-in-box containers were dispensed in a fountain setting, but not all bag-in-box beverages had mixing ratio > 0 (e.g. fresh animal milk is not diluted, and so has mixing ratio = 0); [L / L]

- vol_scen0_dbsc_din+vend.csv; Similar with flow_dbsc_scen0.csv, no mixing ratio, no vol
  - vol_kL; volume of beverage, as drank by the consumer [kilo L]
  
- vol_bsc_allscen.csv; beverage amount, by bev_type, SSB_status, cont_type; all scenarios
  - vol; volume of beverage, as drank by drinker [L]

### Data synthesized from life cycle assessment studies

Provides the environmental impacts of beverages and containers

- bev_imp_l; impacts of beverages (not including container); three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment];  [(impact) / liter as-consumed volume]
- cont_imp_l; impacts of containers, excluding bag-in-box containers; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]; [(impact) / liter as originally packaged volume]
- cont_imp_l_binb; impacts of bag-in-box containers, corrected for bag-in-box dilution; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]; [(impact) / liter as-consumed volume]

See Supp Info document for details.

## /data_gen/

Objects that are generated by one script, and used by others, but that at not meant to be used directly as figure sources or a manuscript table

- imp_bsc_allscen.csv; impacts of beverages, for each scenario by Beverage type ("b"), Sugar sweetened beverage status ("s"), and Container type ("c"); [ (impact {g CO2e, g plastic pollution, liter blue water use}) / yr ]


## /data_tbls/

Objects that 
* Will be published as-is or with minor formatting
* Are used directly as figure source data

- flow_kL_bc_allscen.csv - [thousand liter / yr]; Beverage flow for scenarios, by bev_type and cont_type
- imp_scen0_summ.csv - [impacts / yr]; total impacts for the baseline scenario
- imp_allscen.csv - [impacts / yr]; total impacts for each scenario, by item (beverage and container)
- imp_bsc_allscen_wide.csv - [impacts / yr]; impacts by bev_type, SSB_status, cont_type
- imp_ibs_scen0.csv - [impacts / yr]; impacts for the baseline scenario, by bev_type, SSB_status, and item
- imp_i_allscen_wide.csv - [impacts / yr]; impacts by item

# Procedure
run scripts in main directory, in order
- 1_impacts.R - Use source data to calculate impacts of beverages in the scenarios, and write intermediate objects (to data-gen/)
- 2_polots_tables.R - Use output of "1_impacts.R" to make tables and figures for the manuscript and supplemental info docs


[[- 3_scen6_scpecial.R - Calculate impacts for scenarios 5 and 6, by distribution channel (vending and dining) ]]
