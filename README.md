# bevLCA-UCSB
R code and associated data (input tables, tidy intermediate tables, pivoted tables for viewing) to calculate environmental impacts of beverages sold through UC Santa Barbara Dining. Three environmental impacts are estimated:

* Climate impact (g CO2 eq)
  * and what?
* Plastic pollution (g plastic entering the environment)
* Blue water use (liter blue water)

# Source (input) data
data_raw/
- bev_flow_ucsb_18_19.csv; various units; Beverage sales (flow) data; each row represents a purchase of beverage (or beverage concentrate) by UCSB dining; includes 
  - liter_cont; container volume; [liter / yr]
  - liter_drink; beverage volume, as drank; [liter / yr]
  - distrib; whether the beverage was "purchased"" through UCSB Dining (including in cafeterias and from stores), or whether it as purchased through a vending machine; [factor]
  - sug_add, sug_other, sug_unspec, sug_total; [g sugar / liter of container volume]; sugar content of purchased substane; sug_agg = added sugar, sug_other = sugar not added (i.e. "natural" sugar), sug_unspec = unspecified sugar (don't know whether it is added or natural sugar), sug_total = sum of added, other, and unspecified sugar; [g sugar / liter of originally packaged beverage product]
  - mix_r; mixing ratio for beverags that require dilution (e.g. fountain bag-in-box containers); the mixing ratio is equal to zero for all beverages that do not need dilution (i.e. bought by UC Dining in a ready-to-drink format); in our data, all the beverage products in bag-in-box containers required dilution, and thus have a non-zero mixing ratio (although this does not have to be the case; bag-in-box is relatively common for large-volume fresh coffee purchases, and bag-in-box alternative to plastic bottles are also being developed) [volume of dilution water / volume as originally packaged]
  
data_gen/

* 



- [[maybe not necessary]] flow_bs_allscen_wide.csv - [liter / yr]; Beverage flow for scenarios, by bev_type and SSB_status
- flow_bsc_allscen.csv - [liter / yr]; Beverage flows , by bev_type, SSB_status, and container type

- bev_imp_l; impacts of beverages (not including container); three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment];  [(impact) / liter as-consumed volume]
- cont_imp_l; impacts of containers, excluding bag-in-box containers; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]; [(impact) / liter as originally packaged volume]
- cont_imp_l_binb; impacts of bag-in-box containers, corrected for bag-in-box dilution; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]; [(impact) / liter as-consumed volume]

# Procedure
run scripts in main directory, in order
- 1_impacts.R - Use source data to calculate impacts of beverages in the scenarios, and write intermediate objects (to data-gen/)
- 2_polots_tables.R - Use output of "1_impacts.R" to make tables and figures for the manuscript and supplemental info docs
- 3_scen6_scpecial.R - Calculate impacts for scenarios 5 and 6, by distribution channel (vending and dining)
