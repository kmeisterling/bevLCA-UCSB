# bevLCA-UCSB
R code and associated data (input tables, tidy intermediate tables, pivoted tables for viewing) to calculate environmental impacts of beverages sold through UC Santa Barbara Dining. Three environmental impacts are estimated:

* Climate impact (g CO2 eq)
* Plastic pollution (mg plastic entering the environment)
* Blue water use (liter blue water)

# Source (input) data
data/
- dfucsb.csv - [various units]; Beverage sales (flow) data; each row represents one entry in the original data source; includes container volume (liter_cont) [liter / yr], beverage volume (as drank) [liter / yr], and sugar contents [g sugar / liter of container volume]
- [[maybe not necessary]] flow_bs_allscen_wide.csv - [liter / yr]; Beverage flow for scenarios, by bev_type and SSB_status
- flow_bsc_allscen.csv - [liter / yr]; Beverage flows , by bev_type, SSB_status, and container type

- bev_imp_l - [various units / liter as-consumed volume]; impacts of beverages (not including container); three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]
- cont_imp_l - [various units / liter as-consumed volume]; impacts of containers, excluding bag-in-box containers; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]
- cont_imp_l_binb - [various units / liter as-consumed volume]; impacts of bag-in-box containers, corrected for bag-in-box dilution; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment]

# Procedure
run scripts in main directory, in order
- 1_impacts.R - Use source data to calculate impacts of beverages in the scenarios, and write intermediate objects [[either data-gen/ or data-tbls/]]
- 2_polots_tables.R - Use output of "1_impacts.R" to make tables and figures for the manuscript and supplemental info docs
- 3_scen6_scpecial.R - Calculate impacts for scenarios 5 and 6, by distribution channel (vending and dining)
