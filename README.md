# bevLCA-UCSB
Code and input tables to calculate environmental impacts of beverage sold through UCSB Dining

# Source (input) data
data/
- dfucsb.csv - [liter / yr]; Beverage sales (flow) data; each row represents one entry in the original data source
- [[maybe not necessary]] flow_bs_allscen_wide.csv - [liter / yr]; Beverage flow for scenarios, by bev_type and SSB_status
- flow_bsc_allscen.csv - [liter / yr]; Beverage flows , by bev_type, SSB_status, and container type
- cont_imp_l - [impact / liter]; three columns of impacts per liter; climate [gCO2e], blue water [liter h2o], plastic pollution [g plastic to enviroment
- binb_imp_l
- bev_imp_l


# Calculated tables (intermediate objects, used to calculate final impacts or make polots)
Generated data tables are in data-gen/
- [[currently made by part1]] flow_dbsc_scen0.csv - beverage sales (flow) data for the baseline scenario, by distribution channel (d), bev_type (b), SSB_status (s), and container type (c); used for figure
- 

Calculated 



# Overview

