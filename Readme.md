# Overview

This repository provides replication materials for the paper: (final paper name)

# Step 1: Data Downloading

For replication purposes, you will need to download the following large files from Google Drive 
- Download ```data/url_to_flag_updatedtoFeb24_obitfilter.csv``` from [here](https://drive.google.com/file/d/1R2vDUE8KnHzliLv3kG2RDIcG51DMFy1e/view?usp=sharing)
- Download ```data/theta_Feb27update_withdomtopic.csv``` from [here](https://drive.google.com/file/d/1iz2WcS9aLCQpskEcz59YzFWIjrdmsua5/view?usp=sharing)


# Replicating Paper Results

Results for the main text of the paper can then be replicated using the ```R``` files ```figures_1_2_3.R``` and ```figures_4_%.R```. If you would also like to replicate the supplement, you can do so with the file ```plots_for_supplement.R```. 


# Data Sources

A reminder that the paper uses data from the following sources:
- The [NYTimes COVID Case/Death Rate Repository](https://github.com/nytimes/covid-19-data). This is pulled directly from the repository in ```util.R```
- [MEDSL's Election and County Data](https://github.com/MEDSL/2018-elections-unoffical). This is pulled directly from the repository in ```util.R```
- [Kieran Healy's 2020 Election Results](https://github.com/kjhealy/us_elections_2020_csv). This is pulled directly from the repository in ```util.R```
-  [2019 population estimates of U.S. counties from the Census Bureau](https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/). This is contained in the file ```data/co-est2019-alldata.csv```.
- [Community Resilience Estimates provided by the U.S. Census to estimate the percentage of individuals in each county that had 0 risk factors, 1-2 risk factors, or 3+ risk factors for COVID](https://www.census.gov/data/experimental-data-products/community-resilience-estimates.html). This is contained in the file ```data/cre-2018-a11.csv```.


# Replicating the Topic Model