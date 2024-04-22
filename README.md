# HTN_HTE
### Explore heterogeneous treatment effects (HTE) of first-line hypertension medications

## Overview
1. HTN_HTE.Rmd sources the SQL querying code and analysis code 
2. Run the Rmarkdown file and make changes to the SQL connection details depending on SQL server type (MS-SQL, Postgres, SQLite)
3. Analytical results are stored in the results folder

## Code Components
HTN_HTE divided into 2 Parts and 4 Steps
1. PART 1: Data Querying and Dataset Creation
	1. STEP 0A: SQL Queries
	2. STEP 0B: Analytical Dataset Creation and Data Cleaning
2. PART 2: Heterogeneous Treatment Effects Exploration 
	1. STEP 1: Individual Treatment Effect Estimation 
	2. STEP 2: Casual Forest Analysis to Identify Factors Contributing to Heterogeneity 
	3. STEP 3: TMLE Estimation of Treatment Effects within Different Patient Populations

### Code Execution Instructions
1. Select SQL database type and update SQL connection details
2. Determine whether to save the analytical dataset (contains patient individual data) -> saving dataset saves time on future analysis 
3. Run analysis up to PART 2, STEP 2 
4. Determine what the patient profiles are via the variables of interest and cutoffs 
5. Run PART 2, STEP 3


