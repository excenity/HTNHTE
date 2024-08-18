# HTN_HTE
### Explore heterogeneous treatment effects (HTE) of first-line hypertension medications

## Overview
1. Install package and ensure you have Java installed as this is required when connecting to the OMOP database.
2. Run the execution code Step 1 after specifying the input parameters.
3. Run the execution code Step 2 after specifying the input parameters.

## Installation

Run the code below to install the study package:

```{r}
remotes::install_github('excenity/HTNHTE', ref = 'package')
```

This will download all the functions needed to execute the study.

## Input parameters

Specify where you want the results and the database settings:

```{r}
outputpath <- 'location to save results'
cdmDatabaseSchema <- 'add schema name'
connectionDetails <- DatabaseConnector::createConnectionDetails( 
dbms = 'your dbms',
user = 'you database username',
password = 'your database password', 
server = 'your database server'
port = 'you database port'
)

cohortDatabaseSchema - 'add schema with read/write access'
cohortTable <- 'htn_hte'
```

## Step 1

Run the code that generates the study cohorts, extracts the data, processes the data and then runs statistical analysis:

```{r}
executeStudy(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = 'htn_hte',
    outputpath = outputpath,
    generateCohorts = T,
    extractingData = T,
    runStatisticsAnalysis = T,
    runTreatmentEffects = F
)
```
This will generate csv files and plots that you can inspect in the `outputpath` location you specified.

## Step 2

Now run the treatment effects code:

```{r}
executeStudy(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = 'htn_hte',
    outputpath = outputpath,
    generateCohorts = F,
    extractingData = F,
    runStatisticsAnalysis = F,
    runTreatmentEffects = T
)
```


