library(HTNHTE)
library(Eunomia)
library(DatabaseConnector)
library(SqlRender)
library(tidyverse)


connectionDetails = createConnectionDetails(
  dbms = "sqlite",
  server = '/Users/excenity/Documents/HSIP/Research/Dissertation Project/Data/EDW_OMOP_Full.db',
)

generateCohorts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = 'main',
    cohortDatabaseSchema = 'main',
    cohortTable = 'htn_hte',
)

conn = connect(connectionDetails)
getTableNames(conn, databaseSchema = 'main')

renderTranslateQuerySql(conn, "SELECT * FROM main.htn_hte LIMIT 10")

data <- getData(
    cdmDatabaseSchema = 'main',
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = 'main',
    cohortTable = 'htn_hte'
)





