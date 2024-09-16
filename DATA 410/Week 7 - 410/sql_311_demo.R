# install.packages("sqldf")
library(sqldf)

# Peek at the first few rows of the dataset 
read.table("311_Requests_clean.csv", sep=";", header = TRUE, fill = TRUE, nrows = 5)


# Begin SQL processing by creating a new database
# Then add tables

{
  
  reqphl <- dbConnect(SQLite(), dbname = "311_Requests_clean.db")
  
  if (dbExistsTable(reqphl, "phls"))
    dbRemoveTable(reqphl, "phls")
  
 
  dbWriteTable(
    reqphl,
    "phls",
    "311_Requests_clean.csv",
    sep = ";",
    header = TRUE,
    row.names = FALSE
  ) 
  dbDisconnect(reqphl)
}



#----------------------------------------------------------------------------
# Checking the data
#----------------------------------------------------------------------------

phlcon <- dbConnect(SQLite(), dbname = "311_Requests_clean.db")
res <- dbSendQuery(phlcon, "
                   SELECT *
                   FROM phls")
fetch(res, n = 10)
dbClearResult(res)




#----------------------------------------------------------------------------
# Basic SQL queries
#----------------------------------------------------------------------------

res <- dbSendQuery(phlcon,
                   "
                   SELECT servicename,agencyresponsible,status
                   FROM phls")
fetch(res, n = 10)
dbClearResult(res)


res <- dbSendQuery(phlcon, "
                   SELECT servicename,agencyresponsible,servicenotice,zipcode
                   FROM phls
                   WHERE (zipcode=='19147')")
fetch(res, n = 10)
dbClearResult(res)

res <- dbSendQuery(phlcon, "
                   SELECT servicename,agencyresponsible,servicenotice
                   FROM phls
                   WHERE (zipcode=='19147') AND (agencyresponsible=='Streets Department') 
                   ")
fetch(res, n = -1)
dbClearResult(res)

res <- dbSendQuery(phlcon, "
                   SELECT DISTINCT agencyresponsible
                   FROM phls")
fetch(res, n = -1)
dbClearResult(res)


#------------


res2 <- dbSendQuery(phlcon, "
                    SELECT DISTINCT COUNT(*) 
                    FROM phls
                    ")
fetch(res2, 10)

dbClearResult(res2)

status <- dbSendQuery(phlcon, "
                      SELECT DISTINCT status
                      FROM phls")
fetch(status, n = 10)
dbClearResult(status)


status <- dbSendQuery(phlcon, "
                      SELECT status, COUNT(servicerequestid)
                      FROM phls
                      GROUP BY status")
fetch(status, n = 10)
dbClearResult(status)

dates <- dbSendQuery(phlcon, "
                     SELECT MIN(expecteddatetime), MAX(expecteddatetime)
                     FROM phls
                     WHERE expecteddatetime != '' ")
fetch(dates, n = 10)
dbClearResult(dates)















#-----------------------
# Your turn! Answer

zipcount <- dbSendQuery(phlcon, "
                     SELECT COUNT(*)
                     FROM phls
                     WHERE zipcode = '19104' OR zipcode = '19103'")

fetch(zipcount, n = 10)
dbClearResult(zipcount)



#-------------------------------------------------------------------------
# Creating multiple tables

if(dbExistsTable(phlcon, "service")) dbRemoveTable(phlcon, "service")  

distinct <- dbSendQuery(phlcon, "
                        CREATE TABLE service AS
                        SELECT DISTINCT servicecode,servicename
                        FROM phls
                        ")

dbClearResult(distinct)



















# Answer
codes <- dbSendQuery(phlcon, "
                     SELECT *
                     FROM service
                     ")
fetch(codes, n = -1)
dbClearResult(codes)




if(dbExistsTable(phlcon, "abbr_phls")) dbRemoveTable(phlcon, "abbr_phls")  

distinct <- dbSendQuery(phlcon, "
                        CREATE TABLE abbr_phls AS
                        SELECT DISTINCT servicerequestid,agencyresponsible, servicename
                        FROM phls
                        ")
dbClearResult(distinct)

#-------------------------------------------------------------------------
# Joins



count <- dbSendQuery(phlcon, "
                     SELECT agencyresponsible, COUNT(servicerequestid)
                     FROM abbr_phls AS agency
                     JOIN service
                     ON agency.servicename = service.servicename
                     WHERE service.servicecode = 'SR-IR01'
                     GROUP BY agency.agencyresponsible
                     ")
fetch(count, n = -1)
dbClearResult(count)






















# ANSWER





test<-as.data.frame(fetch(count, n = -1))
sum(test$`COUNT(servicerequestid)`)


dates <- dbSendQuery(phlcon, "
                     SELECT COUNT(*)
                     FROM phls
                     WHERE servicecode = 'SR-IR01' ")
fetch(dates, n = 10)
dbClearResult(dates)
