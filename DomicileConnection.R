library(DBI)

domconnect <- dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = "domicile-reports",
                        host = "a9c250aff69d811e8a82b02c40550189-1435924674.us-west-2.elb.amazonaws.com",
                        port = 5432,
                        user = "reporter",
                        password = "0Qo0m@%F2JCh")