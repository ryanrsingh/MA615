library(RPostgreSQL)

host <- "analyticsga-east2.c20gkj5cvu3l.us-east-1.rds.amazonaws.com"
port <- "5432"
username <- "analytics_student"
password <- "analyticsga"
## Use the name of the specific database you will access
dbname <- "iowa_liquor_sales_database"
## Specify the PostreSQL driver
drv <- dbDriver("PostgreSQL")
## Now establish the connection
con <- dbConnect(drv, user = username, password = password, dbname = dbname, port = port, host = host)


#Run this to test connection
dbListTables(con)
dbListFields(con, "products")


r1 <- dbGetQuery(con, statement = paste(
  "SELECT DISTINCT category_name, cast(proof as integer)",
  "FROM products",
  "WHERE cast(proof as integer) >= 85 and category_name is not null"))

#to see what r1 is
str(r1) 


#Exploring case cost
r3 <- dbGetQuery(con, statement = paste(
  "SELECT case_cost",
  "FROM products"))

dotchart(r3$case_cost)
hist(r3$case_cost)

filter1 <- subset(r3, case_cost < 3000)

qplot(filter1$case_cost)

r2 <- dbGetQuery(con, statement = paste(
  "SELECT category_name, vendor_name, bottle_size, pack, inner_pack, proof, bottle_price, shelf_price, case_cost",
  "FROM products"))

r2$proof <- as.integer(r2$proof)
r2$bottle_price <- sub('.','', r2$bottle_price)
r2$bottle_price <- as.numeric(r2$bottle_price)

reg1 <- lm(r2$case_cost ~ r2$bottle_size + r2$pack + r2$inner_pack + r2$proof + r2$bottle_price + r2$shelf_price)
summary(reg1)

reg2 <- lm(r2$case_cost ~ r2$bottle_size + r2$pack + r2$inner_pack + r2$proof + r2$shelf_price)
summary(reg2)

cor.test(r2$case_cost, r2$bottle_price)
