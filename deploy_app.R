# Push to shinyapp.io
library(rsconnect)

rsconnect::deployApp(
  appFiles = c("app.R", "mydb.duckdb"),
  appName = "EGEx_dev"
)

