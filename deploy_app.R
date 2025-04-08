# Push to shinyapp.io
library(rsconnect)

rsconnect::deployApp(
  appFiles = c("app.R", "mydb.duckdb", './modules/goORA_module.R', './modules/reactomeORA_module.R', './modules/main_module.R', './modules/oddsRatio_module.R'),
  appName = "EGEx_dev"
)

