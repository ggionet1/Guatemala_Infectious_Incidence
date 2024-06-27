# ---------------------------------------------------------
# Using shinyapp.io
library(rsconnect)

# Save secret token for connecting to R shiny account
r_shiny_account_token <- Sys.getenv("r_shiny_account_token")

# Connect the r script below to an R shiny account
rsconnect::setAccountInfo(name='cutrifinio', token='C0BFCA28C1FE0F83CB5E67B30FA01E0E', secret= r_shiny_account_token)

# Delete previous version of app
rsconnect::removeApp(appName = 'code/')

# Deploy app
rsconnect::deployApp('code/')

# ----------------------------------------------------------
# Using Github (extremely slow)

#library(shinylive)
#library(httpuv)

# Deploy app
# rsconnect::deployApp('code/app.R')
# shinylive::export(appdir = "code/", destdir = "docs/")

# Run app from server
# httpuv::runStaticServer("docs/app/", port=8008)

# If do this option, need to go to github repository --> settings --> pages (left side of page) --> check deployment out of the main branch and docs/ folder. app.R must be in "code/" to work.
