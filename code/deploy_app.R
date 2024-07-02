# ---------------------------------------------------------
# Using shinyapp.io
library(rsconnect)

# Save secret token for connecting to R shiny account
r_shiny_account_secret <- Sys.getenv("r_shiny_account_token")
r_shiny_token <- Sys.getenv("r_shiny_token")


# Connect the r script below to an R shiny account
rsconnect::setAccountInfo(name='cutrifinio', token=r_shiny_token, secret= r_shiny_account_secret)

# Deploy app
rsconnect::deployApp('code/', forceUpdate = TRUE, launch.browser = FALSE)

# Restart app (fixes timeout error)
restartApp('code/')

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
