library(rsconnect)
library(shinylive)
library(httpuv)

<<<<<<< Updated upstream
getwd()

=======
>>>>>>> Stashed changes
# Connect the r script below to an R shiny app
rsconnect::setAccountInfo(name='cutrifinio', token='C0BFCA28C1FE0F83CB5E67B30FA01E0E', secret='b6lWE/Jk2CFw+J5Vu6F3zXxgyk/AsEu9iuVcBGxG')

# Deploy app
#rsconnect::deployApp('code/app.R')
shinylive::export(appdir = "code/", destdir = "docs/app/")

# Run app from server
httpuv::runStaticServer("docs/app/", port=8008)
