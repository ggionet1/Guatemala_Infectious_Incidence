# Guatemala_Infectious_Incidence
Incidence of infectious disease in Trifinio, Guatemala as recorded by researchers in studies led by the University of Colorado/Incidencia de Enfermedades Infectuosas en Trifinio, Guatemala según investigacciones por la Universidad de Colorado

This public repository connects Redcap to an R Shiny app.
Este repositorio público crea una aplicación de R Shiny de datos en Redcap.

Steps/Étapas:

1. Create repository secrets holding a key for a Redcap API: On github, go to settings --> secrets --> actions --> new repository secret --> enter redcap API key
2. Ensure the key name you enter matches the key names used in main.yml
3. Main.yml will run every Monday morning, or every time new information is pushed to the github repository.
4. Redcap_data_processing.R makes publicly available excel files for use by R Shiny with no personal protected data.



