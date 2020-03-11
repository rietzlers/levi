# alloy-levi

![status](https://img.shields.io/badge/status-under_development-orange)

Shiny-Web-App zur Analyse von EML-Experimenten

# Usage

1. Online: Visit https://rietzlers.shinyapps.io/levi/ to use the app. 
2. Lokal (Bei dieser Option müssen gegebenenfalls Pakete nachinstalliert werden.)
    - run `shiny::runGitHub('levi', 'rietzlers', subdir = "inst/levi/")` in a R-Console.</br> 
    (Paket wird jedesmal neu heruntergeladen, ist also automatisch aktuell.)
    - Installiere Paket lokal (`devtools::install_github("rietzlers/levi")`) und starte die App mit
    `shiny::runApp(system.file('levi', package='levi'))`





