# alloy-levi

![status](https://img.shields.io/badge/status-under_development-orange)

Shiny-Web-App zur Analyse von EML-Experimenten

# Usage

1. Online: Visit https://rietzlers.shinyapps.io/levi/ to use the app. 
2. Lokal (Bei dieser Option müssen gegebenenfalls Pakete nachinstalliert werden.)
    - **old version** Installiere Paket lokal mit dem Befehl`devtools::install_github("rietzlers/levi", ref = "v0.1.0")` 
    und starte die App mit `shiny::runApp(system.file('levi', package='levi'))`
    - **Modified Version**: Installiere Paket lokal mit dem Befehl `devtools::install_github("rietzlers/levi", ref = "master")` 
    und starte die App mit `shiny::runApp(system.file('levi', package='levi'))`
3. **Installation on shiny-server**
    - install package with `devtools::install_github("rietzlers/levi", ref = "v0.1.0")` or  
    `devtools::install_github("rietzlers/levi", ref = "master")`  (depending on which version you prefer)
    on the machine on which the shiny-server runs. (An up to date version of R and R-Studio should be installed on this
    machine.) Make sure that all packages on which the package levi depends are installed and 
    up to date.
    - navigate to R-packages-installation-folder: there should be a folder named 'levi'.
    In this folder is a subfolder also named 'levi' copy this folder into the folder:
    /srv/shiny-server/
    - start the app by navigation to http://localhost:3838/levi/ (if you are on the
    same machine as the shiny-server) or to http://shiny-server:3838/levi/ 
    (where shiny-server is name/IP of the machine on which the shiny-server runs)in your browser.





