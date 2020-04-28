## Installation

#### Required R libraries:
- bnlearn
- DT
- ggplot2
- graph
- htmltools
- kableExtra
- knitr
- magrittr
- openxlsx
- plotly
- processx
- RColorBrewer
- shiny
- shinyBS
- shinycssloaders
- shinydashboard
- shinydashboardPlus
- shinyjs
- stringr
- vizNetwork
- zip
```
install.packages(c("bnlearn", "DT", "ggplot2", "graph", "htmltools", "kableExtra", "knitr", "magrittr", "openxlsx", "plotly", "processx", "RColorBrewer", "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinydashboardPlus", "shinyjs", "stringr", "visNetwork", "zip", "devtools", "modules","plyr"))
devtools::install_github("ropensci/plotly")
```

#### ORCA for downloads:
- NodeJs (v8)
- electron
- orca
```
npm install 
export PATH=`pwd`/node_modules/.bin:$PATH
```
NOTE: remember to export the path when running the application so that R can find orca

#### Start script (optional)
Assumes application runs under the `shiny` account
```
#!/bin/bash

if [ "$(whoami)" != "shiny" ]; then
    sudo -u shiny $0
    exit 1
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

export PATH=/srv/shiny/bin:$PATH

screen -dmS MESO R --vanilla -e "shiny::runApp('app.R', host = '0.0.0.0', port = 6376)"
```