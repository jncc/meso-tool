# Install R version 3.6.1 and shiny server
FROM rocker/shiny-verse:3.6.1

ARG proxy_setting

ENV http_proxy $proxy_setting
ENV https_proxy $proxy_setting 

#Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    gdebi-core \
    curl \
    software-properties-common 

RUN apt-get update && apt-get install -y \
    curl \
    software-properties-common 

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs

# Install R packages that are required!
RUN R -e "install.packages(c('BiocManager', 'bnlearn', 'DT', 'ggplot2', 'htmltools', 'kableExtra', 'knitr', 'magrittr', 'openxlsx', 'plotly', 'processx', 'RColorBrewer', 'shiny', 'shinyBS', 'shinycssloaders', 'shinydashboard', 'shinydashboardPlus', 'shinyjs', 'stringr', 'visNetwork', 'zip', 'devtools', 'modules','plyr'))"
# Install graph package (because it's in BiocManager, not in the default repo)
RUN R -e "BiocManager::install('graph')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server

# Install npm modules
WORKDIR /srv/shiny-server
RUN npm install 

# Make the ShinyApp available at port 3838
EXPOSE 3838
EXPOSE 8787

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]
