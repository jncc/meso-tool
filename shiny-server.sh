#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

chown -R shiny.shiny /srv/shiny-server

chmod 755 /srv/shiny-server

exec shiny-server >> /var/log/shiny-server.log 2>&1
