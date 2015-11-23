#!/bin/sh

chown -R shiny:shiny /srv/shiny-server
sudo -u shiny shiny-server >> /var/log/shiny-server.log 2>&1

