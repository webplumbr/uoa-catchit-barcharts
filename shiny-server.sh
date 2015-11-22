#!/bin/sh

mkdir -p /var/log/shiny-server
chown -R shiny:shiny /var/log/shiny-server
chown -R shiny:shiny /srv/shiny-server

sudo -u shiny shiny-server >> /var/log/shiny-server.log 2>&1

