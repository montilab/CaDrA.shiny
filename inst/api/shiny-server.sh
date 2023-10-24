#!/bin/bash

RESOURCE_ROOT=/CaDrA.shiny

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
then
    # push the "real" application logs to stdout with xtail in detached mode
    exec xtail /var/log/shiny-server/ &
fi

# Start plumber api server
exec Rscript $RESOURCE_ROOT/inst/api/run_cadra_api.R 2>&1
