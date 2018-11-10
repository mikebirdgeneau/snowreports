#!/bin/sh
cd /home/snowreports/snow-reports.ca/
/usr/bin/Rscript /home/snowreports/snow-reports.ca/src/cron/website_hrdps_update.R
/usr/bin/Rscript /home/snowreports/snow-reports.ca/src/qaqc/file_update_check.R
