# Ski Lodge

## Requirements
Source Code: Data Preparation on R Markdown, Dashboard on RShiny


Libraries: googlesheets, readr, dplyr, ggplot2, shinydashboard

This project was created using R Studio.

## Overview
This project provides a live visualization into the operations of a hypothetical ski lodge.
The focus is on a description of previous sales and prediction of future sales.

The live visualization can be found [here.](https://csehdz.shinyapps.io/ski-lodge/)

## Technical Overview
The deployment of this project is based on R Shiny's live platform.
Refer to the documentation [here](https://www.shinyapps.io/) to build your own.


For persitent storage, this project leverages the GoogleSheets library.
Refer to the documentation [here](https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html) to learn how.

To connect R with the GoogleSheet use:
````
gs_data <- gs_key("COPY KEY FROM REGULAR SHARE LINK HERE", lookup = FALSE, visibility = "private")
````


**__Note: The persistent storage requires the sheet to be shared as [public](https://support.google.com/docs/answer/183965?co=GENIE.Platform%3DDesktop&hl=en).__**



