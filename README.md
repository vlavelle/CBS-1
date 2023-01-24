# Information
This dashboard was created with the end-goal of providing insight into mobility in the Northern Netherlands. Various indicators of mobility are visualised in multiple graphs within a dashboard in Shiny. Data is obtained from the CBS statline website, with a shapefile for maps taken from PDOK. 

Almost all data is imported directly into R using an available package created by CBS themselves, with the exception of one excel file which must be downloaded.

In order for the dashboard to run correctly, it is important that the global.R, server.R, ui.R and excel file "elektrische_personenauto_provincie" are all downloaded into the same location on your computer. Your working directory in R must then be set to this location such that the excel file can be read. 

The dashboard is also hosted on the shinyio platform, with the link to this being added soon.
