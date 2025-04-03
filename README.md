# OWCS Analysis Tool

This Shiny-Webapp uses the team composition googlesheet maintained by CommanderX as a source of team composition data in the OWCS tournament. It provides a more in depth interface to access aggregated and calculated values. Behind the scenes, the data is stored in a collection of relational tables.

# Installation

This application should (hopefully) be hosted on shinyapps.io. There is no need for manual installation. If you prefer to run this app locally, it is suggested you use [RStudio](https://posit.co/download/rstudio-desktop/ "RStudio download"). Clone the repository using git, or create a new Rproject from source control. You can run the app.R file using RStudio. However, you will have to use your own AWS S3 key and secret key since i am too stupid to implement secrets with github.

# User guide

The app is divided into different analysis tabs, each with their own filters. Chose an analysis, adjust filters, and go. There is not much to it yet, but i intend to add more functionality whenever i get around to it. 
