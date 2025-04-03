# OWCS Analysis Tool

An interactive Shiny webapp for analyzing team compositions, hero pick/ban rates, and map-specific strategies in the Overwatch Championship Series tournament.

## Overview

This application provides an in-depth analysis interface for OWCS (Overwatch Championship Series) tournament data. It pulls from the team composition spreadsheet maintained by CommanderX and transforms this data into a relational database structure for more efficient analysis and visualization.

## Features

-   **Overview Analysis**: General hero pick and win rates across all matches
-   **Team Analysis**: Team-specific hero preferences compared to league averages
-   **Map Analysis**: Map-specific hero performance and composition trends
-   **Composition Analysis**: Common team compositions and their success rates
-   **Ban Analysis**: Hero ban frequency and patterns

## Tech Stack

-   R and Shiny for the web application framework
-   Tidyverse for data manipulation
-   ggplot2 for data visualization
-   DT for interactive tables
-   AWS S3 for data storage and retrieval

## Data Source

The data comes from CommanderX's OWCS team composition Google Sheet, which tracks: - Team compositions for each match - Map selections - Ban/pick information - Match results - Regional information (NA, EMEA, Korea)

## Installation & Usage

This application is primarily hosted on shinyapps.io (link to be added). For local use:

1.  Clone this repository
2.  Open the project in RStudio
3.  Install dependencies: `tidyverse`, `shiny`, `bslib`, `DT`, `rdrop2`, `aws.s3`
4.  Configure AWS credentials (for data fetching)
5.  Run the app.R file

## Screenshots

[Screenshots to be added]

## Project Structure

-   `app.R`: Main application file
-   `R/ui/`: UI module definitions
-   `R/server/`: Server logic modules
-   `R/analysis/`: Data analysis functions
-   `scripts/`: Data update and processing scripts

## Development Roadmap

-   Add more advanced statistical analysis
-   Implement machine learning for prediction
-   Enhance visualization options
-   Add team vs team comparison tools

## Contributing

Feel free to submit issues or pull requests if you have suggestions for improvements or new features.
