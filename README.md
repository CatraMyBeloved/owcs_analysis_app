# OWCS Analysis Tool

An interactive Shiny webapp for analyzing team compositions, hero pick/ban rates, and map-specific strategies in the Overwatch Championship Series tournament.

## Overview

This application provides an in-depth analysis interface for OWCS (Overwatch Championship Series) tournament data. It pulls from the team composition spreadsheet maintained by CommanderX and transforms this data into a relational database structure for more efficient analysis and visualization.

## Features

-   **Team Dashboard**: Team-specific hero preferences, signature heroes, map performance, and weekly trends

-   **Overview Analysis**: General hero pick rates and trends across tournament weeks

-   **Map Analysis**: Map-specific hero performance and composition trends

-   **Ban Analysis**: Hero ban frequency and patterns

-   **Interaction Analysis**: Hero synergies and counters to analyze good and bad matchups

-   **Match Prediction**: ML-powered predictions for match outcomes based on team matchups, maps, and bans

-   **Detailed Analysis**: Deep-dive comparisons of hero usage across teams and maps

## Tech Stack

-   R and Shiny for the web application framework

-   Tidyverse for data manipulation

-   ggplot2 with custom themes for data visualization

-   DT for interactive tables

-   AWS S3 for data storage and retrieval

-   Machine Learning:

    -   tidymodels, stacks for model building and ensembling

    -   ranger (Random Forest), C5.0 (Boosted Trees), glmnet (Logistic Regression)

    -   text2vec and textrecipes for feature engineering

## Data Source

The data comes from CommanderX's OWCS team composition Google Sheet, which tracks:

-   Team compositions for each match

-   Map selections

-   Ban/pick information

-   Match results

-   Regional information (NA, EMEA, Korea, China)

## Installation & Usage

This application is primarily hosted on shinyapps.io (link to be added). For local use:

1.  Clone this repository

2.  Open the project in RStudio

3.  Install dependencies (see packages.txt for full list):

    ```         
    install.packages(c("shiny", "bslib", "tidyverse", "DT", "aws.s3", "tidymodels", "ranger", "C50")) 
    ```

4.  Configure AWS credentials (for data fetching)

5.  Run the app.R file

## Project Structure

-   `app.R`: Main application file

-   `R/ui/`: UI module definitions

-   `R/server/`: Server logic modules

-   `R/analysis/`: Data analysis functions

-   `machine_learning_test/`: ML model development scripts

-   `scripts/`: Data update and processing scripts

## Development Roadmap

-   Add more advanced statistical analysis

-   Improve ML model performance and predictions

-   Add composition clustering functionality

-   Enhance data visualizations with interactive elements

-   Implement player-specific analysis when data available

## Screenshots

[Screenshots to be added]

## Contact & Questions

For questions or feedback, reach out to:

-   noidea100 on Twitch

-   derstein on Discord

## Contributing

Feel free to submit issues or pull requests if you have suggestions for improvements or new features.
