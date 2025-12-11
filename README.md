# Data Science Final Project – Baby Name Gender Trends

This repository contains the source code for our final project in DATASCI 306.

The project is an interactive Shiny web application exploring trends in baby names and their level of gender-neutrality or gender-dominance over time.

---

## Repository Structure

- `Data_Cleaning.R`  
  Data cleaner file allowing our main application to run on shiny.io

- `Main_Code.R`  
  Main Shiny application file (source code).

- `README.md`  
  This file, with instructions on how to run the app locally.

---

## Requirements

To run this project locally, you need:

- **R** (version 2025.09.1+401 or higher)
- **RStudio** (optional but recommended)
- The following R packages:
  - `shiny`
  - `dplyr`
  - `ggplot2`
  - `babynames`
  - `tibble`

---

## How to Run Code

First, run the Data_Cleaning file in order to clean up the data. Otherwise, there's too much data in our dataset for shiny.io to process (it still works in RStudio regardless, but it won't work on shiny.io unless you run this step).

Next, run Main_Code (main source code) as normal and interact with the app as designed.




