# Data Science Final Project – Baby Name Gender Trends

This repository contains the source code for our final project in DATASCI 306.

The project is an interactive Shiny web application exploring trends in baby names and their level of gender-neutrality or gender-dominance over time.

---

## Repository Structure

- `EXAMPLE1.R`  
  Data cleaner file allowing our main application to run on shiny.io

- `EXAMPLE2.R`  
  Main Shiny application file.

- `data/`  
  Folder for any data files used in the app (e.g. `names.csv`).

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

---

## How to Run Code

First, run the Cleaner file in order to clean up the data. Otherwise, there's too much data for shiny.io to process (it still works in RStudio regardless, but it won't work on shiny.io unless you run this step).

Next, run the main source code as normal and interact with the app as designed.




