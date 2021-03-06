# GSL_COVID_tracker_paper
Repository for code used for analysis and visualizations for GenderSci Lab's paper on gender/sex disparities in COVID-19 cases and fatalities, using the [GSL U.S. Gender/Sex COVID-19 Data Tracker](https://www.genderscilab.org/gender-and-sex-in-covid19).

Code was written by Katharine MN Lee and Tamara Rushovich with guidance from Luke Miratrix on MLM analyses.

The code assumes you are working from your own directory, with scripts in "scripts" directory and placing placing data into a "data" directory as it is generated and putting figures into their own appropriate directory locations.

Verify you have created these directories before attempting to run code, or you will likely encounter errors!

The basic work flow (and pertinent notes) is as follows:
- Step1: Make datasets
  * Step1a: grabs data from (View-only) googlesheet of the validated dataset and from the ACS census. You will need an api key from [here](https://api.census.gov/data/key_signup.html) for part of this script. This script grabs data from an archived duplicate google sheet, and it is current as of May 24, 2021.
  * Step1b: grabs data from (View-only) googlesheet of the unvalidated dataset and from the ACS census. You will need an api key from [here](https://api.census.gov/data/key_signup.html) for part of this script. This script grabs data from an archived duplicate google sheet, and it is current as of May 24, 2021.
  * Step1c: Combines the datasets from step 1a & 1b together. You will need an api key from [here](https://api.census.gov/data/key_signup.html) for part of this script.
  * Step1d: creates a dataset from the CDC data, which is monthly. 
- Step2: Make Graphs of the data using weekly and cumulative and U.S. overall data.
- Step3: Assemble the dataset that presents "by-wave" data, aggregating within time periods that roughly align with "waves" of the pandemic (dates & references in code)
- Step4: Code that generates the maps presented in manuscript.
- Step5: Code used to generate numbers presented in the Results section.
- Step6: perform multilevel regression model & model comparisons. <-- This file is still missing from repository
