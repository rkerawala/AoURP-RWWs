# AoURP-RWWs
This repository serves to provide access to the code used for a summer internship project 
examining data from the All of Us Research Program's Researcher Workbench Workspaces.

# Packages used
dplyr, readxl, tidyverse, tidyr

# Usage
Code in the aourp_analysis.R file is specific to analysis of the Researcher Workbench workspace descriptions attained
from the All of Us Research Program's Research Projects Directory: https://www.researchallofus.org/research-projects-directory/
Various categories of the workspace descriptions are available at aourp_figure_data.xlsx.

Code was generated using the above packages in R for the following analyses:

Deduplication of workspace descriptions
Disease-Focused v. Non Disease-Focused Research
Counting Race/Ethnicity Categories
Counting Age Categories
Counting Populations of Interest
Counting Research Purposes
Counting Institutional Affiliation
Fisher's Exact Test for race/ethnicity 
Using grepl() to search for specific terms to include in each disease condition category

# Acknowledgements
This work could not be done without the contribution of the participants of the All of Us Program, the researchers utilizing All of Us
Research Program data on the Researcher Workbench, and the Data and Research Center in providing access to the Research Projects Directory information.

# License
Distributed under MIT License. More information available at LICENSE.txt 
