## Welcome to the repository for the **Testing the predictions of reinforcement: long-term empirical data from a damselfly mosaic hybrid zone** article
### by
### 

### Repository contents:

Barriers_Calculation.xlsx: We used this excel file to estimate barriers starting from breeding experiment observations.

01_Absolutes directory contains all files related with absolute barriers plots and statistical analyses:

- bin: Contains all R scripts used to analyse the data. Scripts must be ran in the way they are numbered.
- data: Contains UTF-8 csv files from the Barriers_Calculation.xlsx excel file to be imported into R.
- figures: Contains powerpoint files with plots for final editions to be included in the publication.
- results: Contains text files with GLMs statistics tests results and DHARMA plots to check model fitting.

02_Cumulatives directory contains all files related the plotting of relative barriers and cumulative isolation:

- bin: Contains a single R script used to analyse the data.
- data: Contains a single UTF-8 .csv file from the Barriers_Calculation.xlsx excel file to be imported into R.
- figures: Contains prezygotic and postzygotic plots in powerpoint format for final editions.
	
03_Asymetries directory contains all files related with the asimmetries figure and statistical tests.

- bin: Scripts to estimate asimmetries, plot them and make GLM models for statistical tests.
- data: input data.
- results: Statistical tests in a text file and plot in a power point format file.
