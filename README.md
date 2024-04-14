## Welcome to the repository for the **Testing the predictions of reinforcement: long-term empirical data from a damselfly mottled hybrid zone** article

### by Luis Rodrigo Arce-Valdés, Andrea Viviana Ballén-Guapacha, Anais Rivas-Torres, Jesús Ramsés Chávez-Ríos, Maren Wellenreuther, Bengt Hansson & Rosa Ana Sánchez Guillén

### Objective:

In this manuscript we tested Turelli's et al. prediction 2014 on the damselflies *I. elegans* and *I. graellsii* to evaluate the presence of reinforcement of reproductive isolation in their Spanish mottled hybrid zone.

### Repository contents:

This repository is divided into two main sections. Analyses using the global dataset which are found in the `02_Regions` directory and the populations dataset found in the `01_Localities` directory. Whereas in the former data was compared between allopatric and sympatric crosses, in the latter data was analysed per cross of populations to assess the consistency of reproductive barriers measurements across geographical replicates.

On each sub-directory the following elements can be found:

`Barriers_Calculation.xlsx`: We used this excel file to estimate barriers starting from breeding experiment observations.

`01_Absolutes` directory contains all files related with absolute barriers plots and statistical analyses:

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

Finally, the `00_BasePPTX` directory contains blank powerpoint slides with the sizes employed for our plot exports.