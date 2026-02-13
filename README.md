# yuba-phys

Tree physiology & functional traits along tree size and climatic gradients in the North Yuba landscape

## Data management approach for this repo

The data are stored in Dryad (link). Copy this folder to your computer, and download data to the location of this folder on your computer.

## Analysis approach

**01_trait-data-carpentry.R:** Load trait data collected on conifer seedlings and saplings in the North Yuba, Tahoe National Forests, impute missing data from linear models, and calculate new traits from exisiting data. Save resulting data frame to 'yuba-phys-prepped-field-data-2024_2025.csv' to be used for regenerating data analysis and figures. Only needs to be run once.

**02_vulnerability-curves.R:** Generate vulnerability curves for each tree sampled for P50 in the dataset, save result to a data frame in the environment named 'coef_df' that retains P50 values for each tree ID from generated curves, as well as confidnce intervals. Input data is prepared hydraulic conductance measurements 'yuba-phys-vulnerability-curve-cleaned-data-2024_2025.csv.'

**03_data-analysis.Rmd** Generate figures for the distribution of data (tree size and environmental gradients), and run linear mixed models to regenerate statistics for accompanying paper. Input data includes prepared trait data 'yuba-phys-prepped-field-data-2024_2025.csv'. 

**04_manuscript-figures.Rmd** Regenerate figures from accompanying paper. Input data includes prepared trait data 'yuba-phys-prepped-field-data-2024_2025.csv'.


