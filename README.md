
# Cost-CFS

Cost-CFS contains all the codes used in McCombe et al, Accuracy-time optimisation with cost-sensitive feature selection for Alzheimer's disease diagnosis - 2021

## Requirements

The code uses data from ADNI (adni.loni.usc.edu). The ADNIMERGE R package from ADNI must be installed to use this code with the ADNI data.

All code was written in R version 4.0.5 Shake and Throw, with the exception of the barchart in Figure 4, which was generated with Matlab 2020a

R packages used are listed at the beginning of each step in the code. 

## Usage
### Data Preparation Steps
With ADNIMERGE installed in R, begin by running Step 1 to prepare the class variable and AGE variable. Then Step 2 to merge the tables with cognitive and functional assessments. 

### Feature Selection and Evaluation of Selected Feature Sets

Run Step 3 to perform feature selection. This is the most computationally intensive step. Step 3 will generate a CSV file ("assigntimes.csv" containing all the items selected more than once by feature selection, which can be edited to assign time costs to these items. 
Step 4 evaluates the mean multiclass AUC of each selected feature set in turn and saves the results in a matrix. 

See "Figure 1.pdf" in this repository or Figure 1 in the paper for a graphical schema of Steps 3 and 4. 
The feature sets selected by Step 3 along with the AUC evaluated by Step 4  are shown in the first worksheet of the Excel file JTEHM_McCombe_data.xslx. 

### Generate Graphs
Step 5 creates the circular bar chart in Figure 3 of the paper. 
The Matlab code creates the bar chart in Figure 4 of the paper.


### Perform Cost Sensitive Feature Selection
The cost sensitive feature selection function cost_cfs was adapted from the CFS algorithm in the FSelector package:

https://cran.r-project.org/web/packages/FSelector/FSelector.pdf

Run Step 6a to load some functions which are called by cost_cfs (all taken from the FSelector package).
Run Step 6b to load cost_cfs itself.
Run Step 6c to perform cost sensitive feature selection on this data, over a range of values of the lamda parameter. 
Step 6c requires a csv file as input ("times.csv") with feature names and associated estimated feature costs. We have included our instance of times.csv in this repository.

The feature sets selected by this step, with associated AUC and estimated total assessment time, are shown in the second worksheet of the Excel file JTEHM_McCombe_data.xslx.

### Generate Graphs for Cost Sensitive Feature Selection

Step 7 generates a data frame with estimated assessment time (from the literature) and estimated AUC (from our pipeline) for each of FAQ, MMSE, MoCA, and ADAS.. 

Step 8 will generate the graph found in Figure 5 of the paper, using output from Step 6 and Step 7.

## RShiny App
The app.R file contains the code for the RShiny app found at
https://mac-n.shinyapps.io/costcfs/
The data for this app is not included in this repository as it is ADNI data.
Steps 6a and 6b must be run before app.R, to load costcfs and the functions it depends on into the environment. 

A video to demonstrate the use of this app can be found in this repository as "App Demo.mp4"
