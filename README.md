# VR-Training Project

**Author:**

-   Arsine Chinaryan: [Arsine.Chinaryan\@campus.lmu.de](mailto:Arsine.Chinaryan@campus.lmu.de){.email}
-   Anh Hoang Minh Dang: [An.Dang\@campus.lmu.de](mailto:An.Dang@campus.lmu.de){.email}
-   Iris Dizdari: [Dizdari.Iris\@campus.lmu.de](mailto:Dizdari.Iris@campus.lmu.de){.email}
-   Margarita Khachatryan: [M.Khachatryan\@campus.lmu.de](mailto:M.Khachatryan@campus.lmu.de){.email}

## General instruction

This is the repository for the VR-Training Project. Please read these instructions carefully to ensure optimal program performance before running the code.

We use R version 4.3.2. Some of the packages in this project may require the latest version of R, so please ensure it is installed before proceeding.

## Usage

Please make sure to source the files in the following order:

1.  environmentSetup.R: This will install all the necessary packages, import the required functions and data sets to run the program.

The following files are all in the Analysis folder:

2.  dataProcess.R: This will clean, process, and create the data sets that are used for further analysis. All these data sets are exported to "Data/ProcessedData".

3.  makingCorrelationTables.R: This will create correlation tables for further analysis. All these tables are exported to "Data/ProcessedData".

4.  cohortDifferences.R, correlationPlot.R, correlationRoundPlot.R, subgroupPlot.R: These will analyse and produce plots, using the processed data sets and tables, to answer the analytical questions. All these plots are exported to folder "Result".

## Directory structure

### Root directory

-   README.md
-   environmentSetUp.R
-   Files for the Presentation:
    -   presentation.qmd

    -   presentation.html

    -   customstyle.css: Used for customizing the style in the presentation.

    -   VR Training.jpg & VR Training-2.jpq: Photos for the presentation

### Data

All the files in this directory are data that are used in this project.

-   rawData: This subfolder contains all the raw data provided for the project.

-   dataDescription: This subfolder includes files that were provided as is; no processing or changes have been made. It contains analytical questions, raw data documentation, and explanations about the VR project background.

-   processedData: All the files in this subfolder are csv files. These are all cleaned, processed and created from the raw data for further analysis.

### Script

All the files in this directory are R scripts containing general functions essential for running the program. These include tasks like cleaning and preparing data sets, creating tables data frames for further analysis:

-   calculateBMI.R

-   cleanData.R

-   computeAndSaveCorrelation.R

-   createScatterPlot.R

-   fixPMD.R

-   pivotedData.R

Further detailed explanations of what each function does will be clearly provided in the description within each script itself.

### Analysis

All the files in this directory are R scripts containing further analysis based on the processed data and tables, using the predefined functions in folder "Script". These include tasks like analyzing, producing and exporting plots and visualization:

-   cohortDifferences.R

-   correlationHeatmaps.R

-   correlationPlot.R

-   correlationRoundPlot.R

-   dataProcess.R

-   makingCorrelationTables.R

-   scatterPlotsForQ3Q4.R

-   subgroupPlot.R

Further detailed explanations of what each script does will be clearly provided in the description within each script itself.

### Result

All the files in this directory are images under png format. These are the exported plots and visualization that are created in Analysis. Due to the requirements of the project, especially to analyse the subgroups' behavior and the comparison between two cohorts, we have to create as many plots as possible to be able to compare them and draw the final decision in choosing which plots are important enough to put in the presentation and the final report. Therefore some plots in this directory do not appear in the presentation and the final report.

The plots are divided into 4 subfolders according to 4 analytical questions:

-   Q1: Are there differences between the 2 data cohorts, Linne and Dame?

-   Q2: How are the stress indicators and the physiological measurements related?

-   Q3: Does this correlation change over the rounds?

-   Q4: Are there subgroups within the test subjects that stand out from the rest?
