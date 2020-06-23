# FLIMDAST
_**FLIM DA**ta**S**et **T**ool_ to analyse fluorescence lifetime changes in a time course FLIM experiment

## Description:
*FLIMDAST* is a user interface app that assembles pre-processed fluorescence lifetime imaging (FLIM) data from a time course FLIM experiment and performs the experiment-wide visualisation and quantification of changes in fluorescence lifetimes of each cell/object, compared to its reference measurement.

### Features of _**FLIMDAST**_:
 - Analysis of FLIM data generated by the Time-Correlated Single Photon Counting (TCSPC)-based fluorescence lifetime measurements. 
 
 - Input data in a ASC or TXT format. For each measurement, at least two files are required, containing values of fluorescence lifetimes and photon numbers, respectively, in pixel positions of FLIM image. Typically, these are SPCImage export files in ASC format, or their ImageJ/FIJI derivatives saved in the text image format. 
 
 - Up to two additional regions within measured cells/objects. Each selected region should be provided as an additional text image file with photon numbers in pixel postions, generated in ImageJ/FIJI.
 
 - Visualisation of the intensity-dependent bias in fluorescence lifetime measurements characteristic for TCSPC, by plotting fluorescence lifetimes in each pixel versus the number of detected photons in the same pixel, with optional color-coding of selected regions. The intensity-dependend bias is apparent as a slop in the fluorescence lifetime distribution along the photon number axis.
 
 - Visual and quantitative comparison of fluorescence lifetime distributions between two FLIM measurements, one of which acts as a reference.  These measurements should be of the same cell/object.
 
 - Visual comparison between two FLIM measurements by overlaying the scatterplots that plot fluorescence lifetime versus photon number for each measurement. In this representation, the down- or upward shift of the overlay relative to the reference indicates change in fluorescence lifetime that is independent of fluorescence intensity bias, while a near-horisontal shift of the overlay within the plane of the reference scatterolot indicates change that is due to altered fluorescence intensity. 
 
 - Quantification of fluorescence intensity-independent change in the fluorescence lifetime of an object, relative to a reference measurement of the same object, using the following algorithm: 
  > 1) each dataset is ordered by photon numbers and fitted with a local polynomial regression (loess, span = 0.1) that smoothens the distribution of fluorescence lifetime into a single line. It is furher converted to a sequence that has a single value of fluorescence lifetime for each sequential (integer) number of photons. For this, all fluorescence lifetime values that correspond to the same photon numbers are averaged to produce a single value, and the missing fluorescence lifetime values are filled by the average between fluorescence lifetime values from the next and the previous photon number that has such value; 
  > 2) the range of photon numbers between the lowest and the highest 99.5 percentile for each distribution is calculated, and their overlap is set as the common intensity range for two distributions. It can be further manually narrowed by the user;
  > 3) the change in fluorescence lifetime is calculated as the difference between the sums of all fluorescence lifetimes within the common intensity range for each fitted sequence, divided by the common intensity range.

- Flexible user-designed experiment layout, allowing for multiple experimental conditions/groups, multiple cells/objects per condition and multiple time points. 

- Adjustable visualisation and quantification parameters.

- A single step experiment-wide plotting and/or quantification of fluorescence lifetime changes in all measurements relative to their allocated references, with the user-desiged parameters.

- Quantification of mean values for the average fluorescence lifetimes and the average number of photon per pixel for each dataset in the experiment.
 
## Requirements:
_FLIMDAST_ is developed for R version 3.3.3; other versions were not tested

It is recomended to run _FLIMDAST_ from RStudio. We use RStudio version 1.1.383; other versions were not tested

The _FLIMDAST_ requires the following R packages to be installed before running:
 - shiny
 - shinyFiles
 - DT
 - plyr
 - dplyr
 - ggplot2
 - abind
 - colorpicker
 - shinydashboard
 - shinyWidgets

Data files for each measurement: 
 - A pair of SPCImage export files, containing matrices of fluorescence lifetime ("...\_t1.asc" file) and photon numbers ("...\_photons.asc" file) in pixel positions, respectively. These files are exported from SPCImage (Becker&Hickl) after fluorescence lifetime in each pixel of FLIM measurement has been quantified by fitting a single- (preferred) or a two- exponential decay model. In case of two-exponential fitting, the fluorescence lifetime input file containing mean fluorescence lifetime matrix needs to be generated from four SPCImage export files, "...\_a1.asc","...\_t1.asc", "...\_a2.asc" and "...\_t2.asc" in ImageJ/FIJI, either manually or using the specifically designed tm_2cFLIM.ijm macro (https://github.com/DinaDikovskaya/FLIMacro). 
 - Additional files with selected regions of interest.  Each selected region is provided as a separate file produced in ImageJ/FIJI from the photon number "...\_photons.asc" file, by saving it as a text image file after selecting an area of interest and setting all pixel values outside the selected area to zero.

## Installation:
Download the content of this repository, by clicking "Clone or download" and choosing "Download Zip".
Unzip downloaded folder, find the FLIMDAST folder inside it.
Place the FLIMDAST folder into a working directory on your computer. 
Open RStudio. Install required R packages that has not been installed previously. For this, in a Console of RStudio, type:

<p>install.packages(“shiny”)<br>
install.packages(“shinyFiles”)<br>
install.packages(“DT”)<br>
install.packages(“plyr”)<br>
install.packages(“dplyr”)<br>
install.packages(“shinyWidgets”)<br>
install.packages(“shinydashboard”)<br>
install.packages(“colorpicker”)<br>
install.packages(“abind”)<br>
install.packages(“ggplot2”)

## Examples:
The folder "examples" contains files generated from three repeated FLIM measurements of the same live cell overexpressing proteins tagged with a superfolder GFP. Fluorescence lifetime of superfolder GFP was measured using a Scanning Confocal Microscope (Zeiss) equipped with a pulsed laser and a SimpleTau TCSPC module (Becker&Hickl). 

Each measurement is represented by 5 files that have the same base name, FLIMdataNN_”(where NN is the measurement number), followed by an extension. 

The files with extension "t1.asc" are the fluorescence lifetime files, containing matrices of fluorescence lifetime values in pixel positions, exported from SPCImage software (Becker&Hickl) after processing the original measurement files(not provided). The fluorescence lifetimes in these files were determined using 1-component exponential decay fit. 

The files with extension “photons_ez.txt” are the photon number files for the entire cell. These files were generated in FIJI from the files with extension  ”photons.asc” exported from SPCImage, which contain matrices of number of photons in pixel positions and depict cell morphology. The “photons_ez.txt” files have the same photon number values as ”photons.asc” files in pixels that belong to cellular area, and the value of zero in pixels outside that area. 

Two additional files for each measurement, with extensions “photons_cz.txt” and “photons_nz.txt” are the photon number files for cytoplasmic and nuclear areas of the cell, respectively. They have been generated similarly to the “photons_ez.txt” files, except that values of all pixels outside of cytoplasm (for “photons_cz.txt”) or nucleus (for “photons_nz.txt”) were set to zero. 

To use example files, place the "examples" folder in any accessible location on your computer.

## Starting _FLIMDAST_:
#### Method 1.
Make sure that RStudio's working directory contains downloaded FLIMDAST folder. To check, in a Console of RStudio, type:

list.files(path = getwd())

If the FLIMDAST folder is not listed, set up the correct working directory: under Session menu (top ribbon in RStudio), select "Set Working Directory", select "Choose Directory", navigate to the directory that contains FLIMDAST folder (but not to the FLIMDAST folder itself!) and click "Open". 

In a Console of RStudio, type: 

<p>library("shiny")<br>
runApp("FLIMDAST") 

#### Method 2.
Load app.R script from FLIMDAST folder in RStudio: under File (top ribbon in RStudio), select "Open", navigate to the FLIMDAST folder, and select app.R file inside it. The script will be loaded into a Source pane of RStudio. At the top of the script, click "Run App".

With both methods, the FLIMDAST will start in a local host window.  If desired, open FLIMDAST in a default web browser: click "Open in Browser" at the top of the window. Both options are very similar, apart from handling file downloads. 

Internet connection is not required for running downloaded FLIMDAST.  

## Running _FLIMDAST_:
In the Experiment Layout tab, indicate the desired number of experimental conditions/groups, the number of additional regions of interest within cells, and the number of time points in the time course. If using the example datasets, choose 2 additional regions.

In the Single Object Time Course tab, select the condition, time point and the reference status of first measurement, and add locations of the files describing it by clicking the appropriate “Choose file” buttons and navigating to the requested files. If using the example datasets, select one of the files with extension “t1.asc” for the fluorescence lifetime file button. Switch the “autofill file” switch on. This will automatically detect the other three files for the same measurement (eg the photon number files for the entire cell and for the two selected regions) within the folder with example files, following the pre-set name replacement rule.

To fill table with selected file locations for one measurement, click “Add”. Repeat for other measurements (time points) of the same cell/object. If using example dataset, chose one of the measurements that is different from the reference file. 

Once all file locations for all time points of the object/cell are added to the table, click “Save to main table”. This will send all entries in the table the Main Table tab, empty the Single Object Time Course table and reset the cell number. If desired, repeat filling the Single Object Time Course table with data locations for more cells and sending them to the Main Table, until all files in the experiment had been located. 

In the Main Table tab, inspect the table. It can be saved, emptied or uploaded from previously saved file.

In the Main Table or the Single Object Time Course tab, highlight two entries in the table, one of which should be a reference and the other - a non-reference measurement, and click “Use for settings”.  This will populate the Plot and Quantify tabs with the datasets from these entries. 

In the Plot tab, select overlay settings (including the types of datasets, the number of overlays and their colors, as well as plots scales), and click “Use these settings”. Repeat if more plot layouts are desired. Click “Plot all data” to apply selected designs to all non-reference measurements in the Main table. 

In the Quantify tab, select settings to quantify changes in fluorescence lifetime, including the type of datasets and optional limits to the common intensity range. The “data”/”model” switches change the plot display without affecting quantifications. Click “Use these settings”. Repeat if additional  quantifications with different settings are desired. Click “Quantify all data” to apply selected settings to the entire main table dataset. This will produce a Result table with the values of fluorescence lifetime changes (“shift”) for each non-reference measurement, and the values of mean fluorescence lifetimes and mean photon numbers per pixel for each analysed dataset. Click “Download Results” to download it.

To stop _FLIMDAST_, close the local host window, or press the red "stop" button at the top of Console pane in RStudio


