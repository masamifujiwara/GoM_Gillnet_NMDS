Nonmetric multidimensional scaling analysis of fish data from estuaries along the Texas coast. 
The work is associated with the following manuscript:

Title:
Changes in Species Compositions of Fish in the Bays of Northwestern Gulf of Mexico

Authors:
Milton Torres Ceron1,2, Masami Fujiwara1†, Fernando Martinez-Andrade3

DATA_G.Rdata contains the fish data along with environmental variables. 

DATAG: 600 observations of major_area (Bays), year, season, salinity, temperature, dissolved oxygen, turbidity, and different species of fish
    The numbers used in the column names are species ID numbers used by Texas Parks and Wildlife Department. You can find the corresponding scientific names and common names in SP2 file.
SP2: species-specific information, e.g. species code, scientific names, common names 

The R code should be run in the order of the initial alphabet (starting with b_BC_ANALYSIS.R).

Note:
a_getdata.R arrange the original Excel file we received from TPWD. However, the Excel file is not provided here. The code is provided to understand how the data were arranged. 
h_getmap.R is missing several shape files. You can change lines 11-14 and 42 based on the shape files you can find. 
