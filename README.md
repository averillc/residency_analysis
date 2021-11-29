# residency_analysis
Sample code and data from my Master's project in GIS visualizing and analyzing university-level student residency data.

American Community Survey data is called via API from the Census, then merged with student data from an internal database. 

Student data is cleaned and recoded as necessary for joins, then exported as CSV file.

Students are then coded as residents or non-residents and anomalies between residency descriptions in different columns are identified for further exploration. 

Students are subsetted by their academic standing in order to understand if there was any geographical or course level correlation with academic performance.

Various subsets are created to provide descriptive statistics for a white paper report.
