In this repository there is all the R-code used for the manuscript "Dissenting the environmental variables that determine breeding success of Griffon Vulture Gyps fulvus in Sardinia, Italy". 

In the script “Figure 1” we can find the code to generate a location map of the study area; 

In the “Figure 2” script there is the code to generate the ggplot plot of the time-series of the breeding success data; 

In the script "INLA_model" there is the code for run the final selected model, as well as the exploratory analysis of the data which includes the analysis of the correlation and collinearity of the explanatory variable used in the model. 

The Figure 3 of the manuscript is generated from the output of the model and the code is included at the end of the script. 

The script “HighstatLib” is called from the script "INLA_model" and charge a function to run collinearity analysis. 

The script “Bdiclcpomodel_stack” is called from the script "INLA_model" and charge a function to run a model selection based of the WAIC and LCPO criteria.

Finally the script “Figure_ S1” includes the code of the Figure 1 of the supplementary materials to maps environmental variables 
