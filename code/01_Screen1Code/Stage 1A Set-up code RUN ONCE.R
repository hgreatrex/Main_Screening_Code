rm(list=ls())

#========================================================================================================
# This code creates a .rData file of abstracts for systematic review.
# Originally developed for characterize the existing research on global 
# flash flood vulnerability, risk, and exposure. 
#========================================================================================================
library(bibliometrix)
library(stringr)
library(RISmed)
library(bib2df)
library(knitr)
library(rmdformats)


#-----------------------------------------------------------------------------------
# PREPARATION
#
# We use "bibliometrix" to convert a .bib files from Web of Science into an .RData format
# We also want to merge the smaller .bib files together
# 1) List the files in the 01_Web-Of-Science directory,  using pattern to screen out .bib files, 
#-----------------------------------------------------------------------------------
InputFileNames <- list.files("data/01_Web-Of-Science_Output",pattern=".bib",full.names =TRUE)


#-----------------------------------------------------------------------------------
# 2) Use a for loop to cycle through each file
#-----------------------------------------------------------------------------------
for(n in 1:length(InputFileNames)){
   
   # Choose the "nth" file in folder
   # Print out a message on the screen saying i'm doing it.
   message(paste("reading in ", InputFileNames[n]))
   
   # Read in the data, 
   # dbsource = "isi" indicates from which database the collection has been downloaded (in this case Web of Science)
   tmpinput <- convert2df(InputFileNames[n],dbsource = "isi", format = "bibtex")  
   
   # if n = 1 make a new output container, otherwise add the new rows to the end of data_bib
   if(n == 1){
      data_bib <- tmpinput
   }else{
      data_bib <- merge(data_bib,tmpinput,all=TRUE)
   }
   
}
# clean up to just leave the final dataset
rm(list=c("tmpinput","n","InputFileNames"))


data_bib$AB <- str_to_sentence(data_bib$AB)
data_bib$AU <- str_to_title(data_bib$AU)
data_bib$TI <- str_to_sentence(data_bib$TI)


#-----------------------------------------------------------------------------------
# Now set up the main columns for the screening tagging.  
#-----------------------------------------------------------------------------------
data_bib$Screen1_Assessed           <- FALSE
data_bib$Screen1_MissingDetails     <- FALSE
data_bib$Screen1_GoldStar           <- FALSE
data_bib$Screen1_Reject             <- FALSE
data_bib$Screen1_RejectRain         <- FALSE


#-----------------------------------------------------------------------------------
# Add in flood related things. these will change depending on what is needed for that topic
# They tend to be created iteratively as the papers are screened
#-----------------------------------------------------------------------------------
data_bib$Screen1_Model  <- FALSE
data_bib$Screen1_Social <- FALSE
data_bib$Screen1_Event  <- FALSE
data_bib$Screen1_Notes  <- ""


#----------------------------
# And save
#----------------------------
save(data_bib, file = "data/MainScreeningData_Screen1.RData")
save(data_bib, file = "data/02_Data-ready-to-screen/Bib01_ReadyToScreen.RData")

