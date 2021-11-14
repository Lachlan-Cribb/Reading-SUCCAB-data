
#### INTRODUCTION ####

# This script is for reading the .edat files from the SUCCAB computers and processing it 
# so that it can be merged with the overall ARCLI study datasets. 

## Load packages - before you run library("package.name")
## you must first run install.packages("package.name")
## but only need to do this once 

library("tidyverse")
library("rprime")
library("wrapr")
library("writexl")
library("haven")


## First set the working directory to where the text 
## files are located. From 'session' in menu, select 'set
## working directory'

setwd("PASTE PATH HERE")

## Load 'edat reading functions'. This is the other R script that is in
## in the SUCCAB section of the dropbox. 
## Paths to files can be accessed by running file.choose()
## and selecting the file through the pop-up

file.choose()

source("PASTE PATH HERE")

## Find all files in working directory ending in .txt

files <- dir(pattern = "*.txt")

## check files are correct 

files


## read in all files using reduce_wm function
## produces a list of dataframes 


list_df <- map(files, reduce_wm)


## Run extract scores across each dataframe in the list. 
## The result will be produced in a table  

out <- map_df(list_df, extract_scores)




## rename variables to match ARCLI data

out <- out %>% 
  rename(
    SCB_SimpleRecogMemoverallacc = SRT_overall_acc,
    SCB_SimpleRecogMemoverallRTms = SRT_overall_RT,
    SCB_SimpleRecogMemnovelstimacc = SRT_novel_acc,
    SCB_SimpleRecogMemnovelstimRTms = SRT_novel_RT,
    SCB_SimpleRecogMemoriginalstimacc = SRT_original_acc,
    SCB_SimpleRecogMemoriginalstimRTms = SRT_original_RT,
    SCB_ConStroopacc = ST1T_acc,
    SCB_ConStroopRTms = ST1T_RT,
    SCB_InconStroopacc = ST2T_acc,
    SCB_InconStroopRTms = ST2T_RT,
    SCB_SpatialWorkingMemoverallacc = SWT_overall_acc,
    SCB_SpatialWorkingMemoverallRTms = SWT_overall_RT,
    SCB_SpatialWorkingMemnovelstimacc = SWT_novel_acc,
    SCB_SpatialWorkingMemnovelstimRTms = SWT_novel_RT,
    SCB_SpatialWorkingMemoriginalstimacc = SWT_original_acc,
    SCB_SpatialWorkingMemoriginalstimRTms = SWT_original_RT,
    SCB_ContextualMemacc = CRT_acc,
    SCB_ContextualMemRTms = CRT_RT,
    SCB_DelayedRecogMemoverallacc = DRT_overall_acc,
    SCB_DelayedRecogMemoverallRTms = DRT_overall_RT,
    SCB_DelayedRecogMemnovelstimacc = DRT_novel_acc,
    SCB_DelayedRecogMemnovelstimRTms = DRT_novel_RT,
    SCB_DelayedRecogMemoriginalstimacc = DRT_original_acc,
    SCB_DelayedRecogMemoriginalstimRTms = DRT_original_RT
  )


## reorder variables 

out <- out %>% 
  select(ID,SCB_SimpleRecogMemoverallRTms, SCB_SimpleRecogMemoverallacc,
         SCB_SimpleRecogMemnovelstimRTms, SCB_SimpleRecogMemnovelstimacc,
         SCB_SimpleRecogMemoriginalstimRTms, SCB_SimpleRecogMemoriginalstimacc,
         SCB_ConStroopRTms, SCB_ConStroopacc, SCB_InconStroopRTms,
         SCB_InconStroopacc, SCB_SpatialWorkingMemoverallRTms,
         SCB_SpatialWorkingMemoverallacc, SCB_SpatialWorkingMemnovelstimRTms,
         SCB_SpatialWorkingMemnovelstimacc, SCB_SpatialWorkingMemoriginalstimRTms,
         SCB_SpatialWorkingMemoriginalstimacc, SCB_ContextualMemRTms, 
         SCB_ContextualMemacc, SCB_DelayedRecogMemoverallRTms,
         SCB_DelayedRecogMemoverallacc, SCB_DelayedRecogMemnovelstimRTms,
         SCB_DelayedRecogMemnovelstimacc, SCB_DelayedRecogMemoriginalstimRTms,
         SCB_DelayedRecogMemoriginalstimacc)


## Duplicate variables for each follow-up visit

out$visit <- str_sub(out$ID, start = -1) # take visit number from ID digit

out$visit <- paste("v", out$visit, sep = "") # add 'v'


## wide format so one column per visit for each SUCCAB variable

out_wide <- pivot_wider(out, id_cols = ID, 
                        names_from = visit, 
                        values_from = -c("visit","ID")) 

## Match ID column to main dataset

out_wide$IDno = paste("ARCLI ", sapply(strsplit(out$ID,"-"), `[`, 2), sep = "")

out_wide <- out_wide %>% select(IDno, everything())

out_wide <- out_wide %>% select(-ID)

## round to two decimal places

out_wide <- out_wide %>% mutate_if(is.numeric, round, digits = 2)

#### THIS SECTION IS FOR ADDING NEW SUCCAB DATA TO THE MAIN DATASET
#### (SEE BOTTOM SECTION FOR ERROR CHECKING OLD DATA)


## read in ARCLI data for merging new results

file.choose() # copy paste path into brackets below

arcli <- read_sav("PASTE PATH HERE")


# delete blank rows and sort ascendingly 


arcli <- arcli %>% 
  filter(!IDno == "")

arcli <- arcli %>% 
  arrange(IDno)

## merge new results in


arcli_new <- coalesce_join(out_wide, arcli, by = "IDno")

## save updated file to SPSS
## file will go to working directory
## This is final step for scoring new data

write_sav(arcli_new, "PATH FOR NEW DATASET_DATA")





#### ERROR CHECKING FOR OLD MANUALLY ENTERED DATA ####


# load arcli data 

file.choose() # copy paste path into brackets below

arcli <- read_sav("PASTE PATH HERE")


arcli <- arcli %>% 
  filter(!IDno == "") # remove blank rows


# set -99 to missing 


arcli <- na_if(arcli, -99)


# choose only relevant vars from arcli

scb <- arcli %>% 
  filter(IDno %in% out_wide$IDno) %>% 
  select(IDno,starts_with("SCB"))


# merge with scored results

scb_merged <- full_join(scb, out_wide, by = "IDno")


# sort so new scores and old scores are next to eachother

scb_merged <- scb_merged %>% 
  select(sort(colnames(.)))


# check for differences by subtraction 


for (i in seq(2,length(scb_merged),by = 2)){
  print(scb_merged[i+1] - scb_merged[i])
}
