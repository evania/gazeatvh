################################################################################
################################################################################
####                                                                        ####
####                       importvisualizegaze.R                            ####
####                                                                        ####
####           Importing and visualizing Tobii Unity gaze data              ####
####          from the Virtual Storytellers experiment programs             ####
####                                                                        ####
####               The following script was written using                   ####
####                    R version 2022.07.0 Build 548                       ####
####                                                                        ####
####                      https://github.com/evania                         ####
####                                                                        ####
################################################################################
################################################################################

library("XML")
library(xml2)
library("stringr")
library(tidyverse)
library(ggplot2)
library(ggpubr)


################################################################################
###############             IMPORT AND SUMMARIZE DATA         ##################
###############                                               ##################
################################################################################

# All eye data first needs to be summarized. 
# This import script was not developed with efficiency in mind,
# so, e.g., for 3min data x 600 files, it will run for overnight.

# First, set directory to the folder where XML eye data exists
# setwd()
# Assuming the directory has been set now.

inputfiles <- list.files(path = ".", pattern = ".xml", recursive = TRUE)

# Define the list to be populated
lookatdatalist_3 = list()

###########################################################################
## Leave it to run overnight..                                           ##
###########################################################################

for(i in inputfiles){ 
  print(i)
  xml_inputfiles <- read_xml(i)
  kids <- xml_children(xml_inputfiles)
  
  #get participant number from file name
  p_nr_foundstring <- str_extract(i, "P\\d+") #find the ppt number
  p_nr_itself <- str_extract(p_nr_foundstring, "\\d+") #retrieve the digit only
  p_nr <- c(rep(p_nr_itself, times = length(kids)))
  
  #print(p_nr_foundstring)
  #print(length(kids))
  
  #get trial number from file name
  t_nr_foundstring <- str_extract(i, "Trial\\d")
  t_nr_itself <- str_extract(t_nr_foundstring, "\\d+") #retrieve the digit only
  t_nr <- c(rep(t_nr_itself, times = length(kids)))
  
  #get mimicry condition from file name
  mim_foundstring <- str_extract(i, "Mimic(True|False)")
  mim <- c(rep(mim_foundstring, times = length(kids)))
  
  #get relevant information from the xml
  timestamps <-  c(xml_attr(kids, "TimeStamp"))
  participant_look_at <- c(xml_attr(kids, "ParticipantLookAt"))
  experiment_status <- c(xml_attr(kids, "ExperimentStatus"))
  avatar_dilation <- c(xml_attr(kids, "AvatarDilationValue"))
  
  ############################ G A Z E D A T A ############################
  
  #access GazeData -> CombinedGazeRayScreen
  
  combined_ray_child <- xml_child(kids, search = "CombinedGazeRayScreen")
  
  r_o <- c(xml_attr(combined_ray_child, "Origin"))
  r_d <- c(xml_attr(combined_ray_child, "Direction"))
  r_v <- c(xml_attr(combined_ray_child, "Valid")) #to be used as it is
  
  r_o_x_list = list()
  r_o_y_list = list()
  r_o_z_list = list()
  
  r_d_x_list = list()
  r_d_y_list = list()
  r_d_z_list = list()
  
  # Separate the x, y, and z in r_o
  for(m in r_o) {
    r_o_splitted <- str_split(m, ", ")
    for(n in r_o_splitted) {
      r_o_xyz <- c(str_extract(n, "[\\-]?\\d+[.][\\d]+"))
      #print(coor_l_xy[1])
      r_o_x_list <- append(r_o_x_list, r_o_xyz[1])
      r_o_y_list <- append(r_o_y_list, r_o_xyz[2])
      r_o_z_list <- append(r_o_z_list, r_o_xyz[3])
    }
  }
  
  # Separate the x, y, and z in r_d
  for(m in r_d) {
    r_d_splitted <- str_split(m, ", ")
    for(n in r_d_splitted) {
      r_d_xyz <- c(str_extract(n, "[\\-]?\\d+[.][\\d]+"))
      #print(coor_l_xy[1])
      r_d_x_list <- append(r_d_x_list, r_d_xyz[1])
      r_d_y_list <- append(r_d_y_list, r_d_xyz[2])
      r_d_z_list <- append(r_d_z_list, r_d_xyz[3])
    }
  }
  
  r_o_x <- unlist(r_o_x_list)
  r_o_y <- unlist(r_o_y_list)
  r_o_z <- unlist(r_o_z_list)
  
  r_d_x <- unlist(r_d_x_list)
  r_d_y <- unlist(r_d_y_list)
  r_d_z <- unlist(r_d_z_list)
  
  r_o_x <- as.numeric(r_o_x)
  r_o_y <- as.numeric(r_o_y)
  r_o_z <- as.numeric(r_o_z)
  
  r_d_x <- as.numeric(r_d_x)
  r_d_y <- as.numeric(r_d_y)
  r_d_z <- as.numeric(r_d_z)
  
  
  ########################### P U P I L S I Z E ###########################
  
  #access GazeData -> Left -> PupilDiameter
  pd_l_ <- xml_child(kids, search = "Left")
  pd_l_element <- xml_child(pd_l_, search = "PupilDiameter")
  
  #access GazeData -> Right -> PupilDiameter
  pd_r_ <- xml_child(kids, search = "Right")
  pd_r_element <- xml_child(pd_r_, search = "PupilDiameter")
  
  # OriginalGaze PupilDiameter values seem to be the same with the previous one
  # so maybe just ignore these
  
  
  # Left eye
  pd_l_lue <- c(xml_attr(pd_l_element, "Value"))
  pd_l_lid <- c(xml_attr(pd_l_element, "Valid")) #to be used as it is
  pd_l <- as.numeric(pd_l_lue)
  
  # Right eye
  pd_r_lue <- c(xml_attr(pd_r_element, "Value"))
  pd_r_lid <- c(xml_attr(pd_r_element, "Valid")) #to be used as it is
  pd_r <- as.numeric(pd_r_lue)
  
  # make a new column for average left and right pupil
  pd_meanlr <- (pd_l + pd_r)/2
  
  
  ##################### SAVE TO DATAFRAME AND LIST #####################
  
  lookatdataframe_3 <- data.frame(p_nr, t_nr, mim, timestamps, participant_look_at, experiment_status, avatar_dilation, r_o_x, r_o_y, r_o_z, r_d_x, r_d_y, r_d_z, r_v, pd_l, pd_r, pd_meanlr, pd_l_lid, pd_r_lid)
  
  #recode participant_look_at
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Nothing"] <- 0
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Nowhere"] <- 1
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Background"] <- 2
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Body"] <- 3
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Face"] <- 4
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Mouth"] <- 5
  lookatdataframe_3$participant_look_at[lookatdataframe_3$participant_look_at == "Eyes"] <- 6
  
  #recode mim
  lookatdataframe_3$mim[lookatdataframe_3$mim == "MimicFalse"] <- 0
  lookatdataframe_3$mim[lookatdataframe_3$mim == "MimicTrue"] <- 1
  
  lookatdatalist_3[[i]] <- lookatdataframe_3
  
}

###########################################################################
## The end of overnight script                                           ##
###########################################################################


###########################################################################
## Did it run successfully overnight? Now save the big data              ##
###########################################################################


# convert the list into a dataframe and make a CSV backup file
lookat_big_data_3 <- dplyr::bind_rows(lookatdatalist_3)
write.csv(lookat_big_data_3, "D:/bigdata3.csv", row.names=FALSE)

# remove leftover data that was used in the loop
# they are mostly used as a container per 1 PPT during the loop,
# and were replaced with the next PPT data on the next loop
remove(combined_ray_child)
remove(kids)
remove(pd_l_)
remove(pd_l_element)
remove(pd_r_)
remove(pd_r_element)
remove(r_d_splitted)
remove(r_d_x_list)
remove(r_d_y_list)
remove(r_d_z_list)
remove(r_o_splitted)
remove(r_o_x_list)
remove(r_o_y_list)
remove(r_o_z_list)
remove(xml_inputfiles)
remove(lookatdataframe_3)
#remove leftover values that was used in the loop
remove(avatar_dilation)
remove(experiment_status)
remove(i)
remove(inputfiles)
remove(m)
remove(mim)
remove(mim_foundstring)
remove(n)
remove(p_nr)
remove(p_nr_foundstring)
remove(p_nr_itself)
remove(participant_look_at)
remove(pd_l)
remove(pd_l_lid)
remove(pd_l_lue)
remove(pd_meanlr)
remove(pd_r)
remove(pd_r_lid)
remove(pd_r_lue)
remove(r_d)
remove(r_d_x)
remove(r_d_y)
remove(r_d_z)
remove(r_d_xyz)
remove(r_o)
remove(r_o_x)
remove(r_o_y)
remove(r_o_z)
remove(r_o_xyz)
remove(r_v)
remove(t_nr)
remove(t_nr_foundstring)
remove(t_nr_itself)
remove(timestamps)



################################################################################
###############        DATA CLEANING AND ORGANIZATION         ##################
###############                                               ##################
################################################################################


############################## "ExpStatus" ##############################
# "ExpStatus" is only found on 1st trial some samples at the beginning
# this status was probably sent by Unity when the experiment has not begun
# thus, we can remove these samples in the data
status_per_trial <- lookat_big_data_3 %>% count(t_nr, experiment_status)
# adjust workdata
workdata <- subset(lookat_big_data_3, experiment_status != "ExpStatus")
#remove data that is not going to be used anymore
remove(status_per_trial)


########################### Adjust data types ###########################
# Let's adjust some data types
str(workdata)
workdata$p_nr <- as.integer(workdata$p_nr)
workdata$t_nr <- as.integer(workdata$t_nr)
workdata$mim <- as.integer(workdata$mim)
# don't convert timestamps to integer because there was NA values


############################# Invalid Rays #############################
# There are cases of invalid ray data but the r_o or r_d coordinates
# were still assigned, as 0.000000. So these data should be removed
workdata %>% count(p_nr, t_nr, r_v)
workdata %>% count(r_v)
# Results:
#     r_v       n
# 1 False 2707194
# 2  True 7660837
#
# Now make a subset of only valid Rays
workdata_validrays <- subset(workdata, r_v == "True")
# check the number of samples now and before
n_samples_before <- workdata %>% count(p_nr, t_nr)
n_samples_after <- workdata_validrays %>% count(p_nr, t_nr)
# change the column names for the number of samples
colnames(n_samples_before)[3] <- "n_before"
colnames(n_samples_after)[3] <- "n_after"
# perform left join (https://www.statology.org/dplyr-join-on-multiple-columns/)
# based on multiple columns (there are 2 keys: p_nr and t_nr)
# n_samples_before should be the left table
n_sample_includevalidrays <- left_join(n_samples_before, n_samples_after, by=c('p_nr'='p_nr', 't_nr'='t_nr'))
# the percentage of excluded samples (invalid rays)
n_sample_includevalidrays$n_excluded <- n_sample_includevalidrays$n_before - n_sample_includevalidrays$n_after
n_sample_includevalidrays$percent_excluded <- (n_sample_includevalidrays$n_excluded / n_sample_includevalidrays$n_before) * 100
n_sample_includevalidrays$percent_excluded <- round(n_sample_includevalidrays$percent_excluded, digits = 0)
# remove leftover data
remove(n_samples_after)
remove(n_samples_before)

############## Descriptive Statistics of Invalid Rays ####################

# create a new column inclusion_trial
n_sample_includevalidrays$inclusion_trial[n_sample_includevalidrays$percent_excluded <= 50] <- TRUE
n_sample_includevalidrays$inclusion_trial[n_sample_includevalidrays$percent_excluded > 50] <- FALSE
# make the data more accurate by changing NA values in inclusion_trial
n_sample_includevalidrays$inclusion_trial <- replace(n_sample_includevalidrays$inclusion_trial, is.na(n_sample_includevalidrays$inclusion_trial), FALSE)

# Subset of percent_excluded > 50% per t_nr per p_nr
subset_excluded <- subset(n_sample_includevalidrays, inclusion_trial == FALSE)
# Summarize how many rows there are per p_nr
# (to show if a p_nr lost more than 50% data in more than 50% trials)
subset_excluded_sum <- subset_excluded %>%
  group_by(p_nr) %>%
  summarize(count_excluded_trial = n_distinct(t_nr))
sum(subset_excluded_sum$count_excluded_trial) # = subset_excluded total entries

# Make a new column of inclusion_ppt True or False
# mark inclusion_ppt = True if distinct Trials <= 3
# mark inclusion_ppt = False if distinct Trials >3
subset_excluded_sum$inclusion_ppt[subset_excluded_sum$count_excluded_trial <= 3] <- TRUE
subset_excluded_sum$inclusion_ppt[subset_excluded_sum$count_excluded_trial > 3] <- FALSE

# join subset_excluded_sum to n_sample_includevalidrays
n_sample_includevalidrays_incl50 <- left_join(n_sample_includevalidrays, subset_excluded_sum, "p_nr")
# remove this column in the df because it's not necessary
n_sample_includevalidrays_incl50 <- n_sample_includevalidrays_incl50 %>% select(-c(count_excluded_trial))
# at this point, when inclusion_ppt is empty and inclusion_trial == TRUE, it means that inclusion_ppt == TRUE
n_sample_includevalidrays_incl50$inclusion_ppt[is.na(n_sample_includevalidrays_incl50$inclusion_ppt) & n_sample_includevalidrays_incl50$inclusion_trial == TRUE] <- TRUE

n_sample_includevalidrays_incl50$inclusion_both[n_sample_includevalidrays_incl50$inclusion_trial == TRUE & n_sample_includevalidrays_incl50$inclusion_ppt == TRUE ] <- TRUE
n_sample_includevalidrays_incl50$inclusion_both[n_sample_includevalidrays_incl50$inclusion_trial == FALSE | n_sample_includevalidrays_incl50$inclusion_ppt == FALSE ] <- FALSE


# prepare a subset of the n_sample_includevalidrays_incl50 to be joined with workdata_validrays
n_ <- data.frame(n_sample_includevalidrays_incl50$p_nr, n_sample_includevalidrays_incl50$t_nr, n_sample_includevalidrays_incl50$inclusion_trial, n_sample_includevalidrays_incl50$inclusion_ppt, n_sample_includevalidrays_incl50$inclusion_both)
colnames(n_)[1] <- "p_nr"
colnames(n_)[2] <- "t_nr"
colnames(n_)[3] <- "inclusion_trial"
colnames(n_)[4] <- "inclusion_ppt"
colnames(n_)[5] <- "inclusion_both"


# Join the inclusion columns into the workdata_validrays
workdata_validrays_incl50 <-  left_join(workdata_validrays, n_, by=c("p_nr"="p_nr", "t_nr"="t_nr"))

# Summarize the TRUE vs FALSE, to test/compare the results briefly with n_sample_includevalidrays_incl50
validrays_incl50_sum <- workdata_validrays_incl50 %>%
  group_by(p_nr, t_nr) %>%
  count(inclusion_both)
# Test result: missing P11T6, P42T4-6, P47T6, P49T1-6  (no valid rays)
# Test result conclusion: the workdata_validrays_incl50 is good

# Use the following DF to only include the 50% valid data & trial criteria
workdata_validrays_incl50both <- subset(workdata_validrays_incl50, inclusion_both == TRUE)

n_sample_includevalidrays_incl50 %>%
  count(inclusion_trial)

n_sample_includevalidrays_incl50 %>%
  count(inclusion_ppt)

################### EXCLUDE LOOKAT NOTHING AND NOWHERE #########################
#workdata_validrays_incl50both <- subset(workdata_validrays_incl50, inclusion_both == TRUE)

workdata_final2 <- workdata_validrays_incl50both





################################################################################
###############               DATA VISUALIZATION              ##################
###############                                               ##################
################################################################################

##################
##  ALL FIN     ##
##################

p_workdata_final2_tbl <- subset(workdata_final2, (r_o_x >= -0.027 & r_o_x <= 0.073) &
                                  (r_o_y >= 1.58 & r_o_y <= 1.68))

p_workdata_final2 <- ggplot(p_workdata_final2_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 1000) +
  scale_fill_distiller(palette="Spectral", name = "Count") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y") +
  coord_fixed(ratio = 1)

