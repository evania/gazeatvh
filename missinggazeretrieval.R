################################################################################
################################################################################
####                                                                        ####
####                       missinggazeretrieval.R                           ####
####                                                                        ####
####           Importing and visualizing Tobii Unity gaze data              ####
####                + Retrieving missing gaze information                   ####
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
library(zoom)
options("install.lock"=FALSE)
install.packages("r2mlm")
install.packages("lme4")
install.packages("Matrix")
install.packages("vctrs")
install.packages("tibble")
install.packages("dplyr")
install.packages("fastDummies")

install.packages("afex")
library(afex)
library(fastDummies)

install.packages("scales")

install.packages("ggpubr")

install.packages("showtext")
library(scales)

library(showtext)

library(tidyverse)
library(ggplot2)
library(lme4)

library(r2mlm)

library(ggpubr)

library(hrbrthemes)

.libPaths()

sessionInfo() #getting the R version
packageVersion("lme4") #getting the library version

library(sjPlot)

library('rgl')

################################################################################
###############                         0                     ##################
###############               SUMMARIZE BIG DATA              ##################
################################################################################

# All eye data first needs to be summarized. 
# This script was not developed with efficiency in mind,
# so, e.g., for 3min data x 600 files, it will run for overnight.

# First, set directory to the folder where XML eye data exists
# setwd()
# Assuming the directory has been set now.

inputfiles <- list.files(path = ".", pattern = ".xml", recursive = TRUE)

# F Y I
# There is the list lookatdatalist_3 and also lookatdatalist_3_1
# because the script stopped abruptly in the middle of the night
# that made the first list not completed.
# So the script was run the second time to fill in the second list
# which should be for the rest of XML files that was left

lookatdatalist_3_1 = list()

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
  
  lookatdatalist_3_1[[i]] <- lookatdataframe_3
  
}

###########################################################################
## The end of overnight script                                           ##
###########################################################################

lookatdatalist_3_comb <- append(lookatdatalist_3, lookatdatalist_3_1)

remove(lookatdatalist_3)
remove(lookatdatalist_3_1)

###########################################################################
## Did it run successfully overnight? Now save the big data              ##
###########################################################################


# convert the list into a dataframe and make a CSV backup file
lookat_big_data_3 <- dplyr::bind_rows(lookatdatalist_3_comb)
write.csv(lookat_big_data_3, "C:/.../bigdata3.csv", row.names=FALSE)

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
###############                         1                     ##################
###############          DEFINING THE RELEVANT VARIABLE       ##################
################################################################################


# F Y I
# In reality, some scripts from the section "Data Organization" have been run first.
# That's where the df "workdata_validrays" is defined, for example.


#########################################################################
## LET'S TRY TO PLOT - 1st try (using the r_o)                        ###
#########################################################################

# only trial 1 has participant_look_at data
t1 <- subset(workdata_validrays, t_nr == 1)
t1_face <- subset(t1, participant_look_at == 4)
t1_body <- subset(t1, participant_look_at == 3)
t1_bg <- subset(t1, participant_look_at == 2)

# scatterplot the coordinates


p_t1 <- ggplot(t1, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at)))

p_t1_face <- ggplot(t1_face, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_body <- ggplot(t1_body, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(-0.08, 0.12) +
  ylim(1.53, 1.73)

p_t1_bg <- ggplot(t1_bg, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(-0.08, 0.12) +
  ylim(1.53, 1.73)




# Let's try to plot 3D
#
# first, an example:
# open3d()
# x <- sort(rnorm(1000))
# y <- rnorm(1000)
# z <- rnorm(1000) + atan2(x, y)
# plot3d(x, y, z, col = rainbow(1000))
#
# apparently the 3d plot shows one single z value 0.5 (for t1 face)
open3d()
p3d_t1_face_ro <- plot3d(t1_face$r_o_x, t1_face$r_o_y, t1_face$r_o_z) 
# apparently all Z values is 0.5 when checked on all workdata_validrays:
workdata_validrays_sum <- workdata_validrays %>%
  group_by(p_nr) %>%
  summarise(
    n=n(),
    mean_r_o_z=mean(r_o_z),
    sd=sd(r_o_z)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
#########################################################################
# The range values of r_o                                              ##
#########################################################################
workdata_validrays %>%
  summarise(
    n=n(),
    mean_r_o_z=mean(r_o_z),
    sd=sd(r_o_z)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# these are the results
# n mean_r_o_x         sd          se           ic
# 1 7660837 0.02252241 0.01680552 6.07175e-06 1.190041e-05
# n mean_r_o_y         sd           se           ic
# 1 7660837   1.631861 0.01682088 6.077299e-06 1.191129e-05
# n mean_r_o_z sd se ic
# 1 7660837        0.5  0  0  0



#########################################################################
## The range values of r_d                                             ##
#########################################################################
workdata_validrays %>%
  summarise(
    n=n(),
    mean_r_d_z=mean(r_d_z),
    sd=sd(r_d_z)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# results
# n   mean_r_d_x        sd           se           ic
# 1 7660837 -0.002757481 0.1310721 4.735568e-05 9.281545e-05
# n mean_r_d_y        sd           se           ic
# 1 7660837 0.02157003 0.1394907 5.039728e-05 9.877687e-05
# n mean_r_d_z         sd           se          ic
# 1 7660837 -0.9798745 0.05230206 1.889647e-05 3.70364e-05
#

#########################################################################
## LET'S TRY TO PLOT - 2st try (using the r_d)                         ##
#########################################################################

# plot3d
open3d()
p3d_t1_face <- plot3d(t1_face$r_d_x, t1_face$r_d_y, t1_face$r_d_z)
p3d_t1_body <- plot3d(t1_body$r_d_x, t1_body$r_d_y, t1_body$r_d_z)
p3d_t1_bg <- plot3d(t1_bg$r_d_x, t1_bg$r_d_y, t1_bg$r_d_z)


# plot only the X Y
p_t1_face_d <- ggplot(t1_face, aes(x=r_d_x, y=r_d_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(-1, 1) +
  ylim(-1, 1)

p_t1_body_d <- ggplot(t1_body, aes(x=r_d_x, y=r_d_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(-1, 1) +
  ylim(-1, 1)

p_t1_bg_d <- ggplot(t1_bg, aes(x=r_d_x, y=r_d_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(-1, 1) +
  ylim(-1, 1)


data <- dfwork_long %>% select(lookat, value)
head(data)
# Calculates mean, sd, se and IC
my_sum <- data %>%
  group_by(lookat) %>%
  summarise( 
    n=n(),
    mean_percentage=mean(value),
    sd=sd(value)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Confidence Interval
ggplot(my_sum) +
  geom_bar( aes(x=lookat, y=mean_percentage), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=lookat, ymin=mean_percentage-ic, ymax=mean_percentage+ic), width=0.3, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")





################################################################################
###############                         2                     ##################
###############                DATA ORGANIZATION              ##################
################################################################################


############################  2.1. DATA CLEANING  ############################## 


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


##################### SHOW THE PLOTS #####################################
t1 <- subset(workdata_validrays_incl50both, t_nr == 1)

t1_nowhere <- subset(t1, participant_look_at == 1)
t1_bg <- subset(t1, participant_look_at == 2)
t1_body <- subset(t1, participant_look_at == 3)
t1_face <- subset(t1, participant_look_at == 4)
t1_mouth <- subset(t1, participant_look_at == 5)
t1_eyes <- subset(t1, participant_look_at == 6)

p_t1_nowhere <- ggplot(t1_nowhere, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_bg <- ggplot(t1_bg, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_body <- ggplot(t1_body, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_face <- ggplot(t1_face, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_mouth <- ggplot(t1_mouth, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t1_eyes <- ggplot(t1_eyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) + #, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)



#################  2.2. CREATING A TRAINING AND TEST DATASETS  ################# 



# Create a subset of 70% of Trial 1 data of all participants
t1_70pcnt <- t1 %>%
  slice_sample(prop = 0.7)

# Create a subset of Trial 1 data that is not in t1_70pcnt
t1_30pcnt <- anti_join(t1, t1_70pcnt, by = c("p_nr"="p_nr", "t_nr"="t_nr", "timestamps" = "timestamps"))

t1_70pcnt_face <- subset(t1_70pcnt, participant_look_at == 4)
p_t1_70pcnt_face <- ggplot(t1_70pcnt_face, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

t1_70pcnt_eyes <- subset(t1_70pcnt, participant_look_at == 6)
p_t1_70pcnt_eyes <- ggplot(t1_70pcnt_eyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

################################################################################
###############                        3                      ##################
###############        DEFINING THE FACE AND EYES AREA        ##################
################################################################################

###########  3.1. DEFINING THE FACE AREA USING AN ELLIPSE FILTER   #############

# make a histogram to know the domain and range of the ellipse
# https://mathspace.co/textbooks/syllabuses/Syllabus-501/topics/Topic-9599/subtopics/Subtopic-127511/

phisto_t1_70pcnt_face_x <- t1_70pcnt_face %>%
  filter( r_o_x >= 0.00 & r_o_x <= 0.04) %>%
  ggplot( aes(x=r_o_x)) +
  geom_histogram( binwidth=0.0001, fill="#3498DB", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.0001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t1_70pcnt_face_y <- t1_70pcnt_face %>%
  filter( r_o_y >= 1.61 & r_o_y <= 1.66) %>%
  ggplot( aes(x=r_o_y)) +
  geom_histogram( binwidth=0.0001, fill="#E74C3C", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.0001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

# check using a rectangle (not elipse) filters
rectangle_subset <- subset(t1_70pcnt, (r_o_x >= 0.006 & r_o_x <= 0.037) & (r_o_y >= 1.614 & r_o_y <= 1.657))
# summarize the rectangle subset
ggplot(rectangle_subset, aes(x = participant_look_at)) +
  geom_bar()
# summarize the whole lookat for t1_70pcnt
ggplot(t1_70pcnt, aes(x = participant_look_at)) +
  geom_bar()


###########  3.2. DEFINING THE EYES AREA USING A RECTANGLE FILTER  #############

t1_70pcnt_eyes <- subset(t1_70pcnt, participant_look_at == 6)
p_t1_70pcnt_eyes <- ggplot(t1_70pcnt_eyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

# double check the rectangle filter in the Training dataset itself
# filter1
eyes_filtered_trainingdata <- subset(t1_70pcnt, (r_o_x >= 0.011 & r_o_x <= 0.036) & (r_o_y >= 1.6325 & r_o_y <= 1.648))
# filter2
eyes_filtered_trainingdata <- subset(t1_70pcnt, (r_o_x >= 0.012 & r_o_x <= 0.035) & (r_o_y >= 1.633 & r_o_y <= 1.647))



ggplot(eyes_filtered_trainingdata, aes(x = participant_look_at)) +
  geom_bar()+
  coord_cartesian(ylim = c(0,200000))

table(eyes_filtered_trainingdata$participant_look_at)
11 + 19520 + 2590 + 114 + 167747 + 1615 + 190766 
11/382363*100 #lookat nothing ( 0.00288 %)
19520/382363*100 #lookat nowhere ( 5.10510 %)
114/382363*100 #lookat body    ( 0.02982 %)
167747/382363*100 #lookat face    (43.87114 %)
1615/382363*100 #lookat mouth   ( 0.42237 %)
190766/382363*100 #lookat eyes    (49.89133 %)


table(eyes_filtered_trainingdata$p_nr)


ggplot(eyes_filtered_trainingdata, aes(x = participant_look_at)) +
  geom_bar()+
  coord_cartesian(ylim = c(0,200000))




options("install.lock"=FALSE)
install.packages('rayshader')
library(rayshader)


ggplot(t1_70pcnt_nowhere, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

t1_70pcnt_eyes <- subset(t1_70pcnt, participant_look_at == 6)
t1_70pcnt_mouth <- subset(t1_70pcnt, participant_look_at == 5)
t1_70pcnt_face <- subset(t1_70pcnt, participant_look_at == 4)
t1_70pcnt_body <- subset(t1_70pcnt, participant_look_at == 3)
t1_70pcnt_bg <- subset(t1_70pcnt, participant_look_at == 2)
t1_70pcnt_nowhere <- subset(t1_70pcnt, participant_look_at == 1)







###################  3.3. CALCULATING THE FILTER ACCURACY  #####################


###################             ELLIPSE FILTER             #####################

# domain = [0.006, 0.037]
# range = [1.614, 1.657]

# define the ellipse variables based on the manual look at the plots
h <- 0.0215
k <- 1.6355
a <- 0.0215
b <- 0.0155

# the ellipse formula, must equal 1
# if it equals more than 1, the XY falls outside the ellipse
# if it equals to 0, the XY falls in the middle of the ellipse
# ellipse_f <- ( (((x-h)^2)/b^2) + ((y-k)^2)/a^2 )

t1_30pcnt$ellipse_f_result <- ( (((t1_30pcnt$r_o_x-h)^2)/b^2) + ((t1_30pcnt$r_o_y-k)^2)/a^2 )
t1_30pcnt$participant_look_at_learned <- with(t1_30pcnt, ifelse(ellipse_f_result>1, -1, 4))

# Comparing the learned face with the real face
test_learned_face <- subset(t1_30pcnt, participant_look_at_learned == 4)

p_learned_face <- ggplot(test_learned_face, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

test_real_face <- subset(t1_30pcnt, participant_look_at == 4 | participant_look_at == 5 | participant_look_at == 6)

p_real_face <- ggplot(test_real_face, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

# Summarize the accuracy
correct_positive <- subset(t1_30pcnt, (participant_look_at == 4 | participant_look_at == 5 | participant_look_at == 6) & participant_look_at_learned == 4)
false_positive <- subset(t1_30pcnt, (participant_look_at != 4 & participant_look_at != 5 & participant_look_at != 6) & participant_look_at_learned == 4)
false_negative <- subset(t1_30pcnt, (participant_look_at == 4 | participant_look_at == 5 | participant_look_at == 6) & participant_look_at_learned != 4)
correct_negative <- subset(t1_30pcnt, (participant_look_at != 4 & participant_look_at != 5 & participant_look_at != 6) & participant_look_at_learned != 4)

count(correct_positive)/410108
count(false_positive)/410108
count(false_negative)/410108
count(correct_negative)/410108

p_false_positive <- ggplot(false_positive, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) + 
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

sum(false_positive$participant_look_at == 0) + sum(false_positive$participant_look_at == 1) + sum(false_positive$participant_look_at == 2) + sum(false_positive$participant_look_at == 3)



###################            RECTANGLE FILTER            #####################



t1_30pcnt$participant_look_at_learned_eyes <- with(t1_30pcnt, ifelse(((r_o_x >= 0.011 & r_o_x <= 0.036) & (r_o_y >= 1.6325 & r_o_y <= 1.648)), 6, -1))
# Filter 2 is worse than Filter 1
t1_30pcnt$participant_look_at_learned_eyes_f2 <- with(t1_30pcnt, ifelse(((r_o_x >= 0.012 & r_o_x <= 0.035) & (r_o_y >= 1.633 & r_o_y <= 1.647)), 6, -1))
# Filter 3 is better than Filter 1
t1_30pcnt$participant_look_at_learned_eyes_f3 <- with(t1_30pcnt, ifelse(((r_o_x >= 0.010 & r_o_x <= 0.037) & (r_o_y >= 1.632 & r_o_y <= 1.649)), 6, -1))
# Filter 4 is better than Filter 3 --> to be used
t1_30pcnt$participant_look_at_learned_eyes_f4 <- with(t1_30pcnt, ifelse(((r_o_x >= 0.009 & r_o_x <= 0.038) & (r_o_y >= 1.631 & r_o_y <= 1.65)), 6, -1))



# Plot the results, compare the learned eyes with the real eyes
test_learned_eyes <- subset(t1_30pcnt, participant_look_at_learned_eyes_f4 == 6)

p_learned_eyes <- ggplot(test_learned_eyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

test_real_eyes <- subset(t1_30pcnt, participant_look_at == 6)

p_real_eyes <- ggplot(test_real_eyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)


# plot all original lookat Test data
ggplot(t1_30pcnt, aes(x = participant_look_at)) +
  geom_bar()

# Summarize the accuracy
correct_positive <- subset(t1_30pcnt, participant_look_at == 6 & participant_look_at_learned_eyes_f4 == 6)
correct_positive_maybe <- subset(t1_30pcnt, participant_look_at == 4 & participant_look_at_learned_eyes_f4 == 6)
false_positive <- subset(t1_30pcnt, (participant_look_at != 4 & participant_look_at != 6) & participant_look_at_learned_eyes_f4 == 6)
false_negative <- subset(t1_30pcnt, participant_look_at == 6 & participant_look_at_learned_eyes_f4 != 6)
false_negative_maybe <- subset(t1_30pcnt, participant_look_at == 4 & participant_look_at_learned_eyes_f4 != 6)
correct_negative <- subset(t1_30pcnt, (participant_look_at != 4 & participant_look_at != 6) & participant_look_at_learned_eyes_f4 != 6)
#nothingnowhere_samples <- subset(t1_30pcnt, participant_look_at == 0 | participant_look_at == 1)

count(correct_positive)
count(correct_positive_maybe)
count(false_positive)
count(false_negative)
count(false_negative_maybe)
count(correct_negative)


count(false_positive, participant_look_at == 0)
count(false_positive, participant_look_at == 1)
count(false_positive, participant_look_at == 2)
count(false_positive, participant_look_at == 3)
count(false_positive, participant_look_at == 5)

85326/410108*100
78709/410108*100
12746/410108*100
4670/410108*100
62996/410108*100
165661/410108*100

#Filter2
82189/410108*100
71777/410108*100
10365/410108*100
7807/410108*100
69928/410108*100
168042/410108*100
82189+71777+10365+7807+69928+168042
20.04+17.50+2.52+1.90+17.05+40.97

#filter3
86925+84765+15703+3071+56940+162704
86925/410108*100
84765/410108*100
15703/410108*100
3071/410108*100
56940/410108*100
162704/410108*100

# Filter 4
88084/410108*100
93410/410108*100
19744/410108*100
1912/410108*100
48295/410108*100
158663/410108*100

21.47824+22.77693+4.814342+0.4662187+11.77617+38.6881
88084+93410+19744+1912+48295+158663
21.48+22.78+38.69

21.48+22.78+4.81+0.47+11.78+38.69


# Filter 4 without nowhere nothing samples
88084/375153*100
93410/375153*100
7883/375153*100
1912/375153*100
48295/375153*100
135569/375153*100




20.81 + 19.19 + 3.11 + 1.14 + 15.36 + 40.39
20.81 + 19.19 + 40.39



################################################################################
###############                   4.1                       ####################
###############    APPLYING BOTH FILTERS IN TRIAL 2-6       ####################
################################################################################


t2_to_t6 <- subset(workdata_validrays_incl50both, t_nr != 1)


#########################################################################
## The ellipse filter for face                                         ##
#########################################################################

# Using the same h k a b values as the ones used in the Test data

t2_to_t6$ellipse_f_result <- ( (((t2_to_t6$r_o_x-h)^2)/b^2) + ((t2_to_t6$r_o_y-k)^2)/a^2 )
t2_to_t6$participant_look_at_learned <- with(t2_to_t6, ifelse(ellipse_f_result>1, -1, 4))

# Check the plot of the learned Face
t2_t6_learnedface <- subset(t2_to_t6, participant_look_at_learned == 4)
t2_t6_notlearnedface <- subset(t2_to_t6, participant_look_at_learned != 4)

p_learned_face <- ggplot(t2_t6_learnedface, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at_learned))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_not_learned_face <- ggplot(t2_t6_notlearnedface, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)


# Compare results

# Data of T2 - T6
count(t2_to_t6, participant_look_at == 0)
count(t2_to_t6, participant_look_at == 0 & participant_look_at_learned != 4)
count(t2_to_t6, participant_look_at == 0 & participant_look_at_learned == 4)

count(t2_to_t6, participant_look_at == 1)
count(t2_to_t6, participant_look_at == 1 & participant_look_at_learned != 4)
count(t2_to_t6, participant_look_at == 1 & participant_look_at_learned == 4)

count(t2_to_t6, participant_look_at == 2)
count(t2_to_t6, participant_look_at == 2 & participant_look_at_learned != 4)
count(t2_to_t6, participant_look_at == 2 & participant_look_at_learned == 4)


# Data of T1
count(t1, participant_look_at == 0)
count(t1, participant_look_at == 1)
count(t1, participant_look_at == 2)
count(t1, participant_look_at == 3)
count(t1, participant_look_at == 4)
count(t1, participant_look_at == 5)
count(t1, participant_look_at == 6)

# Plot to see the shapes of the original Lookat values
t2_t6_nothing <- subset(t2_to_t6, participant_look_at == 0)
t2_t6_nowhere <- subset(t2_to_t6, participant_look_at == 1)
t2_t6_bg <- subset(t2_to_t6, participant_look_at == 2)

p_t2_t6_nothing <- ggplot(t2_t6_nothing, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t2_t6_nowhere <- ggplot(t2_t6_nowhere, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_t2_t6_bg <- ggplot(t2_t6_bg, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

# Histogram of the original Lookat shapes
phisto_t2_t6_nothing_x <- t2_t6_nothing %>%
  filter( r_o_x >= 0.00 & r_o_x <= 0.04) %>%
  ggplot( aes(x=r_o_x)) +
  geom_histogram( binwidth=0.001, fill="#3498DB", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t2_t6_nothing_y <- t2_t6_nothing %>%
  filter( r_o_y >= 1.61 & r_o_y <= 1.66) %>%
  ggplot( aes(x=r_o_y)) +
  geom_histogram( binwidth=0.001, fill="#E74C3C", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t2_t6_nowhere_x <- t2_t6_nowhere %>%
  filter( r_o_x >= 0.00 & r_o_x <= 0.04) %>%
  ggplot( aes(x=r_o_x)) +
  geom_histogram( binwidth=0.001, fill="#3498DB", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t2_t6_nowhere_y <- t2_t6_nowhere %>%
  filter( r_o_y >= 1.61 & r_o_y <= 1.66) %>%
  ggplot( aes(x=r_o_y)) +
  geom_histogram( binwidth=0.001, fill="#E74C3C", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t2_t6_bg_x <- t2_t6_bg %>%
  filter( r_o_x >= 0.00 & r_o_x <= 0.04) %>%
  ggplot( aes(x=r_o_x)) +
  geom_histogram( binwidth=0.001, fill="#3498DB", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

phisto_t2_t6_bg_y <- t2_t6_bg %>%
  filter( r_o_y >= 1.61 & r_o_y <= 1.66) %>%
  ggplot( aes(x=r_o_y)) +
  geom_histogram( binwidth=0.001, fill="#E74C3C", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.001") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#########################################################################
## The rectangle filter for eyes                                       ##
#########################################################################

t2_to_t6$participant_look_at_learned_eyes <- with(t2_to_t6, ifelse(((r_o_x >= 0.011 & r_o_x <= 0.036) & (r_o_y >= 1.6325 & r_o_y <= 1.648)), 6, -1))
# use filter 4:
t2_to_t6$participant_look_at_learned_eyes_f4 <- with(t2_to_t6, ifelse(((r_o_x >= 0.009 & r_o_x <= 0.038) & (r_o_y >= 1.631 & r_o_y <= 1.65)), 6, -1))

# Compare the results
count(t2_to_t6, participant_look_at == 0 & participant_look_at_learned_eyes == 6)
count(t2_to_t6, participant_look_at == 1 & participant_look_at_learned_eyes == 6)
count(t2_to_t6, participant_look_at == 2 & participant_look_at_learned_eyes == 6)
# Compare the results Filter4
count(t2_to_t6, participant_look_at == 0 & participant_look_at_learned_eyes_f4 == 6)
count(t2_to_t6, participant_look_at == 1 & participant_look_at_learned_eyes_f4 == 6)
count(t2_to_t6, participant_look_at == 2 & participant_look_at_learned_eyes_f4 == 6)

# Check the plot of the learned Eyes
t2_t6_learnedeyes <- subset(t2_to_t6, participant_look_at_learned_eyes_f4 == 6)
t2_t6_notlearnedeyes <- subset(t2_to_t6, participant_look_at_learned_eyes_f4 != 6)

p_learned_eyes <- ggplot(t2_t6_learnedeyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

p_not_learned_eyes <- ggplot(t2_t6_notlearnedeyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

################################################################################
###############                   4.2                       ####################
###############  APPLYING THE RECTANGLE FILTER IN TRIAL 1   ####################
################################################################################

# use filter 4:
t1$participant_look_at_learned_eyes_f4 <- with(t1, ifelse(((r_o_x >= 0.009 & r_o_x <= 0.038) & (r_o_y >= 1.631 & r_o_y <= 1.65)), 6, -1))

#check plot
t1_learnedeyes <- subset(t1, participant_look_at_learned_eyes_f4 == 6)
ggplot(t1_learnedeyes, aes(x=r_o_x, y=r_o_y)) +
  geom_point(alpha = 1/10, aes(colour = factor(participant_look_at))) +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66)

t1 %>%
  count(lookat_learned_f4)

t2_to_t6 %>%
  count(lookat_learned_f4)

t1_eyes_temp <- subset(t1, lookat_learned_f4==6)

t1_eyes_temp %>%
  count((r_o_x >= 0.00 & r_o_x <= 0.04) & (r_o_y >= 1.61 & r_o_y <= 1.66))

ggplot(t1_eyes_temp, aes(x=r_o_x, y=r_o_y)) +
  geom_hex(bins = 1000) +
  scale_fill_continuous(type = "viridis") +
  xlim(0.00, 0.04) +
  ylim(1.61, 1.66) +
  coord_fixed(ratio = 1)


################################################################################
###############                         7                     ##################
###############       FINAL SUMMARY TABLE PER GAZEDATA        ##################
################################################################################

# Make a new column to combine the Learned Face with the Learned Eyes
# when a Learned Face is recognize as Learned Eyes too,
# use the Learned Eyes value.
t2_to_t6 <- t2_to_t6 %>%
  mutate(lookat_learned_f4 = case_when(participant_look_at_learned_eyes_f4 == 6 ~ "6",
                                       participant_look_at_learned_eyes_f4 != 6 & participant_look_at_learned == 4 ~ "4",
                                       TRUE ~ participant_look_at))

t2_to_t6_subsettojoin <- data.frame(t2_to_t6$p_nr, t2_to_t6$t_nr, t2_to_t6$timestamps, t2_to_t6$lookat_learned_f4)
# change the column names 
colnames(t2_to_t6_subsettojoin)[1] <- "p_nr"
colnames(t2_to_t6_subsettojoin)[2] <- "t_nr"
colnames(t2_to_t6_subsettojoin)[3] <- "timestamps"
colnames(t2_to_t6_subsettojoin)[4] <- "lookat_learned"


# do the same for T1
# but this case, for t1, there will be lookat = 5 (mouth), while it is not the case with t2-t6.
# so later, for t1, lookat=5 is changed to lookat=4
t1 <- t1 %>%
  mutate(lookat_learned_f4 = case_when(participant_look_at_learned_eyes_f4 == 6 ~ "6",
                                       TRUE ~ participant_look_at))

t1_subsettojoin <- data.frame(t1$p_nr, t1$t_nr, t1$timestamps, t1$lookat_learned_f4)
colnames(t1_subsettojoin)[1] <- "p_nr"
colnames(t1_subsettojoin)[2] <- "t_nr"
colnames(t1_subsettojoin)[3] <- "timestamps"
colnames(t1_subsettojoin)[4] <- "lookat_learned"

t1_to_t6_subsettojoin <- data.frame(workdata_validrays_incl50both$p_nr, workdata_validrays_incl50both$t_nr, workdata_validrays_incl50both$timestamps)
colnames(t1_to_t6_subsettojoin)[1] <- "p_nr"
colnames(t1_to_t6_subsettojoin)[2] <- "t_nr"
colnames(t1_to_t6_subsettojoin)[3] <- "timestamps"
t1_to_t6_subsettojoin <- left_join(t1_to_t6_subsettojoin, t1_subsettojoin, by=c('p_nr'='p_nr', 't_nr'='t_nr', 'timestamps'='timestamps'))

t1_to_t6_subsettojoin <- left_join(t1_to_t6_subsettojoin, t2_to_t6_subsettojoin, by=c('p_nr'='p_nr', 't_nr'='t_nr', 'timestamps'='timestamps'))


t1_to_t6_subsettojoin <- t1_to_t6_subsettojoin %>%
  mutate(lookat_learned_final = case_when(is.na(lookat_learned.x) ~ lookat_learned.y,
                                          TRUE ~ lookat_learned.x))

t1_to_t6_subsettojoin <- t1_to_t6_subsettojoin %>% 
  select(-c(lookat_learned.x))
t1_to_t6_subsettojoin <- t1_to_t6_subsettojoin %>% 
  select(-c(lookat_learned.y))


# Left join t1_to_t6_subsettojoin to workdata_validrays_incl50both
workdata_validrays_incl50both_learned <- left_join(workdata_validrays_incl50both, t1_to_t6_subsettojoin, by=c('p_nr'='p_nr', 't_nr'='t_nr', 'timestamps'='timestamps'))

workdata_validrays_incl50both_learned <- workdata_validrays_incl50both_learned %>%
  select(-c(V20))

workdata_final2 <- workdata_validrays_incl50both_learned
#############################################################################
# Count how many in trial 1 data that had original lookat 4                 #
# but it should have been 6                                                 #
#############################################################################
count(workdata_final2, t_nr == 1 & participant_look_at == 4)
count(workdata_final2, t_nr == 1 & participant_look_at == 6)
count(workdata_final2, t_nr == 1 & lookat_learned_final == 6)
count(workdata_final2, t_nr == 1 & lookat_learned_final == 4)

options("install.lock"=FALSE)
install.packages("hexbin")
library(hexbin)
library(RColorBrewer)

help(scale_fill_continuous)


################################################################################
###############                EYE GAZE PLOTS                 ##################
################################################################################

##################
## FACE ORI     ##
##################

p_t1_ori_face_tbl <- subset(workdata_final2, t_nr == 1 & 
                              participant_look_at == 4)

p_t1_ori_face_tbl <- subset(workdata_final2, t_nr == 1 & 
                              participant_look_at == 4 & 
                              (r_o_x >= 0.00 & r_o_x <= 0.04) &
                              (r_o_y >= 1.61 & r_o_y <= 1.66))

ggplot(p_t1_ori_face_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y")

##################
## EYES ORI     ##
##################

p_t1_ori_eyes_tbl <- subset(workdata_final2, t_nr == 1 & 
                              participant_look_at == 6 &
                              (r_o_x >= 0.00 & r_o_x <= 0.04) &
                              (r_o_y >= 1.61 & r_o_y <= 1.66))


ggplot(p_t1_ori_eyes_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y")

##################
## NON-EYES FIN ##
##################

p_fin_others_tbl <- subset(workdata_final2,
                           lookat_learned_final != 6 &
                           (r_o_x >= 0.00 & r_o_x <= 0.04) &
                           (r_o_y >= 1.61 & r_o_y <= 1.66))

ggplot(p_fin_others_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y")

count(workdata_final2, lookat_learned_final == 5)

##################
## ALL FIN     ##
##################

p_fin_tbl <- subset(workdata_final2, (r_o_x >= 0.00 & r_o_x <= 0.04) &
                      (r_o_y >= 1.61 & r_o_y <= 1.66))

p_workdata_final2_tbl <- subset(workdata_final2, (r_o_x >= -0.027 & r_o_x <= 0.073) &
                                  (r_o_y >= 1.58 & r_o_y <= 1.68))

p_workdata_final2 <- ggplot(p_workdata_final2_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 1000) +
  scale_fill_distiller(palette="Spectral", name = "Count") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y") +
  coord_fixed(ratio = 1)

g <- brewer_pal(type="Spectral", palette=1)
g(5)

##################
## FACE FIN     ##
##################

# scale_fill_distiller(palette="Spectral", name = "Count") +
# scale_fill_continuous(type = "viridis", name = "Count")

p_fin_face_tbl <- subset(workdata_final2, 
                         (lookat_learned_final == 4 | lookat_learned_final == 5) & 
                           (r_o_x >= 0.00 & r_o_x <= 0.04) &
                           (r_o_y >= 1.61 & r_o_y <= 1.66))

p_fin_face_plot <- ggplot(p_fin_face_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 1000) +
  ggtitle("Face AOI") +
  scale_fill_distiller(palette="Spectral", name = "Count") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y") +
  coord_fixed(ratio = 1)

##################
## EYES FIN     ##
##################

p_fin_eyes_tbl <- subset(workdata_final2,
                         lookat_learned_final == 6 & 
                           (r_o_x >= 0.00 & r_o_x <= 0.04) &
                           (r_o_y >= 1.61 & r_o_y <= 1.66))

p_fin_eyes_plot <- ggplot(p_fin_eyes_tbl, aes(x=r_o_x, y=r_o_y) ) +
  geom_hex(bins = 1000) +
  ggtitle("Eyes AOI") +
  scale_fill_distiller(palette="Spectral", name = "Count") +
  theme_bw() +
  xlab("Coordinate X") +
  ylab("Coordinate Y") +
  coord_fixed(ratio = 1)

##################################
## EYES and FACE FIN COMBINED   ##
##################################

fig_aoi <- ggarrange(p_fin_eyes_plot, p_fin_face_plot,
                  labels = c("A", "B"),
                  common.legend = TRUE, legend = "right",
                  ncol = 2)

# Maybe don't report the bins because it's confusing
# The legend itself is already straightforward
# fig_aoi <- annotate_figure(fig_aoi, bottom = text_grob("Bin size = 1000"))

count(workdata_final2) 


################################################################################
################################################################################


# For T1 only, there are cases of lookat_learned_final == 5 but none on T2-T6.
# To be able to provide the summary of percentages of the
# three lookat types (lookat = eyes, face, others),
# lookat=5 (mouth) is changed to lookat=4 --> on workdata_final3 only
# update: but actually this doesn't matter
# because when the data is summarized, the percentages of lookat = 4 is
# already including lookat = 5

count(workdata_final2, t_nr ==1 & lookat_learned_final == 5)
count(workdata_final2, t_nr !=1 & lookat_learned_final == 5)

workdata_final3 <- workdata_final2

workdata_final3$lookat_learned_final <-  with(workdata_final3, ifelse(lookat_learned_final == 5, 4, lookat_learned_final)) 

count(workdata_final3, t_nr ==1 & lookat_learned_final == 5)
count(workdata_final3, t_nr !=1 & lookat_learned_final == 5)

# Also for lookat=1 (Nowhere), make it consistant for T1 on lookat_learned_final
# This is done to make the plot accurate. 
# Other than that it really doesn't matter
# because we are only interested in the lookat=6
# and lookat Nowhere data cannot be separated from Face/Eyes on T2-T6
# update : this plan is cancelled because it doesn't matter
count(workdata_final2, t_nr ==1 & lookat_learned_final == 1) 
count(workdata_final2, t_nr !=1 & lookat_learned_final == 1)





################################################################################
###############                   DOUBLE CHECK                ##################
###############   is all invalid rays = invalid pupil? (YES)  ##################
################################################################################

# If the total sum of this is 0, then it is indeed
# all invalid rays == invalid pupil
sum(workdata$r_v == "True" & workdata$pd_l_lid == "False" & workdata$pd_r_lid == "False")
sum(workdata$r_v == "False" & workdata$pd_l_lid == "True" & workdata$pd_r_lid == "True")
# Fortunately below 2 scripts also results in 0
# It means that if there is only one pupil is registered (either left or right),
# the gaze is treated as invalid too
sum(workdata$r_v == "True" & workdata$pd_l_lid == "False" & workdata$pd_r_lid == "True")
sum(workdata$r_v == "True" & workdata$pd_l_lid == "True" & workdata$pd_r_lid == "False")

# The summary results can be stored in the doublecheck table
doublecheck <- workdata %>%
  group_by(p_nr, t_nr) %>%
  count(r_v, pd_l_lid, pd_r_lid) 


