#### Data Cleaning and Prep #### 

### Load packages ###
if (!require("pacman")) {install.packages("pacman"); require("pacman")}
p_load(lme4, lmerTest, sjPlot, ggplot2, tidyr, tidyverse, performance, effects, dplyr, brms, bayestestR, rstudioapi, R.matlab)

### Loading and Cleaning Data ###
#Note: You can configure this script to work in its local folder with the data for encoding and recognition being in
# the same folder as this script with the code below (makes replicating analyses across machines easier)
current_path <- getActiveDocumentContext()$path    # gets path for this R script
setwd(dirname(current_path)) # sets working directory to folder this R script is in

#Load Encoding Data 
E_1 <- read.csv("E_Data/1_E.csv", header = TRUE)
E_2 <- read.csv("E_Data/2_E.csv", header = TRUE)
E_3 <- read.csv("E_Data/3_E.csv", header = TRUE)
E_4 <- read.csv("E_Data/4_E.csv", header = TRUE)
E_5 <- read.csv("E_Data/5_E.csv", header = TRUE)
E_6 <- read.csv("E_Data/6_E.csv", header = TRUE)
E_7 <- read.csv("E_Data/7_E.csv", header = TRUE)
E_8 <- read.csv("E_Data/8_E.csv", header = TRUE)
E_9 <- read.csv("E_Data/9_E.csv", header = TRUE)
E_10 <- read.csv("E_Data/10_E.csv", header = TRUE)
E_12 <- read.csv("E_Data/12_E.csv", header = TRUE)
E_14 <- read.csv("E_Data/14_E.csv", header = TRUE)
E_15 <- read.csv("E_Data/15_E.csv", header = TRUE)
E_16 <- read.csv("E_Data/16_E.csv", header = TRUE)
E_17 <- read.csv("E_Data/17_E.csv", header = TRUE)
E_19 <- read.csv("E_Data/19_E.csv", header = TRUE)
E_20 <- read.csv("E_Data/20_E.csv", header = TRUE)
E_21 <- read.csv("E_Data/21_E.csv", header = TRUE)
E_23 <- read.csv("E_Data/23_E.csv", header = TRUE)
E_24 <- read.csv("E_Data/24_E.csv", header = TRUE)
E_27 <- read.csv("E_Data/27_E.csv", header = TRUE)
E_28 <- read.csv("E_Data/28_E.csv", header = TRUE)
E_29 <- read.csv("E_Data/29_E.csv", header = TRUE)
E_30 <- read.csv("E_Data/30_E.csv", header = TRUE)
E_31 <- read.csv("E_Data/31_E.csv", header = TRUE)
E_33 <- read.csv("E_Data/33_E.csv", header = TRUE)
E_34 <- read.csv("E_Data/34_E.csv", header = TRUE)
E_35 <- read.csv("E_Data/35_E.csv", header = TRUE)
E_36 <- read.csv("E_Data/36_E.csv", header = TRUE)
E_37 <- read.csv("E_Data/37_E.csv", header = TRUE)
E_38 <- read.csv("E_Data/38_E.csv", header = TRUE)
E_39 <- read.csv("E_Data/39_E.csv", header = TRUE)
E_40 <- read.csv("E_Data/40_E.csv", header = TRUE)
E_41 <- read.csv("E_Data/41_E.csv", header = TRUE)
E_42 <- read.csv("E_Data/42_E.csv", header = TRUE)
E_43 <- read.csv("E_Data/43_E.csv", header = TRUE)
E_44 <- read.csv("E_Data/44_E.csv", header = TRUE)
E_45 <- read.csv("E_Data/45_E.csv", header = TRUE)
E_46 <- read.csv("E_Data/46_E.csv", header = TRUE)
E_47 <- read.csv("E_Data/47_E.csv", header = TRUE)
E_48 <- read.csv("E_Data/48_E.csv", header = TRUE)
E_50 <- read.csv("E_Data/50_E.csv", header = TRUE)
E_51 <- read.csv("E_Data/51_E.csv", header = TRUE)
E_52 <- read.csv("E_Data/52_E.csv", header = TRUE)
E_53 <- read.csv("E_Data/53_E.csv", header = TRUE)
E_55 <- read.csv("E_Data/55_E.csv", header = TRUE)
E_56 <- read.csv("E_Data/56_E.csv", header = TRUE)
E_57 <- read.csv("E_Data/57_E.csv", header = TRUE)
E_59 <- read.csv("E_Data/59_E.csv", header = TRUE)
E_60 <- read.csv("E_Data/60_E.csv", header = TRUE)
E_61 <- read.csv("E_Data/61_E.csv", header = TRUE)
E_62 <- read.csv("E_Data/62_E.csv", header = TRUE)
E_63 <- read.csv("E_Data/63_E.csv", header = TRUE)
E_64 <- read.csv("E_Data/64_E.csv", header = TRUE)
E_65 <- read.csv("E_Data/65_E.csv", header = TRUE)


#Load Recognition Responses
RO_1 <- read.csv("RO_Data/1_RO.csv", header = TRUE)
RO_2 <- read.csv("RO_Data/2_RO.csv", header = TRUE)
RO_3 <- read.csv("RO_Data/3_RO.csv", header = TRUE)
RO_4 <- read.csv("RO_Data/4_RO.csv", header = TRUE)
RO_5 <- read.csv("RO_Data/5_RO.csv", header = TRUE)
RO_6 <- read.csv("RO_Data/6_RO.csv", header = TRUE)
RO_7 <- read.csv("RO_Data/7_RO.csv", header = TRUE)
RO_8 <- read.csv("RO_Data/8_RO.csv", header = TRUE)
RO_9 <- read.csv("RO_Data/9_RO.csv", header = TRUE)
RO_10 <- read.csv("RO_Data/10_RO.csv", header = TRUE)
RO_12 <- read.csv("RO_Data/12_RO.csv", header = TRUE)
RO_14 <- read.csv("RO_Data/14_RO.csv", header = TRUE)
RO_15 <- read.csv("RO_Data/15_RO.csv", header = TRUE)
RO_16 <- read.csv("RO_Data/16_RO.csv", header = TRUE)
RO_17 <- read.csv("RO_Data/17_RO.csv", header = TRUE)
RO_19 <- read.csv("RO_Data/19_RO.csv", header = TRUE)
RO_20 <- read.csv("RO_Data/14_RO.csv", header = TRUE)
RO_21 <- read.csv("RO_Data/21_RO.csv", header = TRUE)
RO_23 <- read.csv("RO_Data/23_RO.csv", header = TRUE)
RO_24 <- read.csv("RO_Data/24_RO.csv", header = TRUE)
RO_27 <- read.csv("RO_Data/27_RO.csv", header = TRUE)
RO_28 <- read.csv("RO_Data/28_RO.csv", header = TRUE)
RO_29 <- read.csv("RO_Data/29_RO.csv", header = TRUE)
RO_30 <- read.csv("RO_Data/30_RO.csv", header = TRUE)
RO_31 <- read.csv("RO_Data/31_RO.csv", header = TRUE)
RO_33 <- read.csv("RO_Data/33_RO.csv", header = TRUE)
RO_34 <- read.csv("RO_Data/34_RO.csv", header = TRUE)
RO_35 <- read.csv("RO_Data/35_RO.csv", header = TRUE)
RO_36 <- read.csv("RO_Data/36_RO.csv", header = TRUE)
RO_37 <- read.csv("RO_Data/37_RO.csv", header = TRUE)
RO_38 <- read.csv("RO_Data/38_RO.csv", header = TRUE)
RO_39 <- read.csv("RO_Data/39_RO.csv", header = TRUE)
RO_40 <- read.csv("RO_Data/40_RO.csv", header = TRUE)
RO_41 <- read.csv("RO_Data/41_RO.csv", header = TRUE)
RO_42 <- read.csv("RO_Data/42_RO.csv", header = TRUE)
RO_43 <- read.csv("RO_Data/43_RO.csv", header = TRUE)
RO_44 <- read.csv("RO_Data/44_RO.csv", header = TRUE)
RO_45 <- read.csv("RO_Data/45_RO.csv", header = TRUE)
RO_46 <- read.csv("RO_Data/46_RO.csv", header = TRUE)
RO_47 <- read.csv("RO_Data/47_RO.csv", header = TRUE)
RO_48 <- read.csv("RO_Data/48_RO.csv", header = TRUE)
RO_50 <- read.csv("RO_Data/50_RO.csv", header = TRUE)
RO_51 <- read.csv("RO_Data/51_RO.csv", header = TRUE)
RO_52 <- read.csv("RO_Data/52_RO.csv", header = TRUE)
RO_53 <- read.csv("RO_Data/53_RO.csv", header = TRUE)
RO_55 <- read.csv("RO_Data/55_RO.csv", header = TRUE)
RO_56 <- read.csv("RO_Data/56_RO.csv", header = TRUE)
RO_57 <- read.csv("RO_Data/57_RO.csv", header = TRUE)
RO_59 <- read.csv("RO_Data/59_RO.csv", header = TRUE)
RO_60 <- read.csv("RO_Data/60_RO.csv", header = TRUE)
RO_61 <- read.csv("RO_Data/61_RO.csv", header = TRUE)
RO_62 <- read.csv("RO_Data/62_RO.csv", header = TRUE)
RO_63 <- read.csv("RO_Data/63_RO.csv", header = TRUE)
RO_64 <- read.csv("RO_Data/64_RO.csv", header = TRUE)
RO_65 <- read.csv("RO_Data/65_RO.csv", header = TRUE)


#Load Recognition Data
R_1 <- read.csv("R_Data/1_R.csv", header = TRUE)
R_2 <- read.csv("R_Data/2_R.csv", header = TRUE)
R_3 <- read.csv("R_Data/3_R.csv", header = TRUE)
R_4 <- read.csv("R_Data/4_R.csv", header = TRUE)
R_5 <- read.csv("R_Data/5_R.csv", header = TRUE)
R_6 <- read.csv("R_Data/6_R.csv", header = TRUE)
R_7 <- read.csv("R_Data/7_R.csv", header = TRUE)
R_8 <- read.csv("R_Data/8_R.csv", header = TRUE)
R_9 <- read.csv("R_Data/9_R.csv", header = TRUE)
R_10 <- read.csv("R_Data/10_R.csv", header = TRUE)
R_12 <- read.csv("R_Data/12_R.csv", header = TRUE)
R_14 <- read.csv("R_Data/14_R.csv", header = TRUE)
R_15 <- read.csv("R_Data/15_R.csv", header = TRUE)
R_16 <- read.csv("R_Data/16_R.csv", header = TRUE)
R_17 <- read.csv("R_Data/17_R.csv", header = TRUE)
R_19 <- read.csv("R_Data/19_R.csv", header = TRUE)
R_20 <- read.csv("R_Data/15_R.csv", header = TRUE)
R_21 <- read.csv("R_Data/21_R.csv", header = TRUE)
R_23 <- read.csv("R_Data/23_R.csv", header = TRUE)
R_24 <- read.csv("R_Data/24_R.csv", header = TRUE)
R_27 <- read.csv("R_Data/27_R.csv", header = TRUE)
R_28 <- read.csv("R_Data/28_R.csv", header = TRUE)
R_29 <- read.csv("R_Data/29_R.csv", header = TRUE)
R_30 <- read.csv("R_Data/30_R.csv", header = TRUE)
R_31 <- read.csv("R_Data/31_R.csv", header = TRUE)
R_33 <- read.csv("R_Data/33_R.csv", header = TRUE)
R_34 <- read.csv("R_Data/34_R.csv", header = TRUE)
R_35 <- read.csv("R_Data/35_R.csv", header = TRUE)
R_36 <- read.csv("R_Data/36_R.csv", header = TRUE)
R_37 <- read.csv("R_Data/37_R.csv", header = TRUE)
R_38 <- read.csv("R_Data/38_R.csv", header = TRUE)
R_39 <- read.csv("R_Data/39_R.csv", header = TRUE)
R_40 <- read.csv("R_Data/40_R.csv", header = TRUE)
R_41 <- read.csv("R_Data/41_R.csv", header = TRUE)
R_42 <- read.csv("R_Data/42_R.csv", header = TRUE)
R_43 <- read.csv("R_Data/43_R.csv", header = TRUE)
R_44 <- read.csv("R_Data/44_R.csv", header = TRUE)
R_45 <- read.csv("R_Data/45_R.csv", header = TRUE)
R_46 <- read.csv("R_Data/46_R.csv", header = TRUE)
R_47 <- read.csv("R_Data/47_R.csv", header = TRUE)
R_48 <- read.csv("R_Data/48_R.csv", header = TRUE)
R_50 <- read.csv("R_Data/50_R.csv", header = TRUE)
R_51 <- read.csv("R_Data/51_R.csv", header = TRUE)
R_52 <- read.csv("R_Data/52_R.csv", header = TRUE)
R_53 <- read.csv("R_Data/53_R.csv", header = TRUE)
R_55 <- read.csv("R_Data/55_R.csv", header = TRUE)
R_56 <- read.csv("R_Data/56_R.csv", header = TRUE)
R_57 <- read.csv("R_Data/57_R.csv", header = TRUE)
R_59 <- read.csv("R_Data/59_R.csv", header = TRUE)
R_60 <- read.csv("R_Data/60_R.csv", header = TRUE)
R_61 <- read.csv("R_Data/61_R.csv", header = TRUE)
R_62 <- read.csv("R_Data/62_R.csv", header = TRUE)
R_63 <- read.csv("R_Data/63_R.csv", header = TRUE)
R_64 <- read.csv("R_Data/64_R.csv", header = TRUE)
R_65 <- read.csv("R_Data/65_R.csv", header = TRUE)


# Load valence data and information
Neg_stim <- read.csv("Arousal_Neg.csv", header = TRUE)
Neut_stim <- read.csv("Arousal_Neutral.csv", header = TRUE)
#Change formatting of stim for comparison
Neg_stim$Stim <- paste0(Neg_stim$Stim, '.jpg')
Neut_stim$Stim <- paste0(Neut_stim$Stim, '.jpg')

## Encoding ##
#Define variables of interest 
#  e.g. Relevant Columns: 
#  L_Scene (Picture on Left)
#  R_Scene (Picture on Right)
#  L_Pts (Trial Points)
#  Attn_resp.keys (Attention trial responses)
#  Encoding_Resp.keys (Image selection)
#  Transition_Enc.keys (End of instructions)

#Select Encoding Variables of Interest for each participant, and remove training data
E_1_selected <- E_1 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 1)
E_2_selected <- E_2 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 2)
E_3_selected <- E_3 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 3)
E_4_selected <- E_4 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 4)
E_5_selected <- E_5 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 5)
E_6_selected <- E_6 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 6)
E_7_selected <- E_7 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 7)
E_8_selected <- E_8 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 8)
E_9_selected <- E_9 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 9)
E_10_selected <- E_10 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 10)
E_12_selected <- E_12 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 12)
E_14_selected <- E_14 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 14)
E_15_selected <- E_15 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 15)
E_16_selected <- E_16 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 16)
E_17_selected <- E_17 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 17)
E_19_selected <- E_19 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 19)
E_20_selected <- E_20 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 20)
E_21_selected <- E_21 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 21)
E_23_selected <- E_23 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 23)
E_24_selected <- E_24 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 24)
E_27_selected <- E_27 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 27)
E_28_selected <- E_28 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 28)
E_29_selected <- E_29 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 29)
E_30_selected <- E_30 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 30)
E_31_selected <- E_31 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 31)
E_33_selected <- E_33 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 33)
E_34_selected <- E_34 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 34)
E_35_selected <- E_35 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 35)
E_36_selected <- E_36 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 36)
E_37_selected <- E_37 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 37)
E_38_selected <- E_38 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 38)
E_39_selected <- E_39 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 39)
E_40_selected <- E_40 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 40)
E_41_selected <- E_41 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 41)
E_42_selected <- E_42 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 42)
E_43_selected <- E_43 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 43)
E_44_selected <- E_44 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 44)
E_45_selected <- E_45 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 45)
E_46_selected <- E_46 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 46)
E_47_selected <- E_47 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 47)
E_48_selected <- E_48 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 48)
E_50_selected <- E_50 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 50)
E_51_selected <- E_51 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 51)
E_52_selected <- E_52 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 52)
E_53_selected <- E_53 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 53)
E_55_selected <- E_55 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 55)
E_56_selected <- E_56 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 56)
E_57_selected <- E_57 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 57)
E_59_selected <- E_59 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 59)
E_60_selected <- E_60 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 60)
E_61_selected <- E_61 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 61)
E_62_selected <- E_62 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 62)
E_63_selected <- E_63 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 63)
E_64_selected <- E_64 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 64)
E_65_selected <- E_65 %>%
  select(L_Scene, R_Scene, Points_L, Points_R, PtTotal, Attn_resp.keys, Encoding_Resp.keys) %>% mutate(participant = 65)


#Filter for available trials
E_1_cleaned <- E_1_selected[-(1:12), ]
E_2_cleaned <- E_2_selected[-(1:12), ]
E_3_cleaned <- E_3_selected[-(1:12), ]
E_4_cleaned <- E_4_selected[-(1:12), ]
E_5_cleaned <- E_5_selected[-(1:12), ]
E_6_cleaned <- E_6_selected[-(1:12), ]
E_7_cleaned <- E_7_selected[-(1:12), ]
E_8_cleaned <- E_8_selected[-(1:12), ]
E_9_cleaned <- E_9_selected[-(1:12), ]
E_10_cleaned <- E_10_selected[-(1:12), ]
E_12_cleaned <- E_12_selected[-(1:12), ]
E_14_cleaned <- E_14_selected[-(1:12), ]
E_15_cleaned <- E_15_selected[-(1:12), ]
E_16_cleaned <- E_16_selected[-(1:12), ]
E_17_cleaned <- E_17_selected[-(1:12), ]
E_19_cleaned <- E_19_selected[-(1:12), ]
E_20_cleaned <- E_20_selected[-(1:12), ]
E_21_cleaned <- E_21_selected[-(1:12), ]
E_23_cleaned <- E_23_selected[-(1:12), ]
E_24_cleaned <- E_24_selected[-(1:12), ]
E_27_cleaned <- E_27_selected[-(1:12), ]
E_28_cleaned <- E_28_selected[-(1:12), ]
E_29_cleaned <- E_29_selected[-(1:12), ]
E_30_cleaned <- E_30_selected[-(1:12), ]
E_31_cleaned <- E_31_selected[-(1:12), ]
E_33_cleaned <- E_33_selected[-(1:12), ]
E_34_cleaned <- E_34_selected[-(1:12), ]
E_35_cleaned <- E_35_selected[-(1:12), ]
E_36_cleaned <- E_36_selected[-(1:12), ]
E_37_cleaned <- E_37_selected[-(1:12), ]
E_38_cleaned <- E_38_selected[-(1:12), ]
E_39_cleaned <- E_39_selected[-(1:12), ]
E_40_cleaned <- E_40_selected[-(1:12), ]
E_41_cleaned <- E_41_selected[-(1:12), ]
E_42_cleaned <- E_42_selected[-(1:12), ]
E_43_cleaned <- E_43_selected[-(1:12), ]
E_44_cleaned <- E_44_selected[-(1:12), ]
E_45_cleaned <- E_45_selected[-(1:12), ]
E_46_cleaned <- E_46_selected[-(1:12), ]
E_47_cleaned <- E_47_selected[-(1:12), ]
E_48_cleaned <- E_48_selected[-(1:12), ]
E_50_cleaned <- E_50_selected[-(1:12), ]
E_51_cleaned <- E_51_selected[-(1:12), ]
E_52_cleaned <- E_52_selected[-(1:12), ]
E_53_cleaned <- E_53_selected[-(1:12), ]
E_55_cleaned <- E_55_selected[-(1:12), ]
E_56_cleaned <- E_56_selected[-(1:12), ]
E_57_cleaned <- E_57_selected[-(1:12), ]
E_59_cleaned <- E_59_selected[-(1:12), ]
E_60_cleaned <- E_60_selected[-(1:12), ]
E_61_cleaned <- E_61_selected[-(1:12), ]
E_62_cleaned <- E_62_selected[-(1:12), ]
E_63_cleaned <- E_63_selected[-(1:12), ]
E_64_cleaned <- E_64_selected[-(1:12), ]
E_65_cleaned <- E_65_selected[-(1:12), ]

#Attention Checks (with keys)
Odd_key <- c(4, 5, 2, 1, 2, 5, 1, 2, 0, 2, 4, 2, 3, 4, 2, 4, 3, 1, 1, 2)
Even_key <- c(3, 1, 4, 5, 1, 3, 5, 3, 2, 0, 4, 4, 4, 4, 2, 2, 5, 1, 0, 2)
E1_Enc <- cbind(E_1_cleaned[!is.na(E_1_cleaned$Attn_resp.keys),][1:nrow(E_1_cleaned[!is.na(E_1_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E1_Enc %>% summarise((sum(Enc_ans)/20)*100)
E2_Enc <- cbind(E_2_cleaned[!is.na(E_2_cleaned$Attn_resp.keys),][1:nrow(E_2_cleaned[!is.na(E_2_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E2_Enc %>% summarise((sum(Enc_ans)/20)*100)
E3_Enc <- cbind(E_3_cleaned[!is.na(E_3_cleaned$Attn_resp.keys),][1:nrow(E_3_cleaned[!is.na(E_3_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E3_Enc %>% summarise((sum(Enc_ans)/20)*100)
E4_Enc <- cbind(E_4_cleaned[!is.na(E_4_cleaned$Attn_resp.keys),][1:nrow(E_4_cleaned[!is.na(E_4_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E4_Enc %>% summarise((sum(Enc_ans)/20)*100)
E5_Enc <- cbind(E_5_cleaned[!is.na(E_5_cleaned$Attn_resp.keys),][1:nrow(E_5_cleaned[!is.na(E_5_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E5_Enc %>% summarise((sum(Enc_ans)/20)*100)
E6_Enc <- cbind(E_6_cleaned[!is.na(E_6_cleaned$Attn_resp.keys),][1:nrow(E_6_cleaned[!is.na(E_6_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E6_Enc %>% summarise((sum(Enc_ans)/20)*100)
E7_Enc <- cbind(E_7_cleaned[!is.na(E_7_cleaned$Attn_resp.keys),][1:nrow(E_7_cleaned[!is.na(E_7_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E7_Enc %>% summarise((sum(Enc_ans)/20)*100)
E8_Enc <- cbind(E_8_cleaned[!is.na(E_8_cleaned$Attn_resp.keys),][1:nrow(E_8_cleaned[!is.na(E_8_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E8_Enc %>% summarise((sum(Enc_ans)/20)*100)
E9_Enc <- cbind(E_9_cleaned[!is.na(E_9_cleaned$Attn_resp.keys),][1:nrow(E_9_cleaned[!is.na(E_9_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E9_Enc %>% summarise((sum(Enc_ans)/20)*100)
E10_Enc <- cbind(E_10_cleaned[!is.na(E_10_cleaned$Attn_resp.keys),][1:nrow(E_10_cleaned[!is.na(E_10_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E10_Enc %>% summarise((sum(Enc_ans)/20)*100)
E12_Enc <- cbind(E_12_cleaned[!is.na(E_12_cleaned$Attn_resp.keys),][1:nrow(E_12_cleaned[!is.na(E_12_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E12_Enc %>% summarise((sum(Enc_ans)/20)*100)
E14_Enc <- cbind(E_14_cleaned[!is.na(E_14_cleaned$Attn_resp.keys),][1:nrow(E_14_cleaned[!is.na(E_14_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E14_Enc %>% summarise((sum(Enc_ans)/20)*100)
E15_Enc <- cbind(E_15_cleaned[!is.na(E_15_cleaned$Attn_resp.keys),][1:nrow(E_15_cleaned[!is.na(E_15_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E15_Enc %>% summarise((sum(Enc_ans)/20)*100)
E16_Enc <- cbind(E_16_cleaned[!is.na(E_16_cleaned$Attn_resp.keys),][1:nrow(E_16_cleaned[!is.na(E_16_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E16_Enc %>% summarise((sum(Enc_ans)/20)*100)
E17_Enc <- cbind(E_17_cleaned[!is.na(E_17_cleaned$Attn_resp.keys),][1:nrow(E_17_cleaned[!is.na(E_17_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E17_Enc %>% summarise((sum(Enc_ans)/20)*100)
E19_Enc <- cbind(E_19_cleaned[!is.na(E_19_cleaned$Attn_resp.keys),][1:nrow(E_19_cleaned[!is.na(E_19_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E19_Enc %>% summarise((sum(Enc_ans)/20)*100)
E20_Enc <- cbind(E_20_cleaned[!is.na(E_20_cleaned$Attn_resp.keys),][1:nrow(E_20_cleaned[!is.na(E_20_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E20_Enc %>% summarise((sum(Enc_ans)/20)*100)
E21_Enc <- cbind(E_21_cleaned[!is.na(E_21_cleaned$Attn_resp.keys),][1:nrow(E_21_cleaned[!is.na(E_21_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E21_Enc %>% summarise((sum(Enc_ans)/20)*100)
E23_Enc <- cbind(E_23_cleaned[!is.na(E_23_cleaned$Attn_resp.keys),][1:nrow(E_23_cleaned[!is.na(E_23_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E23_Enc %>% summarise((sum(Enc_ans)/20)*100)
E24_Enc <- cbind(E_24_cleaned[!is.na(E_24_cleaned$Attn_resp.keys),][1:nrow(E_24_cleaned[!is.na(E_24_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E24_Enc %>% summarise((sum(Enc_ans)/20)*100)
E27_Enc <- cbind(E_27_cleaned[!is.na(E_27_cleaned$Attn_resp.keys),][1:nrow(E_27_cleaned[!is.na(E_27_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E27_Enc %>% summarise((sum(Enc_ans)/20)*100)
E28_Enc <- cbind(E_28_cleaned[!is.na(E_28_cleaned$Attn_resp.keys),][1:nrow(E_28_cleaned[!is.na(E_28_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E28_Enc %>% summarise((sum(Enc_ans)/20)*100)
E29_Enc <- cbind(E_29_cleaned[!is.na(E_29_cleaned$Attn_resp.keys),][1:nrow(E_29_cleaned[!is.na(E_29_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E29_Enc %>% summarise((sum(Enc_ans)/20)*100)
E30_Enc <- cbind(E_30_cleaned[!is.na(E_30_cleaned$Attn_resp.keys),][1:nrow(E_30_cleaned[!is.na(E_30_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E30_Enc %>% summarise((sum(Enc_ans)/20)*100)
E31_Enc <- cbind(E_31_cleaned[!is.na(E_31_cleaned$Attn_resp.keys),][1:nrow(E_31_cleaned[!is.na(E_31_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E31_Enc %>% summarise((sum(Enc_ans)/20)*100)
E33_Enc <- cbind(E_33_cleaned[!is.na(E_33_cleaned$Attn_resp.keys),][1:nrow(E_33_cleaned[!is.na(E_33_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E33_Enc %>% summarise((sum(Enc_ans)/20)*100)
E34_Enc <- cbind(E_34_cleaned[!is.na(E_34_cleaned$Attn_resp.keys),][1:nrow(E_34_cleaned[!is.na(E_34_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E34_Enc %>% summarise((sum(Enc_ans)/20)*100)
E35_Enc <- cbind(E_35_cleaned[!is.na(E_35_cleaned$Attn_resp.keys),][1:nrow(E_35_cleaned[!is.na(E_35_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E35_Enc %>% summarise((sum(Enc_ans)/20)*100)
E36_Enc <- cbind(E_36_cleaned[!is.na(E_36_cleaned$Attn_resp.keys),][1:nrow(E_36_cleaned[!is.na(E_36_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E36_Enc %>% summarise((sum(Enc_ans)/20)*100)
E37_Enc <- cbind(E_37_cleaned[!is.na(E_37_cleaned$Attn_resp.keys),][1:nrow(E_37_cleaned[!is.na(E_37_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E37_Enc %>% summarise((sum(Enc_ans)/20)*100)
E38_Enc <- cbind(E_38_cleaned[!is.na(E_38_cleaned$Attn_resp.keys),][1:nrow(E_38_cleaned[!is.na(E_38_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E38_Enc %>% summarise((sum(Enc_ans)/20)*100)
E39_Enc <- cbind(E_39_cleaned[!is.na(E_39_cleaned$Attn_resp.keys),][1:nrow(E_39_cleaned[!is.na(E_39_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E39_Enc %>% summarise((sum(Enc_ans)/20)*100)
E40_Enc <- cbind(E_40_cleaned[!is.na(E_40_cleaned$Attn_resp.keys),][1:nrow(E_40_cleaned[!is.na(E_40_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E40_Enc %>% summarise((sum(Enc_ans)/20)*100)
E41_Enc <- cbind(E_41_cleaned[!is.na(E_41_cleaned$Attn_resp.keys),][1:nrow(E_41_cleaned[!is.na(E_41_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E41_Enc %>% summarise((sum(Enc_ans)/20)*100)
E42_Enc <- cbind(E_42_cleaned[!is.na(E_42_cleaned$Attn_resp.keys),][1:nrow(E_42_cleaned[!is.na(E_42_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E42_Enc %>% summarise((sum(Enc_ans)/20)*100)
E43_Enc <- cbind(E_43_cleaned[!is.na(E_43_cleaned$Attn_resp.keys),][1:nrow(E_43_cleaned[!is.na(E_43_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E43_Enc %>% summarise((sum(Enc_ans)/20)*100)
E44_Enc <- cbind(E_44_cleaned[!is.na(E_44_cleaned$Attn_resp.keys),][1:nrow(E_44_cleaned[!is.na(E_44_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E44_Enc %>% summarise((sum(Enc_ans)/20)*100)
E45_Enc <- cbind(E_45_cleaned[!is.na(E_45_cleaned$Attn_resp.keys),][1:nrow(E_45_cleaned[!is.na(E_45_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E45_Enc %>% summarise((sum(Enc_ans)/20)*100)
E46_Enc <- cbind(E_46_cleaned[!is.na(E_46_cleaned$Attn_resp.keys),][1:nrow(E_46_cleaned[!is.na(E_46_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E46_Enc %>% summarise((sum(Enc_ans)/20)*100)
E47_Enc <- cbind(E_47_cleaned[!is.na(E_47_cleaned$Attn_resp.keys),][1:nrow(E_47_cleaned[!is.na(E_47_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E47_Enc %>% summarise((sum(Enc_ans)/20)*100)
E48_Enc <- cbind(E_48_cleaned[!is.na(E_48_cleaned$Attn_resp.keys),][1:nrow(E_48_cleaned[!is.na(E_48_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E48_Enc %>% summarise((sum(Enc_ans)/20)*100)
E50_Enc <- cbind(E_50_cleaned[!is.na(E_50_cleaned$Attn_resp.keys),][1:nrow(E_50_cleaned[!is.na(E_50_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E50_Enc %>% summarise((sum(Enc_ans)/20)*100)
E51_Enc <- cbind(E_51_cleaned[!is.na(E_51_cleaned$Attn_resp.keys),][1:nrow(E_51_cleaned[!is.na(E_51_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E51_Enc %>% summarise((sum(Enc_ans)/20)*100)
E52_Enc <- cbind(E_52_cleaned[!is.na(E_52_cleaned$Attn_resp.keys),][1:nrow(E_52_cleaned[!is.na(E_52_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E52_Enc %>% summarise((sum(Enc_ans)/20)*100)
E53_Enc <- cbind(E_53_cleaned[!is.na(E_53_cleaned$Attn_resp.keys),][1:nrow(E_53_cleaned[!is.na(E_53_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E53_Enc %>% summarise((sum(Enc_ans)/20)*100)
E55_Enc <- cbind(E_55_cleaned[!is.na(E_55_cleaned$Attn_resp.keys),][1:nrow(E_55_cleaned[!is.na(E_55_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E55_Enc %>% summarise((sum(Enc_ans)/20)*100)
E56_Enc <- cbind(E_56_cleaned[!is.na(E_56_cleaned$Attn_resp.keys),][1:nrow(E_56_cleaned[!is.na(E_56_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E56_Enc %>% summarise((sum(Enc_ans)/20)*100)
E57_Enc <- cbind(E_57_cleaned[!is.na(E_57_cleaned$Attn_resp.keys),][1:nrow(E_57_cleaned[!is.na(E_57_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E57_Enc %>% summarise((sum(Enc_ans)/20)*100)
E59_Enc <- cbind(E_59_cleaned[!is.na(E_59_cleaned$Attn_resp.keys),][1:nrow(E_59_cleaned[!is.na(E_59_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E59_Enc %>% summarise((sum(Enc_ans)/20)*100)
E60_Enc <- cbind(E_60_cleaned[!is.na(E_60_cleaned$Attn_resp.keys),][1:nrow(E_60_cleaned[!is.na(E_60_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E60_Enc %>% summarise((sum(Enc_ans)/20)*100)
E61_Enc <- cbind(E_61_cleaned[!is.na(E_61_cleaned$Attn_resp.keys),][1:nrow(E_61_cleaned[!is.na(E_61_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E61_Enc %>% summarise((sum(Enc_ans)/20)*100)
E62_Enc <- cbind(E_62_cleaned[!is.na(E_62_cleaned$Attn_resp.keys),][1:nrow(E_62_cleaned[!is.na(E_62_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E62_Enc %>% summarise((sum(Enc_ans)/20)*100)
E63_Enc <- cbind(E_63_cleaned[!is.na(E_63_cleaned$Attn_resp.keys),][1:nrow(E_63_cleaned[!is.na(E_63_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E63_Enc %>% summarise((sum(Enc_ans)/20)*100)
E64_Enc <- cbind(E_64_cleaned[!is.na(E_64_cleaned$Attn_resp.keys),][1:nrow(E_64_cleaned[!is.na(E_64_cleaned$Attn_resp.keys),]),], Even_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Even_key, 1, 0)); E64_Enc %>% summarise((sum(Enc_ans)/20)*100)
E65_Enc <- cbind(E_65_cleaned[!is.na(E_65_cleaned$Attn_resp.keys),][1:nrow(E_65_cleaned[!is.na(E_65_cleaned$Attn_resp.keys),]),], Odd_key) %>% mutate(Enc_ans = ifelse(Attn_resp.keys==Odd_key, 1, 0)); E65_Enc %>% summarise((sum(Enc_ans)/20)*100)

#Join dataframes together
Encoding_Data <- bind_rows(E_1_cleaned, E_2_cleaned, E_3_cleaned, E_4_cleaned, E_5_cleaned, E_6_cleaned, E_7_cleaned, E_8_cleaned, E_9_cleaned, E_10_cleaned, E_12_cleaned, E_14_cleaned, E_15_cleaned, E_16_cleaned, E_17_cleaned, E_19_cleaned, E_20_cleaned, E_21_cleaned, E_23_cleaned, E_24_cleaned, E_27_cleaned, E_28_cleaned, E_29_cleaned, E_30_cleaned, E_31_cleaned, E_33_cleaned, E_34_cleaned, E_35_cleaned, E_36_cleaned, E_37_cleaned, E_38_cleaned, E_39_cleaned, E_40_cleaned, E_41_cleaned, E_42_cleaned, E_43_cleaned, E_44_cleaned, E_45_cleaned, E_46_cleaned, E_47_cleaned, E_48_cleaned, E_50_cleaned, E_51_cleaned, E_52_cleaned, E_53_cleaned, E_55_cleaned, E_56_cleaned, E_57_cleaned, E_59_cleaned, E_60_cleaned, E_61_cleaned, E_62_cleaned, E_63_cleaned, E_64_cleaned, E_65_cleaned)  

#Filter data for attention checks
Encoding_Attn <- Encoding_Data[!is.na(Encoding_Data$Attn_resp.keys),]
Encoding_Attn %>% group_by(participant) %>% summarize(count = n()) #Count the number of encoding trials per participant

## Item Recognition ##
#Combine Recognition Data
RO_1_selected <- RO_1 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 1)
RO_2_selected <- RO_2 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 2)
RO_3_selected <- RO_3 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 3)
RO_4_selected <- RO_4 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 4)
RO_5_selected <- RO_5 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 5)
RO_6_selected <- RO_6 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 6)
RO_7_selected <- RO_7 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 7)
RO_8_selected <- RO_8 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 8)
RO_9_selected <- RO_9 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 9)
RO_10_selected <- RO_10 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 10)
RO_12_selected <- RO_12 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 12)
RO_14_selected <- RO_14 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 14)
RO_15_selected <- RO_15 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 15)
RO_16_selected <- RO_16 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 16)
RO_17_selected <- RO_17 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 17)
RO_19_selected <- RO_19 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 19)
RO_20_selected <- RO_20 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 20)
RO_21_selected <- RO_21 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 21)
RO_23_selected <- RO_23 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 23)
RO_24_selected <- RO_24 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 24)
RO_27_selected <- RO_27 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 27)
RO_28_selected <- RO_28 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 28)
RO_29_selected <- RO_29 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 29)
RO_30_selected <- RO_30 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 30)
RO_31_selected <- RO_31 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 31)
RO_33_selected <- RO_33 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 33)
RO_34_selected <- RO_34 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 34)
RO_35_selected <- RO_35 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 35)
RO_36_selected <- RO_36 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 36)
RO_37_selected <- RO_37 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 37)
RO_38_selected <- RO_38 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 38)
RO_39_selected <- RO_39 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 39)
RO_40_selected <- RO_40 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 40)
RO_41_selected <- RO_41 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 41)
RO_42_selected <- RO_42 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 42)
RO_43_selected <- RO_43 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 43)
RO_44_selected <- RO_44 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 44)
RO_45_selected <- RO_45 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 45)
RO_46_selected <- RO_46 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 46)
RO_47_selected <- RO_47 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 47)
RO_48_selected <- RO_48 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 48)
RO_50_selected <- RO_50 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 50)
RO_51_selected <- RO_51 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 51)
RO_52_selected <- RO_52 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 52)
RO_53_selected <- RO_53 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 53)
RO_55_selected <- RO_55 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 55)
RO_56_selected <- RO_56 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 56)
RO_57_selected <- RO_57 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 57)
RO_59_selected <- RO_59 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 59)
RO_60_selected <- RO_60 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 60)
RO_61_selected <- RO_61 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 61)
RO_62_selected <- RO_62 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 62)
RO_63_selected <- RO_63 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 63)
RO_64_selected <- RO_64 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 64)
RO_65_selected <- RO_65 %>%
  select(L_Scene, R_Scene, L_Pts, R_Pts, Encoding_Resp.keys) %>% mutate(participant = 65)


#Filter out practice data
RO_1_cleaned <- RO_1_selected[-(1:12), ]
RO_2_cleaned <- RO_2_selected[-(1:12), ]
RO_3_cleaned <- RO_3_selected[-(1:12), ]
RO_4_cleaned <- RO_4_selected[-(1:12), ]
RO_5_cleaned <- RO_5_selected[-(1:12), ]
RO_6_cleaned <- RO_6_selected[-(1:12), ]
RO_7_cleaned <- RO_7_selected[-(1:12), ]
RO_8_cleaned <- RO_8_selected[-(1:12), ]
RO_9_cleaned <- RO_9_selected[-(1:12), ]
RO_10_cleaned <- RO_10_selected[-(1:12), ]
RO_12_cleaned <- RO_12_selected[-(1:12), ]
RO_14_cleaned <- RO_14_selected[-(1:12), ]
RO_15_cleaned <- RO_15_selected[-(1:12), ]
RO_16_cleaned <- RO_16_selected[-(1:12), ]
RO_17_cleaned <- RO_17_selected[-(1:12), ]
RO_19_cleaned <- RO_19_selected[-(1:12), ]
RO_20_cleaned <- RO_20_selected[-(1:12), ]
RO_21_cleaned <- RO_21_selected[-(1:12), ]
RO_23_cleaned <- RO_23_selected[-(1:12), ]
RO_24_cleaned <- RO_24_selected[-(1:12), ]
RO_27_cleaned <- RO_27_selected[-(1:12), ]
RO_28_cleaned <- RO_28_selected[-(1:12), ]
RO_29_cleaned <- RO_29_selected[-(1:12), ]
RO_30_cleaned <- RO_30_selected[-(1:12), ]
RO_31_cleaned <- RO_31_selected[-(1:12), ]
RO_33_cleaned <- RO_33_selected[-(1:12), ]
RO_34_cleaned <- RO_34_selected[-(1:12), ]
RO_35_cleaned <- RO_35_selected[-(1:12), ]
RO_36_cleaned <- RO_36_selected[-(1:12), ]
RO_37_cleaned <- RO_37_selected[-(1:12), ]
RO_38_cleaned <- RO_38_selected[-(1:12), ]
RO_39_cleaned <- RO_39_selected[-(1:12), ]
RO_40_cleaned <- RO_40_selected[-(1:12), ]
RO_41_cleaned <- RO_41_selected[-(1:12), ]
RO_42_cleaned <- RO_42_selected[-(1:12), ]
RO_43_cleaned <- RO_43_selected[-(1:12), ]
RO_44_cleaned <- RO_44_selected[-(1:12), ]
RO_45_cleaned <- RO_45_selected[-(1:12), ]
RO_46_cleaned <- RO_46_selected[-(1:12), ]
RO_47_cleaned <- RO_47_selected[-(1:12), ]
RO_48_cleaned <- RO_48_selected[-(1:12), ]
RO_50_cleaned <- RO_50_selected[-(1:12), ]
RO_51_cleaned <- RO_51_selected[-(1:12), ]
RO_52_cleaned <- RO_52_selected[-(1:12), ]
RO_53_cleaned <- RO_53_selected[-(1:12), ]
RO_55_cleaned <- RO_55_selected[-(1:12), ]
RO_56_cleaned <- RO_56_selected[-(1:12), ]
RO_57_cleaned <- RO_57_selected[-(1:12), ]
RO_59_cleaned <- RO_59_selected[-(1:12), ]
RO_60_cleaned <- RO_60_selected[-(1:12), ]
RO_61_cleaned <- RO_61_selected[-(1:12), ]
RO_62_cleaned <- RO_62_selected[-(1:12), ]
RO_63_cleaned <- RO_63_selected[-(1:12), ]
RO_64_cleaned <- RO_64_selected[-(1:12), ]
RO_65_cleaned <- RO_65_selected[-(1:12), ]


#Add Old/New trial information
# Create function to determine Old & New identities based on encoding data
OldNew_Stim <- function(Encoding_df, Response_df) {
  
  # Determine which encoding trials were used to create OldStim (combining both columns)
  Step1 <- Encoding_df$L_Scene
  Step2 <- Encoding_df$R_Scene
  OldStim <- c(Step1, Step2)  # Combine and remove duplicates
  
  # Vectorized check for L_Scene or R_Scene in OldStim
  Response_df <- Response_df %>%
    mutate(OldNew = case_when(
      L_Scene %in% OldStim & R_Scene %in% OldStim ~ 'OO',  # Both old
      L_Scene %in% OldStim & !(R_Scene %in% OldStim) ~ 'ON',  # Left old, right new
      !(L_Scene %in% OldStim) & R_Scene %in% OldStim ~ 'ON',  # Left new, right old
      TRUE ~ 'NN'  # Default case: Both new
    ))
  
  return(Response_df)
}


#Add Old/New stim information to dataframe
RO_1_cleaned <- OldNew_Stim(E_1_cleaned, RO_1_cleaned)
RO_2_cleaned <- OldNew_Stim(E_2_cleaned, RO_2_cleaned)
RO_3_cleaned <- OldNew_Stim(E_3_cleaned, RO_3_cleaned)
RO_4_cleaned <- OldNew_Stim(E_4_cleaned, RO_4_cleaned)
RO_5_cleaned <- OldNew_Stim(E_5_cleaned, RO_5_cleaned)
RO_6_cleaned <- OldNew_Stim(E_6_cleaned, RO_6_cleaned)
RO_7_cleaned <- OldNew_Stim(E_7_cleaned, RO_7_cleaned)
RO_8_cleaned <- OldNew_Stim(E_8_cleaned, RO_8_cleaned)
RO_9_cleaned <- OldNew_Stim(E_9_cleaned, RO_9_cleaned)
RO_10_cleaned <- OldNew_Stim(E_10_cleaned, RO_10_cleaned)
RO_12_cleaned <- OldNew_Stim(E_12_cleaned, RO_12_cleaned)
RO_14_cleaned <- OldNew_Stim(E_14_cleaned, RO_14_cleaned)
RO_15_cleaned <- OldNew_Stim(E_15_cleaned, RO_15_cleaned)
RO_16_cleaned <- OldNew_Stim(E_16_cleaned, RO_16_cleaned)
RO_17_cleaned <- OldNew_Stim(E_17_cleaned, RO_17_cleaned)
RO_19_cleaned <- OldNew_Stim(E_19_cleaned, RO_19_cleaned)
RO_20_cleaned <- OldNew_Stim(E_20_cleaned, RO_20_cleaned)
RO_21_cleaned <- OldNew_Stim(E_21_cleaned, RO_21_cleaned)
RO_23_cleaned <- OldNew_Stim(E_23_cleaned, RO_23_cleaned)
RO_24_cleaned <- OldNew_Stim(E_24_cleaned, RO_24_cleaned)
RO_27_cleaned <- OldNew_Stim(E_27_cleaned, RO_27_cleaned)
RO_28_cleaned <- OldNew_Stim(E_28_cleaned, RO_28_cleaned)
RO_29_cleaned <- OldNew_Stim(E_29_cleaned, RO_29_cleaned)
RO_30_cleaned <- OldNew_Stim(E_30_cleaned, RO_30_cleaned)
RO_31_cleaned <- OldNew_Stim(E_31_cleaned, RO_31_cleaned)
RO_33_cleaned <- OldNew_Stim(E_33_cleaned, RO_33_cleaned)
RO_34_cleaned <- OldNew_Stim(E_34_cleaned, RO_34_cleaned)
RO_35_cleaned <- OldNew_Stim(E_35_cleaned, RO_35_cleaned)
RO_36_cleaned <- OldNew_Stim(E_36_cleaned, RO_36_cleaned)
RO_37_cleaned <- OldNew_Stim(E_37_cleaned, RO_37_cleaned)
RO_38_cleaned <- OldNew_Stim(E_38_cleaned, RO_38_cleaned)
RO_39_cleaned <- OldNew_Stim(E_39_cleaned, RO_39_cleaned)
RO_40_cleaned <- OldNew_Stim(E_40_cleaned, RO_40_cleaned)
RO_41_cleaned <- OldNew_Stim(E_41_cleaned, RO_41_cleaned)
RO_42_cleaned <- OldNew_Stim(E_42_cleaned, RO_42_cleaned)
RO_43_cleaned <- OldNew_Stim(E_43_cleaned, RO_43_cleaned)
RO_44_cleaned <- OldNew_Stim(E_44_cleaned, RO_44_cleaned)
RO_45_cleaned <- OldNew_Stim(E_45_cleaned, RO_45_cleaned)
RO_46_cleaned <- OldNew_Stim(E_46_cleaned, RO_46_cleaned)
RO_47_cleaned <- OldNew_Stim(E_47_cleaned, RO_47_cleaned)
RO_48_cleaned <- OldNew_Stim(E_48_cleaned, RO_48_cleaned)
RO_50_cleaned <- OldNew_Stim(E_50_cleaned, RO_50_cleaned)
RO_51_cleaned <- OldNew_Stim(E_51_cleaned, RO_51_cleaned)
RO_52_cleaned <- OldNew_Stim(E_52_cleaned, RO_52_cleaned)
RO_53_cleaned <- OldNew_Stim(E_53_cleaned, RO_53_cleaned)
RO_55_cleaned <- OldNew_Stim(E_55_cleaned, RO_55_cleaned)
RO_56_cleaned <- OldNew_Stim(E_56_cleaned, RO_56_cleaned)
RO_57_cleaned <- OldNew_Stim(E_57_cleaned, RO_57_cleaned)
RO_59_cleaned <- OldNew_Stim(E_59_cleaned, RO_59_cleaned)
RO_60_cleaned <- OldNew_Stim(E_60_cleaned, RO_60_cleaned)
RO_61_cleaned <- OldNew_Stim(E_61_cleaned, RO_61_cleaned)
RO_62_cleaned <- OldNew_Stim(E_62_cleaned, RO_62_cleaned)
RO_63_cleaned <- OldNew_Stim(E_63_cleaned, RO_63_cleaned)
RO_64_cleaned <- OldNew_Stim(E_64_cleaned, RO_64_cleaned)
RO_65_cleaned <- OldNew_Stim(E_65_cleaned, RO_65_cleaned)

#Add Old stim information to dataframe
# Create function to determine Old stim identity based on encoding data
Old_Stim <- function(Encoding_df, Response_df) {
  
  # Determine which encoding trials were used to create OldStim (combining both columns)
  Step1 <- Encoding_df$L_Scene
  Step2 <- Encoding_df$R_Scene
  OldStim <- c(Step1, Step2)  # Combine and remove duplicates
  
  # Vectorized check for L_Scene or R_Scene in OldStim
  Response_df <- Response_df %>%
    mutate(Old = case_when(
      L_Scene %in% OldStim & R_Scene %in% OldStim ~ 'Both',  # Both old
      L_Scene %in% OldStim & !(R_Scene %in% OldStim) ~ 'L',  # Left old, right new
      !(L_Scene %in% OldStim) & R_Scene %in% OldStim ~ 'R',  # Left new, right old
      TRUE ~ 'NN'  # Default case: Both new
    ))
  
  return(Response_df)
}

#Add Old stim information to dataframe
RO_1_cleaned <- Old_Stim(E_1_cleaned, RO_1_cleaned)
RO_2_cleaned <- Old_Stim(E_2_cleaned, RO_2_cleaned)
RO_3_cleaned <- Old_Stim(E_3_cleaned, RO_3_cleaned)
RO_4_cleaned <- Old_Stim(E_4_cleaned, RO_4_cleaned)
RO_5_cleaned <- Old_Stim(E_5_cleaned, RO_5_cleaned)
RO_6_cleaned <- Old_Stim(E_6_cleaned, RO_6_cleaned)
RO_7_cleaned <- Old_Stim(E_7_cleaned, RO_7_cleaned)
RO_8_cleaned <- Old_Stim(E_8_cleaned, RO_8_cleaned)
RO_9_cleaned <- Old_Stim(E_9_cleaned, RO_9_cleaned)
RO_10_cleaned <- Old_Stim(E_10_cleaned, RO_10_cleaned)
RO_12_cleaned <- Old_Stim(E_12_cleaned, RO_12_cleaned)
RO_14_cleaned <- Old_Stim(E_14_cleaned, RO_14_cleaned)
RO_15_cleaned <- Old_Stim(E_15_cleaned, RO_15_cleaned)
RO_16_cleaned <- Old_Stim(E_16_cleaned, RO_16_cleaned)
RO_17_cleaned <- Old_Stim(E_17_cleaned, RO_17_cleaned)
RO_19_cleaned <- Old_Stim(E_19_cleaned, RO_19_cleaned)
RO_20_cleaned <- Old_Stim(E_20_cleaned, RO_20_cleaned)
RO_21_cleaned <- Old_Stim(E_21_cleaned, RO_21_cleaned)
RO_23_cleaned <- Old_Stim(E_23_cleaned, RO_23_cleaned)
RO_24_cleaned <- Old_Stim(E_24_cleaned, RO_24_cleaned)
RO_27_cleaned <- Old_Stim(E_27_cleaned, RO_27_cleaned)
RO_28_cleaned <- Old_Stim(E_28_cleaned, RO_28_cleaned)
RO_29_cleaned <- Old_Stim(E_29_cleaned, RO_29_cleaned)
RO_30_cleaned <- Old_Stim(E_30_cleaned, RO_30_cleaned)
RO_31_cleaned <- Old_Stim(E_31_cleaned, RO_31_cleaned)
RO_33_cleaned <- Old_Stim(E_33_cleaned, RO_33_cleaned)
RO_34_cleaned <- Old_Stim(E_34_cleaned, RO_34_cleaned)
RO_35_cleaned <- Old_Stim(E_35_cleaned, RO_35_cleaned)
RO_36_cleaned <- Old_Stim(E_36_cleaned, RO_36_cleaned)
RO_37_cleaned <- Old_Stim(E_37_cleaned, RO_37_cleaned)
RO_38_cleaned <- Old_Stim(E_38_cleaned, RO_38_cleaned)
RO_39_cleaned <- Old_Stim(E_39_cleaned, RO_39_cleaned)
RO_40_cleaned <- Old_Stim(E_40_cleaned, RO_40_cleaned)
RO_41_cleaned <- Old_Stim(E_41_cleaned, RO_41_cleaned)
RO_42_cleaned <- Old_Stim(E_42_cleaned, RO_42_cleaned)
RO_43_cleaned <- Old_Stim(E_43_cleaned, RO_43_cleaned)
RO_44_cleaned <- Old_Stim(E_44_cleaned, RO_44_cleaned)
RO_45_cleaned <- Old_Stim(E_45_cleaned, RO_45_cleaned)
RO_46_cleaned <- Old_Stim(E_46_cleaned, RO_46_cleaned)
RO_47_cleaned <- Old_Stim(E_47_cleaned, RO_47_cleaned)
RO_48_cleaned <- Old_Stim(E_48_cleaned, RO_48_cleaned)
RO_50_cleaned <- Old_Stim(E_50_cleaned, RO_50_cleaned)
RO_51_cleaned <- Old_Stim(E_51_cleaned, RO_51_cleaned)
RO_52_cleaned <- Old_Stim(E_52_cleaned, RO_52_cleaned)
RO_53_cleaned <- Old_Stim(E_53_cleaned, RO_53_cleaned)
RO_55_cleaned <- Old_Stim(E_55_cleaned, RO_55_cleaned)
RO_56_cleaned <- Old_Stim(E_56_cleaned, RO_56_cleaned)
RO_57_cleaned <- Old_Stim(E_57_cleaned, RO_57_cleaned)
RO_59_cleaned <- Old_Stim(E_59_cleaned, RO_59_cleaned)
RO_60_cleaned <- Old_Stim(E_60_cleaned, RO_60_cleaned)
RO_61_cleaned <- Old_Stim(E_61_cleaned, RO_61_cleaned)
RO_62_cleaned <- Old_Stim(E_62_cleaned, RO_62_cleaned)
RO_63_cleaned <- Old_Stim(E_63_cleaned, RO_63_cleaned)
RO_64_cleaned <- Old_Stim(E_64_cleaned, RO_64_cleaned)
RO_65_cleaned <- Old_Stim(E_65_cleaned, RO_65_cleaned)


#Join dataframes together
Recognition_Data <- bind_rows(RO_1_cleaned, RO_2_cleaned, RO_3_cleaned, RO_4_cleaned, RO_5_cleaned, RO_6_cleaned, RO_7_cleaned, RO_8_cleaned, RO_9_cleaned, RO_10_cleaned, RO_12_cleaned, RO_14_cleaned, RO_15_cleaned, RO_16_cleaned, RO_17_cleaned, RO_19_cleaned, RO_20_cleaned, RO_21_cleaned, RO_23_cleaned, RO_24_cleaned, RO_27_cleaned, RO_28_cleaned, RO_29_cleaned, RO_30_cleaned, RO_31_cleaned, RO_33_cleaned, RO_34_cleaned, RO_35_cleaned, RO_36_cleaned, RO_37_cleaned, RO_38_cleaned, RO_39_cleaned, RO_40_cleaned, RO_41_cleaned, RO_42_cleaned, RO_43_cleaned, RO_44_cleaned, RO_45_cleaned, RO_46_cleaned, RO_47_cleaned, RO_48_cleaned, RO_50_cleaned, RO_51_cleaned, RO_52_cleaned, RO_53_cleaned, RO_55_cleaned, RO_56_cleaned, RO_57_cleaned, RO_59_cleaned, RO_60_cleaned, RO_61_cleaned, RO_62_cleaned, RO_63_cleaned, RO_64_cleaned, RO_65_cleaned) 

#Define vector of relevant variables for item responses
#Select relevant variables
#Change the confidence responses to be binary recognition
# Note: each confidence response has both a qualitative (yes/no) and quantitative (1, 2, 3) dimension
Recognition_Data <- Recognition_Data %>%
  mutate(Binary_Recognition = case_when(
    Encoding_Resp.keys %in% c('a', 's', 'd') ~ 'L',
    Encoding_Resp.keys %in% c('j', 'k', 'l') ~ 'R',
    TRUE ~ NA_character_  # This handles any other cases, assigning NA to them
  ))

#Create function which considers each users answer key to determine correctness
Correct_Resp <- function(Response_df) {
  # Create an empty vector to store the results
  Response_df_acc <- vector("character", nrow(Response_df))
  
  # Loop over rows of the dataframe
  for (i in 1:nrow(Response_df)) {
    # Extract substrings and compare them for each row
    Response_df_acc[i] <- ifelse(as.integer(substring(Response_df[i, 4], 1, 1)) > as.integer(substring(Response_df[i, 3], 1, 1)), 
                                 "R", 
                                 ifelse(as.integer(substring(Response_df[i, 4], 1, 1)) < as.integer(substring(Response_df[i, 3], 1, 1)), "L", NA))
  }
  
  return(Response_df_acc)
}
#Append right answer to data frame
Recognition_Data$CorResp <- Correct_Resp(Recognition_Data)
#Code responses for Signal Detection THeory
#Create dataframes for descriptive statistics by dummy coding responses
Recognition_SDT <- Recognition_Data %>% mutate(ItemRespType = case_when((CorResp == 'R') & (Binary_Recognition == 'R') ~ 'Hit',
                                                                            (CorResp == 'L') & (Binary_Recognition == 'R') ~ 'FA',
                                                                            (CorResp == 'R') & (Binary_Recognition == 'L') ~ 'Miss',
                                                                            (CorResp == 'L') & (Binary_Recognition == 'L') ~ 'CoRej')) 
Recognition_Binary <- Recognition_Data %>% mutate(ItemRespType = case_when((CorResp == 'R') & (Binary_Recognition == 'R') ~ '1',
                                                                     (CorResp == 'L') & (Binary_Recognition == 'R') ~ '0',
                                                                     (CorResp == 'R') & (Binary_Recognition == 'L') ~ '0',
                                                                     (CorResp == 'L') & (Binary_Recognition == 'L') ~ '1'))

#Calculate total points
#Create function which considers each users answer key to determine total points earned
Choice_Pts <- function(Response_df) {
  # Create an empty vector to store the results
  Response_df_pts <- vector("character", nrow(Response_df))
  
  # Loop over rows of the dataframe
  for (i in 1:nrow(Response_df)) {
    # Extract substrings and compare them for each row
    Response_df_pts[i] <- ifelse(as.integer(substring(Response_df[i, 4], 1, 1)) > as.integer(substring(Response_df[i, 3], 1, 1)), 
                                 as.integer(substring(Response_df[i, 4], 1, 1)), 
                                 ifelse(as.integer(substring(Response_df[i, 4], 1, 1)) < as.integer(substring(Response_df[i, 3], 1, 1)), as.integer(substring(Response_df[i, 3], 1, 1)), NA))
  }
  
  return(Response_df_pts)
}

#Create function which considers each users options to calculate point difference
Pt_diff <- function(Response_df) {
  # Create an empty vector to store the results
  pts <- vector("character", nrow(Response_df))
  
  # Loop over rows of the dataframe
  for (i in 1:nrow(Response_df)) {
    # Extract substrings and compare them for each row
    pts[i] <- as.integer(substring(Response_df[i, 4], 1, 1)) - as.integer(substring(Response_df[i, 3], 1, 1))
  }
  return(pts)
}

#Create function which considers each users choice to calculate the accuracy of decisions
Chc_Acc <- function(Response_df) {
  # Create an empty vector to store the results
  Corr <- vector("character", nrow(Response_df))
  
  # Loop over rows of the dataframe
  for (i in 1:nrow(Response_df)) {
    # Extract substrings and compare them for each row
    Corr[i] <- ifelse(Response_df[i, 9] == Response_df[i, 10], 1, 0)
  }
  return(Corr)
}

#Add valence information to dataframe
# Create function to determine Valence identity based on rating data
Val_Stim <- function(Neutral_df, Negative_df, Response_df) {
  
  # Determine the valence of stimuli used to create stimulus sets
  Neutral <- Neutral_df$Stim
  Negative <- Negative_df$Stim
  
  # Vectorized check for L_Scene or R_Scene in OldStim
  Response_df <- Response_df %>%
    mutate(Valence = case_when(
      L_Scene %in% Neutral | R_Scene %in% Neutral ~ 'Neutral', 
      L_Scene %in% Negative | R_Scene %in% Negative ~ 'Negative',  
      TRUE ~ 'Error'
    ))
  
  return(Response_df)
}

#Append points columns to recognition dataframe
Recognition_Data$Pts <- Choice_Pts(Recognition_Data) #Points earned (based on choices) 
Recognition_Data$Diff <- Pt_diff(Recognition_Data) #Point difference between available choices

#Append accuracy column to recognition dataframe
Recognition_Data$Chc_Accuracy <- Chc_Acc(Recognition_Data)

#Append valence column to recognition dataframe
Recognition_Data <- Val_Stim(Neut_stim, Neg_stim, Recognition_Data)

#Filter out trials if necessary
Recognition_Data <- Recognition_Data[!is.na(Recognition_Data$Chc_Accuracy),]

#Count and display how many trials each participant has
Recognition_Data %>% group_by(participant) %>% summarize(count = n())
Recognition_Data %>% group_by(participant) %>% summarize(count = n()) %>% ggplot(aes(x=count))+geom_bar()

#Add confidence ratings
Recognition_Data <- Recognition_Data %>% mutate(Confidence = case_when(
  Encoding_Resp.keys %in% c('d', 'j') ~ 1,
  Encoding_Resp.keys %in% c('s', 'k') ~ 2,
  Encoding_Resp.keys %in% c('a', 'l') ~ 3
))

#List of exclusion criteria
# Encoding Checks
# 1) Fail to respond on more than 4 encoding trials (>1%)
# None

# 2) who perform below 90% on the encoding attention checks.
# None

# Recognition Checks
# 3) Did not use the complete range of the confidence scale (as explicitly instructed)
Recognition_Binary %>% ggplot(aes(x = Encoding_Resp.keys)) + geom_bar() + facet_wrap(.~participant, ncol=4) #Plot Confindence (Visual inspection)
Recognition_Binary %>% group_by(participant, Encoding_Resp.keys) %>% count()   # Tally confidence (numerical inspection)
Incomplete_Conf <- c(3, 7, 10, 41, 42)

#Filter data for usable participants
Usbl_Rec <- Recognition_Data %>% filter(!participant %in% c(Incomplete_Conf))

#### Encoding Performance ####
## Exploratory data analysis ##

#Overall Scores
Recognition_Data %>% group_by(participant) %>% summarise(Ovrl_Pts = sum(as.numeric(Pts), na.rm = TRUE))


#### H-Meta-d' Dataprep ####

#FITTING META D'
## Data from each subject need to be coerced into two vectos nR_S1 & nR_S2 , which CONTAIN CONFIDENCE-RATING COUNTS
##  for when the 'stimulus' was S1 & S2, respectively.
## Confidence counts are entered such that the first entry refers to counts of maximum confidence in an S1 response,
##  and the last entry to maximum confidence in an S2 response. (S1_Pres_3, S1_Pres_2, S1_Pres_1, S2_Abs_1, S2_Abs_2, S2_Abs_3)

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R1_Old)
Confidence_Data_S1_R1 <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'R') %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success
Confidence_Data_S1_R1$`1`[is.na(Confidence_Data_S1_R1$`1`)] <- 0; Confidence_Data_S1_R1$`2`[is.na(Confidence_Data_S1_R1$`2`)] <- 0; Confidence_Data_S1_R1$`3`[is.na(Confidence_Data_S1_R1$`3`)] <- 0
Confidence_Data_S1_R1 <- Confidence_Data_S1_R1 %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S1_R1<-data.frame(Confidence_Data_S1_R1) 
colnames(Confidence_Data_S1_R1) <- c("ID", "3", "2","1")
Confidence_Data_S1_R1 <- as.matrix(Confidence_Data_S1_R1)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R2_New)
Confidence_Data_S1_R2 <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'L') %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of failure
Confidence_Data_S1_R2[is.na(Confidence_Data_S1_R2)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R2 <- as.matrix(Confidence_Data_S1_R2)
Confidence_Data_S1_R2 <- data.frame(Confidence_Data_S1_R2) 
colnames(Confidence_Data_S1_R2) <- c("ID", "1", "2","3")
Confidence_Data_S1_R2 <- as.matrix(Confidence_Data_S1_R2)[,2:4]

#Combine matrices into single matrix representing confidence data x response for stimulus 1
#Confidence Rating count when the 'stimulus' was S1
nR_S1 <-  cbind(Confidence_Data_S1_R1, Confidence_Data_S1_R2)     

#Coerce Data frame to quantify confidence counts x Stimulus type (New) x response (Old)
Confidence_Data_S2_R1 <- Usbl_Rec %>%
  filter(CorResp == "L" & Binary_Recognition == "R") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
# Convert the column to numeric type
Confidence_Data_S2_R1$`1`[is.na(Confidence_Data_S2_R1$`1`)] <- 0; Confidence_Data_S2_R1$`2`[is.na(Confidence_Data_S2_R1$`2`)] <- 0.; Confidence_Data_S2_R1$`3`[is.na(Confidence_Data_S2_R1$`3`)] <- 0
Confidence_Data_S2_R1 <- Confidence_Data_S2_R1 %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S2_R1<-data.frame(Confidence_Data_S2_R1) 
colnames(Confidence_Data_S2_R1) <- c("ID", "3", "2","1")
Confidence_Data_S2_R1 <- as.matrix(Confidence_Data_S2_R1)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (New) x response (New))
Confidence_Data_S2_R2 <-  Usbl_Rec %>%
  filter(CorResp == "L" & Binary_Recognition == "L") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of failure
Confidence_Data_S2_R2[is.na(Confidence_Data_S2_R2)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R2<-data.frame(Confidence_Data_S2_R2) 
colnames(Confidence_Data_S2_R2) <- c("ID", "1", "2","3")
Confidence_Data_S2_R2 <- as.matrix(Confidence_Data_S2_R2)[,2:4]

#Combine matrices into single matrix representing confidence data x response for stimulus 2
#Confidence Rating count when the 'stimulus' was S2
nR_S2 <-  cbind(Confidence_Data_S2_R1, Confidence_Data_S2_R2)

## Each vector has length k x 2 where k is the number of ratings available. (i.e. it should be 6[1-3x2])
dim(nR_S1) #6 Columns
dim(nR_S2) #6 Columns

## They should approximately be mirror images in magnitude
nR_S1; nR_S2 #Grouping is approximately mirrored in nature

#Export data matrices
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S1_50.mat", x=nR_S1) #Write Mat file for Stim 1
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S2_50.mat", x=nR_S2) #Write Mat file for Stim 2


#### H-Meta-d' Dependent OO/ON DataPrep ####
#ONLY OLD STIM
#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R1_Old) x Old-Old Recognition
Confidence_Data_S1_R1_OO <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'R') %>%
  filter(OldNew == "OO") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R1_OO[is.na(Confidence_Data_S1_R1_OO)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R1_OO <- Confidence_Data_S1_R1_OO %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S1_R1_OO<-data.frame(Confidence_Data_S1_R1_OO) 
colnames(Confidence_Data_S1_R1_OO) <- c("ID", "3", "2","1")
Confidence_Data_S1_R1_OO <- as.matrix(Confidence_Data_S1_R1_OO)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R1_Old) x Old-New Recognition
Confidence_Data_S1_R1_ON <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'R') %>%
  filter(OldNew == "ON") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R1_ON[is.na(Confidence_Data_S1_R1_ON)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R1_ON <- Confidence_Data_S1_R1_ON %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S1_R1_ON<-data.frame(Confidence_Data_S1_R1_ON) 
colnames(Confidence_Data_S1_R1_ON) <- c("ID", "3", "2","1")
Confidence_Data_S1_R1_ON <- as.matrix(Confidence_Data_S1_R1_ON)[,2:4]


#NEW RESPONSE
#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R2_New) x Old-Old Recognition
Confidence_Data_S1_R2_OO <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'L') %>%
  filter(OldNew == "OO") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R2_OO[is.na(Confidence_Data_S1_R2_OO)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R2_OO <- Confidence_Data_S1_R2_OO %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S1_R2_OO<-data.frame(Confidence_Data_S1_R2_OO) 
colnames(Confidence_Data_S1_R2_OO) <- c("ID", "1", "2","3")
Confidence_Data_S1_R2_OO <- as.matrix(Confidence_Data_S1_R2_OO)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R2_New) x Old-New Recognition
Confidence_Data_S1_R2_ON <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'L') %>%
  filter(OldNew == "ON") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R2_ON[is.na(Confidence_Data_S1_R2_ON)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R2_ON <- Confidence_Data_S1_R2_ON %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S1_R2_ON<-data.frame(Confidence_Data_S1_R2_ON) 
colnames(Confidence_Data_S1_R2_ON) <- c("ID", "1", "2","3")
Confidence_Data_S1_R2_ON <- as.matrix(Confidence_Data_S1_R2_ON)[,2:4]


#NEW STIM
#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R1_Old) x Old-Old Recognition
Confidence_Data_S2_R1_OO <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'R') %>%
  filter(OldNew == "OO") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R1_OO[is.na(Confidence_Data_S2_R1_OO)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R1_OO <- Confidence_Data_S2_R1_OO %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S2_R1_OO<-data.frame(Confidence_Data_S2_R1_OO) 
colnames(Confidence_Data_S2_R1_OO) <- c("ID", "3", "2","1")
Confidence_Data_S2_R1_OO <- as.matrix(Confidence_Data_S2_R1_OO)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R1_Old) x Valance (Neutral)
Confidence_Data_S2_R1_ON <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'R') %>%
  filter(OldNew == "ON") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R1_ON[is.na(Confidence_Data_S2_R1_ON)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R1_ON <- Confidence_Data_S2_R1_ON %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S2_R1_ON<-data.frame(Confidence_Data_S2_R1_ON) 
colnames(Confidence_Data_S2_R1_ON) <- c("ID", "3", "2","1")
Confidence_Data_S2_R1_ON <- as.matrix(Confidence_Data_S2_R1_ON)[,2:4]

#NEW RESPONSE
#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R2_New) x Valence (Negative)
Confidence_Data_S2_R2_OO <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'L') %>%
  filter(OldNew == "OO") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R2_OO[is.na(Confidence_Data_S2_R2_OO)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R2_OO <- Confidence_Data_S2_R2_OO %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S2_R2_OO<-data.frame(Confidence_Data_S2_R2_OO) 
colnames(Confidence_Data_S2_R2_OO) <- c("ID", "1", "2","3")
Confidence_Data_S2_R2_OO <- as.matrix(Confidence_Data_S2_R2_OO)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R2_New) x Valence (Negative)
Confidence_Data_S2_R2_ON <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'L') %>%
  filter(OldNew == "ON") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R2_ON[is.na(Confidence_Data_S2_R2_ON)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R2_ON <- Confidence_Data_S2_R2_ON %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S2_R2_ON<-data.frame(Confidence_Data_S2_R2_ON) 
colnames(Confidence_Data_S2_R2_ON) <- c("ID", "1", "2","3")
Confidence_Data_S2_R2_ON <- as.matrix(Confidence_Data_S2_R2_ON)[,2:4]

#Combine matrices into single matrix representing confidence data x response for stimulus 1
#Confidence Rating count when the 'stimulus' was S1 and Valence was negative
nR_S1_OO <-  cbind(Confidence_Data_S1_R1_OO, Confidence_Data_S1_R2_OO)

#Combine matrices into single matrix representing confidence data x response for stimulus 1
#Confidence Rating count when the 'stimulus' was S1 and Valence was neutral
nR_S1_ON <-  cbind(Confidence_Data_S1_R1_ON, Confidence_Data_S1_R2_ON)

#Combine matrices into single matrix representing confidence data x response for stimulus 2
#Confidence Rating count when the 'stimulus' was S1 and Valence was negative
nR_S2_OO <-  cbind(Confidence_Data_S2_R1_OO, Confidence_Data_S2_R2_OO)

#Combine matrices into single matrix representing confidence data x response for stimulus 2
#Confidence Rating count when the 'stimulus' was S2 and Valence was neutral
nR_S2_ON <-  cbind(Confidence_Data_S2_R1_ON, Confidence_Data_S2_R2_ON)

#Combine matrices into necessary format for H-meta-d'
nR_S1_1 <- cbind(nR_S1_OO, nR_S1_ON)
nR_S2_2<- cbind(nR_S2_OO, nR_S2_ON)

## Each vector has length k x 2 x2 where k is the number of ratings available. (i.e. it should be 12[1-3x2x2])
dim(nR_S1_1) #12 Columns
dim(nR_S2_2) #12 Columns

#Export data matrices
#library(R.matlab)
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S1_50_1ON2.mat", x=nR_S1_1) #Write Mat file for Stim 1
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S2_50_2ON2.mat", x=nR_S2_2) #Write Mat file for Stim 2


#### H-Meta-d' Dependent Valenced DataPrep ####
#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R1_Old) x Negative
Confidence_Data_S1_R1_neg <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'R') %>%
  filter(Valence == "Negative") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R1_neg[is.na(Confidence_Data_S1_R1_neg)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R1_neg <- Confidence_Data_S1_R1_neg %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S1_R1_neg<-data.frame(Confidence_Data_S1_R1_neg) 
colnames(Confidence_Data_S1_R1_neg) <- c("ID", "3", "2","1")
Confidence_Data_S1_R1_neg <- as.matrix(Confidence_Data_S1_R1_neg)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R1_Old) x Neutral
Confidence_Data_S1_R1_neut <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'R') %>%
  filter(Valence == "Neutral") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R1_neut[is.na(Confidence_Data_S1_R1_neut)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R1_neut <- Confidence_Data_S1_R1_neut %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S1_R1_neut<-data.frame(Confidence_Data_S1_R1_neut) 
colnames(Confidence_Data_S1_R1_neut) <- c("ID", "3", "2","1")
Confidence_Data_S1_R1_neut <- as.matrix(Confidence_Data_S1_R1_neut)[,2:4]


#NEW RESPONSE
#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R2_New) x Old-Old Recognition
Confidence_Data_S1_R2_neg <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'L') %>%
  filter(Valence == "Negative") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R2_neg[is.na(Confidence_Data_S1_R2_neg)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R2_neg <- Confidence_Data_S1_R2_neg %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S1_R2_neg<-data.frame(Confidence_Data_S1_R2_neg) 
colnames(Confidence_Data_S1_R2_neg) <- c("ID", "1", "2","3")
Confidence_Data_S1_R2_neg <- as.matrix(Confidence_Data_S1_R2_neg)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S1_Old) x response (R2_New) x Old-New Recognition
Confidence_Data_S1_R2_neut <- Usbl_Rec %>%
  filter(CorResp == 'R' & Binary_Recognition == 'L') %>%
  filter(Valence == "Neutral") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S1_R2_neut[is.na(Confidence_Data_S1_R2_neut)] = 0 #Order still needs to be flipped
Confidence_Data_S1_R2_neut <- Confidence_Data_S1_R2_neut %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S1_R2_neut<-data.frame(Confidence_Data_S1_R2_neut) 
colnames(Confidence_Data_S1_R2_neut) <- c("ID", "1", "2","3")
Confidence_Data_S1_R2_neut <- as.matrix(Confidence_Data_S1_R2_neut)[,2:4]


#NEW STIM
#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R1_Old) x Old-Old Recognition
Confidence_Data_S2_R1_neg <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'R') %>%
  filter(Valence == "Negative") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R1_neg[is.na(Confidence_Data_S2_R1_neg)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R1_neg <- Confidence_Data_S2_R1_neg %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S2_R1_neg<-data.frame(Confidence_Data_S2_R1_neg) 
colnames(Confidence_Data_S2_R1_neg) <- c("ID", "3", "2","1")
Confidence_Data_S2_R1_neg <- as.matrix(Confidence_Data_S2_R1_neg)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R1_Old) x Valance (Neutral)
Confidence_Data_S2_R1_neut <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'R') %>%
  filter(Valence == "Neutral") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R1_neut[is.na(Confidence_Data_S2_R1_neut)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R1_neut <- Confidence_Data_S2_R1_neut %>% select('participant','3','2','1')  #Correct order
Confidence_Data_S2_R1_neut<-data.frame(Confidence_Data_S2_R1_neut) 
colnames(Confidence_Data_S2_R1_neut) <- c("ID", "3", "2","1")
Confidence_Data_S2_R1_neut <- as.matrix(Confidence_Data_S2_R1_neut)[,2:4]

#NEW RESPONSE
#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R2_New) x Valence (Negative)
Confidence_Data_S2_R2_neg <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'L') %>%
  filter(Valence == "Negative") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R2_neg[is.na(Confidence_Data_S2_R2_neg)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R2_neg <- Confidence_Data_S2_R2_neg %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S2_R2_neg<-data.frame(Confidence_Data_S2_R2_neg) 
colnames(Confidence_Data_S2_R2_neg) <- c("ID", "1", "2","3")
Confidence_Data_S2_R2_neg <- as.matrix(Confidence_Data_S2_R2_neg)[,2:4]

#Coerce Data frame to quantify confidence counts x Stimulus type (S2_New) x response (R2_New) x Valence (Negative)
Confidence_Data_S2_R2_neut <- Usbl_Rec %>%
  filter(CorResp == 'L' & Binary_Recognition == 'L') %>%
  filter(Valence == "Neutral") %>%
  group_by(participant, Confidence) %>%
  summarize(count = n()) %>%
  spread("Confidence", "count") #Create counts of success 
Confidence_Data_S2_R2_neut[is.na(Confidence_Data_S2_R2_neut)] = 0 #Order still needs to be flipped
Confidence_Data_S2_R2_neut <- Confidence_Data_S2_R2_neut %>% select('participant','1','2','3')  #Correct order
Confidence_Data_S2_R2_neut<-data.frame(Confidence_Data_S2_R2_neut) 
colnames(Confidence_Data_S2_R2_neut) <- c("ID", "1", "2","3")
Confidence_Data_S2_R2_neut <- as.matrix(Confidence_Data_S2_R2_neut)[,2:4]

#Combine matrices into single matrix representing confidence data x response for stimulus 1
#Confidence Rating count when the 'stimulus' was S1 and Valence was negative
nR_S1_neg <-  cbind(Confidence_Data_S1_R1_neg, Confidence_Data_S1_R2_neg)

#Combine matrices into single matrix representing confidence data x response for stimulus 1
#Confidence Rating count when the 'stimulus' was S1 and Valence was neutral
nR_S1_neut <-  cbind(Confidence_Data_S1_R1_neut, Confidence_Data_S1_R2_neut)

#Combine matrices into single matrix representing confidence data x response for stimulus 2
#Confidence Rating count when the 'stimulus' was S1 and Valence was negative
nR_S2_neg <-  cbind(Confidence_Data_S2_R1_neg, Confidence_Data_S2_R2_neg)

#Combine matrices into single matrix representing confidence data x response for stimulus 2
#Confidence Rating count when the 'stimulus' was S2 and Valence was neutral
nR_S2_neut <-  cbind(Confidence_Data_S2_R1_neut, Confidence_Data_S2_R2_neut)

#Combine matrices into necessary format for H-meta-d'
nR_S1_1 <- cbind(nR_S1_neg, nR_S1_neut)
nR_S2_2<- cbind(nR_S2_neg, nR_S2_neut)

## Each vector has length k x 2 x2 where k is the number of ratings available. (i.e. it should be 12[1-3x2x2])
dim(nR_S1_1) #12 Columns
dim(nR_S2_2) #12 Columns

#Export data matrices
#library(R.matlab)
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S1_50_1.mat", x=nR_S1_1) #Write Mat file for Stim 1
#writeMat(con="/Users/johnnycastillo/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Gaston/nR_S2_50_2.mat", x=nR_S2_2) #Write Mat file for Stim 2

#### SDT x Valence ####
### Negative ###
## d ##
neg_dec_d <- c(-0.46, 0.28, 0.81, 1.55, 0.54, 0.26, 0.17, 0.44, 0.089, -0.018, 0.25, 0.1, 0.1, 0.089, 0.17, 0.97, 0.64, 0.02, -0.3, 0.17, 0.37, 0.63, 0.71, 0.35, 0.13, 0.46, 0.94, 0.32, 0.036, 0.51, -0.25, 0.56, 0.68, -0.34, 0.32, 0.55, 0.21, 0.72, 0.7, 0.28, 0.038, 0.46, 0.58, 0.42, 0.56, 0.24, -0.26, -0.47, 0.63, 0.31)
mean(neg_dec_d); sd(neg_dec_d) #0.33 (0.38) #Not great recall of what the correct options were

## c ##
neg_dec_c <- c(0.015, 0.06, 0.25, -0.21, -0.36, 0.25, 0.12, 0.22, 0.27, -0.11, 0.05, 0.2, 0.12, 0.27, 0.16, 0.16, -0.071, -0.19, 0.0014, 0.13, -0.011, 0.13, -0.14, -0.086, 0.026, -0.08, 0.21, 0.24, 0.19, 0.25, 0.19, 0.017, -0.078, 0.17, 0.042, -0.091, 0.14, 0.021, -0.041, -0.023, 0.22, 0.079, 0.091, 0.064, -0.08, 0.15, 0.069, 0.24, 0.24, 0.16)
mean(neg_dec_c); sd(neg_dec_c) # 0.073 (0.14) #Left option slightly preferred

###. Neutral ###
## d ##
neut_dec_d <- c(0.22, 0.69, 0.46, 1.68, 1.3, 0.67, 0.63, 0.64, -0.35, 0.034, 0.28, -0.28, 0.64, -0.35, 0.32, 0.95, 0.35, 0.71, 0.24, 0.63, 0.99, 0.16, 1.19, -0.077, 0.65, 0.47, 0.98, 0.23, 0.29, 0.64, 0.15, 0.18, 0.84, 0.35, 1.19, 0.46, 0.7, 0.45, 0.69, 1.26, 0.47, 0.98, 0.59, 0.52, 0.74, 0.098, 0.59, -0.03, -0.068, 0.53)
mean(neut_dec_d); sd(neut_dec_d) #0.51 (0.43) #Not great recall of what the correct options were

## c ##
neut_dec_c <- c(-0.05, -0.097, -0.23, -0.008, -0.056, -0.088, -0.38, 0.072, -0.074, 0.044, -0.21, 0.045, -0.061, -0.074, -0.099, -0.056, -0.21, 0.14, -0.087, 0.029, 0.075, -0.045, -0.032, -0.21, -0.15, -0.067, 0.17, 0.12, 0.01, -0.048, -0.13, 0.12, 0.13, 0.2, -0.048, 0.016, 0.16, -0.052, 0.097, 0.026, -0.032, -0.047, 0.025, -0.26, 0.052, -0.14, -0.29, -0.17, -0.034, 0.048)
mean(neut_dec_c); sd(neut_dec_c) # -0.039 (0.13) #Right option slightly preferred

#Thoughts: Negative is more sensitive 


#### SDT x Familiarity ####
### Old-Old ###
## d ##
OO_dec_d <- c(0.62, 0.56, 1.01, 1.9, 1.21, 0.32, 0.89, 0.91, -0.049, 0.061, 0.24, 0.077, 0.69, -0.049, 0.57, 1.26, 0.62, 0.62, 0.18, 0.58, 0.5, 0.56, 1.42, -0.0075, 0.49, 0.19, 1.13, 0.45, 0.26, 0.95, 0.12, 0.44, 1.02, 0.26, 1.38, 0.82, 0.79, 0.69, 0.7, 1.26, 0.18, 1.16, 0.99, 0.43, 0.95, -0.068, 0.5, -0.056, 0.08, 0.79)
mean(OO_dec_d); sd(OO_dec_d) #0.61 (0.45) #Ok recall of what the correct options were

## c ##
OO_dec_c <- c(-0.024, -0.057, 0.13, -0.065, -0.32, 0.13, -0.16, 0.18, 0.31, 0.0007, -0.15, 0.16, 0.0087, 0.31, 0.14, 0.17, -0.024, -0.024, 0.0022, 0.18, 0.10, 0.071, -0.29, -0.15, -0.089, 0.0637, 0.23, 0.2, 0.22, 0.14, 0.032, 0.13, 0.049, 0.16, -0.13, 0.077, 0.24, 0.0087, 0.14, 0.17, -0.12, -0.09, 0.22, -0.021, -0.056, -0.12, 0.038, 0.12, 0.31, 0.24)
mean(OO_dec_c); sd(OO_dec_c) # 0.056 (0.15) #Left option slightly preferred

###. Old-New ###
## d ##
ON_dec_d <- c(0.061, 0.49, 0.32, 1.33, 0.62, 0.75, -0.02, 0.25, -0.13, -0.058, 0.37, -0.25, 0.12, -0.13, 0.0045, 0.75, 0.38, 0.058, -0.18, 0.31, 0.88, 0.25, 0.56, 0.29, 0.36, 0.76, 0.81, 0.13, 0.12, 0.33, -0.12, 0.25, 0.43, -0.16, 0.27, 0.19, 0.18, 0.49, 0.69, 0.37, 0.46, 0.37, 0.24, 0.58, 0.37, 0.43, -0.017, -0.3, 0.49, 0.12)
mean(ON_dec_d); sd(ON_dec_d) #0.3 (0.33) #Not great recall of what the correct options were

## c ##
ON_dec_c <- c(-0.0007, -0.025, -0.13, -0.13, -0.15, -0.023, -0.14, 0.1, -0.094, -0.061, -0.027, 0.096, 0.029, -0.094, -0.092, -0.066, -0.25, -0.061, -0.12, -0.058, -0.09, 0.034, 0.057, -0.11, -0.08, -0.22, 0.15, 0.16, -0.032, 0.044, -0.029, 0.034, 0.0053, 0.2, 0.02, -0.13, 0.059, -0.025, -0.074, -0.22, 0.26, 0.089, -0.09, -0.2, -0.089, 0.12, -0.34, -0.12, -0.089, -0.032)
mean(ON_dec_c); sd(ON_dec_c) # -0.041 (0.12) #Right option slightly preferred

#Thoughts: OO is more sensitive


#### Precheck Goals ####
## Overall ##
# (DONE) Calculate percentage of excluded participants: 66%
# (DONE) Calulate percentage of time "Old" stim chosen for (OLD-NEW) trials (Measure of Mnemonic bias)
Recognition_Data %>% filter(OldNew == "ON") %>% mutate(OldChsn = Old == Binary_Recognition) %>% group_by(participant, OldChsn) %>% summarize(count = n()) %>% spread(OldChsn, count) %>% mutate(total = `FALSE` +  `TRUE`) %>% mutate(bias = (`TRUE`/total)*100)  %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)) %>% summarise(avg = mean(bias))
Recognition_Data %>% filter(OldNew == "ON") %>% mutate(OldChsn = Old == Binary_Recognition) %>% group_by(participant, OldChsn) %>% summarize(count = n()) %>% spread(OldChsn, count) %>% mutate(total = `FALSE` +  `TRUE`) %>% mutate(bias = (`TRUE`/total)*100)  %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)) %>% ungroup() %>% summarise(avg = mean(bias), stdev = sd(bias))
#Participants are more likely to choose preiously seen stimuli

# (DONE) Calculate value based choice accuracy of selections (Measure of choice rationality)
(value_based_acc <- Recognition_Data %>% group_by(participant, Chc_Accuracy) %>% summarize(count = n()) %>% spread(Chc_Accuracy, count) %>% mutate(total = `0` +  `1`) %>% mutate(acc = (`1`/total)*100) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)))
(value_based_acc <- Recognition_Data %>% group_by(participant, Chc_Accuracy) %>% summarize(count = n()) %>% spread(Chc_Accuracy, count) %>% mutate(total = `0` +  `1`) %>% mutate(acc = (`1`/total)*100) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta))) %>% ungroup() %>% summarise(avg = mean(acc), stdev = sd(acc))
value_based_acc %>% ggplot(aes(x= participant, y = acc)) + geom_bar(stat = "identity")
# Accuracy 58.8%
# Note: The accuracy of value based choices in Ostrovesky, Gluth (2018) was ~ 50%


# (DONE) Calculate Hit-FA (crude measure of discrimination sensitivity)
(Z_ans <- Recognition_SDT %>% group_by(participant, ItemRespType) %>% summarize(count = n()) %>% spread(ItemRespType, count) %>% select(!'<NA>') %>% mutate(Z = Hit-FA) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)))
#Most positive (more Hits than FA) // 1 =, 2 (-)

# (DONE) Calculate recognition trial value difference between left & right choice option

## Valenced (Negative/Neutral) ##
# (DONE) Calulate percentage of time "Old" stim chosen for (OLD-NEW) trials x Valence (Measure of Mnemonic bias)
Recognition_Data %>% filter(OldNew == "ON") %>% mutate(OldChsn = Old == Binary_Recognition) %>% group_by(participant, Valence, OldChsn) %>% summarize(count = n()) %>% spread(OldChsn, count) %>% mutate(total = `FALSE` +  `TRUE`) %>% mutate(bias = (`TRUE`/total)*100)  %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)) %>% summarise(avg = mean(bias))
Recognition_Data %>% filter(OldNew == "ON") %>% mutate(OldChsn = Old == Binary_Recognition) %>% group_by(participant, Valence, OldChsn) %>% summarize(count = n()) %>% spread(OldChsn, count) %>% mutate(total = `FALSE` +  `TRUE`) %>% mutate(bias = (`TRUE`/total)*100)  %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)) %>% ungroup() %>% group_by(Valence) %>% summarise(avg = mean(bias), stdev = sd(bias))
#Most were more likely to pick old trials when they were neutral

# Calculate value based choice accuracy of selections x Valence (Measure of choice rationality)
(value_based_acc_val <- Recognition_Data %>% group_by(participant, Valence, Chc_Accuracy) %>% summarize(count = n()) %>% spread(Chc_Accuracy, count) %>% mutate(total = `0` +  `1`) %>% mutate(acc = (`1`/total)*100) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)))
(value_based_acc_val <- Recognition_Data %>% group_by(participant, Valence, Chc_Accuracy) %>% summarize(count = n()) %>% spread(Chc_Accuracy, count) %>% mutate(total = `0` +  `1`) %>% mutate(acc = (`1`/total)*100) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta))) %>% ungroup() %>% group_by(Valence) %>% summarise(avg = mean(acc), stdev = sd(acc))
value_based_acc_val %>% ggplot(aes(x= participant, y = acc)) + geom_bar(stat = "identity") + facet_grid(.~Valence)
#Most had higher accuracy for neutral valence trials

# Calculate Hit-FA x Valence (crude measure of discrimination sensitivity)
(Z_ans_val <- Recognition_SDT %>% left_join(Recognition_Data) %>% group_by(participant, ItemRespType, Valence) %>% summarize(count = n()) %>% spread(ItemRespType, count) %>% select(!'<NA>') %>% mutate(Z = Hit-FA) %>% filter(!participant %in% c(Poor_enc, Incomplete_Conf, poor_d, neg_meta)))
#Most negative Hit-FA occured in Neutral


#### Prepare dataframes for export ####
usbl_ppt <- unique(Usbl_Rec$participant)

# Overall Performance
#Ovrl_df <- as.data.frame(cbind(usbl_ppt, dec_d, dec_c))
#colnames(Ovrl_df) <- c("ID", "d", "c")
#Ovrl_df$ID <- as.factor(Ovrl_df$ID)
#Ovrl_df$d <- as.numeric(Ovrl_df$d)
#Ovrl_df$c <- as.numeric(Ovrl_df$c)

# Valenced Performance
#Negative
Neg_df <- as.data.frame(cbind(usbl_ppt, neg_dec_d, neg_dec_c, "Negative"))
colnames(Neg_df) <- c("ID", "d", "c", "Valence")
Neg_df$ID <- as.factor(Neg_df$ID)
Neg_df$d <- as.numeric(Neg_df$d)
Neg_df$c <- as.numeric(Neg_df$c)
Neg_df$Valence <- as.factor(Neg_df$Valence)
#Neutral
Neut_df <- as.data.frame(cbind(usbl_ppt, neut_dec_d, neut_dec_c, "Neutral"))
colnames(Neut_df) <- c("ID", "d", "c", "Valence")
Neut_df$ID <- as.factor(Neut_df$ID)
Neut_df$d <- as.numeric(Neut_df$d)
Neut_df$c <- as.numeric(Neut_df$c)
Neut_df$Valence <- as.factor(Neut_df$Valence)
#Combine
Valenced_df <- Neg_df %>% full_join(Neut_df)

# Memory Performance
#Old-New (ON)
ON_df <- as.data.frame(cbind(usbl_ppt, ON_dec_d, ON_dec_c, "ON"))
colnames(ON_df) <- c("ID", "d", "c", "MemoryType")
ON_df$ID <- as.factor(ON_df$ID)
ON_df$d <- as.numeric(ON_df$d)
ON_df$c <- as.numeric(ON_df$c)
ON_df$MemoryType <- as.factor(ON_df$MemoryType)
#Old-Old (OO)
OO_df <- as.data.frame(cbind(usbl_ppt, OO_dec_d, OO_dec_c, "OO"))
colnames(OO_df) <- c("ID", "d", "c", "MemoryType")
OO_df$ID <- as.factor(OO_df$ID)
OO_df$d <- as.numeric(OO_df$d)
OO_df$c <- as.numeric(OO_df$c)
OO_df$MemoryType <- as.factor(OO_df$MemoryType)
#Combine
Memory_df <- ON_df %>% full_join(OO_df)

# Confidence ratings
Confidence_df <- Recognition_Data %>% select(participant, OldNew, Valence, Confidence) %>% group_by(participant, OldNew, Valence) %>% summarise(Avg_conf = mean(Confidence)) %>% filter(!participant %in% Incomplete_Conf)
colnames(Confidence_df) <- c("ID", "MemoryType", "Valence", "Avg_Confidence")


# Value dataframe
Value_df <- Recognition_Data %>% select(participant, OldNew, Valence, Diff, Chc_Accuracy) %>% mutate(abs_diff = abs(as.numeric(Diff))) %>% select(participant, OldNew, Valence, abs_diff, Chc_Accuracy) %>% filter(!participant %in% Incomplete_Conf)
colnames(Value_df) <- c("ID", "MemoryType", "Valence", "AbsDiff", "Accuracy")

## Export dataframes for Brecon
#write_csv(Ovrl_df, "Overall_perf_17.csv")
write_csv(Valenced_df, "Valenced_perf_50.csv")
write_csv(Memory_df, "Memory_perf_50.csv")
write_csv(Confidence_df, "Confidence_df_50edit.csv")
write_csv(Value_df, "Value_acc_50edit.csv")

#### Brecon Salvagable ####

# Load in saved data
Confidence_df <- read_csv("Confidence_df_50.csv")
Value_acc <- read_csv("Value_acc_50.csv")
Memory_perf <- read_csv("Memory_perf_50.csv")
#Overall_perf <- read_csv("Overall_perf_17.csv")
Valenced_perf <- read_csv("Valenced_perf_50.csv")

#### Confidence ~ emotional valence ####
# Exploratory Data Analysis (EDA)
Confidence_df_edit %>% ggplot(aes(x = Valence, y = Avg_Confidence, fill = Valence)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_boot", width = 0.2) +
  scale_fill_manual(values = c("lightcoral", "grey")) +
  theme_minimal() +
  labs(x = "Valence", y = "Average Confidence") +
  geom_signif(comparisons = list(c("Negative", "Neutral")), 
              map_signif_level = TRUE)

# Statistical Modeling
Confidence_df_edit$Valence <- relevel(factor(Confidence_df$Valence), ref = "Negative")
conf_1 <- lmer (Avg_Confidence ~ Valence + (1|ID), data = Confidence_df_edit)
summary(conf_1)
valence_summary <- Confidence_df_edit %>%
  group_by(Valence) %>%
  summarize(mean_confidence = mean(Avg_Confidence, na.rm = TRUE),
            sd_confidence = sd(Avg_Confidence, na.rm = TRUE))

#### Confidence ~ Familiarity ####
# Exploratory Data Analysis
Confidence_df_edit %>%
  group_by(MemoryType) %>%
  summarize(mean_confidence = mean(Avg_Confidence, na.rm = TRUE),
            sd_confidence = sd(Avg_Confidence, na.rm = TRUE))

#novelty bar plot
Confidence_df_edit %>%
  filter(MemoryType != "NN")
Confidence_df_edit$MemoryType <- factor(Confidence_df_edit$MemoryType)
Confidence_df_edit$MemoryType <- relevel(Confidence_df_edit$MemoryType, ref = "OO")
conf_mem <- ggplot(Confidence_df_edit, aes(x = MemoryType, y = Avg_Confidence, fill = MemoryType)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_boot", width = 0.2) +
  scale_fill_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal() +
  labs(x = "Novelty", y = "Average Confidence") +
  geom_signif(comparisons = list(c("OO", "ON")), 
              map_signif_level = TRUE)

# Statistical Modeling
Confidence_df_edit <- Confidence_df_edit %>%
  filter(MemoryType != "NN")
Confidence_df_edit$MemoryType <- factor(Confidence_df_edit$MemoryType)
Confidence_df_edit$MemoryType <- relevel(Confidence_df_edit$MemoryType, ref = "OO")
conf_2 <- lmer (Avg_Confidence ~ MemoryType + (1|ID), data = Confidence_df_edit)
summary(conf_2)

#### d' ~ Valence ####
Valenced_perf$Valence <- relevel(factor(Valenced_perf$Valence), ref = "Neutral")

# Summary statistics
d_Val_ss <- Valenced_perf %>%
  group_by(Valence) %>%
  summarize(mean_d = mean(d, na.rm = TRUE),
            sd_d = sd(d, na.rm = TRUE))

# Bayesian Linear Modeling
Val_d_prior <-
  prior(normal(0, .5), class = "b", coef = "") +
  prior(normal(0, .5), class = "b", coef = "ValenceNegative")
Valenced_perf %>% ggplot(aes(x=Valence, y= d)) + xlab("Valence") + ylab("d'") + geom_point() + geom_line(aes(group=ID)) + theme_classic() + geom_smooth(method = "lm", color="red", linetype = "dashed", se = TRUE)
if (!file.exists("SciRep50_Val_d.rda")) {
  d_Val <- brm(d ~ Valence + (1|ID), data = Valenced_perf, family = gaussian(), iter = 10000, prior= Val_d_prior, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
  save(d_Val, file = "./SciRep50_Val_d.rda") 
} else {
  load("SciRep50_Val_d.rda")  
}
pp_check(d_Val, ndraws = 40) #Great fit
print(summary(d_Val),digits=4)
mcse(d_Val)
hdi(d_Val, ci = 0.89)
pd(d_Val)
plot_model(d_Val, type="pred", colors = "bw")
#Negative valence is associated with a meaningful decrease in discrimination sensitivity
# -0.19 [-0.28, -0.09]  
# 99.93% pd

#### d' ~ Memory ####
Memory_perf$MemoryType <- factor(Memory_perf$MemoryType)
Memory_perf$MemoryType <- relevel(Memory_perf$MemoryType, ref = "OO")

# Exploratory Data Analysis 
d_Mem_ss <- Memory_perf %>%
  group_by(MemoryType) %>%
  summarize(mean_d = mean(d, na.rm = TRUE),
            sd_d = sd(d, na.rm = TRUE))

# Bayesian Linear Modeling
Mem_d_prior <-
  prior(normal(0, .5), class = "b", coef = "") +
  prior(normal(0, .5), class = "b", coef = "MemoryTypeOO")
Memory_perf %>% ggplot(aes(x=MemoryType, y= d)) + xlab("Trial Type") + ylab("d'") + geom_point() + geom_line(aes(group=ID)) + theme_classic() + geom_smooth(method = "lm", color="red", linetype = "dashed", se = TRUE)
if (!file.exists("SciRep50_Mem_d.rda")) {
  d_Mem <- brm(d ~ MemoryType + (1|ID), data = Memory_perf, family = gaussian(), iter = 10000, prior= Mem_d_prior, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
  save(d_Val, file = "./SciRep50_Mem_d.rda") 
} else {
  load("SciRep50_Mem_d.rda")  
}
pp_check(d_Mem, ndraws = 40) #Great fit
print(summary(d_Mem),digits=4)
mcse(d_Mem)
hdi(d_Mem, ci = 0.89)
pd(d_Mem)
plot_model(d_Mem, type="pred", colors = "bw")
#Familiarity is associated with a meaningful increase in discrimination sensitivity
# 0.31 [0.22, 0.4]  
# 100% pd

#### c ~ Valence ####
# Summary statistics
c_Val_ss <- Valenced_perf %>%
  group_by(Valence) %>%
  summarize(mean_c = mean(c, na.rm = TRUE),
            sd_c = sd(c, na.rm = TRUE))

# Bayesian Linear Modeling
Val_c_prior <-
  prior(normal(0, .5), class = "b", coef = "") +
  prior(normal(0, .5), class = "b", coef = "ValenceNegative")
Valenced_perf %>% ggplot(aes(x=Valence, y= c)) + xlab("Valence") + ylab("d'") + geom_point() + geom_line(aes(group=ID)) + theme_classic() + geom_smooth(method = "lm", color="red", linetype = "dashed", se = TRUE)
if (!file.exists("SciRep50_Val_c.rda")) {
  c_Val <- brm(c ~ Valence + (1|ID), data = Valenced_perf, family = gaussian(), iter = 10000, prior= Val_c_prior, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
  save(c_Val, file = "./SciRep50_Val_c.rda") 
} else {
  load("SciRep50_Val_c.rda")  
}
pp_check(c_Val, ndraws = 40) #Great fit
print(summary(c_Val),digits=4)
mcse(c_Val)
hdi(c_Val, ci = 0.89)
pd(c_Val)
plot_model(c_Val, type="pred", colors = "bw")
#Negative valence is associated with a meaningful increase in conservative response biases (left choice)
# 0.11 [0.07, 0.15]  
# 100% pd

#### c ~ Memory ####
# Exploratory Data Analysis 
c_Mem_ss <- Memory_perf %>%
  group_by(MemoryType) %>%
  summarize(mean_c = mean(c, na.rm = TRUE),
            sd_c = sd(c, na.rm = TRUE))

# Bayesian Linear Modeling
Mem_c_prior <-
  prior(normal(0, .5), class = "b", coef = "") +
  prior(normal(0, .5), class = "b", coef = "MemoryTypeOO")
Memory_perf %>% ggplot(aes(x=MemoryType, y= c)) + xlab("Trial Type") + ylab("c") + geom_point() + geom_line(aes(group=ID)) + theme_classic() + geom_smooth(method = "lm", color="red", linetype = "dashed", se = TRUE)
if (!file.exists("SciRep50_Mem_c.rda")) {
  c_Mem <- brm(c ~ MemoryType + (1|ID), data = Memory_perf, family = gaussian(), iter = 10000, prior= Mem_c_prior, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.99))
  save(c_Val, file = "./SciRep50_Mem_c.rda") 
} else {
  load("SciRep50_Mem_c.rda")  
}
pp_check(c_Mem, ndraws = 40) #Great fit
print(summary(c_Mem),digits=4)
mcse(c_Mem)
hdi(c_Mem, ci = 0.89)
pd(c_Mem)
plot_model(c_Mem, type="pred", colors = "bw")
#Familiarity is associated with a meaningful increase in conservative response bias (left)
# 0.0.096 [0.05, 0.14]  
# 99.98% pd

#### Logistic regression - P(Correct)####
Value_df$Accuracy <- as.factor(Value_df$Accuracy)
Value_df$ID <- as.factor(Value_df$ID)
Value_df$MemoryType <- as.factor(Value_df$MemoryType)
Value_df$Valence <- as.factor(Value_df$Valence)
logistic_acc <- glmer(Accuracy ~ MemoryType + Valence + AbsDiff + (1|ID),family = binomial(), data=Value_df)
Value_df$MemoryType[Value_df$MemoryType == "NN"] <- "OO"
summary(logistic_acc)
effects(logistic_acc)
plot_model(logistic_acc, type="pred")

exp(coef(logistic_acc)[1]) / (1 + exp(coef(logistic_acc)[1]))
#Intercept (Baseline): 0.50 ~ Perfectly even chance of selecting correct answer
#MemoryTypeOO: 0.28929 ~ 34% increase in likelihood of correctness (p = 9.58e-10)
#ValenceNeutral: 0.12249 ~ 13% increase in likelihood of correctness (p = 0.00838)
#AbsDiff: 0.05356 ~ 5.5% increase in likelihood of correctness (p = 0.00091)
