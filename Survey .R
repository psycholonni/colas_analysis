#Survey dataframe 
#select catch variables for analysis
survey_vars<- c("participant", "textbox.text", "textbox_2.text", "textbox_3.text", "textbox_4.text", "textbox_5.text", "textbox_6.text")

, "textbox_7.text"
#make catch data frame 
surveydata<-data[data$participant == "61652222bfd9351c91a6585a", survey_vars]
surveydata<-data[, survey_vars]
#remove empty cells
surveydata <- surveydata %>% drop_na()
