#Survey dataframe 
#select catch variables for analysis
survey_vars<- c("participant", "textbox.text", "textbox_2.text", "textbox_3.text", "textbox_4.text", "textbox_5.text", "textbox_6.text")
#, "textbox_7.text")

#make catch data frame 
surveydata<-data[, survey_vars]
#surveydata<-data[data$participant == "61652222bfd9351c91a6585a", survey_vars]

#rename answers to make some sense
surveydata <- surveydata %>% rename(location.diffi=textbox.text,
                                    colour.diffi= textbox_2.text,
                                    normal.acuity= textbox_3.text,
                                    colour.vision= textbox_4.text,
                                    answer.strategy = textbox_5.text,
                                    serious.check = textbox_6.text)
#remove empty cells
surveydata <-surveydata[!apply(surveydata=="",1,any),]

#could try to do a word cloud or something but would need to reformat data so that y/Y/Yes/yes
