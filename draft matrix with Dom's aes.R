viz.comp <- colourpairs

#APPLY COLOUR FACTORS TO COLOURPAIRS DATAFRAME
viz.comp$fhex1 <- with(viz.comp, factor(hex1, levels = row.facs))
viz.comp$fhex2 <- with(viz.comp, factor(hex2, levels = row.facs))

#creating one object with setting to ensure they all have the same, also only one thing to update
matrix.plot.settings <- list(geom_raster(),
                             theme_pubr(25),
                             theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs)),
                             theme(axis.text.y = element_text(size= 9, colour=row.facs)),
                             theme(legend.position = "bottom", legend.key.size = unit(1.5,'cm')),
                             scale_x_discrete(labels=block_rep), scale_y_discrete(labels=block_rep),
                             labs(x= "Colour presented first", y= "Colour presented second"))

dom <- c("#F8961E","#7E8F50","#216D85","#4C5299","#9A37AD","#A837CE","#CA33FB")
#F8B21C,#7E8F50, #216D85, #4C5299, #9A37AD, #A837CE, #CA33FB
#F8AB07,#748441, #1C607A, #3C4390, #9124A6, #A024CA, #B026EE

mydata.wdom <- ggplot(viz.comp) +
  aes(x = fhex1, y = fhex2, fill = mean.similarity) +
  #scale_fill_fermenter(name= "Similarity",palette = c("#F8B21C","#7E8F50","#216D85","#4C5299","#9A37AD","#A837CE","#CA33FB"), direction = 1,breaks= c(0,1,2,3,4,5,6,7))+
  scale_fill_stepsn(colors= dom,n.breaks= 6, name= "Dissimilarity")+
  matrix.plot.settings 

ggsave("mydata.wdom.png", mydata.wdom, dpi=800, height=15,width=13)

# 1         2       3         4       5       6         7
