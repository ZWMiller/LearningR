# clean data
rm(list=ls())

# web url
  site <- "http://www.metoffice.gov.uk/climate/uk/stationdata/armaghdata.txt"

# call in data with try command in while loop
  i <- 1
  while (i < 2){
    aa <- try(read.table(site,sep="\t"))
      if (class(aa) == "try-error") {
        next
      } else {
        i <- i + 1
      }
  }

# grand! now inspect and trim off crap
aa <- aa[6:dim(aa)[1],]

# data is melted together so some tidying required
bb <- cc <- dd <- c()
  for (i in (1:length(aa))){
    bb <- unlist(strsplit(as.character(aa[i]), " "))
      cc <- bb[nchar(bb)>0] ; cc <- cc[1:7]
      dd <- rbind(dd,cc)
  }

row.names(dd) <- dd[,1]

colnm <- c(dd[1,1],dd[1,2],paste(dd[1,3],dd[2,1],sep=" "), paste(dd[1,4],
      dd[2,2],sep=" "), paste(dd[1,5],dd[2,3],sep=" "), 
    paste(dd[1,6],dd[2,4],sep=" "), paste(dd[1,7],dd[2,5],sep=" "))

colnames(dd) <- colnm

armagh <- data.frame(dd[-c(1,2),])

  for (i in (1:dim(armagh)[2])){
    armagh[,i] <- as.numeric(as.character(armagh[,i]))
  }

decmin <- armagh[armagh[,2]==12,4]
  year <- armagh[armagh[,2]==12,1]
  wh1 <- data.frame(cbind(armagh$yyyy[armagh$mm==12],armagh$tmin.degC[armagh$mm==12]))
wh1 <- na.omit(wh1)
plot(wh1, type="o")

# nice plot
library(ggplot2)
  ggplot(wh1, aes(X2,X1)) + 
  geom_line(colour="red") + 
  theme_bw() +
  scale_x_continuous('Year') + 
  scale_y_continuous('Minimum Temperature - Degree Celsius') #+
  #opts(title = expression("December Average Daily Minimum Temperature - Armagh 1865-2011"))
