############################################################################################################################
# Please read the readme file from github before running this script to understand the scope of this program and its limitations.
#
# In this code, we scrape shot chart data from ESPN for every shot by every player in every game for a specified 
# team for an entire season. Before running this program, be sure that the following packages in thise saction of code are 
# installed. Additionally, uncomment the first line of code if this is your first time using RSelenium to download the binary 
# needed to run the package. Finally, be sure to download the Mozilla Firefox web browser before runnning this program.
#
#############################################################################################################################
#RSelenium::checkForServer()
library(RSelenium)
library(stringr)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)
library(hexbin)

###################################################################################################################
# First, in this section, we load the appropriate packages and set the initial parameters needed to run the script. 
# Be sure to set your working directory (where you want the output data set to be written), the site that corresponds 
# to your team's ESPN homepage, and your team, as specified on ESPN. This may be the long or abbreviated version. 
###################################################################################################################
setwd("C:/Users/Andres/Documents/LSU Basketball/shotchart/shotchartresults")
site <- "http://espn.go.com/core/mens-college-basketball/team/_/id/99"
team <- "LSU"

###################################################################################################################
# Now that all of the parameters have been specified, we are ready to begin processing. In this section, we declare
# the data frame "shotchart" that will hold all of the shot data. Then, we use RSelenium to open a Firefox web 
# browser and navigate to the specified ESPN team page. From there, R reads the number of games that are on the 
# schedule and stores that value.
###################################################################################################################
shotchart <- data.frame(shot=as.vector(0), class=as.vector(0), data_homeaway=as.vector(0), 
                        data_period=as.vector(0), player_id=as.vector(0), data_text=as.vector(0),
                        location=as.vector(0), gamenumber= as.vector(0))
RSelenium::startServer()
remDr <- remoteDriver()
remDr$open()
remDr$navigate(site)
Sys.sleep(4)
schedule <- remDr$findElements(using = 'css selector', '.game-info')

###################################################################################################################
# Now we are ready to begin the main processing component. This loop processes through every game in the schedule, 
# navigating to the ESPN page fot the game and moving to the play by play page. Then the program looks for a shot 
# chart. If there is no shot chart available, the program moves back to the home page and looks at the next game. 
# But, if there are shot chart data, the program reads and records information on the type of shot, location, half, 
# player, and whether or not it was assisted.
###################################################################################################################
for (k in (1:length(schedule)+1)) {
#this loop makes the browser scroll down so that it can click on the appropriate game button  
    for (j in 2:k-2) {
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "down_arrow"))
  }
  WebElem <- remDr$findElement(using = "css selector", paste('li:nth-child(',k,') .game-info', sep=""))
  remDr$mouseMoveToLocation(webElement = WebElem)
  remDr$click(1)
  Sys.sleep(3)
  
  if (length(remDr$findElements(using = "css selector", '.pbp .link-text'))>0){
    WebElem <- remDr$findElement(using = "css selector", '.pbp .link-text')
    remDr$mouseMoveToLocation(webElement = WebElem)
    Sys.sleep(3)
    remDr$click(1)
    Sys.sleep(3)
#these done objects are flags to indicate if all of the shots have been processed. They are set to T after the last shot is recorded.
    done1 <- F
    done2 <- F
    
    webElems <- remDr$findElements(using = 'css selector', ".home .long-name")
    homeoraway <- unlist(lapply(webElems, function(x){x$getElementText()}))  
    
    if (homeoraway == team){
      
      for (i in 1:100){
        if (done1==TRUE) {break}
        text <-paste0("//*[(@class='shots home-team')]//*[(@id)][",i,"]")
        tryCatch(
          {
            shotchart[nrow(shotchart)+1,1]<- unlist(remDr$findElement(using='xpath', text)$getElementAttribute('id'))
            shotchart[nrow(shotchart),2]<- unlist(remDr$findElement(using='xpath', text)$getElementAttribute('class'))
            shotchart[nrow(shotchart),3]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-homeaway'))
            shotchart[nrow(shotchart),4]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-period'))
            shotchart[nrow(shotchart),5]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-shooter'))
            shotchart[nrow(shotchart),6]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-text'))
            shotchart[nrow(shotchart),7]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('style'))
            shotchart[nrow(shotchart),8]<-k
          },
          error=function(cond) {
            message(paste(i))
            done1 <<- TRUE
            shotchart <<- shotchart 
          }
        )
      }
    }else {
      for (i in 1:100){
        if (done2==TRUE) {break}
        text <-paste0("//*[(@class='shots away-team')]//*[(@id)][",i,"]")
        tryCatch(
          {
            shotchart[nrow(shotchart)+1,1]<- unlist(remDr$findElement(using='xpath', text)$getElementAttribute('id'))
            shotchart[nrow(shotchart),2]<- unlist(remDr$findElement(using='xpath', text)$getElementAttribute('class'))
            shotchart[nrow(shotchart),3]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-homeaway'))
            shotchart[nrow(shotchart),4]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-period'))
            shotchart[nrow(shotchart),5]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-shooter'))
            shotchart[nrow(shotchart),6]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('data-text'))
            shotchart[nrow(shotchart),7]<-unlist(remDr$findElement(using='xpath', text)$getElementAttribute('style'))
            shotchart[nrow(shotchart),8]<-k
          },
          error=function(cond) {
            message(paste(i-1))
            done2 <<- TRUE
          }
        )
      }
    }
  }
  remDr$navigate(site)
  Sys.sleep(3)
}
remDr$closeWindow()

###################################################################################################################
# Here we do processing to clean the data frame and get it ready to plot the chart data. We break character columns
# into more descriptive variables and ensure that the placement variables are numeric.
###################################################################################################################
shotchart <- shotchart[-1,]
coord <- str_extract_all(shotchart$location, "\\d+(\\.\\d+){0,1}%")
coord <- as.data.frame(do.call(rbind, coord), stringsAsFactors=FALSE)
shotchart <- cbind(shotchart, coord)
shotchart$V1 <- gsub("%", "",shotchart$V1)
shotchart$V2 <- gsub("%", "",shotchart$V2)
shotchart$V1 <- as.numeric(shotchart$V1)
shotchart$V2 <- as.numeric(shotchart$V2)

shotchart$V2 <- ifelse(shotchart$V1 < 50, 100-shotchart$V2,shotchart$V2)
shotchart$V1 <- ifelse(shotchart$V1 < 50, 100-shotchart$V1,shotchart$V1)
shotchart$V3 <- ifelse(shotchart$V2, 100 - shotchart$V2, shotchart$V2)
shotchart$V4 <- ifelse(shotchart$V1, 100 - shotchart$V1, shotchart$V1)

shotchart$shot_type <- ifelse(grepl('d Jumper', shotchart$data_text), 'Jumper',
                              ifelse(grepl('t Jumper', shotchart$data_text), 'Three',
                                     ifelse(grepl('Layup', shotchart$data_text), 'Layup',
                                            ifelse(grepl('Dunk', shotchart$data_text), 'Dunk',
                                                   ifelse(grepl('e Jumper', shotchart$data_text), 'Jumper',
                                                          ifelse (grepl('Tip Shot', shotchart$data_text), 'TipShot', 1))))))

shotchart$assist <- ifelse(grepl('Assisted', shotchart$data_text), 'yes','no')

###################################################################################################################
# Finally, we output the resulting data frame and provide a basic visualization. The user should specify background
# images, charts, and data to suit her needs. For example, it may only be useful to include data for one player for 
# 5 games, in which case the following code would need to be amended. 
###################################################################################################################
write.csv(shotchart, "shot_data_to_plot1.csv")

courtImg.URL <- "http://www.pyware.com/images/newproducts/NBABBALL1.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

ggplot(shotchart[shotchart$player_id==3907386,], aes(as.numeric(V1), as.numeric(V2))) + 
  annotation_custom(court, -3, 100, -102, 3) +
  geom_point(aes(colour = class, size = assist, shape = shot_type, alpha = 0.8)) +
  scale_color_manual(values = c("#008000", "#FF6347")) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(-1, 102) +
  ylim(102, -3) +
  coord_fixed()
