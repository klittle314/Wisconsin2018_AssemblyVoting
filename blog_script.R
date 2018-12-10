##################################
# Kevin Little, Ph.D.  Informing Ecological Design, LLC
# script used to create graph posted in blog Plotting Wisconsin's Gerrymander December 2018
##################################
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#sheet 2 has different number of rows than all the other sheets, read separately
df1 <- read_excel("Ward by Ward Report-Gen Election-Assembly.xlsx", 
                                          sheet = "Sheet2", skip = 7)
#get string of Office Total
OfficeTotal <- as.character(df1[49,1])



#build function that will create dataframe with numbers of votes from the Excel file
df_data_maker <- function(i,OfficeTotal1=OfficeTotal){
  df1 <- read_excel("Ward by Ward Report-Gen Election-Assembly.xlsx", 
                    sheet = paste0("Sheet",as.character(i+1)), range = "A7:J120")
  ifelse(i==1,df_data<- df1[3,],df_data <- df1[2,])
  names(df_data)[1] <- "district"
  df_data$district <- i
  df_OfficeTotal <- df1[df1[,1]==OfficeTotal1,]
  votes <- df_OfficeTotal[!(rowSums(is.na(df_OfficeTotal))==NCOL(df_OfficeTotal)),] 
  names(votes)[1] <- "district"
  votes[1,1] <- i
  #transpose the df, remove the first two rows and insert the district number
  df_data <- as.data.frame(t(rbind.data.frame(df_data,votes)),row.names=FALSE)[-c(1:2),]
  df_data$District <- i
  names(df_data)[1] <- "Group"
  names(df_data)[2] <- "Votes"
  return(df_data)
}

#Now make a long data frame from all the districts

df_all <- do.call(rbind.data.frame,lapply(1:99,df_data_maker))
df_all$Group <- as.character(df_all$Group)
df_all$Votes <- as.numeric(as.character(df_all$Votes))

df_all_NArem <- filter(df_all, !(is.na(Group)) & !(is.na(Votes)))

#rename Republicans who have two candidates in three Assembly races
index <- c(63,95,276)

for(i in index){
  df_all_NArem$Group[i] <- "Rep_2"
}

#change the Total Votes Cast category to Total_Votes_Cast
df_all_NArem$Group <- gsub("Total Votes Cast","Total_Votes_Cast",df_all_NArem$Group)

#now go from long to wide format
df_spread <- spread(data=df_all_NArem,key=Group,value=Votes) %>%
              mutate(pctRep = 100*REP/Total_Votes_Cast,
                     pctDem = 100*DEM/Total_Votes_Cast) %>%
              mutate_at(vars(pctRep:pctDem),funs(replace(.,is.na(.),0)))
#note that none of the districts have observed Zero pcts, Zero only occurs if no candidate on ballot.                   

#election board uses Scattered to describe individual write-in votes
df_spread$Scattered= df_spread$Total_Votes_Cast - rowSums(df_spread[2:7],na.rm=TRUE)

df_spread$winner <- ifelse(df_spread$pctRep>df_spread$pctDem,"R","D")

df_spread$winpct <- ifelse(df_spread$pctRep>df_spread$pctDem,
                           df_spread$pctRep,df_spread$pctDem)

#Check the no contest districts
df_no_contest_Dems <- df_spread[df_spread$pctRep==0 & df_spread$winner=="D",]
df_no_contest_Reps <- df_spread[df_spread$pctDem==0 & df_spread$winner=="R",]
        
#make the display plot
p2 <- ggplot(data=df_spread,aes(x=winpct, y=District))+
      geom_point(aes(colour=winner))

df_spread$winner = as.factor(df_spread$winner)
p2 <- ggdotchart(df_spread, x = "District", y ="winpct",
                 group="winner",color="winner",
                 palette=c("#3350FF", "#FF334F"),
                 size = 2.5,
                 shape = 1,
                 #rotate = TRUE,
                 sorting = "ascending",
                 font.ytickslab=c(10,"plain","black"),
                 font.xtickslab=c(5,"plain","black"),
                 #facet.by = "winner",
                 title = "2018 Wisconsin Assembly Election Results",
                 subtitle="Districts with winner receiving > 94% had no major party opponent*",
                 ggtheme = theme_bw(),
                 ylab="Winning percentage",
                 legend.title="Party")

p2 + geom_hline(yintercept=94,linetype="dashed")
      
