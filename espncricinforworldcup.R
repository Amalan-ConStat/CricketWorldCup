# Load the packages
library(rvest)
library(stringr)
library(splitstackshape)

# Load hte links into separate variables
link1975<-"http://stats.espncricinfo.com/ci/engine/series/60793.html"
link1979<-"http://stats.espncricinfo.com/ci/engine/series/60806.html"
link1983<-"http://stats.espncricinfo.com/ci/engine/series/60832.html"
link1987<-"http://stats.espncricinfo.com/ci/engine/series/60876.html"
link1992<-"http://stats.espncricinfo.com/ci/engine/series/60924.html"
link1996<-"http://stats.espncricinfo.com/ci/engine/series/60981.html"
link1999<-"http://stats.espncricinfo.com/ci/engine/series/61046.html"
link2003<-"http://stats.espncricinfo.com/ci/engine/series/61124.html"
link2007<-"http://stats.espncricinfo.com/ci/engine/series/125929.html"
link2011<-"http://stats.espncricinfo.com/ci/engine/series/381449.html"
link2015<-"http://stats.espncricinfo.com/ci/engine/series/509587.html"
link2019<-"http://www.espncricinfo.com/ci/engine/series/1144415.html"

WorldCup<-function(weblink)
{
  #extract the webpage information from cricinfo page
  base_url<-weblink
  readpage<-read_html(base_url)
  dialogue<-html_nodes(readpage,'.small-20')
  
  # extract the data from html components
  data<-html_text(dialogue)
  
  # add the infor into a matrix
  data_mat<-matrix(data)
  
  # remove unnecessary rows     
  data_mat_edit<-as.matrix(data_mat[-c((1:3),((dim(data_mat)[1]-3):dim(data_mat)[1]))])
  
  # replace \n, \t and " " with - 
  data_mat_rep<-data.frame(str_replace_all(data_mat_edit,c("\n"="-","\t"="-"," "="-")))
  
  names(data_mat_rep)<-"Column"
  
  # remove the --- patterns and split it into  columns
  data_mat_tab<-cSplit(data_mat_rep,"Column","---")
  data_mat_tab_good<-data_mat_tab[,c(2,5,8,11,12)]
  
  # acquire information about matches, basically which one and what type
  Matches<-str_remove(data_mat_tab_good$Column_02,":")
  
  # acquiring who vs who and locations
  WhovsWhos<-data.frame(cSplit(data_mat_tab_good,"Column_05","-at-")[,c("Column_05_1","Column_05_2")])
  Location<-data.frame("Location"=WhovsWhos$Column_05_2)
  WhovsWho<-str_replace(WhovsWhos$Column_05_1,"--","")
  
  # acquiring the Date
  Date<-str_replace(str_replace(data_mat_tab_good$Column_08,",",""),"-","")
  
  # Information of how wins
  won<-str_replace_all(str_replace(data_mat_tab_good$Column_11,"-",""),"-"," ")
  
  # match info extracted
  onetwo<-data.frame(cSplit(data_mat_tab_good,"Column_12",";")[,c("Column_12_1","Column_12_2")])
  names(onetwo)<-c("Team1","Team2")
  
  # Team 1 overs and Team 2 overs extracted
  team1_overs<-str_remove(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", onetwo$Team1, perl=T),"-ov")
  team2_overs<-str_remove(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", onetwo$Team2, perl=T),"-ov")
  
  # Runs 1 and Runs 2 extracted
  Runs1<-str_match(onetwo$Team1, "\\-(\\d{2,3})")[,2]
  Runs2<-str_match(onetwo$Team2, "\\-(\\d{2,3})")[,2]
  
  # Team 1 and Team 2 extracted
  Team1<-dplyr::recode_factor(cSplit(onetwo,"Team1","-")$Team1_1,"Sri"="Sri Lanka","West"="West Indies",
                       "South"="South Africa","New"="New Zealand","East"="East Africa","United"="United Arab Emirates")
  Team2<-dplyr::recode_factor(cSplit(onetwo,"Team2","-")$Team2_2,"Sri"="Sri Lanka","West"="West Indies",
                              "South"="South Africa","New"="New Zealand","East"="East Africa","United"="United Arab Emirates")
  
  # extract Wickets lost on both teams
  wickets1<-str_match(onetwo$Team1, "\\-(\\d{2,3})\\/(\\d{0,1})\\-")[,3]
  wickets1[is.na(wickets1)]<-10
  
  wickets2<-str_match(onetwo$Team2, "\\-(\\d{2,3})\\/(\\d{0,1})\\-")[,3]
  wickets2[is.na(wickets2)]<-10
    
  # create that one dataframe for a worldcup
  output<-cbind.data.frame("Matches" = Matches, "Location" = Location,"WhovsWho" = WhovsWho,"Date" = Date, 
                           "WinningConditions" = won, 
                           "Team1" = Team1, "Overs1" = team1_overs, "Runs1" =  Runs1,"Wickets1"=wickets1,
                           "Team2" = Team2, "Overs2" = team2_overs, "Runs2" = Runs2,"Wickets2"=wickets2)
  
  return(output)
}

# using the worldcup function to extract data
# saving them in individual data.frames
Cup1975<-WorldCup(link1975)
Cup1979<-WorldCup(link1979)
Cup1983<-WorldCup(link1983)
Cup1987<-WorldCup(link1987)
Cup1992<-WorldCup(link1992)
Cup1996<-WorldCup(link1996)
Cup1999<-WorldCup(link1999)
Cup2003<-WorldCup(link2003)
Cup2007<-WorldCup(link2007)
Cup2011<-WorldCup(link2011)
Cup2015<-WorldCup(link2015)
Cup2019<-WorldCup(link2019)

# Combining those individual data.frames into one whole data.frame
WholeData<-rbind(Cup1975,Cup1979,Cup1983,Cup1987,Cup1992,Cup1996,
                 Cup1999,Cup2003,Cup2007,Cup2011,Cup2015,Cup2019)

# extracting the match types such as pools, groups, super eights
WholeDatanew<-cSplit(WholeData,"Matches",",-")

# converting the match references into all lower case characters
WholeDatanew$Matches_1<-str_to_lower(WholeDatanew$Matches_1)

# recoding factors such as quarter finals and semi finals into one level.
WholeDatanew$Matches_1<-dplyr::recode_factor(WholeDatanew$Matches_1,"1st-quarter-final"="1st-qf","2nd-quarter-final"="2nd-qf",
                                             "3rd-quarter-final"="3rd-qf","4th-quarter-final"="4th-qf",
                                             "1st-semi-final"="1st-sf","2nd-semi-final"="2nd-sf","3rd-semi-final"="3rd-sf",
                                             "4th-semi-final"="4th-sf")

# extracting which team won from winning conditions
Winning_conditions<-str_split(WholeDatanew$WinningConditions,"won")
Winning_conditions<-do.call("rbind",Winning_conditions)

# adding the new column of who won to the existing data.frame 
FinalData<-data.table::data.table(WholeDatanew,Winning_conditions[,1])

# Renaming the column names again for the new data set
names(FinalData)<-c("Location","WhosWho","Date","WinningConditions","Team1","Overs1","Runs1","Wickets1",
                    "Team2","Overs2","Runs2","Wickets2","Matches","MatchType","Winner")

# even though match was not played wickets 10 have been added so replacing them with NA
FinalData[c(20,368,417,422,424,93),c("Wickets1","Wickets2")]<-NA

# even though match was not played wickets 10 have been added so replacing them with NA
FinalData[c(139,244,328,421),c("Wickets2")]<-NA

# creating a duck worth lewis method column for matches which the technique was applied
FinalData<-data.table::data.table(FinalData,str_extract(FinalData$WinningConditions,"(D/L method)"))

# naming that duck worth lewis column
names(FinalData)[names(FinalData) == 'V2'] <- 'D/LMethod'


