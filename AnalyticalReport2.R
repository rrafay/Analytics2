setwd("/Users/Rafay/RWork/Areport2")  ###
library(readxl)                       ###                      
library(igraph)                       ###
######################################### 

##################################################
#Import friends                               ####
FDset<- read_excel("Fall2017.xlsx")           ####
                                              ####
# Get Gibbons Section                         ####
cdF<- FDset[which(FDset$Class == 'Gibbons'),] ####
                                              ####
#Get Onyewu                                   ####
cdO<- FDset[which(FDset$Class == 'Onyewu'),]  ####
                                              ####
#Milhouse                                     ####
cdM<- FDset[which(FDset$Class == 'Milhouse'),]####
                                              ####
#Sampson                                      ####
cdS<-FDset[which(FDset$Class == 'Sampson'),]  ####
##################################################



#Making a final friendship graph for Gibbons
#So first I'm gonna take out the friendships at the end of the semester: EF1-EF4
#EF1
EF1<-cdF[,c(1,11)]
EF1.1<-as.matrix(EF1)
graphEF1<-graph.edgelist(EF1.1, directed = TRUE)
V(graphEF1)$color<-"pink"
#plot(graphEF1, edge.arrow.size = .2)

#EF2
EF2<-cdF[,c(1,12)]
EF2.2<-as.matrix(EF2)
graphEF2<-graph.edgelist(EF2.2, directed= TRUE)
V(graphEF2)$color<-"pink"
#plot(graphEF2, edge.arrow.size =.2)

#EF3
EF3<-cdF[,c(1,13)]
EF3.2<-EF3[which(EF3$EF3 != 'NA'),]
EF3.3<-as.matrix(EF3.2)
graphEF3<-graph.edgelist(EF3.3, directed=TRUE)
V(graphEF3)$color<-"pink"
#plot(graphEF3, edge.arrow.size=.2)

#EF4
EF4<-cdF[,c(1,14)]
EF4.2<-EF4[which(EF4$EF4 != 'NA'),]
EF4.3<-as.matrix(EF4.2)
graphEF4<-graph.edgelist(EF4.3, directed=TRUE)
V(graphEF4)$color<-"pink"

#plot(graphEF4, edge.arrow.size=.2)


#Gibbons final grades
GradesEF<-cdF[,c(1,5)]
GradesDF<-as.matrix(GradesEF)
graphGEF<-graph.edgelist(GradesDF, directed=TRUE)
V(graphGEF)$color<-"pink"
#plot(graphGEF, edge.arrow.size=.2)

#Combining grades and friendships
finalGraph<- graphEF1+ graphEF2+graphEF3 +graphEF4 + graphGEF
V(finalGraph)$color[V(finalGraph)$name=="1"]<-"lightgreen"
V(finalGraph)$color[V(finalGraph)$name=="2"]<-"lightgreen"
V(finalGraph)$color[V(finalGraph)$name=="3"]<-"maroon"
V(finalGraph)$color[V(finalGraph)$name=="4"]<-"maroon"
set.seed(1989)
plot(finalGraph, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Final Friendship Graph for Gibbons")
#legend(-1,-2, c("Maroon: bad grade| Green: good grades
#Pink: friends within class| White: friends outside class"))

#####################################################################
##################################################################################
########################################################################################

#Making the initial friendship graph for Gibbons
#Getting friendships at the beginning of the semester
#SF1
SF1<-cdF[,c(1,6)]
SF1.1<-SF1[which(SF1$SF1 != 'NA'),]
SF1.2<-as.matrix(SF1.1)
graphSF1<-graph.edgelist(SF1.2, directed = TRUE)
V(graphSF1)$color<-"yellow"
#plot(graphSF1, edge.arrow.size = .2)

#SF2
SF2<-cdF[,c(1,7)]
SF2.1<-SF2[which(SF2$SF2 != 'NA'),]
SF2.2<-as.matrix(SF2.1)
graphSF2<-graph.edgelist(SF2.2, directed = TRUE)
V(graphSF2)$color<-"yellow"
#plot(graphSF2, edge.arrow.size = .2)

#SF3
SF3<-cdF[,c(1,8)]
SF3.1<-SF3[which(SF3$SF3 != 'NA'),]
SF3.2<-as.matrix(SF3.1)
graphSF3<-graph.edgelist(SF3.2, directed = TRUE)
V(graphSF3)$color<-"yellow"
#plot(graphSF3, edge.arrow.size = .2)

#Gibbons Initial Grade graph
GradesSF<-cdF[,c(1,4)]
GradesSF<-as.matrix(GradesSF)
graphSFG<-graph.edgelist(GradesSF, directed=TRUE)
V(graphSFG)$color<-"yellow"
#plot(graphSFG, edge.arrow.size=.2)

#Combining grades and friendships
initGraph<- graphSF1+ graphSF2+ graphSF3 + graphSFG
V(initGraph)$color<-"yellow"
V(initGraph)$color[V(initGraph)$name=="1"]<-"lightgreen"
V(initGraph)$color[V(initGraph)$name=="2"]<-"lightgreen"
V(initGraph)$color[V(initGraph)$name=="3"]<-"maroon"
V(initGraph)$color[V(initGraph)$name=="4"]<-"maroon"
V(initGraph)$color[V(initGraph)$name=="Josie Geller"]<-"white"
V(initGraph)$color[V(initGraph)$name=="Roger Bomman"]<-"white"
set.seed(1989)
plot(initGraph, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Initial Friendship Graph for Gibbons")
###########################################################################

#Making a final friendship graph for Onyewu
#So first I'm gonna take out the friendships at the end of the semester: EF1-EF4
#EF1
cdEF1<-cdO[,c(1,11)]
cdEF1.1<-as.matrix(cdEF1)
graphcdEF1<-graph.edgelist(cdEF1.1, directed = TRUE)
V(graphcdEF1)$color<-"sky blue"
#plot(graphcdEF1, edge.arrow.size = .2)

#EF2
cdEF2<-cdO[,c(1,12)]
cdEF2.1<-as.matrix(cdEF2)
graphcdEF2<-graph.edgelist(cdEF2.1, directed = TRUE)
V(graphcdEF2)$color<-"sky blue"
#plot(graphcdEF2, edge.arrow.size = .2)



#EF3
cdEF3<-cdO[,c(1,13)]
cdEF3.2<-cdEF3[which(cdEF3$EF3 != 'NA'),]
cdEF3.3<-as.matrix(cdEF3.2)
graphcdEF3<-graph.edgelist(cdEF3.3, directed=TRUE)
V(graphcdEF3)$color<-"sky blue"
#plot(graphcdEF3, edge.arrow.size=.2)


#EF4
cdEF4<-cdO[,c(1,14)]
cdEF4.2<-cdEF3[which(cdEF4$EF4 != 'NA'),]
cdEF4.3<-as.matrix(cdEF4.2)
graphcdEF4<-graph.edgelist(cdEF4.3, directed=TRUE)
V(graphcdEF4)$color<-"sky blue"
#plot(graphcdEF4, edge.arrow.size=.2)




# Final Grades for class O
fgO<-cdO[,c(1,5)]
fgOMatrix<-as.matrix(fgO)
fgOGrades<-graph.edgelist(fgOMatrix, directed=TRUE)
V(fgOGrades)$color<-"sky blue"
#plot(fgOGrades, edge.arrow.size=.2)

#join grades and friends
mixedO<- graphcdEF1+ graphcdEF2 + graphcdEF3 + graphcdEF4 + fgOGrades
V(mixedO)$color[V(mixedO)$name=="1"]<-"lightgreen"
V(mixedO)$color[V(mixedO)$name=="2"]<-"lightgreen"
V(mixedO)$color[V(mixedO)$name=="3"]<-"maroon"
V(mixedO)$color[V(mixedO)$name=="4"]<-"maroon"
set.seed(1989)
plot(mixedO, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Final Friendship Graph for Onyewu")

######################################################################

#Making the initial friendship graph for Onyewu
#Getting friendships at the beginning of the semester
#SF1
cdSF1<-cdO[,c(1,6)]
cdSF1.1<-as.matrix(cdSF1)
graphcdSF1<-graph.edgelist(cdSF1.1, directed = TRUE)
V(graphcdSF1)$color<-"sky blue"
#plot(graphcdSF1, edge.arrow.size = .2)

#SF2
cdSF2<-cdO[,c(1,7)]
cdSF2.2<-cdSF2[which(cdSF2$SF2 != 'NA'),]
cdSF2.3<-as.matrix(cdSF2.2)
graphcdSF2<-graph.edgelist(cdSF2.3, directed=TRUE)
V(graphcdSF2)$color<-"sky blue"
#plot(graphcdSF2, edge.arrow.size=.2)


#SF3
cdSF3<-cdO[,c(1,8)]
cdSF3.2<-cdSF3[which(cdSF3$SF3 != 'NA'),]
cdSF3.3<-as.matrix(cdSF3.2)
graphcdSF3<-graph.edgelist(cdSF3.3, directed=TRUE)
V(graphcdSF3)$color<-"sky blue"
#plot(graphcdSF3, edge.arrow.size=.2)

#SF4
cdSF4<-cdO[,c(1,9)]
cdSF4.2<-cdSF4[which(cdSF4$SF4 != 'NA'),]
cdSF4.3<-as.matrix(cdSF4.2)
graphcdSF4<-graph.edgelist(cdSF4.3, directed=TRUE)
V(graphcdSF4)$color<-"sky blue"
#plot(graphcdSF4, edge.arrow.size=.2)


#SF5
cdSF5<-cdO[,c(1,10)]
cdSF5.2<-cdSF5[which(cdSF5$SF5 != 'NA'),]
cdSF5.3<-as.matrix(cdSF5.2)
graphcdSF5<-graph.edgelist(cdSF5.3, directed=TRUE)
V(graphcdSF5)$color<-"sky blue"
#plot(graphcdSF5, edge.arrow.size=.2)

#Initial grades for class O
igO<-cdO[,c(1,4)]
igOMatrix<-as.matrix(igO)
igOGrades<-graph.edgelist(igOMatrix, directed=TRUE)
V(igOGrades)$color<-"sky blue"
#plot(igOGrades, edge.arrow.size=.2)


#Join grades and friendships
initialO<- graphcdSF1+ graphcdSF2 + graphcdSF3 + graphcdSF4 + graphcdSF5 + igOGrades
V(initialO)$color<-"sky blue"
V(initialO)$color[V(initialO)$name=="1"]<-"lightgreen"
V(initialO)$color[V(initialO)$name=="2"]<-"lightgreen"
V(initialO)$color[V(initialO)$name=="3"]<-"maroon"
V(initialO)$color[V(initialO)$name=="4"]<-"maroon"
V(initialO)$color[V(initialO)$name=="5"]<-"maroon"
V(initialO)$color[V(initialO)$name=="Kit Keller"]<-"white"
V(initialO)$color[V(initialO)$name=="Ricky Vaughn"]<-"white"
set.seed(1989)
plot(initialO, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Initial Friendship Graph for Onyewu")
#######################################################################################

#Making the final friendship graph for Milhouse
#Getting friendships at the end of the semester
#EF1
Mef1<-cdM[,c(1,11)]
mefMatrix1<-as.matrix(Mef1)
graphmef1<-graph.edgelist(mefMatrix1, directed = TRUE)
V(graphmef1)$color<-"violet"
#plot(graphmef1, edge.arrow.size = .2)


#EF2
Mef2<-cdM[,c(1,12)]
mefMiss2<-Mef2[which(Mef2$EF2 != 'NA'),]
mef2Matrix<-as.matrix(mefMiss2)
graphmef2<-graph.edgelist(mef2Matrix, directed=TRUE)
V(graphmef2)$color<-"violet"
#plot(graphmef2, edge.arrow.size=.2)

#EF3
Mef3<-cdM[,c(1,13)]
mefMiss3<-Mef3[which(Mef3$EF3 != 'NA'),]
mefMatrix3<-as.matrix(mefMiss3)
graphmef3<-graph.edgelist(mefMatrix3, directed=TRUE)
V(graphmef3)$color<-"violet"
#plot(graphmef3, edge.arrow.size=.2)

#EF4
Mef4<-cdM[,c(1,14)]
mefMiss4<-Mef4[which(Mef4$EF4 != 'NA'),]
mefMatrix4<-as.matrix(mefMiss4)
graphmef4<-graph.edgelist(mefMatrix4, directed=TRUE)
V(graphmef4)$color<-"violet"
#plot(graphmef4, edge.arrow.size=.2)

#FInal grades

fgM<-cdM[,c(1,5)]
fgMMatrix<-as.matrix(fgM)
fgGraph<-graph.edgelist(fgMMatrix, directed=TRUE)
V(fgGraph)$color<-"Violet"
#plot(fgGraph, edge.arrow.size=.2)



#Joint final grades and friendships final
fGradeM<- graphmef1+ graphmef2 + graphmef3 + graphmef4 + fgGraph
V(fGradeM)$color[V(fGradeM)$name=="1"]<-"lightgreen"
V(fGradeM)$color[V(fGradeM)$name=="2"]<-"lightgreen"
V(fGradeM)$color[V(fGradeM)$name=="3"]<-"maroon"
V(fGradeM)$color[V(fGradeM)$name=="4"]<-"maroon"
V(fGradeM)$color[V(fGradeM)$name=="5"]<-"maroon"
set.seed(1989)
plot(fGradeM, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Final Friendship Graph for Milhouse")

#Making the initial friendship graph for Milhouse
#Getting friendships at the beginning of the semester
#SF1
Msf1<-cdM[,c(1,6)]
msfMiss1<-Msf1[which(Msf1$SF1 != 'NA'),]
msfMatrix1<-as.matrix(msfMiss1)
graphmsf1<-graph.edgelist(msfMatrix1, directed = TRUE)
V(graphmsf1)$color<-"violet"
#plot(graphmsf1, edge.arrow.size = .2)


#SF2
Msf2<-cdM[,c(1,7)]
msfMiss2<-Msf2[which(Msf2$SF2 != 'NA'),]
msfMatrix2<-as.matrix(msfMiss2)
graphmsf2<-graph.edgelist(msfMatrix2, directed = TRUE)
V(graphmsf2)$color<-"violet"
#plot(graphmsf2, edge.arrow.size = .2)


#SF3
Msf3<-cdM[,c(1,8)]
msfMiss3<-Msf3[which(Msf3$SF3 != 'NA'),]
msfMatrix3<-as.matrix(msfMiss3)
graphmsf3<-graph.edgelist(msfMatrix3, directed = TRUE)
V(graphmsf3)$color<-"violet"
#plot(graphmsf3, edge.arrow.size = .2)


#SF4
Msf4<-cdM[,c(1,9)]
msfMiss4<-Msf4[which(Msf4$SF4 != 'NA'),]
msfMatrix4<-as.matrix(msfMiss4)
graphmsf4<-graph.edgelist(msfMatrix4, directed = TRUE)
V(graphmsf4)$color<-"violet"
#plot(graphmsf4, edge.arrow.size = .2)


#Initial grades for class M
MiG<-cdM[,c(1,4)]
migMatrix<-as.matrix(MiG)
migGraph<-graph.edgelist(migMatrix, directed=TRUE)
V(migGraph)$color<-"violet"
#plot(migGraph, edge.arrow.size=.2)


#Joint initial grades and friendships for Milhouse
iGradeM<- graphmsf1+ graphmsf2 + graphmsf3 + graphmsf4 + migGraph
V(iGradeM)$color[V(iGradeM)$name=="1"]<-"lightgreen"
V(iGradeM)$color[V(iGradeM)$name=="2"]<-"lightgreen"
V(iGradeM)$color[V(iGradeM)$name=="3"]<-"maroon"
V(iGradeM)$color[V(iGradeM)$name=="4"]<-"maroon"
V(iGradeM)$color[V(iGradeM)$name=="5"]<-"maroon"
set.seed(1989)
plot(iGradeM, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Initial Friendship Graph for Milhouse")
#########################################################################

#Making the final friendship graph for Sampson
#Getting friendships at the end of the semester

#EF1
cef1<-cdS[,c(1,11)]
cefMatrix1<-as.matrix(cef1)
graphcef1<-graph.edgelist(cefMatrix1, directed = TRUE)
V(graphcef1)$color<-"blue"
#plot(graphcef1, edge.arrow.size = .2)


#EF2
cef2<-cdS[,c(1,12)]
cefMiss2<-cef2[which(cef2$EF2 != 'NA'),]
cefMatrix2<-as.matrix(cefMiss2)
graphcef2<-graph.edgelist(cefMatrix2, directed = TRUE)
V(graphcef2)$color<-"blue"
#plot(graphcef2, edge.arrow.size = .2)


#EF3
cef3<-cdS[,c(1,13)]
cefMiss3<-cef3[which(cef3$EF3 != 'NA'),]
cefMatrix3<-as.matrix(cefMiss3)
graphcef3<-graph.edgelist(cefMatrix3, directed = TRUE)
V(graphcef3)$color<-"blue"
#plot(graphcef3, edge.arrow.size = .2)


#EF4
cef4<-cdS[,c(1,14)]
cefMiss4<-cef4[which(cef4$EF4 != 'NA'),]
cefMatrix4<-as.matrix(cefMiss4)
graphcef4<-graph.edgelist(cefMatrix4, directed = TRUE)
V(graphcef4)$color<-"blue"
#plot(graphcef4, edge.arrow.size = .2)


#FInal grades

fgSE<-cdS[,c(1,5)]
fgsMatrix<-as.matrix(fgSE)
fgsGraph<-graph.edgelist(fgsMatrix, directed=TRUE)
V(fgsGraph)$color<-"blue"
#plot(fgsGraph, edge.arrow.size=.2)

#Joint final grades and friendship for Sampson

samfinalGrade<- graphcef1+ graphcef2 + graphcef3 + graphcef4 + fgsGraph
V(samfinalGrade)$color[V(samfinalGrade)$name=="1"]<-"lightgreen"
V(samfinalGrade)$color[V(samfinalGrade)$name=="2"]<-"lightgreen"
V(samfinalGrade)$color[V(samfinalGrade)$name=="3"]<-"maroon"
V(samfinalGrade)$color[V(samfinalGrade)$name=="4"]<-"maroon"
V(samfinalGrade)$color[V(samfinalGrade)$name=="5"]<-"maroon"
set.seed(1989)
plot(samfinalGrade, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Final Friendship Graph for Sampson")
#############################################################


#Making the initial friendship graph for Sampson
#Getting friendships at the beginning of the semester
#SF1
SSF1<-cdS[,c(1,6)]
SSFMiss1<-SSF1[which(SSF1$SF1 != 'NA'),]
SSFMatrix1<-as.matrix(SSFMiss1)
graphSSF1<-graph.edgelist(SSFMatrix1, directed = TRUE)
V(graphSSF1)$color<-"blue"
#plot(graphSSF1, edge.arrow.size = .2)

#SF2
SSF2<-cdS[,c(1,7)]
SSFMiss2<-SSF2[which(SSF2$SF2 != 'NA'),]
SSFMatrix2<-as.matrix(SSFMiss2)
graphSSF2<-graph.edgelist(SSFMatrix2, directed = TRUE)
V(graphSSF2)$color<-"blue"
#plot(graphSSF2, edge.arrow.size = .2)

#Initial Grades for Sampson 
igS<-cdS[,c(1,4)]
igSMatrix<-as.matrix(igS)
igSGraph<-graph.edgelist(igSMatrix, directed=TRUE)
V(igSGraph)$color<-"blue"
#plot(igSGraph, edge.arrow.size=.2)

#Joint intial grades and friendship for Sampson

saminitGrade<- graphSSF1+ graphSSF2 + igSGraph 
V(saminitGrade)$color[V(saminitGrade)$name=="1"]<-"lightgreen"
V(saminitGrade)$color[V(saminitGrade)$name=="2"]<-"lightgreen"
V(saminitGrade)$color[V(saminitGrade)$name=="3"]<-"maroon"
V(saminitGrade)$color[V(saminitGrade)$name=="4"]<-"maroon"
V(saminitGrade)$color[V(saminitGrade)$name=="5"]<-"maroon"
set.seed(1989)
plot(saminitGrade, edge.arrow.size=.2, layout= layout.fruchterman.reingold)
title("Initial Friendship Graph for Sampson")


