#' Get Maximum Relief
#'
#' Returns maximum of primary and secondary releif. Acceptable values for relief include "None", "Covered_Low_Relief_Hard_Bottom","Semi_Exposed_Low_Relief_Hard_Bottom","Low_Relief_Hard_Bottom","High_Relief_Hard_Bottom",and "Pinnacle"
#' @param p_relief column of prelief data
#' @param s_relief column of secondary relief data
#' @keywords relief
#' @export

max_relief<- function(p_relief, s_relief){
  n=length(p_relief)
  p_num<- vector(mode = "numeric", length = n) #dimension variable
  p_num[1:n]<- NA #initialize as NA
  s_num<- vector(mode = "numeric", length = n) #dimension variable
  s_num[1:n]<- NA #initialize as NA
  p_num[p_relief=="None"]=0 #Convert relief to numbers
  p_num[p_relief=="Covered_Low_Relief_Hard_Bottom"]=1
  p_num[p_relief=="Semi_Exposed_Low_Relief_Hard_Bottom"]=2
  p_num[p_relief=="Low_Relief_Hard_Bottom"]=3
  p_num[p_relief=="Moderate_Relief_Hard_Bottom"]=4
  p_num[p_relief=="High_Relief_Hard_Bottom"]=5
  p_num[p_relief=="Pinnacle"]=6
  s_num[s_relief=="None"]=0
  s_num[s_relief=="Covered_Low_Relief_Hard_Bottom"]=1
  s_num[s_relief=="Semi_Exposed_Low_Relief_Hard_Bottom"]=2
  s_num[s_relief=="Low_Relief_Hard_Bottom"]=3
  s_num[s_relief=="Moderate_Relief_Hard_Bottom"]=4
  s_num[s_relief=="High_Relief_Hard_Bottom"]=5
  s_num[s_relief=="Pinnacle"]=6
  max_num<- pmax(s_num,p_num) #Get max relief
  max_relief<- vector(mode = "character", length = n) #dimension variable
  max_relief[max_num==0]="None" #Convert max relief back to character
  max_relief[max_num==1]="Covered_Low_Relief_Hard_Bottom"
  max_relief[max_num==2]="Semi_Exposed_Low_Relief_Hard_Bottom"
  max_relief[max_num==3]="Low_Relief_Hard_Bottom"
  max_relief[max_num==4]="Moderate_Relief_Hard_Bottom"
  max_relief[max_num==5]="High_Relief_Hard_Bottom"
  max_relief[max_num==6]="Pinnacle"
  return(max_relief)
}
