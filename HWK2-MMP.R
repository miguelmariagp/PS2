#HOMEWORK 2 - MIGUEL PEREIRA

# Exercise 1 - Calculating violations

ben.function<-function(x,m=TRUE, d=TRUE){
  #If x is matrix, becomes a vector
  x.vec<-as.vector(x)
  #Now the function takes the first digit of all numbers in the vector
  x.vec<-substr(x.vec,1,1)
  #And computes frequencies
  frequencies<-prop.table(table(x.vec))
  #Here I create a vector of length 9 with 0s, and plug in the frequencies for each number extracted from the vector
  x.i<-rep(0,9)
  x.i[as.numeric(names(frequencies))]<-as.vector(frequencies)
  #Next, I compute the formula used in both methods and assign it to object formula
  formula<-NULL
  for (i in 1:9){
    formula[i]<-x.i[i]-log(1+1/i,base=10)
  }
  #Finally, object leem corresponds to Leemis' m and object cg to Cho-Gains' d
  leem<-max(formula)
  cg<-sqrt(sum(formula^2))
  AllFalse<-"Don't be so false."
  #Here I deal with the optional features of the function
  ifelse(m==FALSE&d==FALSE, return(AllFalse), ifelse(m==TRUE&d==FALSE, return(list(Leemis=leem)), 
          ifelse(m==FALSE&d==TRUE, return(list(ChoGains=cg)), return(list(Leemis=leem,ChoGains=cg)))))
}

#TESTS
#Using a matrix
z<-matrix(c(1,2,13,41,1,1),ncol=2)
ben.function(z,m=TRUE,d=FALSE)

#Using a vector
s<-sample(1:10000,500,replace=TRUE)
b<-ben.function(s,m=TRUE,d=TRUE)


################
#Exercise 2
################

#This function prints a table with Benfords' statistics and levels of reliability
print.benfords<-function(x){
  #Compute estimates from the previous function and store them in two objects (m,cg)
  estimates<-ben.function(x)
  m<-round(estimates$Leemis,3)
  cg<-round(estimates$ChoGains,3)
  #Create significance thresholds
  m.thresholds<-c(.851,.967,1.212)
  cg.thresholds<-c(1.212,1.330,1.569)
  #Gives me a number from 1 to 4 for each statistic
  #1 meaning not significant, 2 meaning p<.1, 3 meaning p<.05, and 4 meaning p<.01
  m.level<-sum(m.thresholds<m)+1
  cg.level<-sum(cg.thresholds<cg)+1
  #Those numbers are used to extract the asterisks from this object
  sig<-c("","*","**","***")
  #Creating the table
  the.table<-data.frame(Statistic=c(m,cg),Sig.=c(sig[m.level],sig[cg.level]))
  #And adding the footnote at the bottom of the table
  the.table<-rbind(the.table,c("",""))
  rownames(the.table)<-c("Leemis' m","Cho-Gains' d","*** p<.01; ** p<.05; * p<.1")
  return(the.table)
}

#Test
print.benfords(s)


#Function to export benfords' table to a csv
#I added the Working directory as a default for export
benford.to.csv<-function(x,export=getwd()){
  #If directory is not explicitly stated
  if (export==getwd()){
    write.csv(print.benfords(x),file=paste(export,"/output.csv",sep=""))
  #I added this because almost always I wonder where did the file go to  
    return(paste("Table saved in",paste(export,"/output.csv",sep="")))
  }
  else
  #If directory is explicitly stated
    write.csv(print.benfords(x),file=export)
  #I added this because almost always I wonder where did the file go to
    return(paste("Table saved in",export))
}

#Test
benford.to.csv(s)
benford.to.csv(s,export="C:/Users/ststest/Dropbox/Spr16/Programming/table1.csv")



#This function makes both together, if you feel like it
print.save.benfords<-function(x,export=NULL){
  #Compute estimates from the previous function
  estimates<-ben.function(x)
  m<-round(estimates$Leemis,3)
  cg<-round(estimates$ChoGains,3)
  #Create significance thresholds
  m.thresholds<-c(.851,.967,1.212)
  cg.thresholds<-c(1.212,1.330,1.569)
  #And a trick to extract the respective asterisks
  #Each statistic has a number associated that goes from 0 (not sig.) to 4(p<.01)
  m.level<-sum(m.thresholds<m)+1
  cg.level<-sum(cg.thresholds<cg)+1
  sig<-c("","*","**","***")
  #Creating the table
  the.table<-data.frame(Statistic=c(m,cg),Sig.=c(sig[m.level],sig[cg.level]))
  #Adding the footnote
  the.table<-rbind(the.table,c("",""))
  rownames(the.table)<-c("Leemis' m","Cho-Gains' d","*** p<.01; ** p<.05; * p<.1")
  #Output if the user decides to export the table
  if (is.null(export)==FALSE){
    write.csv(the.table,file=export)
    return(list(Table=the.table,Output=paste("Table saved in",export)))}
  #Output otherwise
  else return(list(Table=the.table))
}

print.save.benfords(s,export=NULL)
print.save.benfords(s,export="C:/Users/ststest/Dropbox/Spr16/Programming/table2.csv")
