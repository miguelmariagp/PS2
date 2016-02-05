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

#Using a matrix
z<-matrix(c(1,2,13,41,1,1),ncol=2)
ben.function(z,m=TRUE,d=FALSE)

#Using a vector
s<-sample(1:10000,500,replace=TRUE)
ben.function(s,m=TRUE,d=TRUE)


#Exercise 2 - 


