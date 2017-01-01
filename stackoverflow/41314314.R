# http://stackoverflow.com/questions/41314314/forloop-conditional-statements-storing-data

#Parameters
c=0.2
A=5
d=8
d0=5
s=0.5
e=0.1
p=0.6
ER=e/A

#Colonization Equation Probabilities
C2 = c*A*exp(-d*s/d0)               #ML to SS

#Empty Vectors
l=vector(mode="integer", length=100) #open vectors to store the different probability values from the forloop
b=vector(mode="integer", length=100) 
prb=l+b                             #total probability of SS being colonized 

#Island States
ML=1                    
SS=prb[0:1]            
n.I=c(ML, SS)   

#Forloop and Conditional Statements

for(i in 1:100) {

      if (SS >= 1 ) { 
           (prb[i]=(prb[i-1]*ER)+b[i])

  }   
}
