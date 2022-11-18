## a) Expected number of dice throws

p<-1/4
         #1,2,3,4,6
mdata<-c(p,p,p,p,0,   #1
         p,0,p,p,p,   #2
         2*p,0,0,p,p, #3
         2*p,p,0,0,p, #4
         0,0,0,0,1)   #6
m<-matrix(mdata,5,5,byrow = TRUE)

Q<-m[0:4,0:4]
R<-m[0:4,5]
O<-m[5,0:4]

I<-diag(4)
F=solve(I-Q)
NOF <- F%*%c(1,1,1,1) # Number of throws
1/4*(NOF[1] + NOF[2] + NOF[3] + NOF[4]) + 1 # Total throws


## b) Probability that the player will visit state 3
p<-1/4
        #1,2,4,3,6
mdata<-c(p,p,p,p,0,   #1
         p,0,p,p,p,   #2
         2*p,p,0,0,p, #4
         0,0,0,1,0,   #3
         0,0,0,0,1)   #6
m<-matrix(mdata,5,5,byrow = TRUE)

Q<-m[0:3,0:3]
R<-m[0:3,4:5]
O<-m[4:5,0:3]

I<-diag(3)
F=solve(I-Q)
F%*%R # Prob for stepping on 3 or 6 from 1,2 or 4