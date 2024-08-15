BRITANNIA

B<- read.xlsx("C:/Users/HP/Desktop/Project/Britannia data.xlsx")
B

#Confidence interval:#

yB = mean(B$ChangeXn,na.rm = T)
yB
zB = qt(0.95, df = 59)
zB
wB = sd(B$ChangeXn,na.rm = T)/sqrt(60)
wB
lB = zB*wB
lB
limitB<-c(yB-lB,yB+lB)
limitB

#Transition Prob matrix :#

transitionmatrixB = table(B$StateXn,B$StateXn1)
print(transitionmatrixB)
initialprobmatrixB = matrix(data = rowSums(transitionmatrixB)/60, nrow = 1,ncol = 3,byrow = T)
initialprobmatrixB
transprobmatrixB = transitionmatrixB/rowSums(transitionmatrixB)
transprobmatrixB

#m step Transition Probability Matrix:#

matrixobjTPMB<-unclass(transprobmatrixB)
markovobjTPMB <- new("markovchain", states = c("D","I","S"), byrow = T, transitionMatrix = matrixobjTPMB, name = "TPMB")
markovobjTPMB
plot(markovobjTPMB)
steadyStates(markovobjTPMB)


JUBILANT


J<- read.xlsx("C:/Users/HP/Desktop/Project/Jubilant data.xlsx")
J

#Confidence interval:#

yJ= mean(J$ChangeXn,na.rm = T)
yJ
zJ= qt(0.95, df = 59)
zJ
wJ = sd(J$ChangeXn,na.rm = T)/sqrt(60)
wJ
lJ= zJ*wJ
lJ
limitJ<-c(yJ-lJ,yJ+lJ)
limitJ

#Transition Prob matrix :#

transitionmatrixJ = table(J$StateXn,J$StateXn1)
print(transitionmatrixJ)
initialprobmatrixJ = matrix(data = rowSums(transitionmatrixJ)/60, nrow = 1,ncol = 3,byrow = T)
initialprobmatrixJ
transprobmatrixJ = transitionmatrixJ/rowSums(transitionmatrixJ)
transprobmatrixJ

#m step Transition Probability Matrix:#

matrixobjTPMJ<-unclass(transprobmatrixJ)
markovobjTPMJ <- new("markovchain", states = c("D","I","S"), byrow = T, transitionMatrix = matrixobjTPMJ, name = "TPMJ")
markovobjTPMJ
plot(markovobjTPMJ)
steadyStates(markovobjTPMJ)




NESTLE


N<- read.xlsx("C:/Users/HP/Desktop/Project/Nestle data.xlsx")
N

#Confidence interval:#

yN= mean(N$ChangeXn,na.rm = T)
yN
zN= qt(0.95, df = 59)
zN
wN = sd(N$ChangeXn,na.rm = T)/sqrt(60)
wN
lN= zN*wN
lN
limitN<-c(yN-lN,yN+lN)
limitN

#Transition Prob matrix :#

transitionmatrixN = table(N$StateXn,N$StateXn1)
print(transitionmatrixN)
initialprobmatrixN = matrix(data = rowSums(transitionmatrixN)/60, nrow = 1,ncol = 3,byrow = T)
initialprobmatrixN
transprobmatrixN = transitionmatrixN/rowSums(transitionmatrixN)
transprobmatrixN


#m step Transition Probability Matrix:#

matrixobjTPMN<-unclass(transprobmatrixN)
markovobjTPMN <- new("markovchain", states = c("D","I","S"), byrow = T, transitionMatrix = matrixobjTPMN, name = "TPMN")
markovobjTPMN
plot(markovobjTPMN)
steadyStates(markovobjTPMN)

