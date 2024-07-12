#D-Efficiency Ranking-Based Conjoint Analysis
RBC=matrix(c("2","2","1","1",   "1","1","2","1",  "3","3","3","1",
             "1","3","1","2",   "3","2","2","2",  "2","1","3","2",
             "3","1","1","3",   "2","3","2","3",  "1","2","3","3"),
           nrow = 9, ncol = 4, byrow = TRUE);
RBC=as.data.frame(RBC);names(RBC)=c("Destination","Transportation","Accommodation", "Activities")
X=as.matrix(model.matrix(~.,RBC));J=dim(X) [1]
XsX=t(X)%*%X; XsXinv=solve(XsX); p=dim(X) [2]
D_RBC=1/(J*det(XsXinv)**(1/p)); D_RBC

#D-Efficiency Choice-Based Conjoint Analysis
CBC=matrix(c("3","2","1","3",   "3","1","2","2",  "2","2","2","1",
             "1","3","2","3",   "1","2","3","2",  "2","1","3","3",
             "3","3","3","1",   "1","1","1","1",  "2","3","1","2"),
           nrow = 9, ncol = 4, byrow = TRUE);
CBC=as.data.frame(CBC);names(CBC)=c("Destination","Transportation","Accommodation", "Activities")
X=as.matrix(model.matrix(~.,CBC));J=dim(X) [1]
XsX=t(X)%*%X; XsXinv=solve(XsX); p=dim(X) [2]
D_CBC=1/(J*det(XsXinv)**(1/p)); D_CBC

#D-Efficiency Full Factorial Design

Full=expand.grid(Destination=c("1","2","3"), Transportation=c("1","2","3"), Accommodation=c("1","2","3"),Activities=c("1","2","3"))
X=as.matrix(model.matrix(~.,Full));J=dim(X) [1]
XsX=t(X)%*%X; XsXinv=solve(XsX); p=dim(X) [2]
D_Full=1/(J*det(XsXinv)**(1/p)); D_Full; D_RBC/D_Full; D_CBC/D_Full
