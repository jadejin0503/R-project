#################################
#  Course Work 2 - group 7      #
#################################

#################################
#       Group Member:           #
#################################
# Qingqing Fan s2493867         #
# Xuan Jin s2477282             #
# Zifan Huang s2550946          #
#################################

# Contribution: This is group 7, we work cooperatively such as converting real-world problems into coding, debugging, plotting, and commenting.

## Overview:
# This code is supposed to set up a model by first writing a function to simulate cars passing through French and then British passport control stations at a French ferry terminal.
# Then, run the finished simulating function 'qsim' to get the simulation result which reflects obvious dynamic changes in two stations. This process is also analyzed by plotting some graphs with different parameters.
# The logical assumption for our code: there is no car driving into french passport control station within the last 30 mins, cars in british and french passport control stations can still be processed. In addition, if cars finish their processing in british passports within 2 hours, they can catch up with ferry departure;  
qsim <- function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){
  ## function: 
  #qsim: the function to analyze and simulate the car's passing situation.
  ## input variables:
  #mf: the number of french passport control stations;
  #mb: the number of british passport control stations;
  #a.rate: the probability of a car arriving french station at each second;
  #trb, trf, tmb, tmf: the model constant given for the Uniform distribution of processing time;
  #maxb: the maximum british queue length (per station);
  ## output:
  #nf: a vector giving the average length of the french queues, for each simulation second;
  #nb: a vector giving the average length of the british queues, for each simulation second;
  #eq: a vector giving the average expected waiting time for a car at the start of the french queue for each second of the simulation;
  #################################
  
  #F_queue: The number of cars in each french station, where there are 'mf' stations;
  #F_time: the processing time for the first car in each station in french stations;
  #B_queue: The number of cars in each british station, where there are 'mb' stations;
  #B_time: the processing time for the first car in each station in british stations;
  F_queue <- F_time<- array(rep(0,mf))
  B_queue <- B_time <- array(rep(0,mb))
  nf <- nb<- eq <- array(rep(0,7200))
  
  ##Update model each second during 2 hours;
  for(i in 1:7200){ 
    ##This 'for' loop is supposed to simulate cars passing in british stations with each loop representing each station;
    for(k in 1:mb){
      if(B_time[k]<1 && B_queue[k]>0){ #determine which station just finishing a car processing and there are cars behind;
        B_queue[k] <- B_queue[k]-1 #the chosen station drive away a car;
        if(B_queue[k]==0){B_time[k]<-0} #judge if this station has no car after driving away one car;
        else {B_time[k] <- runif(1,min=tmb,max=tmb+trb)} #else, assign a new processing time for the next car;
      }
      else if(B_time[k]>=1){B_time[k] <- B_time[k]-1} #if the car is in processing, the time passed one second;
    }
    
    ##This 'for' loop is supposed to simulate cars passing in french passport, with each loop representing each station;
    for(j in 1:mf){
      Bmin <- min(B_queue) #the least number of british stations;
      ##following 'if/else' is used to choose the No. of the least number of british station;
      if(length(which(B_queue == Bmin))==1){Bmin_queue<-which(B_queue == Bmin)}
      else {Bmin_queue <- sample(which(B_queue == Bmin),1)}
      ##following 'if/else' is supposed to simulate there is a car finished processing and then drive to british from french;
      if(F_time[j]<1 && F_queue[j]>0 && Bmin<maxb){
        F_queue[j] <- F_queue[j]-1
        if(Bmin == 0){B_time[Bmin_queue] <- runif(1,min=tmb,max=tmb+trb)} #if there is no car in station when car drive to british from french;
        B_queue[Bmin_queue] <- B_queue[Bmin_queue]+1
        if(F_queue[j]==0){F_time[j] <- 0} else{F_time[j] <- runif(1,min=tmf,max=tmf+trf)} #if there are cars in station when a car drives to british from french;
      }
      ##following 'else if' is simulated if cars in british stations are full, which gets to 'maxb' cars in each;
      else if(F_time[j]<1 && min(B_queue)==maxb){F_time[j] <- 0}
      ##if the car is in processing, the time passed one second;
      else if(F_time[j]>=1){F_time[j] <- F_time[j] -1}
    }
    ##following 'if' is supposed to simulate if a new car drives to french passport 30 mins before the british passport closed, with the probability 'a.rate';
    if(i<5400){
      
      p <- sample(0:1,1,replace=TRUE,c(1-a.rate,a.rate))
      ##if driving in a new car in french station;
      if(p==1){
        Fmin <- min(F_queue) #the least number of stations in french passport;
        if(length(which(F_queue == Fmin))==1){Fmin_queue<-which(F_queue == Fmin)}
        else {Fmin_queue <- sample(which(F_queue == Fmin),1)} #the No. of the least number of stations in british passport control station;
        ##if there is no car in french station when car drive in;
        if(Fmin == 0){
          F_time[Fmin_queue] <- runif(1,min=tmf,max=tmf+trf)
        }
        F_queue[Fmin_queue] <- F_queue[Fmin_queue]+1
      }
    }
    #updating our vectors
    nf[i] <- mean(F_queue)
    nb[i] <- mean(B_queue)
    eq[i] <- mean(F_time)+(nf[i]-1)*(tmf+trf+tmf)/2+mean(B_time)+(nb[i]-1)*(tmb+trb+tmb)/2
  }
  return(list(nf,nb,eq))
}


##testing the time of simulating one time, which is less than 1 second;
timing <- system.time(qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20))
print(timing)
# user  system elapsed 
# 0.359   0.003   0.362 


##plotting
t <- c(1:7200)
plot_r1 <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)
plot_r2 <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)
##layout: 2*2 plot
par(mfrow= c(2,2), bg='snow',pin=c(2,2),oma=c(0.8,0.8,0.8,0.8),mar=c(2,2,2,2))
##the first row of the plot
##the figure for 'Comparison for the queue length', with the default value of parameters;
plot(t, plot_r1[[1]], type ='l',xlab='t',ylab = 'length of station queue', main ='Comparison for the queue length',col = 'blue')      
lines(t, plot_r1[[2]], col= 'red')
legend("topleft",  legend=c('France','British'), col = c("blue","red"),lty=1,lwd=2,cex = 0.65) 
##the figure for 'The Expected queuing time', with the default value of parameters;
plot(t, plot_r1[[3]],type ='l',xlab='t',ylab = 'expected time', main ='The Expected queuing time (tmb=30s)',col='darkolivegreen4')
##the second row of the plot
##the figure for 'Comparison for the queue length', with the new value of parameters, tmb=40;
plot(t, plot_r2[[1]],type ='l',xlab='t',ylab= 'length of station queue', main = 'Comparison for the queue length',col = 'blue')
lines(t, plot_r2[[2]],col= 'red')
legend("topleft",  legend=c('France','British'), col = c("blue","red"),lty=1,lwd=2,cex = 0.65) 
##the figure for 'The Expected queuing time', with the new value of parameters, tmb=40;
plot(t, plot_r2[[3]],type ='l',xlab='t',ylab = 'expected time', main = 'The Expected queuing time (tmb=40s)',col='lightsalmon')


##estimate the probability of at least one car missing the ferry departure
system.time({
  n <- 0
  for(i in 1:100){
    result <- qsim(mf=5,mb=5,a.rate=0.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)
    f <- result[[1]][7200]+result[[2]][7200]
    if(f>0){n<-n+1}
  }
  prob <- n/100
  cat(prob) })  
  #roughly 35s
  #the probability is greater than zero but very small;


## What happens if extend the processing time for british passport control stations?
#Numerically, the number of cars in each british passport will increase, also in french stations. Since the extended time will contribute to the increased number of cars in british which will get to the limit of each station. Then, the car in french could not get to british in this situation, causing an increase in french stations as well.
#Graphically, the number of british passports has a significant increase after extending, and the fluctuation of french changed frequently. Moreover, the expected waiting time has increased a lot.
#The extended processing time also caused the probability of missing departure according to the above estimation, though this is a low number. Longer processing time may lead to a higher missing departure probability. Therefore, the government should better to take measures to decrease this delay as much as possible so that address this issue.
  
## According to the simulation we did, and the conclusion we got, what is the suggestion to the government?
#As we see, since some force majeure factors will extend the processing time contributes to a much longer waiting time. So,
#1. Extend the maximum length for british stations or the number of stations in british is necessary;
#2. The government can decrease the probability of car access to french flexibly;
#3. Setting up a waiting station between french and british passports could improve this situation.
