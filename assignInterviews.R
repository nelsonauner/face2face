###################################################################################
#-------------------------------------FUNCTIONS-----------------------------------#
###################################################################################

#Brownian motion to simulate interviewer availability. This is to replicate 
#actual sign-ups, which follow "trends", like
#I am free from 3PM-6PM, but not 6PM-9PM, rather than I am free with 50% chance
#every half hour from 3PM-9PM
gentime <- function(T) {
	x <- cumsum(rnorm(T,sd=2))
	y <- (x>0)*1
}

#Count adjacencies. Take the sign-up matrix and subtract it from its one-period lag
adjacency <- function(t,S) {
	adj <- numeric(n)
	s <- S[,1:t]
	diff <- cbind(rep(0,n),s[,-1]-s[,-t])
	for(i in 1:n) {
		if(which.max(diff[i,])>which.min(diff[i,]) & s[i,t-1]==1) {
			adj[i] <- t-which.max(diff[i,])
		}
	}
	return(adj)
}

#Score Function
#Method: Penalize going over max slots, but also penalize "unused" slots (ie penalize if a 
#consultant gets less interviews than they are permitted). HOWEVER, make this vary by 
#time since otherwise leadership will get all time slots at first and none later. 
#Assign experienced consultant first, then assign next consultant, prefering to pair with 
#a new consultant
assign <- function(t,S,A,Max) {
	avail <- A[,t]
	if (sum(avail)==0) {return()}
	
	#Penalize going over max time slots, but do not overly reward being under min
	#as this would heavily assign slots in the beginning to leadership
	intCt <- pmin(Max$max-rowSums(S),1)
	
	#goodAdj rewards adjacency up to the max specified number of adjacencies (maxAdj)
	adj <- adjacency(t,S)
	goodAdj <- pmin(maxAdj-adj,1)
	
	#dist increases propensity to assign slots to people who are below the max
	#as time goes on, so that as we get nearer the end, the preference to assign 
	#to under-assigned interviewers increases
	dist <- log(pmax(Max$max-rowSums(S),1))/(T-t+2)
	
	#Score, and add small amount of random noise to prevent ties
	Score <- (intCt + S[,t-1] + goodAdj + dist + rnorm(n,sd=.02))*A[,t]
	
	#Which experienced consultant has highest score
	expind <- which.max(Score[1:sum(Max$type)])
	
	#Eliminate the assigned consultant
	Score[expind] <- 0
	
	#Give advantage to new consultants
	Score[sum(Max$type):n] <- Score[sum(Max$type):n]+1
	
	#Assign the second interviewer
	newind <- which.max(Score)
	
	#Change the sign-up matrix to assign the interviewers
	S[c(expind,newind),t] <- 1
	
	#Return the updated sign-up matrix
	return(S)
}





###################################################################################
#-----------------------------------TRAINING DATA---------------------------------#
###################################################################################
# # #Availability matrix
# A <- matrix(0,n,T)

# #Level of consultant, 1=leadership, 2=EM, 3=experienced, 4=new
# L <- data.frame("level"=c(rep(1,4),rep(2,4),rep(3,6),rep(4,9)))

# for (i in 1:n) {
	# A[i,] <- gentime(T)
# }


setwd("/Users/dreidpilch/Dropbox/EC Leadership 2011-2012/2013-2014/InterviewSignUps_Automated")

###################################################################################
#------------------------------------TESTING DATA---------------------------------#
###################################################################################
#Testing data - actuall Fall 2013 interview availability sign-ups
d <- read.table("interviewinghoursfall.csv",header=FALSE,sep=',')

#Number of time slots (9AM-5:30PM Mon-Thurs): 
T <- 72

#Number of interviewers:
n <- 13

A <- as.matrix(d[,3:74])
L <- data.frame("level"=d[,2])

#Max time by level, where 1 is leadership, 2 is EM, 3 is experienced consultant,
#4 is new consultant. Each level corresponds to a maximum number of interviews, called 
#"max". Type refers to who can be paired with whom, so always prefer pairing a 1 with a 0
#to a 1 with a 1, and never pair a 0 with a 0
M <- data.frame("level"=c(1,2,3,4),"max"=c(16,16,8,4),"type"=c(1,1,0,0))
Max <- merge(L,M,by="level")

#Penalize adjacency after maxAdj, which is 6 slots or 3 hours of consecutive interviews
maxAdj <- 6

#Actual assignments, with 1 for interviewing, 0 for not
S <- matrix(0,n,T)
#Assign first time slot (just by looking at who is available):
S[,1] <- c(0,0,0,0,0,0,0,0,0,1,0,0,1)

#For each time slot, assign:
for(i in 2:T) {
	t <- i
	S <- assign(t,S,A,Max)
}

#Look and see how it turned out:
cbind(rowSums(S),rowSums(A),Max$max)

