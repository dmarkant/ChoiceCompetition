################################ Choice Competition ################################################################
#
# processing data of Experiment 1
#


options(stringsAsFactors = F)

DIR = '/Users/wulff/Dropbox/PhD/Projects/Side topics/14 Technion Competition/'

# - - - - - - - - - - - -
# get Data
# - - - - - - - - - - - -

d <- read.table(paste0(DIR,'2 Data/RawDataExperiment1sorted.csv'),sep=',',header=T)
d$SubjID[d$Location == 'Rehovot'] <- d$SubjID[d$Location == 'Rehovot'] + 1000
d <- d[order(d$SubjID,d$GameID,d$Trial),]

paperProbs <- read.table(paste0(DIR,'2.1 partitionedData/problemsExp1fromPaper.txt'))

d <- data.frame(d)


## Variables
# 
# SubjID        - Subject identification
# Location      - Location of testing (I presume)
# Gender
# Age
# set           - seemingly irrelevant
# Condition     - ByFB: separation by format | ByProb: separation by problem
# GameID        - Problem identification
# Ha            - High outcome in option A
# pHa           - Probability in high outcome in option A
# La            - Low outcome in option A
# Hb            - High outcome in option B
# pHb           - Probability in high outcome in option B
# Lb            - Low outcome in option B
# Manipulation  - Framing of the choice: Abstract = Standard description; StPb = St. Petersburg type; AcRj = accept/reject type
# Amb           - Ambiguity parameter: 0 = probabilities of B are described, 1 = probabilities of B are not described
# Lot Shape     - If LotNum > 1, shape of the distribution of multiple outcomes, more explanation in Appendix
# LotNum        - If LotNum > 1, then option B has more than 2 outcomes, i.e. either 3 or 8, as indicated by LotNum in those cases,
#                 Note: 3 outcome problems needed for Splitting effects, 8 outcome problems for St. Petersburg type problems
# Corr          - Correlation between outcomes, mostly 0, for one problem 1, for another -1
#                 Note: needed for Correlation effect; don't really get this one
# Order         - Position of the problem in the experimental sequence, 1 to 30
# Trial         - Position of the choice within one problem, 1 to 25
# Button        - Choice of 'L'eft and 'R'ight option
# Risk          - Choice of option A = 0, choice of option B (risky) = 1
# Payoff        - Outcome obtained from chosen option
# Forgone       - Outcome obtained from non-chosen option 
# Feedback      - Format: 0 for description, 1 for experience
# Block         - Relevant Problem subunit, splits problem in subsequent packages of 5 choices
# problem       - problem according to paper

# - - - - - - - - - - - -
# get Problems
# - - - - - - - - - - - -

# - Problem -> purpose

problemPurpose = data.frame(matrix(c(
 1  , 'Allais paradox','Certainty effect','Reflection effect', 
 2  , 'Allais paradox','Certainty effect', NA,
 3  , 'Reflection effect','Reversed reflection effect','Break-even Effect',
 4  , 'Reflection effect','Reversed reflection effect','Get-something effect',
 5  , 'Reflection effect','Reversed reflection effect','Break-even Effect',
 6  , 'Weighting of rare events','Break-even Effect',NA,
 7  , 'Weighting of rare events',NA,NA,
 8  , 'Weighting of rare events',NA,NA,
 9  , 'Weighting of rare events','Get-something effect',NA,
 10 , 'Weighting of rare events','Get-something effect',NA,
 11 , 'Weighting of rare events',NA,NA,
 12 , 'Loss Aversion','Magnitude effect',NA,
 13 , 'Loss Aversion',NA,NA,
 14 , 'Loss Aversion','Magnitude effect',NA,
 15 , 'Loss Aversion',NA,NA,
 16 , 'Loss Aversion',NA,NA, 
 17 , 'Loss Aversion',NA,NA,
 18 , 'Loss Aversion',NA,NA,
 19 , 'St. Petersburg paradox',NA,NA,
 20 , 'St. Petersburg paradox',NA,NA, 
 21 , 'Ambiguity aversion',NA,NA, 
 22 , 'Ambiguity aversion',NA,NA,  
 23 , 'Ambiguity aversion',NA,NA, 
 24 , 'Break-even Effect',NA,NA,
 25 , 'Get-something effect',NA,NA,
 26 , 'Splitting effect',NA,NA,
 27 , 'Splitting effect',NA,NA,
 28 , 'Payoff variability effect','Correlation effect',NA, 
 29 , 'Payoff variability effect',NA,NA,
 30 , 'Correlation effect',NA,NA
),ncol=4,byrow=T)) 

rownames(problemPurpose) = NULL
names(problemPurpose)    = c('problem',paste0('problem',1:3))

effects = unique(unlist(problemPurpose[,2:4]))
effects = effects[!is.na(effects)]


## Problem -> purpose

purposeProblems = data.frame(t(sapply(effects,function(x) {
  ps = unique(c(which(problemPurpose[,2] == x),
                which(problemPurpose[,3] == x),
                which(problemPurpose[,4] == x)))
  return(c(x,ps,rep(NA,7-length(ps))))
  })))

rownames(purposeProblems) = NULL
names(purposeProblems)    = c('effect',paste0('problem',1:7))

## Problems (and comparison between paper and data)

prob      <- c('GameID','Ha','pHa','La','Hb','pHb','Lb','LotNum','LotShape','Corr','Amb')
problems  <- data.frame(d[!duplicated(d$GameID),prob])
problems  <- problems[order(problems$GameID),]
rownames(problems) <- NULL ; names(problems)[1] <- 'problem'

dataPs  <- problems
paperPs <- paperProbs[,paste0('V',1:11)] 
names(paperPs) <- names(dataPs) 
paperPs[,'LotShape'] <- substr(paperPs[,'LotShape'],1,1)

paperPs == dataPs


## write data

write.table(problems,paste0(DIR,'2.1 partitionedData/problemsExp1.txt'))
write.table(problemPurpose,paste0(DIR,'2.1 partitionedData/problemPurposeExp1.txt'))
write.table(purposeProblems,paste0(DIR,'2.1 partitionedData/purposeProblemExp1.txt'))


# - - - - - - - - - - - -
# split Data
# - - - - - - - - - - - -

## standard 2-outcome Problems

sel <- d$Manipulation == 'Abstract' & d$LotNum == 1 & d$Corr == 0 & d$Amb == 0

stData <- subset(d,sel)
nam           <- c('SubjID','GameID','Trial','Risk','Condition')
stData        <- stData[,nam]
names(stData) <- c('subject','problem','trial','choice','condition')

for(i in 1:ncol(stData)) {
  if(sum(letters %in% strsplit(as.character(stData[1,i]),'')[[1]]) == 0) stData[,i] <- as.numeric(stData[,i])
  }
stData        <- merge(stData,problems[,c(1,3:8)],by='problem') 

stData        <- stData[order(stData$subject,stData$problem,stData$trial),]
mean(stData$choice == d[sel,'Risk'])


## special Problems

spData  <- subset(d,!sel)

spProb <- spData$GameID
spProb[spProb=='12'] <- 'acceptReject'
spProb[spProb=='13'] <- 'acceptReject'
spProb[spProb=='20'] <- 'StPetersburg - Abstract'
spProb[spProb=='19'] <- 'StPetersburg - CoinToss'
spProb[spProb=='30'] <- 'posCorrelation'
spProb[spProb=='28'] <- 'negCorrelation'
spProb[spProb=='16'] <- 'threeOutcome'


nam           <- c('SubjID','GameID','Trial','Risk','Condition')
spData        <- data.frame(spData[,nam],'type'=spProb)
names(spData) <- c('subject','problem','trial','choice','condition','task')
for(i in 1:ncol(spData)) {
  if(sum(letters %in% strsplit(as.character(spData[1,i]),'')[[1]]) == 0) spData[,i] <- as.numeric(spData[,i])
  }

spData         <- merge(spData,problems[,-2],by='problem')


write.table(standardData,paste0(DIR,'2.1 partitionedData/stDataExp1.txt'))
write.table(specialData,paste0(DIR,'2.1 partitionedData/spDataExp1.txt'))


# - - - - - - - - - - - -
# checkData
# - - - - - - - - - - - -


results <- data.frame(1:30,paperProbs[,paste0('V',12:16)])
names(results) <- c('problem',paste0('block',1:5))

combData <- rbind(stData[,1:5],spData[,1:5])
combData <- combData[order(combData$subject,combData$problem,combData$trial),][,c(2,1,3:5)]
d.ord    <- d[order(d$SubjID,d$GameID,d$Trial),]

mean(combData$subject == d.ord$SubjID)
mean(combData$problem == d.ord$GameID)
mean(combData$choice == d.ord$Risk)


res <- dlply(combData,.(problem),function(x) round(tapply(x$choice,ceiling(x$trial/5),mean),2))
res <- do.call(rbind,res)

res.d <- dlply(d,.(GameID),function(x) round(tapply(x$Risk,ceiling(x$Trial/5),mean),2))
res.d <- do.call(rbind,res.d)


diffs1 <- res-results[,-1]
diffs2 <- res.d-results[,-1]
colMeans(diffs1);colMeans(diffs2)


# - - - - - - - - - - - -
# check ByProb vs FB
# - - - - - - - - - - - -

resByProb <- dlply(subset(combData,condition=='ByProb'),.(problem),function(x) round(tapply(x$choice,ceiling(x$trial/5),mean),2))
resByProb <- do.call(rbind,resByProb)

resByFB <- dlply(subset(combData,condition=='ByFB'),.(problem),function(x) round(tapply(x$choice,ceiling(x$trial/5),mean),2))
resByFB <- do.call(rbind,resByFB)

resByProb - resByFB




