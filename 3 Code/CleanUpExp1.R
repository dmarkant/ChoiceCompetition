################################ Choice Competition ################################################################
#
# processing data of Experiment 1
#


options(stringsAsFactors = F)

DIR = '/Users/wulff/Dropbox/PhD/Projects/Side topics/14 Technion Competition/'

# - - - - - - - - - - - -
# get Data
# - - - - - - - - - - - -

d <- read.table(paste0(DIR,'2 Data/RawDataExperiment1.csv'),sep=',',header=T)

paperProbs <- read.table(paste0(DIR,'2.1 partitionedData/problemsExp1fromPaper.txt'))
paperProbsIDs <- apply(paperProbs,1,function(x) paste0(x[2:7],collapse='_'))

sel          <- c('Ha','pHa','La','Hb','pHb','Lb')
dataProbsIDs <- apply(d,1,function(x) paste0(x[sel],collapse='_'))
mean(paperProbsIDs%in%dataProbsIDs);mean(dataProbsIDs%in%paperProbsIDs)

for(id in paperProbsIDs){
  dataProbsIDs[dataProbsIDs==id] <-  rownames(paperProbs)[which(id==paperProbsIDs)]   
  }

d <- data.frame(d,'problem'=dataProbsIDs)

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

prob      <- c('problem','GameID','Ha','pHa','La','Hb','pHb','Lb','LotNum','LotShape','Corr','Amb')
problems  <- data.frame(d[!duplicated(d$problem),prob])
names(problems)[2] <- 'problemInData' ; problems[,1] <- as.numeric(problems[,1])
problems  <- problems[order(problems$problem),]
rownames(problems) <- NULL

dataPs  <- problems[,-2]
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

standardData <- subset(d,Manipulation == 'Abstract' & LotNum == 1 & Corr == 0 & Amb == 0)

nam                 <- c('SubjID','problem','Trial','Risk','Condition')
standardData        <- standardData[,nam]
names(standardData) <- c('subject','problem','trial','choice','condition')
for(i in 1:ncol(standardData)) {
  if(sum(letters %in% strsplit(as.character(standardData[1,i]),'')[[1]]) == 0) standardData[,i] <- as.numeric(standardData[,i])
  }
standardData        <- merge(standardData,problems[,c(1,3:8)],by='problem') 



## special Problems

specialData  <- subset(d,!(Manipulation == 'Abstract' & LotNum == 1 & Corr == 0 & Amb == 0))

specialProb <- specialData$problem
specialProb[specialProb=='12'] <- 'acceptReject'
specialProb[specialProb=='13'] <- 'acceptReject'
specialProb[specialProb=='20'] <- 'StPetersburg - Abstract'
specialProb[specialProb=='19'] <- 'StPetersburg - CoinToss'
specialProb[specialProb=='30'] <- 'posCorrelation'
specialProb[specialProb=='28'] <- 'negCorrelation'
specialProb[specialProb=='16'] <- 'threeOutcome'


nam                 <- c('SubjID','problem','Trial','Risk','Condition')
specialData         <- data.frame(specialData[,nam],'type'=specialProb)
names(specialData)  <- c('subject','problem','trial','choice','condition')
for(i in 1:ncol(specialData)) {
  if(sum(letters %in% strsplit(as.character(specialData[1,i]),'')[[1]]) == 0) specialData[,i] <- as.numeric(specialData[,i])
  }

specialData         <- merge(specialData,problems[,-2])


write.table(standardData,paste0(DIR,'2.1 partitionedData/stDataExp1.txt'))
write.table(specialData,paste0(DIR,'2.1 partitionedData/spDataExp1.txt'))



# - - - - - - - - - - - -
# checkData
# - - - - - - - - - - - -

results <- data.frame(1:30,paperProbs[,paste0('V',12:16)])
names(results) <- c('problem',paste0('block',1:5))

combData <- rbind(standardData[,1:5],specialData[,1:5])

res <- dlply(combData,.(problem),function(x) round(tapply(x$choice,ceiling(x$trial/5),mean),2))
res <- do.call(rbind,res)


write.table(paste0(DIR,'2.1 partitionedData/stDataExp1.txt'))





