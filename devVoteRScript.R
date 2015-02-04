#####################################################
# This is an R script I use when doing development work on the package. The end user should not have a need to use this script.
#
#####################################################
rm(list = ls(all = TRUE))


#################
# R OPTIONS
#################
options(scipen=2) #scientific notation off


##################
# Globals
##################
numRunsGlobal <- 4
numDimsGlobal <- 2
numVotersGlobal <- 100

##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################



###############################################################
# Finished Functions that are already in \R
###############################################################
  
  
  #######################
  ## Voter Functions
  #######################

  
  
  #######################
  ## Competitor Functions
  #######################
  
  
  
  
  #######################
  ## Model Functions
  #######################
  
  
  
  

###############################################################
# Functions being written to go into VoteR as seperate .R files
###############################################################
  
  #######################
  ## Voter Functions
  #######################
#' genIdeals
#' Generates one or two dimensional ideals from one of given set of distributions, from calibrated data, or from a user provided distribution.
#' 
#' @param numVotersGenIdeals Number of voters
#' @param numDimsGenIdeals The number of policy dimensions. 
#' @param distnTypeGenIdeals A string identifying the base R discribution to draw the ideal points from. Uses the base R random number generation family of commands rxxxx (see ?distributions). The user should specify the distribution as a string using the standard R abreviation for the distribution (see ?distributions for a list). Example for a Normal(0,1), use "norm", with the quotes because it is a string. Current valid distributions are "unif" and "norm".
#' @param distnParamsGenIdeals A vector that contains the additional parameters needed by the particular rxxxx function for a distribtuion. (see ?rxxxx where xxxx is a function listed under ?distribution). Example for a Normal(0,1), use: c(0,1). 
#' @return outIdeals An ideal point matrix that is numVoters x numDimensions
#' @export
genIdeals <- function(numVotersGenIdeals, numDimsGenIdeals, distnTypeGenIdeals="unif",distnParamsGenIdeals=c(-1,1)){
  if(distnTypeGenIdeals!="unif" & distnTypeGenIdeals!="norm"){
    stop("Currently the following distributions are supported: Uniform, and Normal.")
  }
  
  if(distnTypeGenIdeals=="unif"){
    rawIdeals <- runif(numVotersGenIdeals*numDimsGenIdeals, distnParamsGenIdeals[1], distnParamsGenIdeals[2]) # Long vector of length numVoters x numDims    
  }

  if(distnTypeGenIdeals=="norm"){
    rawIdeals <- rnorm(numVotersGenIdeals*numDimsGenIdeals,distnParamsGenIdeals[1],distnParamsGenIdeals[2]) # Long vector of length numVoters x numDims    
  }
  
  
  outIdeals <- matrix(data=rawIdeals,nrow=numVotersGenIdeals, ncol=numDimsGenIdeals)
} 
  
  
  #######################
  ## Competitor Functions
  #######################
  # Cox's Competitor Assumptons: 
    # C1) 
      # i) Competitors choose postions to maximize E(seat share) 
      # ii) Competitors choose positions to maximize E(vote share)
    # C2) There is a fixed number of competitors m, and m >= 2
    # C3) 
      #  i) Competitiors can make incremental changes in their policy position without cost. 
      # ii) Competitiors can make any change in their policy position without cost. 
  
  
  
  #######################
  ## Model Functions
  #######################
  
  

####################################################################
# 
####################################################################


# 1) Gen a set of voters and store them for use across all runs of the model.
set.seed(123) # for development purposes

idealsGened <- genIdeals(numVotersGenIdeals=numVotersGlobal, numDimsGenIdeals=numDimsGlobal, distnTypeGenIdeals="norm", c(0,1))
idealsGened

# 2) Gen a set of Competitors


alts <- genIdealsUnif(3,2)
salience <- genSalience(nrow(ideals),ncol(ideals),.8,2)


# 2) Place them in to a set of electoral-legislative systems






# 3) Calculate the Pareto set 






minkoWithSaliSubs <- minkowskiUtilityDistance(ideals,alts1,1,salience)
minkoWithSaliEuclidian <- minkowskiUtilityDistance(ideals,alts1,2,salience)
minkoWithSaliComps <- minkowskiUtilityDistance(ideals,alts1,100,salience)

minkoWithSaliSubs
minkoWithSaliEuclidian
minkoWithSaliComps

minkoWithSaliSubs2 <- minkowskiUtilityDistance(ideals,alts2,1,salience)
minkoWithSaliEuclidian2 <- minkowskiUtilityDistance(ideals,alts2,2,salience)
minkoWithSaliComps2 <- minkowskiUtilityDistance(ideals,alts2,100,salience)

minkoWithSaliSubs2
minkoWithSaliEuclidian2
minkoWithSaliComps2

alts3 <- rbind(c(0,0),c(0,1),c(.5,0),c(-.5,0),c(0,-1))
minkoWithSaliSubs3 <- minkowskiUtilityDistance(ideals,alts3,1,salience)
minkoWithSaliEuclidian3 <- minkowskiUtilityDistance(ideals,alts3,2,salience)
minkoWithSaliComps3 <- minkowskiUtilityDistance(ideals,alts3,100,salience)

minkoWithSaliSubs3
minkoWithSaliEuclidian3
minkoWithSaliComps3


getPrefOrder()

# Generic Function and methods for Finding a Preference Ordering. For a  

getPrefOrder <- function(x) UseMethod("getPrefOrder")


getPrefOrder.proximity <- function(x) { 
n = length(x)
p = mean(x)
mu = n*p
sigma = sqrt(n*p*(1-p))
return( list(mu = mu, sigma = sigma, n = n) )
										}


getPrefOrder.directional <- function(x) {
n = length(x)
mu = mean(x)
sigma = sd(x)
return( list(mu = mu, sigma = sigma, n = n) ) 
										}

getPrefOrder.default <- function(x) stop("This package does not know how to calculate a preference ordering for an object of that type.")
