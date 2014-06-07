# http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/
#latest/src/chapter10.html
myvector <- numeric()
for(i in 1:10){
  myvector[i] <- i * i
  }
myvector
heights <- c(100, 170, 175, 100, 183, 177, 179, 182)
weight <- c(90, 88, 100, 68, 95, 120, 88, 93)
mymatrix <- matrix(c(heights, weight), 2, 8, byrow = TRUE)
mymatrix
rownames(mymatrix) <- c("Height", "Weight")
colnames(mymatrix) <- c("P1", "P2", "P3", "p4", "P5", "P6", "P7", "P8")
-------------
nucleotides <- c("A", "C", "G", "T")
probabilities <- c(0.2, 0.3, 0.5, 0.2)
seqlength <- 30
sample(nucleotides, seqlength, rep = TRUE, prob = probabilities)
probabilities2 <- c(0.1, 0.41, 0.39, 0.1)
sample(nucleotides, seqlength, rep = TRUE, prob = probabilities2)
# For Markov sequence there are four different probabilities
nucleotides <- c("A", "C", "G", "T") 
afterAprobs <- c(0.2, 0.3, 0.3, 0.2)         
afterCprobs <- c(0.1, 0.41, 0.39, 0.1)       
afterGprobs <- c(0.25, 0.25, 0.25, 0.25)     
afterTprobs <- c(0.5, 0.17, 0.17, 0.17)      
mytransitionmatrix <- matrix(c(afterAprobs, afterCprobs, 
  afterGprobs, afterTprobs), 4, 4, byrow = TRUE)
rownames(mytransitionmatrix) <- nucleotides
colnames(mytransitionmatrix) <- nucleotides
mytransitionmatrix                        
---------------------------
generatemarkovseq <- function(transitionmatrix, initialprobs, seqlength) {
nucleotides     <- c("A", "C", "G", "T") 
mysequence      <- character()           
firstnucleotide <- sample(nucleotides, 1, rep=TRUE, prob=initialprobs)
mysequence[1]   <- firstnucleotide       
for (i in 2:seqlength)
{
  prevnucleotide <- mysequence[i-1]     
  probabilities  <- transitionmatrix[prevnucleotide,]
  nucleotide     <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
  mysequence[i]  <- nucleotide          
  }
  return(mysequence)
}
myinitialprobs <- c(0.25, 0.25, 0.25, 0.25)
generatemarkovseq(mytransitionmatrix, myinitialprobs, 30)
--------------------------------
states<- c("AT-rich", "GC-rich") # Define the names of the states
ATrichprobs <- c(0.7, 0.3)  
GCrichprobs <- c(0.1, 0.9)
thetransitionmatrix <- matrix(c(ATrichprobs, GCrichprobs), 
                              2, 2, byrow = TRUE) # Create a 2 x 2 matrix
rownames(thetransitionmatrix) <- states
colnames(thetransitionmatrix) <- states
thetransitionmatrix     

----------------------------------------------
nucleotides         <- c("A", "C", "G", "T")   
ATrichstateprobs    <- c(0.39, 0.1, 0.1, 0.41) 
GCrichstateprobs    <- c(0.1, 0.41, 0.39, 0.1) 
theemissionmatrix <- matrix(c(ATrichstateprobs, GCrichstateprobs), 
                            2, 4, byrow = TRUE) 
rownames(theemissionmatrix) <- states
colnames(theemissionmatrix) <- nucleotides
theemissionmatrix                              
--------------------------------------------------------------
# Function to generate a DNA sequence, given a HMM and the length of the sequence 
generatehmmseq <- function(transitionmatrix, emissionmatrix, initialprobs,  
                           seqlength)  {
    nucleotides     <- c("A", "C", "G", "T")   
    states          <- c("AT-rich", "GC-rich") 
    mysequence      <- character()             
    mystates        <- character()             
    firststate      <- sample(states, 1, rep=TRUE, prob=initialprobs)
    probabilities   <- emissionmatrix[firststate,]
    firstnucleotide <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
    mysequence[1]   <- firstnucleotide      
    mystates[1]     <- firststate             
    
    for (i in 2:seqlength)
    {
      prevstate    <- mystates[i-1]
      stateprobs   <- transitionmatrix[prevstate,]
      state        <- sample(states, 1, rep=TRUE, prob=stateprobs)
      probabilities <- emissionmatrix[state,]
      nucleotide   <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
      mysequence[i] <- nucleotide 
      mystates[i]  <- state       
    }
    
for (i in 1:length(mysequence))
    {
      nucleotide   <- mysequence[i]
      state        <- mystates[i]
      print(paste("Position", i, ", State", state, ", Nucleotide = ", nucleotide))
    }
  }
theinitialprobs <- c(0.5, 0.5)
generatehmmseq(thetransitionmatrix, theemissionmatrix, theinitialprobs, 30)
---------------------------------------------------------------------------
viterbi <- function(sequence, transitionmatrix, emissionmatrix) 
  {
# This carries out the Viterbi algorithm.
# Adapted from "Applied Statistics for Bioinformatics using R" by Wim P. Krijnen, page 209
# ( cran.r-project.org/doc/contrib/Krijnen-IntroBioInfStatistics.pdf )
# Get the names of the states in the HMM:
states <- rownames(theemissionmatrix)
# Make the Viterbi matrix v:
v <- makeViterbimat(sequence, transitionmatrix, emissionmatrix)
  
# Go through each of the rows of the matrix v (where each row represents
# a position in the DNA sequence), and find out which column has the
# maximum value for that row (where each column represents one state of
# the HMM):
mostprobablestatepath <- apply(v, 1, function(x) which.max(x))
    
# Print out the most probable state path:
prevnucleotide <- sequence[1]
prevmostprobablestate <- mostprobablestatepath[1]
prevmostprobablestatename <- states[prevmostprobablestate]
startpos <- 1
  for (i in 2:length(sequence))
    {
      nucleotide <- sequence[i]
      mostprobablestate <- mostprobablestatepath[i]
      mostprobablestatename <- states[mostprobablestate]
      if (mostprobablestatename != prevmostprobablestatename)
      {
print(paste("Positions",startpos,"-",(i-1), "Most probable state = ", 
            prevmostprobablestatename))
        startpos <- i
      }
      prevnucleotide <- nucleotide
      prevmostprobablestatename <- mostprobablestatename
    }
print(paste("Positions",startpos,"-",i, "Most probable state = ", 
            prevmostprobablestatename))
  }
# The viterbi() function requires a second function makeViterbimat():
# This must be made first.

----------------------------------------------------------------------
myseq <- c("A", "A", "G", "C", "G", "T", "G", "G", "G", "G", "C", 
           "C", "C", "C", "G", "G", "C", "G", "A", "C", "A", 
           "T", "G", "G", "G", "G", "T", "G", "T", "C")
viterbi(myseq, thetransitionmatrix, theemissionmatrix)
