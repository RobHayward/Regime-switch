# http://a-little-book-of-r-for-bioinformatics.
	readthedocs.org/en/latest/src/chapter10.html
myvector <- numeric()
	for(i in 1:10) {myvector[i] <- i*i}
myvector
heights <- c(180, 170, 175, 160, 183, 177, 179, 182)
weights <- c(90, 88, 100, 68, 95, 120, 88, 93)
mymatrix <- matrix(c(heights, weights), 2, 8, byrow = TRUE)
mymatrix
----------------
nucleoids <- c("A", "C", "G", "T")
probabilities <- c(0.2, 0.3, 0.3, 0.2)
seqlength <- 30
sample(nucleoids, seqlength, rep = TRUE, prob = probabilities)
probabilities2 <- c(0.1, 0.41, 0.39, 0.1)
sample(nucleoids, seqlength, rep = TRUE, prob = probabilities2)
nucleotides <- c("A", "C", "G", "T")
AfterAprobs <- c(0.2, 0.3, 0.3, 0.2)
AfterCprobs <- c(0.1, 0.41, 0.39, 0.1)
AfterGprobs <- c(0.25, 0.25, 0.25, 0.25)
AfterTprobs <- c(0.50, 0.17,  0.17, 0.17)
mytransitionmatrix <- matrix(c(AfterAprobs, AfterCprobs, AfterGprobs, 
		AfterTprobs), 4, 4, byrow = TRUE)
rownames(mytransitionmatrix) <- nucleoids
colnames(mytransitionmatrix) <- nucleoids
mytransitionmatrix
generatemarkovseq <- function(transitionmatrix, initialprobs, seqlength) {
nucleotides <- c("A", "C", "G", "T")
mysequence <- character()
firstnucleotide <- sample(nucleotides, 1, rep = TRUE, prob = initialprobs)
mysequence[1] <- firstnucleotide
 for(i in 2:seqlength)
{
prevnucleotide <- mysequence[i-1]
probabilities <- transitionmatrix[prevnucleotide, ]
nucleotide <- sample(nucleotides, 1, rep = TRUE, prob = probabilities)
mysequence[i] <- nucleotide
}
return(mysequence)
}
initialprobs <- c(0.25, 0.25, 0.25, 0.25)
generatemarkovseq(mytransitionmatrix, myinitialprobs, 30)
states <- c("AT-rich", "GC-rich")
ATrichprobs <- c(0.7, 0.3)
GCrichprobs <- c(0.1, 0.9)
thetransitionmatrix <- matrix(c(ATrichprobs, GCrichprobs), 2, 2, byrow = TRUE)
rownames(thetransitionmatrix) <- states
colnames(thetransitionmatrix) <- states
thetransitionmatrix
nucleotides <- c("A", "C", "G", "T")
ATrichstateprobs <- c(0.39, 0.1, 0.1, 0.41)
GCrichstateprobs <- c(0.1, 0.41, 0.39, 0.1)
theemissionmatrix <- matrix(c(ATrichstateprobs, GCrichstateprobs), 2, 2, 
	byrow = TRUE)
thetransitionmatrix
# Function to generate a DNA sequence, 
# given a HMM and the length of the sequence to be generated.
generatehmmseq <- function(transitionmatrix, emissionmatrix, 
	initialprobs, seqlength)
{
nucleotides <- c("A", "C", "G", "T")   # Define the alphabet of nucleotides
states <- c("AT-rich", "GC-rich") # Define the names of the states
mysequence <- character() # Create a vector for storing the new sequence
mystates  <- character()  # Create a vector for storing the state that 
# each position in the new sequence was generated by
# Choose the state for the first position in the sequence:
firststate <- sample(states, 1, rep=TRUE, prob=initialprobs)
# Get the probabilities of the current nucleotide, given that we are 
# in the state "firststate":
probabilities   <- emissionmatrix[firststate,]
# Choose the nucleotide for the first position in the sequence:
firstnucleotide <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
mysequence[1] <- firstnucleotide # Store the nucleotide for the first 
# position of the sequence
mystates[1] <- firststate # Store the state that the first position in the sequence was generated by
  for (i in 2:seqlength)
{
prevstate <- mystates[i-1] # Get the state that the 
# previous nucleotide in the sequence was generated by
# Get the probabilities of the current state, 
# given that the previous nucleotide was generated by state "prevstate"
stateprobs   <- transitionmatrix[prevstate,]
# Choose the state for the ith position in the sequence:
state <- sample(states, 1, rep=TRUE, prob=stateprobs)
# Get the probabilities of the current nucleotide, given that we are in the state "state":
probabilities <- emissionmatrix[state,]
# Choose the nucleotide for the ith position in the sequence:
nucleotide   <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
mysequence[i] <- nucleotide # Store the nucleotide 
# for the current position of the sequence
mystates[i]  <- state # Store the state that the 
# current position in the sequence was generated by
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

