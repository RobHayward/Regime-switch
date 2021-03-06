#test
makeViterbimat <- function(sequence, transitionmatrix, emissionmatrix) {
  # This makes the matrix v using the Viterbi algorithm.
  # Adapted from "Applied Statistics for Bioinformatics using R" by Wim P. Krijnen, page 209
  # ( cran.r-project.org/doc/contrib/Krijnen-IntroBioInfStatistics.pdf )
  # Change the sequence to uppercase
  sequence <- toupper(sequence)
  # Find out how many states are in the HMM
  numstates <- dim(transitionmatrix)[1]
  # Make a matrix with as many rows as positions in the sequence, and as many
  # columns as states in the HMM
  v <- matrix(NA, nrow = length(sequence), ncol = dim(transitionmatrix)[1])
  # Set the values in the first row of matrix v (representing the 
  # first position of the sequence) to 0
  v[1, ] <- 0
  # Set the value in the first row of matrix v, first column to 1
  v[1,1] <- 1
  # Fill in the matrix v:
  for (i in 2:length(sequence)) {
    # For each position in the DNA sequence:
    for (l in 1:numstates) # For each of the states of in the HMM:
    {
      # Find the probabilility, if we are in state l, of choosing the nucleotide 
      # at position in the sequence
      statelprobnucleotidei <- emissionmatrix[l,sequence[i]]
      
      # v[(i-1),] gives the values of v for the (i-1)th row of v, ie. the 
      # (i-1)th position in the sequence.  In v[(i-1),] there are values of 
      # v at the (i-1)th row of the sequence for each possible state k.
      # v[(i-1),k] gives the value of v at the (i-1)th row of the sequence 
      # for a particular state k.
      
      # transitionmatrix[l,] gives the values in the lth row of the 
      # transition matrix, xx should not be transitionmatrix[,l]?
      # probabilities of changing from a previous state k to a current 
      # state l.
      
      # max(v[(i-1),] * transitionmatrix[l,]) is the maximum probability 
      # for the nucleotide observed at the previous position in the 
      # sequence in state k, followed by a transition from previous
      # state k to current state l at the current nucleotide position.
      
      # Set the value in matrix v for row i (nucleotide position i), 
      # column l (state l) to be:
      v[i,l] <-  statelprobnucleotidei * max(v[(i-1),] * transitionmatrix[,l])
    }
  }
  return(v)
}


viterbi <- function(sequence, transitionmatrix, emissionmatrix) 
{
  # This carries out the Viterbi algorithm.
  # Adapted from "Applied Statistics for Bioinformatics using R" by Wim 
  # P. Krijnen, page 209
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
myseq <- c("A", "A", "G", "C", "G", "T", "G", "G", "G", "G", "C", 
           "C", "C", "C", "G", "G", "C", "G", "A", "C", "A", 
           "T", "G", "G", "G", "G", "T", "G", "T", "C")
viterbi(myseq, thetransitionmatrix, theemissionmatrix)
