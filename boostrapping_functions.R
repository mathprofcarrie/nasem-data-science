## Statistical test for comparing two data sets, see manual at
## http://users.ics.aalto.fi/lijffijt/bootstraptest
## This code is free to use for academic purposes. If you use this test, please
## cite the following paper: Jefrey Lijffijt, Terttu Nevalainen, Tanja SÃ¤ily,
## Panagiotis Papapetrou, Kai PuolamÃ¤ki, Heikki Mannila. /Forthcoming/.
## "Significance testing of word frequencies in corpora".

# Step function
H <- function(value1, value2) {
  x = 0;
  if(value1 > value2) {
    x = 1;
  }
  if(value1 == value2) {
    x = 0.5;
  }
  x
}

# Bootstrap test
bootstraptest <- function(data1, data2, N) {
  # Compute number of elements in the shortest vector
  n <- min(length(data1),length(data2));
  
  # Compute (unnormalized) p1
  p1 = 0;
  for (i in 1:N) {
    p1 = p1 + H(mean(sample(data1,n,replace=TRUE)),mean(sample(data2,n,replace=TRUE)));
  }
  p1 = p1/N;
  
  # Compute p2
  p2 = (1+2*N*min(p1,1-p1))/(1+N);
  
  # Return p2
  p2
}

bootstraptest(interim_100_word_freqs, final_100_word_freqs, 10000)
