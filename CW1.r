# Coursework_1
# Team member:
## Qingqing Fan s2493867
## Xuan Jin s2477282
## Zifan Huang s2550946

# Contributions:
## Firstly, we did our project-1 collaboratively by using git and github to pull and push our codes so that we could have a clear division of work.
## We would think we have the same proportion of work for each member since we came up with the solution idea together. The comments on codes were written by someone who did that question.
## Qingqing Fan wrote the code for Q6&Q9, helped Jin Xuan modify Q4&Q5, and completed Q8 and Q10 together with my teammates.
## Xuan Jin did most parts of questions Q4&Q5&Q8, and Qingqing discussed with me to figure out ideas about the task.
## Zifan Huang completed the Q7, and participated in solving Q10 which is a team work to figure out the question after discussion, revision, and so on.


a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE)  ## Remove "_("

# Q4
# Firstly, we need to write a function called "split_punct", which has a variable word_vector as vector input and another variable punc as a punctuation that we want to split
split_punct <- function(word_vector , punc){
  ip <- grep(punc,word_vector,fixed=TRUE)   # Find the index of words with a punctuation
  n.p <- length(ip)  # This is the number of words containing a punctuation
  word_vectors <- rep("",length(word_vector)+n.p) # Create an empty vector for storing the result, so the length should be the total length of words and punctuation
  ips <- ip+1:n.p   # This is the location in the new word_vectors which should be punctuation
  word_vectors[ips] <- punc  # Use index to fill this new vector with punctuation
  
  # In terms of "." is ambiguous when using function gsub, so there is a if-else statement
  if(punc == "."){
    word_vector <- gsub("\\.","",word_vector)  # Replace every punctuation in words with empty ones
  } else {
    word_vector <- gsub(punc,"",word_vector)}
  
  word_vectors[-ips]<- word_vector # The complementary set of word_vectors should be filled with word after substituting punctuation
  return(word_vectors) # Return the result
}

# Q5
# Split each required punctuation by using the function written in Q4
a <- split_punct(a,",")
a <- split_punct(a,".")
a <- split_punct(a,";")
a <- split_punct(a,"!")
a <- split_punct(a,":")
a <- split_punct(a,"?")

## Q6
# Then we need to pick out about 1000 common words(lower case). 
a1 <- tolower(a) # Replace the capital letters in words with lower case letters
u <- unique(a1) # Find the vector of unique words in the Ulysses text
a_u <- match(a1,u) # Find the index of corresponding word in unique words
t <- tabulate(a_u) # Count up how many time each unique word occurs in the text
tn <- 2 # Set an initial threshold number
while(abs(sum(t>tn)-1000)>abs(sum(t>tn+1)-1000)){tn <- tn+1} # use while loop to find the suitable threshold number
# tn = 26
# m=995
b <- u[t>tn] # Create b which indicate the m most commonly occurring words


## Q7
# For Question 7, we need to create triplets and pairs with the common words
# create a vector which our most common words corresponds to the full text. NA in vector t1 means that the word in full text does not exist in our most common words;
t1 <- match(a1,b) 
# Since we want to explore the adjacent words in the text, we move forward a unit by omitting the last element and appending a NA before the first element;
t2 <- append(head(t1,-1),NA,0) 
# This step is as the same as the last step, one more unit forward;
t3 <- append(head(t2,-1),NA,0)
# Then, using cbind, we combine these first two columns, and get D1 which for the pairs, and combine all three columns for the triplets;
D1 <- cbind(t1,t2)
T1 <- cbind(t1,t2,t3)
# Finally, extracts all the rows with no NA, which are the pairs/triplets of common words. And P/T is the two/three column common word pairs/triplets matrix.
P <- D1[which(is.na(rowSums(D1))==FALSE),]
T <- T1[which(is.na(rowSums(T1))==FALSE),]

## Q8
w1 = sample(T[,1],1) # Select one word from the first column in T randomly as the first word
w2 = sample(P[which(P[,1]== w1),2],1) # Similarly, select the second word from the the second column in P, given the first word is w1

words = c(w1,w2) # Combine two randomly selected words in a vector
bt <- t[t>tn] # The number of times each word in b appears in the text
prob_b <- bt/sum(bt) # Calculate the common word frequencies

# Write a function that can generate the third word if the fist two words are known
# Therefore, the input should be two words
simulate_1_word = function(w_i,w_j){
  sub_t1 = T[ which(T[,1]== w_i), ]  
  sub_T = sub_t1[which(sub_t1[,2]==w_j), ] # Based on two words w_i and w_j are given, we can find the sub-matrix sub_T as its first and second columns are w_i and w_j
  sub_P = P[which(P[,1]== w_j),] # Only know the second word w_j also can extract a sub-matrix sub_P its first column are w_j
  
  # There is a case that the sub-matrix could be one row with two columns, thus we need a if-else statement to get possible word
  ## sub-matrix sub_P case
  if(length(sub_P)==2){
    possible_word_P = sub_P[2]
  }
  else{
    possible_word_P = sub_P[,2]
  }
  ## sub-matrix sub_T case
  if(length(sub_T)==3){
    possible_word_T = sub_T[3]
  }
  else{
    possible_word_T = sub_T[,3]
  }
  
  # Another case need to be considered is whether these two sub-matrix are empty
  if(length(sub_T)==0 && length(sub_P)==0){
    ## If they are both empty, we just randomly choose a word based on frequency
    word = sample(c(1:length(b)),1,replace = FALSE, prob = prob_b)
    
  }else if(length(sub_T)==0 && length(sub_P)!=0 ){
    ## If sub-matrix T has no rows, we can use pairs sub-matrix to simulate the next word
    word = sample(possible_word_P,1)
  }
  else{
    ## Generally, the triple sub-matrix T has rows for simulating the next word
    word = sample(possible_word_T,1)
  }
  return(word)
}

# Simulating this process by using a for-loop combined with the function we wrote above, the returned word will be appended to our words vector in each time
for ( i in 1:48) {
  words = append(words , simulate_1_word(words[i],words[i+1]))
}
# Using cat function to connect all words into a sentence
words2 <- cat(b[words])


## Q9
words3 <- sample(b,50,replace = FALSE, prob = prob_b) # based on the common word frequencies, simulate 50 words by using sample function
words4 <- cat(words3) # print out the text


## Q10
# This question is suppose to considerate the capital words. By modifying the version of b, which add the words most often start  with a capital letter.
# Find the words start with a capital letter by matching a and a1;
B <- match(a,a1)
# Get the position of these words;
bb <- which(is.na(B)==TRUE)
# Output these words as BB
BB <- a[bb]
# The following steps are the same as previous, find the words most often start with a capital letter.
uu <- unique(BB) # Find the vector of unique words in the Ulysses text
a_uu <- match(BB,uu) # Find the index of corresponding word in unique words
tt <- tabulate(a_uu) # Count up how many time each unique word occurs in the text
bbb <- uu[tt>tn] # Create b which indicate the m most commonly occurring words
# recalculate the common word frequencies
btt <- tt[tt>tn]  # The number of times each common capital word appears in the text
# the total number of uppercase and lowercase common words minus the number of uppercase words
for (i in 1:length(bbb)) {
  index = which(b==tolower(bbb[i]))
  bt[index] = bt[index]-btt[i]
}
# The number of times all common words(including lower words and capital words) appear in the original text
bttt <- append(bt,btt)
prob_b <- bttt/sum(bttt) # calculate its frequencies

# This Capital_letter function is used to apply the list contains the words start with a capital letter in our simulation. Where parameter 'a' is our main text, parameter 'b' is our new version of b;
Capital_letter <-function(a,b){#create a vector which our most common words corresponds to the full text. NA in vector t1 means that the word in full text does not exist in our most common words;
  t1 <- match(a,b) 
  # Since we want to explore the adjacent words in the text, we move forward a unit by omitting the last element and appending a NA before the first element;
  t2 <- append(head(t1,-1),NA,0) 
  # This step is as the same as the last step, one more unit forward;
  t3 <- append(head(t2,-1),NA,0)
  # Then, using cbind, we combine these first two columns, and get D1 which for the pairs, and combine all three columns for the triplets;
  D1 <- cbind(t1,t2)
  T1 <- cbind(t1,t2,t3)
  # Finally, extracts all the rows with no NA, which are the pairs/triplets of common words. And P/T is the two/three column common word pairs/triplets matrix.
  P <- D1[which(is.na(rowSums(D1))==FALSE),]
  T <- T1[which(is.na(rowSums(T1))==FALSE),]
  
  w1 = sample(T[,1],1)
  w2 = sample(P[which(P[,1]== w1),2],1)
  words = c(w1,w2)
  
  simulate_1_word = function(w_i,w_j){
    sub_t1 = T[ which(T[,1]== w_i), ]
    sub_T = sub_t1[which(sub_t1[,2]==w_j), ]
    sub_P = P[which(P[,1]== w_j),]
    
    if(length(sub_P)==2){
      possible_word_P = sub_P[2]
    }
    else{
      possible_word_P = sub_P[,2]
    }
    
    if(length(sub_T)==3){
      possible_word_T = sub_T[3]
    }
    else{
      possible_word_T = sub_T[,3]
    }
    
    if(length(sub_T)==0 && length(sub_P)==0){
      word = sample(c(1:length(b)),1,replace = FALSE, prob = prob_b)
      
    }else if(length(sub_T)==0 && length(sub_P)!=0 ){
      word = sample(possible_word_P,1)
    }
    else{
      word = sample(possible_word_T,1)
    }
    return(word)
  }
  
  for ( i in 1:48) {
    words = append(words , simulate_1_word(words[i],words[i+1]))
  }
  
  words2 <- cat(b[words])}

Capital_letter(a,append(b,bbb))
