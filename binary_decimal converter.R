library(stringr)

# Binary to dual number converter 
# Steps: 
  # Ask user to input number
  # Make sure it is only 0's and 1's
  # Convert
  # Spit decimal out (output)



# INPUT
binary_number <- as.character(readline(prompt="Enter binary number: "))

# Make a vector of  1's and 0's 
# Remove NA due to decimals
bits <- as.numeric(unlist(strsplit(binary_number, "")))
bits_clean <-  bits[!is.na(bits)]



# POSITION NUMBERS

# Lil function to split a vector based on na's
# Romain Francois
split_na <- function(x){
  idx <- 1 + cumsum(is.na(x))
  not.na <- ! is.na(x)
  split(x[not.na], idx[not.na])
}
# Apply function and split bits into two vectors at the decimal
split_bits <- split_na(bits)

# Assign the new bits' vectors as positive or negative
pos_bits <- split_bits$'1'
neg_bits <- split_bits$'2'

# Make a vector of exponents (positions)
pos_positions <- (length(pos_bits)-1):0
neg_positions <- (-1 :-length(neg_bits))

# Put them back together
positions <- c(pos_positions, neg_positions)
  


# SUM FUNCTION

decimal_number <- vector()
  
# This boy is the kicker
for (i in 1:length(positions)){
  decimal_number[i] <- as.numeric(bits_clean[i]*2^(positions[i]))
  }

# Sum numbers in the vector for the big finale!
decimal_number <- sum(decimal_number)



# OUTPUT

# Give user their answer 
cat("The binary number", binary_number,"is",decimal_number ,"in decimal. :-) Pretty neat, huh?")



