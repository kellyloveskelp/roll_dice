set.seed(123)

# Initallize reef
# reef <- matrix(0, nrow = 5, ncol = 5)

# What we did yesterday
# random_coral <- sample(1:25, size = 8)
# for (rc in random_coral){
#   rc_row <- (rc -1) %/% 5 + 1
#   rc_col <- (rc -1) %% 5 + 1
#   reef[rc_row, rc_col] <- 1
# }
# print(reef)

# Write a function to encapsulate the logic for putting 8 random corals on the initialized reef
# Rather than hard-coding the number at 8, make the number of corals an input

coral_samp <- function(size) {
  reef <- matrix(0, nrow = 5, ncol = 5)
  random_coral <- sample(1:25, size)
  for (rc in random_coral){
    rc_row <- (rc -1) %/% 5 + 1
    rc_col <- (rc -1) %% 5 + 1
    reef[rc_row, rc_col] <- 1
  }
  return(reef)
}
reef <- coral_samp(8)
reef

# If I wanted this to keep compounding on itself,
# set prev_reef as a function parameter

# coral_samp <- function(size, prev_reef) {
#   random_coral <- sample(1:25, size)
#   for (rc in random_coral){
#     rc_row <- (rc -1) %/% 5 + 1
#     rc_col <- (rc -1) %% 5 + 1
#     prev_reef[rc_row, rc_col] <- 1
#   }
#   return(prev_reef)
# }
# reef <- coral_samp(8, prev_reef = reef)
# reef

# Create a function to apply the coral growth/mortality dynamics through every year
coral_evolution <- function(generations) {
  for (t in 2: generations) {
    prev_reef <- reef
    next_reef <- matrix(0, nrow = 5, ncol = 5)
  }
  # Loop over the rows and columns. Call your iterators r and c.
  for (r in 1:5) {
    for (c in 1:5) {
        # Roll 2d6 for this coral
        coral_outcome <- sum(sample(1:6, size = 2, replace = TRUE))
        # Fill in the conditions below for mortality and survival
        # Update the reef accordingly
        if (coral_outcome <= 5) {
          # Mortality
          next_reef[r, c] <- 0
        } else {
          # Survival
          next_reef[r, c] <- 1
        }
      }
    }
  print("1st Generation")
  return(next_reef)
}
coral_evolution(generations = 11)
print("nth Generation")
