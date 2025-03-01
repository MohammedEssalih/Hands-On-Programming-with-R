# Define the die and calculate all possible rolls
die <- 1:6
rolls <- expand.grid(Var1 = die, Var2 = die)
rolls$value <- rolls$Var1 + rolls$Var2

# Define fair probabilities for each die face
prob <- rep(1/6, 6)
names(prob) <- as.character(die)

# Assign probabilities to each roll
rolls$prob1 <- prob[as.character(rolls$Var1)]
rolls$prob2 <- prob[as.character(rolls$Var2)]
rolls$prob <- rolls$prob1 * rolls$prob2

# Expected value of the sum of two dice
expected_value <- sum(rolls$prob * rolls$value)
print(expected_value)

# Define the slot machine symbols and their probabilities
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
prob_wheel <- c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
names(prob_wheel) <- wheel

# Generate all possible slot machine outcomes
combos <- expand.grid(Var1 = wheel, Var2 = wheel, Var3 = wheel, stringsAsFactors = FALSE)

# Assign probabilities to each outcome
combos$prob1 <- prob_wheel[combos$Var1]
combos$prob2 <- prob_wheel[combos$Var2]
combos$prob3 <- prob_wheel[combos$Var3]
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

# Verify that the total probability sums to 1
print(sum(combos$prob))

# Function to randomly sample slot machine symbols
get_symbols <- function() {
  sample(wheel, size = 3, replace = TRUE, prob = prob_wheel)
}

# Example usage
symbols <- get_symbols()
print(symbols)

# Compute prizes for each combination
combos$prize <- sapply(1:nrow(combos), function(i) {
  score(c(combos$Var1[i], combos$Var2[i], combos$Var3[i]))
})

# Expected payout from the slot machine
expected_payout <- sum(combos$prob * combos$prize)
print(expected_payout)
