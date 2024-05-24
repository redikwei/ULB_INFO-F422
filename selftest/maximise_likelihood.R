#  find value u to maximises likelihood of the dataset

## uniform distribute U(-2,u)
## 10 observations
## 10 given u

# Define the dataset
observations <- c(1.3, -0.3, 1.0, -1.3, 0.4, -1.5, -0.9, -0.3, 0.1, 2.4)

# Define possible values of u
u_values <- c(-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)

# Initialize a vector to store likelihoods
likelihoods <- numeric(length(u_values))

# Calculate likelihood for each u
for (i in seq_along(u_values)) {
  u <- u_values[i]
  # Check if all observations are within the range [-2, u]
  if (all(observations >= -2 & observations <= u)) {
    # Calculate likelihood: proportional to 1/(u+2)^N
    likelihoods[i] <- 1 / (u + 2)^length(observations)
  } else {
    # If any observation is outside [-2, u], likelihood is 0
    likelihoods[i] <- 0
  }
}

# Find the u with the maximum likelihood
best_u <- u_values[which.max(likelihoods)]

# Print the result
print(best_u)
