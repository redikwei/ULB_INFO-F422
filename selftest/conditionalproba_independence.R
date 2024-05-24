# conditional probability / check independence

## yes / no

# Load the required data
load("D:\\ULB\\9-ULBstudy\\bloc1_sem2\\info_f422_statistical foundation for machine learning\\exam\\selftest\\EXAM_2021_1s.Rdata")

# Display the data to understand its structure
print(Q1.G1.D)

# Computing the required conditional probabilities

# P(y = no | x1 = yes, x2 = no, x3 = no)
numerator1 <- Q1.G1.D$prob[Q1.G1.D$y == "no" & Q1.G1.D$x1 == "yes" & Q1.G1.D$x2 == "no" & Q1.G1.D$x3 == "no"]
denominator1 <- sum(Q1.G1.D$prob[Q1.G1.D$x1 == "yes" & Q1.G1.D$x2 == "no" & Q1.G1.D$x3 == "no"])
p_y_no_given_x1_yes_x2_no_x3_no <- numerator1 / denominator1

# P(x3 = no | x1 = yes, y = no)
numerator2 <- Q1.G1.D$prob[Q1.G1.D$x3 == "no" & Q1.G1.D$x1 == "yes" & Q1.G1.D$y == "no"]
denominator2 <- sum(Q1.G1.D$prob[Q1.G1.D$x1 == "yes" & Q1.G1.D$y == "no"])
p_x3_no_given_x1_yes_y_no <- numerator2 / denominator2

# P(x2 = no | x1 = yes, y = no)
numerator3 <- Q1.G1.D$prob[Q1.G1.D$x2 == "no" & Q1.G1.D$x1 == "yes" & Q1.G1.D$y == "no"]
denominator3 <- sum(Q1.G1.D$prob[Q1.G1.D$x1 == "yes" & Q1.G1.D$y == "no"])
p_x2_no_given_x1_yes_y_no <- numerator3 / denominator3

# P(x1 = no | x2 = no, x3 = no)
numerator4 <- Q1.G1.D$prob[Q1.G1.D$x1 == "no" & Q1.G1.D$x2 == "no" & Q1.G1.D$x3 == "no"]
denominator4 <- sum(Q1.G1.D$prob[Q1.G1.D$x2 == "no" & Q1.G1.D$x3 == "no"])
p_x1_no_given_x2_no_x3_no <- numerator4 / denominator4

# Print the computed probabilities
cat("P(y = no | x1 = yes, x2 = no, x3 = no) =", p_y_no_given_x1_yes_x2_no_x3_no, "\n")
cat("P(x3 = no | x1 = yes, y = no) =", p_x3_no_given_x1_yes_y_no, "\n")
cat("P(x2 = no | x1 = yes, y = no) =", p_x2_no_given_x1_yes_y_no, "\n")
cat("P(x1 = no | x2 = no, x3 = no) =", p_x1_no_given_x2_no_x3_no, "\n")

# Checking independence of y and x1
p_y_x1_yes <- sum(Q1.G1.D$prob[Q1.G1.D$y == "yes" & Q1.G1.D$x1 == "yes"])
p_y_yes <- sum(Q1.G1.D$prob[Q1.G1.D$y == "yes"])
p_x1_yes <- sum(Q1.G1.D$prob[Q1.G1.D$x1 == "yes"])
independent_y_x1 <- (p_y_x1_yes == p_y_yes * p_x1_yes)

cat("Are the two variables y and x1 independent?", ifelse(independent_y_x1, "Yes", "No"), "\n")

# Checking conditional independence of x3 and x1 given y = no
p_x3_yes_x1_yes_given_y_no <- sum(Q1.G1.D$prob[Q1.G1.D$y == "no" & Q1.G1.D$x3 == "yes" & Q1.G1.D$x1 == "yes"])
p_x3_yes_given_y_no <- sum(Q1.G1.D$prob[Q1.G1.D$y == "no" & Q1.G1.D$x3 == "yes"])
p_x1_yes_given_y_no <- sum(Q1.G1.D$prob[Q1.G1.D$y == "no" & Q1.G1.D$x1 == "yes"])
conditionally_independent_x3_x1_given_y_no <- (p_x3_yes_x1_yes_given_y_no == p_x3_yes_given_y_no * p_x1_yes_given_y_no)

cat("Are the two variables x3 and x1 conditionally independent given y=no?", ifelse(conditionally_independent_x3_x1_given_y_no, "Yes", "No"), "\n")
