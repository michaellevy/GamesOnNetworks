library(network)
library(viridis)

# Initialization
###############################################

# Load functions
source('gameLogicFunctions.R')

#### Constants
kN = 8  # number players
kr = 0  # interest rate

# Define empty adjacency matrix 
adjmat = matrix(rep(0, kN^2), nrow = kN)

# Empty accumulated wealth vector for each player
wealth = numeric(kN)

# Allocation to each player for each round
alloc = rep(10, kN)

# Plot the empty network
plotInit()


# Each Round
###############################################

#### Get created and destroyed ties from users
# Ties are undirected and can be toggled by either player involved,
# ie, if i or j clicks to create or destroy the ij tie, it gets toggled

# E.g. Player 1 links to players 2 and 3, and 3 to 2 and 4
adjmat[1, 2:3] = 1
adjmat[3, c(2, 4)] = 1

# Since the network is undirected, symmetrize it:
adjmat = adjmat + t(adjmat)

# Update everyone's viz
if(sum(wealth == 0))  # If it's the first round
    plotInit() else
        plotNet()


#### Do CPR game

# Get each players contribution, e.g.
contrib = c(5, 5, 0, 10, 0, 0, 0, 0)

# test no one's contrib is greater than their alloc
if(any(contrib > alloc))
    stop('player ', which(contrib > alloc), ' contribued more than they were allotted.')

# Calculate players' returns from pools
returns = calculatePayoffs(contrib, adjmat)

# And their net
netPay = returns - contrib
    
# Update each player's wealth
wealth = wealth + alloc + netPay

# Update viz
plotNet()
