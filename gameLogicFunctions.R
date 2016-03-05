#### Function definitions for gameLogic.R

# Calculate everyone's payoffs 
calculatePayoffs = function(contrib, adjmat) {
    # Get the players degrees
    degrees = colSums(adjmat)
    
    # Initialize returns vector
    returns = numeric(kN)
    
    # Loop over each player
    for(i in seq_along(contrib)) {
        # Find the position of its immediate neighbors
        neighbors = which(as.logical(adjmat[i, ]))
        # If i isn't an isolate:
        if(length(neighbors) > 0) {
            # Loop over the neighbors (j), calculate the payoff to i, and sum
            returns[i] = sum(sapply(neighbors, function(j) payoff(i, j, contrib, degrees)))
        } else {  # If i is an isolate, get back what they put in:
            returns[i] = contrib[i]
        }
    }
    returns
}

# Calculate payoff to i from j
payoff = function(i, j, contrib, degrees, r = kr) {
    # Each edge in the network forms a pool
    # Each player's contribution is split evenly among their edges
    (contrib[i] / degrees[i] + contrib[j] / degrees[j]) *
        # The pool from both players is linearly augmented
        (1 + r) *  
        # And the payout to each is proportional to the number of pools they're in
        (degrees[j] / (degrees[i] + degrees[j]))  
}

# Coloring function for plots
color = function(vec, n = 256) {
    # vec is numeric vector to make colors on; n is how many breaks in palette
    viridis(n)[as.numeric(cut(vec, breaks = n))]  # Works for all color blindedness, but is less intuitive
    # pal = colorRampPalette(c('darkred', 'darkgreen'))
    # pal(n)[as.numeric(cut(vec, breaks = n))]
}

# Plotting functions, one for round one; one for thereafter
plotInit = function(mat = adjmat) {
    plot(network(mat, directed = FALSE), displaylabels = TRUE, 
         vertex.cex = 7, vertex.col = 'gray', label.pos = 99, pad = .5)    
}

plotNet = function(mat = adjmat, .wealth = wealth, .netPay = netPay) {
    plot(network(mat, directed = FALSE), displaylabels = TRUE, pad = .5, label.pos = 99, 
         vertex.cex = 1.5 * .wealth / sd(.wealth), vertex.col = color(.netPay))
    legend('bottomleft', bty = 'n', title.adj = 0,
           legend = round(c(max(.netPay), 0, min(.netPay)), 1), 
           fill = color(c(max(.netPay), 0, min(.netPay))), 
           title = "Last round's\nnet payouts")
}
plotNet()