cr <- corr(".", 2000)
n <- length(cr)
cr <- corr(".", 1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))
