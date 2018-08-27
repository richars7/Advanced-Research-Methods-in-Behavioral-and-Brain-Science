###HW2 -- Create a contingency table and compute a Chi2 test
##The objectives of this homework are:
	## 1: use functions to create a matrix.
	## 2: use functions to name items in a matrix.
	## 3: compute degrees of freedom from the table -- with functions designed for matrices
	## 4: compute the p-value
	## 5: print to screen an APA-like statement
		##be very brief.


#*** 1: Create a contingency table
	##use the contingency table from Wikipedia: https://en.wikipedia.org/wiki/Chi-squared_test
	##the table should be stored in a variable called 'chi2.data'

 chi2.data <- rbind(cbind(90,60,104,95),
                   cbind(30,50,51,20),
                   cbind(30,40,45,35)
                   ) 
#*** 2: Use functions to name the rows and columns of the contingency table.
 rownames(chi2.data) <- c('White Collar','Blue Collar','No Collar')
 colnames(chi2.data) <- c('A','B','C','D')

#####Do not touch anything in this block#####
 row.sums <- rowSums(chi2.data)
 column.sums <- colSums(chi2.data)
 grand.sum <- sum(chi2.data)
	
 row.frequencies <- row.sums/grand.sum
 column.frequencies <- column.sums/grand.sum
	
 observed.table <- chi2.data
 expected.table <- (row.frequencies %o% column.frequencies) * grand.sum
 observed.table.flattened <- c(observed.table)
 expected.table.flattened <- c(expected.table)
	
 chi2.vector <- (observed.table.flattened - expected.table.flattened)^2 / expected.table.flattened
 chi2 <- sum(chi2.vector)
#####Do not touch anything in this block#####

#*** 3: With functions designed specifically to tell you about matrices, compute the degrees of freedom for the 'chi2.data' table.
	###save the degrees of freedom in a variable called 'df'
 chisq <- chisq.test(chi2.data)
 df <- chisq.test(chi2.data)$parameter

#*** 4: Compute the p-value for the results. To note: you're going to have to do a bit of digging via R help (i.e., '?' or '??') and perhaps even Google. You need to find a function that deals with probability distributions.
	###save the p-value in a variable called 'chi.p'

 #chi.p <- pchisq(24.57, 6, lower.tail=FALSE)
 chi.p <- chisq.test(chi2.data)$p.value 

#*** 5: With the all or some of the following functions, print a brief APA statement to the screen. You have a decimial limit of 6 decimal places.
	### c(), paste(), round() -- you may need to use all or some of these zero, one, or multiple times.
	### print()
 print(paste("A chi-square test of independence was performed to examine the relation between neighbourhood of residence and personal occupational classification.
        The relation between these variables was significant, X2 (",df ,") = ",round(chi.p,digits = 6),", p <.01."))
 

