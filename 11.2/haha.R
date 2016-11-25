# Install the "arm" package and load the library
install.packages("arm")
library(arm)


# Load data given in pre-class work: 
# https://drive.google.com/file/d/0B13Y-zv3esJHQ0N2LU9aNm91dmc/view


# RUN THE REGRESSION
lm.4 <- lm (post.test ~ treatment + pre.test + I(treatment*pre.test))
display (lm.4)


# APPLY THE "SIM" FUNCTION TO SIMULATE THE UNCERTAINTY
lm.4.sim <- sim (lm.4)


coef(lm.4.sim)  # explore the simulated coefficients 
# all equally likely under this model


# QUESTION: Which columns are the key coefficients--
#        the coefficients for the treatment indicator and interaction term?
#        Confirm that they are the SECOND and FOURTH columns...
#        According to this model...


#        treatment effect = coef(treatment) + coef(interaction.term)*pre.test


#        NOTICE!!! In a figure that has pre-treatment on the 
#        x-axis and treatment effect on the y-axis, coef(treatment)
#	   is an intercept and coef(interaction.term) a slope**


# REPRODUCE THE first line in the PLOT we saw earlier
plot (0, 0, xlim=c(80, 120), ylim=c(-5,10),
      xlab="pre-test", ylab="treatment effect", 
      main="treatment effect in grade 4")


abline (h = 0, lwd=.5, lty=2) # draws a horizontal line


# "abline" draws a line with intercept "a" and slope "b". 
for (i in 1:nrow(coef(lm.4.sim))) {
    abline (a = coef(lm.4.sim)[i,2], b = coef(lm.4.sim)[i,4], 
            lwd = .5, col = "gray")
}

abline (a = lm.4$coefficients[2], b = lm.4$coefficients[4], 
        lwd = 1, col = "black")


## Additional OPTIONAL exercise…
#(1) Recover the average treatment effect previously obtained by regression by simulating and averaging over the simulated coefficients.]
#(2) Estimate standard error of 'treatment' coefficient by calculating the standard deviation of simulated "treatment" coefficients.  Use confint(lm.4.sim) to get confidence intervals, and then confirm the confidence intervals by simulation (using “quantile” function)...