#Plot the data, what does it tell us about impact on regression?

par(mfrow=c(1,1))

plot(beef$SIZE, beef$YES, xlab="Size of farm (hundred acres)", ylab="Percent Yes Votes",
    main = "Farm Size on % Yes Vote",
    pch = 18)
plot(beef$VAL, beef$YES, xlab="Farm Sales ($ thousands)", ylab="Percent Yes Votes", 
    main = "Farm Sales on % Yes Vote",
    pch = 18)
plot(beef$SIZE, beef$VAL,
    xlab = "Size of farm (hundred acres)",ylab = "Farm Sales ($ thousands)", 
    main = "Farm Size on Sales", 
    pch = 18
)

reg1 <- lm(beef$VAL ~ beef$SIZE)
summary(reg1)
plot(
  reg1
)


#Fit regression with both size and val as covariates, what regression assumptions have we violated?)
vote <- lm(beef$YES ~ beef$SIZE + beef$VAL)
logvote <- lm(beef$YES~ beef$SIZE + log(beef$VAL))
voteint <- lm(beef$YES ~ beef$SIZE*beef$VAL)
votelogint <- lm(beef$YES ~ beef$SIZE*log(beef$VAL))

simple <- summary(vote)
logsimple <- summary(logvote)
better <- summary(voteint)
logbetter <- summary(votelogint)

# abline(logvote, col=c("red"))
# summary(logvote)
# anova(logvote)

par(mfrow = c(3,2))
plot(beef$YES,vote$fitted)
abline(0,1,lty=2, col=8)
plot(vote$fitted, rstudent(vote))
plot(logvote$fitted, beef$YES)
abline(0,1,lty=2, col=8)
plot(logvote$fitted, rstudent(logvote))

plot(voteint$fitted, beef$YES)
abline(0,1,lty=2, col=8)
plot(voteint$fitted, rstudent(voteint))

plot(beef$YES, votelogint$fitted)
abline(0,1,lty=2, col=8)
plot(votelogint$fitted, rstudent(votelogint))


#Find a better model, does effect of size change depending on log(val)? What is your estimate of unit change in Size on effect on YES? interpret 

logbetter

beef$logval <- log(beef$VAL)

null <- lm(beef$YES ~ beef$Size,data = beef)
full<- lm(beef$YES ~ . + .^2, data = beef)
fwd <- step(null, scope=formula(full), direction="forward", k=log(56))

## BIC
print(BIC <- c(reg1=extractAIC(vote, k=log(56))[2],
               reg2=extractAIC(logvote, k=log(56))[2],
               reg3=extractAIC(voteint, k=log(56))[2],
               reg4=extractAIC(votelogint, k=log(56))[2]))

## slide 44
## Model probabilities
print(eBIC <- exp(-.5*(BIC-min(BIC))))
round(probs <- eBIC/sum(eBIC), 2)
