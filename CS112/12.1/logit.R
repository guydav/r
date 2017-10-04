library(Matching)
data(lalonde)

model <- glm(treat~age + I(age^2) + educ + I(educ^2) + black +
                 hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
                 u74 + u75, data=lalonde, family='binomial')

summary(model)
predict(model, lalonde, type = 'response')