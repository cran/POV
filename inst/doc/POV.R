## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(POV)

## -----------------------------------------------------------------------------
hist(dt2$Response, main = "")

## -----------------------------------------------------------------------------
sd(dt2$Response)
var(dt2$Response)

## -----------------------------------------------------------------------------
plot(factor(dt2$Group),dt2$Response, xlab="Group", ylab="Response")

## -----------------------------------------------------------------------------
hist(dt$Response, main = "")
plot(factor(dt$Machine),dt$Response, xlab="Machine", ylab="Response")
plot(factor(dt$Metrology),dt$Response, xlab="Metrology", ylab="Response")

## -----------------------------------------------------------------------------
anova(lm(dt$Response ~ dt$Machine * dt$Metrology))

## -----------------------------------------------------------------------------
VarTable

## -----------------------------------------------------------------------------
anova(lm(VarTable$popVar ~ VarTable$Machine * VarTable$Metrology))

## -----------------------------------------------------------------------------
POV(Response ~ Machine * Metrology, dt, Complete = TRUE)

