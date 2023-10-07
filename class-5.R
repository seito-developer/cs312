library(swirl)
uninstall_all_courses()
install_course_zip("cs112-swirl-courses-master.zip")
swirl()
library(arm)
library(Matching)

data(lalonde)

lm1 <- lm(re78 ~ age, data=lalonde)


lm1 <- lm(lalonde$re78 ~ lalonde$age + lalonde$educ + I(lalonde$age*lalonde$educ), data = lalonde)


info(lm1)
lm1$coefficients
I(lalonde$age + lalonde$educ)


sim_results <- sim(lm1, n.sims = 5000)
sim_results@coef
slotNames(sim_results)
