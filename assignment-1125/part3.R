# Part 3
install.packages("Synth")
library(Synth)
data(basque)

## Select Cantabria as a treatment unit
treatment_unit <- 6

## Try Synthetic Control Method
synth.out <- synth(dataprep(foo = basque, 
                            predictors = c("school.illit", "school.prim", "school.med", 
                                           "school.high", "school.post.high", "invest", 
                                           "gdpcap"),
                            dependent = "gdpcap",
                            unit.variable = "regionno",
                            time.variable = "year",
                            treatment.identifier = treatment_unit,
                            controls.identifier = c(2:5, 7:16, 18),  # remobe unit 1 and 17
                            time.predictors.prior = c(1964:1975),
                            time.optimize.ssr = c(1964:1975),
                            unit.names.variable = "regionname",
                            time.plot = 1955:1997))


# パスプロットの生成
path.plot(synth.res = synth.out, dataprep.res = dataprep.out, Ylab = "GDP per Capita")
title("Path Plot of GDP per Capita for Treated Unit vs. Synthetic Control", cex.main = 0.8)

# ギャッププロットの生成
par(mar = c(5.1, 4.1, 6, 2.1))
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out, Ylab = "GDP per Capita Gap",
          Main = "") +
title("Gap Plot of GDP per Capita between Treated Unit and Synthetic Control", cex.main = 0.8)

