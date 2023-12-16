library(Synth)
df <- read.csv("https://tinyurl.com/y3qtakq6")
head(df)

###

# This may take 3-4 mins to run on the platform!

df$state <- as.character(df$state) # required by dataprep()

dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:2009, "mean"),
             list("proportion.extreme.poverty",
                  1990:2009, "mean"),
             list("gini.imp", 1990:2009, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(11:17, 21:27, 31:33,
                                     41:43, 50:53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo"),
          Legend.position = c("bottomleft")
)


##########################


## To obtain gaps
gaps <- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps


sum(gaps[9:20])

gaps[9:20]
