library("Synth")
library("ggplot2")
data("basque")
basque.prepped <- dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med", 
                   "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1964:1969,
    special.predictors = list(
        list("gdpcap", 1960:1969, "mean"),
        list("sec.agriculture", seq(1961, 1969, 2), "mean"),
        list("sec.industry", seq(1961, 1969, 2), "mean"),
        list("sec.construction", seq(1961, 1969, 2), "mean"),
        list("sec.services.venta", seq(1961, 1969, 2), "mean"),
        list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
        list("popdens", 1969, "mean")),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = 17,
    controls.identifier = c(2:16, 18),
    time.optimize.ssr = 1960:1969,
    time.plot = 1955:1997)

basque.synth.out <- synth(data.prep.obj = basque.prepped, method = "BFGS")
gaps <- basque.prepped$Y1plot - (basque.prepped$Y0plot %*% basque.synth.out$solution.w)
gaps[1:3, 1]

basque.synth.tables <- synth.tab(dataprep.res = basque.prepped, synth.res = basque.synth.out)
names(basque.synth.tables)

path.plot(synth.res = basque.synth.out, dataprep.res = basque.prepped,
        Ylab = "real per-capita GDP (1986 USD, thousand)", Xlab = "year",
        Ylim = c(0, 12), 
        Legend = c("Basque country","synthetic Basque country"), 
        Legend.position = "bottomright")
