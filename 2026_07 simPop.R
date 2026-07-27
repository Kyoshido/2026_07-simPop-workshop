library(simPop)
SEED <- 27072026
set.seed(SEED)

data(eusilc13puf, package = "simPop")
# ??eusilc13puf

df <- eusilc13puf[,c(1:6, 8:9,14, 16, 46)]
df$age <- as.numeric(df$age)
df$pid <- as.factor(df$pid)

vars <- c("hhid", "hsize", "region", "age", "sex", "pid", "eco_stat", 
          "citizenship", "pgrossIncome","hgrossIncome","weight")

colnames(df) <- vars

df$eco_stat <- as.character(df$eco_stat)
df$eco_stat[is.na(df$eco_stat)] <- "Unknown"
df$eco_stat <- factor(df$eco_stat)
df$citizenship <- as.character(df$citizenship)
df$citizenship[is.na(df$citizenship)] <- "Unknown"
df$citizenship <- factor(df$citizenship)
df$pgrossIncome[is.na(df$pgrossIncome)] <- 0

df$weight <- df$weight / 100

head(df, 10)

inp <-
  specifyInput(
    data = df,
    hhid = "hhid",
    hhsize = "hsize",
    strata = "region",
    weight = "weight",
    population = FALSE
  )
inp

simStr <-
  simStructure(
    data = inp,
    method = "direct", #  distribution, multinom
    basicHHvars = c("age", "sex", "region"),
    seed = SEED
  )
pop <- simStr@pop@data
hh<- unique(pop, by = "hhid") 
addmargins(table(hsize = hh$hsize, region = hh$region))

simCat <-
  simCategorical(
    simStr,
    additional = c("eco_stat", "citizenship"),
    method = "multinom",
    seed = SEED
  )

simCon <- simContinuous(
  simCat,
  method = "lm",
  additional = "pgrossIncome",
  regModel = ~ sex + hsize, #+ eco_stat + citizenship + age,
  zeros = TRUE,
  log = FALSE,
  alpha = NULL, 
  residuals = TRUE,
  seed = SEED,   
  nr_cpus = 1
)

simPop <- simCon

data_sample <- data.frame(sampleData(simPop))
data_pop <- data.frame(popData(simPop))

tableWt(data_sample$citizenship, weights = data_sample$weight)
table(data_pop$citizenship)

tab <- spTable(simPop, select = c("sex", "region", "hsize"))
spMosaic(tab, labeling = labeling_border(abbreviate = c(region = TRUE)))

tab <- spTable(simPop, select = c("sex", "eco_stat"))
spMosaic(tab, method = "color")

spCdfplot(simPop, "pgrossIncome", cond = "sex", layout = c(1, 2)
)

q <- quantileWt(df$pgrossIncome[df$pgrossIncome > 0],
                df$weight[df$pgrossIncome > 0], 
                probs = 0.99)
spCdfplot(simPop, "pgrossIncome", 
          cond = "sex", layout = c(1,2), xlim = c(0, q))

spBwplot(simPop, x = "pgrossIncome", #cond = "pgrossIncome", layout = c(1, 2)
)
