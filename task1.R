
data <- read.csv("dataset_mood_smartphone.csv", header=TRUE)

all_ids = unique(data$id)
all_vars = unique(data$variable)

# Check if all data is normal for each id
data$variable = as.factor(data$variable)

moods = data[data$variable=="mood",]
arousal = data[data$variable=="circumplex.arousal",]
valences = data[data$variable=="circumplex.valence",]
activity = data[data$variable=="activity",]
screen = data[data$variable=="screen",]
call = data[data$variable=="call",]
sms = data[data$variable=="sms",]
games = data[data$variable=="appCat.game",]
entertainments = data[data$variable=="appCat.entertainment",]


# QQ plots
qqnorm(moods$value)
qqnorm(arousal$value)

# Shapiro wilk
shapiro.test(moods[1:5000,]$value)


# Plots
plot(moods$value)
plot(arousal$value)
plot(games$value) # couple of outliers
plot(entertainments$value) # outliers
plot(valences$value) # valence score
plot(activity$value) # Activity score between 0 and 1
plot(screen$value) # Duration of screen activity
plot(call$value) # call at certain time
plot(sms$value) # sms at certain time


# Correlation test between mood and arousal
arousal_which = which(is.na(arousal$value))

arousal_clean = na.omit(arousal)
moods_clean = na.omit(moods)

dim(moods_clean)
dim(arousal_clean[1:5641,])

cor.test(moods_clean$value, arousal_clean[1:5641,]$value)

# There exists correlation between moods and arousal
# Reject H0, Accept H1 saying correlation is not equal to 0
#
# data:  moods_clean$value and arousal_clean[1:5641, ]$value
# t = 1.2077, df = 5595, p-value = 0.2272
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.01006020  0.04232448
# sample estimates:
#  cor 
# 0.01614322 


boxplot(moods_clean$value)

moods_clean$time = as.factor(moods_clean$time)

model <- glm(value ~ variable, data=data)
summary(model)
plot(fitted(model), residuals(model))




