#download.file(url = "https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt",
# destfile = "./ecg.txt")
Sys.setlocale("LC_ALL", 'ru_RU.UTF-8')
Sys.setlocale("LC_ALL", 'ru_RU.cp1251')
df <- read.csv("./ecg.txt",
               header = T,
               skip = 46,
               dec = ",",
               sep = "\t",
               encoding = 'cp1251')
View(df)
mask <- is.na.data.frame(df)
nas <- which(apply(mask, 1, sum) >= 4)
n <- length(nas) / 5 # Количество стадий эксперемента
dfs <- c()
dfs[[1]] <- df[1:(nas[1] - 1),]
for (i in 2:n) {
  dfs[[i]] <- df[(nas[(i - 1) * 5] + 2):(nas[(i - 1) * 5 + 1] - 1),]
}
metadata <- as.vector(read.csv2("./ecg.txt",
                                header = F,
                                dec = ",",
                                sep = "\t",
                                encoding = 'cp1251')[6:35, 1])
metadata
tracs_info <- c()
# strsplit(metadata[(1-1)*6+5], split = ' ')[[1]][1]
for (i in 1:(length(metadata) / 6)) {
  name <- strsplit(metadata[(i - 1) * 6 + 2], split = ' ')[[1]][1]
  id <- strsplit(metadata[(i - 1) * 6 + 3], split = ' ')[[1]][1]
  type <- strsplit(metadata[(i - 1) * 6 + 4], split = ' ')[[1]][1]
  unit <- strsplit(metadata[(i - 1) * 6 + 5], split = ' ')[[1]][1]
  freq <- strsplit(metadata[(i) * 6], split = ' ')[[1]][1]
  tracs_info[[i]] <- c(name, id, type, unit, freq)
}
print(tracs_info)

tmp_data <- as.vector(read.csv2("./ecg.txt",
                                header = F,
                                dec = ",",
                                sep = "\t",
                                encoding = 'cp1251')[36:40, 1])
time_start <- as.numeric(strsplit(tmp_data[3], split = ' ')[[1]][1])
time_ms <- as.numeric(strsplit(tmp_data[4], split = ' ')[[1]][1])
time_units <- as.numeric(strsplit(tmp_data[5], split = ' ')[[1]][1])
realtime <- c()
for (j in 1:time_units) {
  realtime[j] <- time_start + (j * time_ms) / time_units
}
dfs[[1]][, length(dfs[[1]]) + 1] <- realtime
for (i in 2:n) {
  time_start <- as.numeric(strsplit(df[nas[(i - 2) * 5 + 3], 1], split = ' ')[[1]][1])
  time_ms <- as.numeric(strsplit(df[nas[(i - 2) * 5 + 4], 1], split = ' ')[[1]][1])
  time_units <- as.numeric(strsplit(df[nas[(i - 2) * 5 + 5], 1], split = ' ')[[1]][1])
  realtime <- c()
  for (j in 1:time_units) {
    realtime[j] <- time_start + (j * time_ms) / time_units
  }
  dfs[[i]][, length(dfs[[i]][1,]) + 1] <- realtime
}

length(dfs[[2]][,1])


library(ggplot2)
subs <- dfs[[2]][1000:3000,]
plot(x = subs[,1], y = subs[,2],type = "o", pch = 19, cex = I(0.5))
abline(v = (0:40) * (4000), col = "red")

acf(x = subs[,2], lag.max = 2000)

# нахождение автокорреляции для всех параметров для всех исследований
#for(i in 1:length(dfs)){
#  for(j in 1:length(tracs_info)){
#    acf(x = dfs[[i]][,1+j], lag.max = 2000)
#  }
#}






