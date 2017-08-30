rm(list = ls())
names <- list.files(path = "data", pattern = ".RData")
names <- unlist(lapply(names, function(x) strsplit(x, split = ".RData")[[1]][1]))
path <- list.files(path = "data", pattern = ".RData", full.names = T)

for (x in path) load(x)
for (i in 1:length(names)) {
  save(list = names[i], file = paste0("data/", names[i], ".RData"), compress = T)
  print(i)
}




