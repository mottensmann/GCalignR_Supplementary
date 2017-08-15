summary_stats <- function(data = NULL, measurevar, groupvars = NULL, na.rm= T, conf.interval = 0.95, .drop=TRUE) {

    length2 <- function(x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

datac <- plyr::ddply(data, groupvars, .drop = .drop,
           .fun = function(xx, col) {
           c(N = length2(xx[[col]], na.rm = na.rm),
         mean = mean(xx[[col]], na.rm  = na.rm),
         sd = stats::sd(xx[[col]], na.rm = na.rm),
         ci_l = quantile(xx[[col]], na.rm = na.rm, probs = 0.025)[[1]],
         ci_h = quantile(xx[[col]], na.rm = na.rm, probs = 0.975)[[1]])},
         measurevar)

    # Rename the "mean" column
    datac <- plyr::rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd/sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    # ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    # datac$ci <- datac$se * ciMult

    return(datac)
}
