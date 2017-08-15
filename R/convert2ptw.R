convert2ptw <- function(data = NULL, sep = "\t", rt_col_name = NULL, conc_col_name) {
    check <- check_input(data = data, sep = sep, rt_col_name = rt_col_name, message = FALSE)
    if (check == FALSE) stop("data is malformed. See check_input and fix issues raised there.")
    if (is.character(data)) {
        dat <- read_peak_list(data = data, sep = sep, rt_col_name = rt_col_name)
    } else if (is.list(data)) {
        dat <- data
    }
    dat <- lapply(dat, function(x) x[, c(rt_col_name, conc_col_name)])
    dat <- lapply(dat, function(x) {
        colnames(x) <- c("rt","I")
        return(x)})
    dat <- lapply(dat, function(x) as.matrix(x))
    return(dat)
}

