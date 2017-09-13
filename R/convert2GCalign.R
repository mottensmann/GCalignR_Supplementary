convert2GCalign <- function(path = NULL, var_names = NULL, sheet = 1) {

    matrix <- as.data.frame(readxl::read_xls(path = path,
                                           sheet = sheet,
                                           skip = 3,
                                           col_names = F))
    matrix <- apply(matrix, 2, as.numeric)
    ## replace NA by 0
    matrix[is.na(matrix)] <- 0

    ## read ind names
    ind_names <- as.character(readxl::read_xls(path = path,
                                              sheet = sheet,
                                              skip = 1,
                                              n_max = 1,
                                              col_names = F))
    ## convert to list
    list <- GCalignR:::conv_gc_mat_to_list(gc_data = matrix,
                                              ind_names = ind_names[seq(1,ncol(matrix),length(var_names))],
                                              var_names = var_names)
    ## convert to GCalign object
    rt_mat <- as.matrix(matrix[,seq(1,ncol(matrix),length(var_names))])
    rt_mat <- apply(rt_mat,2,as.numeric)
    mean_per_row <- apply(rt_mat,1, function(x) {if (all(x == 0)) 0 else mean(x[x != 0])})
    out <- lapply(var_names, function(y) as.data.frame(do.call(cbind, lapply(list, function(x) x[y]))))
    out <- lapply(out, function(x){
        names(x) <- names(list)
        x
    })

    out <- lapply(out, function(x){
        x <- cbind(mean_per_row, x)
        x
    })

    out <- lapply(out, function(x){
        names(x)[1] <- "mean_RT"
        x
    })
    names(out) <- var_names
    output <- list()
    output$aligned <- (out)
    return(output)
    }
