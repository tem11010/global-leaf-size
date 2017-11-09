AIC.R2.table <- function(..., mnames) {
    
    # Get the model names if they weren't specified
    if (missing(mnames)) {
        Call <- match.call()
        if (!is.null(names(Call))) {
            xargs <- which(names(Call) %in% names(formals())[-1])
        }
        else xargs <- numeric(0)
        mnames <- as.character(Call)[c(-1, -xargs)]
    }
    
    models <- list(...)
    len <- length(models)
    
    #AICc <- MuMIn::AICc(...)
    
    #print(AICc)
    
    tbl <- data.frame(Model = character(len), AIC = numeric(len), 
                      R2.marg = numeric(len), R2.cond = numeric(len), 
                      stringsAsFactors = F)
    
    
    for (i in 1:length(models)){
        tbl$Model[i] <- mnames[i]
        tbl$AIC[i] <- AIC(models[[i]])
        
        R2 <- r.squaredGLMM(models[[i]])
        
        tbl$R2.marg[i] <- round(R2[1], 2)
        tbl$R2.cond[i] <- round(R2[2], 2)
    }
    
    tbl <- tbl %>%
        
        arrange(AIC) %>%
        
        mutate(dAIC = round(AIC - min(AIC), 2)) %>%
        
        select(Model, AIC, dAIC, R2.marg, R2.cond)
    
    return(tbl)
}
