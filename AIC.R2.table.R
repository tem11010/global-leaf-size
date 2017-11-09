
# This function constructs a table of GLMMs ordered from best to worst by AIC
# It includes the AIC, dAIC, df, and marginal and conditional R^2 for each model.
# The R^2 are calculated using methods from Nakagawa & Schielzeth 2013 and 
# require the MuMIn package
# Optionally, you can include a vector of names to use for the models instead 
# of their variable names
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
    
    # Get a list of the models sent to the function
    models <- list(...)
    
    # Get the number of models
    len <- length(models)
    
    # Make an empty dataframe to hold the AIC/R^2 table
    tbl <- data.frame(Model = character(len), AIC = numeric(len), 
        df = numeric(len), R2.marg = numeric(len), R2.cond = numeric(len), 
        stringsAsFactors = F)
    
    # Calculate AIC and R^2 for each model
    for (i in 1:length(models)){
        
        # Store the model name
        tbl$Model[i] <- mnames[i]
        
        # Store the AIC
        tbl$AIC[i] <- AIC(models[[i]])
        
        # Store the degrees of freedom
        tbl$df[i] <- attr(logLik(models[[i]]), "df") 
        
        # Get the marginal and conditional R^2 using MuMIn
        R2 <- MuMIn::r.squaredGLMM(models[[i]])
        
        # Store the marginal and conditional R^2
        tbl$R2.marg[i] <- round(R2[1], 3)
        tbl$R2.cond[i] <- round(R2[2], 3)
    }
    
    # Clean up the table using some dplyr
    tbl <- tbl %>%
        
        # Order by AIC with the best model first
        arrange(AIC) %>%
        
        # Add a column for delta AIC
        mutate(dAIC = round(AIC - min(AIC), 2)) %>%
        
        # Reorder the columns
        select(Model, AIC, dAIC, df, R2.marg, R2.cond)
    
    # Return the table
    return(tbl)
}
