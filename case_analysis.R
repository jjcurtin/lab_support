# Creates a combined histogram, density plot, and rug plot for x
# `data` is a tibble
# `x_name` is a quoted column name
# `n_bins` is the number of bins for the histogram
plot_univariate <- function(data, x_name, n_bins = 10) {
  data |> 
    ggplot(aes(x = !!sym(x_name))) +
    geom_histogram(aes(y = after_stat(density)),
                   color = "black", fill = "light grey", bins = n_bins) +
    geom_density() +
    geom_rug(color = "red")
}




#############################################################################
# Creates a combined box and violin plot for x
# `data` is a tibble
# `x_name` is a quoted column name
plot_box <- function(data, x_name) {
  data |> 
    ggplot(aes(x = 1, y = !!sym(var_name))) +
    geom_boxplot() +
    geom_violin(aes(x = 1), fill = "green", color = NA, alpha = .4) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())
}



#############################################################################
# Creates a scatter plot with a linear regression line and an optional loess line 
# `data` is a tibble
# `x_name` and `y_name` are quoted column names
# `loess` is boolean that determines whether to include a loess line
plot_bivariate <- function(data, x_name, y_name, loess = TRUE) {
  plot <- data |> 
    ggplot(aes(x = !!sym(x_name), y = !!sym(y_name))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, col = "red") +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
  
  if(loess) {
    plot <- plot + geom_smooth(method = "loess", formula = y ~ x, col = "green")
  }
  
return(plot)
}


#############################################################################
# Creates a leverage plot for a model
# `model` is a linear model
plot_leverage <- function(model) {
  tibble(x = hatvalues(model)) |> 
    ggplot(aes(x = x)) +
      geom_histogram(fill = "light grey", color = "black", bins = 10) +
      geom_rug(color = "blue") +
      labs(x = "hat values",
           y = "Frequency") +
      geom_vline(xintercept = 2 * mean(hatvalues(model)), color = "yellow", 
                 linewidth = .75) +
      geom_vline(xintercept = 3 * mean(hatvalues(model)), color = "red", 
                 linewidth = .75) +
      labs(subtitle = "Red: 3 * mean(hat) for N < 100;  Yellow: 2 * mean(hat) for N >= 100")
}


#############################################################################
# Creates a residuals plot for a model
# `model` is a linear model
plot_residuals <- function(model){
  res <-tibble(x = rstudent(model))
  t_cut_point <- qt(p = .025/nrow(res), 
                    df = nrow(res) - length(coef(model)) - 1 - 2, 
                    lower.tail = FALSE)
  res |> 
      ggplot(aes(x = x)) +
        geom_histogram(fill = "light grey", color = "black", bins = 10) +
        geom_rug(color = "blue") +
        labs(x = "Studentized Residuals",
           y = "Frequency") +
        geom_vline(xintercept = c(-1, 1) * t_cut_point, color = "red", linewidth = .75) +
        labs(subtitle = "Red cut: Bonferroni corrected p-value < 0.05")
}


#############################################################################
# Creates a cooks distance plot for a model
# `model` is a linear model
plot_cooks <- function(model){
  cooks <-tibble(x = cooks.distance(model))
  cut_cooks <- 4/(nrow(cooks) - length(coef(model)))
  
  cooks |> 
    ggplot(aes(x = x)) +
      geom_histogram(fill = "light grey", color = "black", bins = 10) +
      geom_rug(color = "blue") +
      labs(x = "Cooks Distances",
           y = "Frequency") +
      geom_vline(xintercept = 4/(nrow(data) - length(coef(model)) - 1 - 1),
                 color = c("red"), linewidth = .75) +
      labs(subtitle = "Cut: 4/(N-P)")
}


#############################################################################
# Creates a dfbetas plot for a specific predictor in a model
# `model` is a linear model
# `x_name` is a quoted column name
plot_dfbetas <- function(model, x_name){
   dfb <- as_tibble(dfbetas(model)) |> 
     select(matches(x_name))
   colnames(dfb) <- "x" 
  
  dfb |> 
    ggplot(aes(x = x)) +
      geom_histogram(fill = "light grey", color = "black", bins = 10) +
      geom_rug(color = "blue") +
      labs(x = "dfBetas",
           y = "Frequency") +
      geom_vline(xintercept = c(-2, 2),
                 color = c("red"), linewidth = .75) +
      labs(title = x_name, subtitle = "Cut at |2|")
}


#############################################################################
# Creates an added variable plot for all X in a model
# `data` is a tibble
# `model` is a linear model
# `label_name` is the quoted column name to use as labels
plot_av <- function(data, model, label_name = "subid"){
 
  # avPlots() wants a traditional dataframe with rownames set to subid or 
  # other label for points
  data <- as.data.frame(data)
  rownames(data) <- pull(data, matches(label_name))

  car::avPlots(model, intercept = TRUE,
              id = list(method = list(abs(residuals(model, type = "pearson"))), 
              n = 5, cex = 1, col = 'red', location = "lr"))
}



plot_covratio <- function(model){
   cr <- tibble(x = covratio(model)) 
  
  cr |> 
    ggplot(aes(x = x)) +
      geom_histogram(fill = "light grey", color = "black", bins = 10) +
      geom_rug(color = "blue") +
      labs(x = "Cov Ratio",
           y = "Frequency") +
      geom_vline(xintercept = abs((3 * (length(coef(model)))/nrow(cr) - 1)),
                 color = "red", linewidth = .75) +
      labs(subtitle = "Cut: 3*(P/N) - 1")
}
# case_analysis <- function(Model, Type='RESIDUALS', Term = NULL, ID=row.names(Model$model))
# {
# switch(toupper(Type),
# 
#    UNIVARIATE =
#    {
#       d=Model$model
#       {Vars = names(d)}
#       for(varname in Vars)
#       {
#           par(cex.lab=1.5, cex.axis=1.2, lwd=2, ask=TRUE)  #set graphics parameters for pretty graphs
#           if(is.factor(d[[varname]]))
#           {
#              plot(d[varname], xlab=varname, ylab = "Frequency")
#           } else
#           {
#              #hist(thedata[[varname]],nclass=n.bins(thedata[[varname]]),xlab=varname, main='Red: Mean +- 3SD; Green: Median +- 2.2IQR')
#              hist(d[[varname]],xlab=varname, main='Red: Mean +- 3SD; Green: Median +- 2.2IQR')
#              points(d[[varname]],rep(0,length(d[[varname]])),pch = "|", col="blue")
#              abline(v=c(-3,0, 3) * sd(d[[varname]]) + mean(d[[varname]]),col='red', lty=c(1,2,1))
#              abline(v=c(-2.2,0, 2.2) * IQR(d[[varname]]) + median(d[[varname]]),col='green', lty=c(1,2,1))  #IQR = 1.34896SD for normal distribution
#              if (!is.null(ID)) {
#                Indices = identify(d[[varname]],rep(0,length(d[[varname]])),labels=ID)
#                Values = d[[varname]][Indices]
#              }
#           
#           }
#       }
#    },
# 
#    HATVALUES =
#    {
#       #Diagnostics for Leverage (Hat values)
#       #NOTE: Mahalanobis distance = (N - 1)(h - 1/N).   SPSS reports centered leverage (h - 1/N)
#       #NOTE: 3 * mean(h) is cut for small sample.  2 * mean(h) is cut for large sample
#       par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
#       TheTitle = paste('Model: ', Model$call[2], '\n', 'Small sample cut (green) = 3 * mean(Hat)\nLarge sample cut: 2 * mean(Hat)', sep='')
#       hist(hatvalues(Model),xlab='Hat Values', main=TheTitle)
#       abline(v= c(2,3) * mean(hatvalues(Model)),col=c('red', 'green'))
#       points(hatvalues(Model),rep(0,length(hatvalues(Model))),pch = "|", col="blue")
#       if (!is.null(ID)) {
#         Indices = identify(hatvalues(Model),rep(0,length(hatvalues(Model))),labels=ID)
#         Values = hatvalues(Model)[Indices]
#       }
#    },
# 
#    RESIDUALS =
#    {
#       #Diagnostics for Regresssion Outliers (studentized residuals;E*i)
#       #NOTE:  SPSS calls these Studentized Deleted Residuals.  Cohen calls these Externally Studentized Residual
#       #E*i follows t-distribution with N-k-2 dfs
#       #outlier.test(Model, labels=ID) #to get corrected p-value for worst outlier
#       #NOTE: Can get Bonferroni corrected p-value for any Studentized t as: N * 2 * pt(t, N-k-2, lower.tail = FALSE)
#       par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
#       N=length(rstudent(Model))
#       k= length(coef(Model))-1
#       TCut <- qt(p = .025/N, df = N-k-2, lower.tail = FALSE) #Bonferroni corrected t cut for studentized residuals
#       TheTitle = paste('Model: ', Model$call[2], '\n', 'Bonferroni corrected p < .05 cut-off in red', sep='')
#       hist(rstudent(Model), xlab='Studentized Residuals', main=TheTitle)
#       abline(v= c(-1,1) * TCut,col="red")
#       points(rstudent(Model),rep(0,length(rstudent(Model))),pch = "|", col="blue")
#       if (!is.null(ID)) {
#         Indices = identify(rstudent(Model),rep(0,length(rstudent(Model))),labels=ID)
#         Values = rstudent(Model)[Indices]
#       }
#    },
# 
#    COOKSD =
#    {
#       #Diagnositics for Influence
#       #NOTE: Alternative cuts are 4/(N-k-1) & qf(.5,k+1,N-k-1)
#       par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
#       N=length(cooks.distance(Model))
#       k= length(coef(Model))-1
#       TheTitle = paste('Model: ', Model$call[2], '\n', '4/(N-P) cut-off (red)\nqf(.5,P,N-P) cut-off (green)', sep='')
#       hist(cooks.distance(Model), xlab='Cooks d', main=TheTitle)
#       abline(v=c((4/(N-k-1)),qf(.5,k+1,N-k-1)) ,col=c('red', 'green'))
#       points(cooks.distance(Model),rep(0,length(cooks.distance(Model))),pch = "|", col="blue")
#       if (!is.null(ID)) {
#         Indices = identify(cooks.distance(Model),rep(0,length(cooks.distance(Model))),labels=ID)
#         Values = cooks.distance(Model)[Indices]
#       }
#    },
# 
#    DFBETAS =
#    {
#      if (is.null(Term)){
#       {Vars = dimnames(dfbetas(Model))[[2]]}  #get dfbetas for all parameters
#      }else{
#        if (!(Term %in% dimnames(dfbetas(Model))[[2]])){
#           stop('Term specified for DFBETAS not valid')
#        }
#        else Vars = Term
#      }
#      
#      for(varname in Vars){
#          par(cex.lab=1.5, cex.axis=1.2, lwd=2, ask=TRUE)  #set graphics parameters for pretty graphs
#          TheTitle = paste('Model: ', Model$call[2], '\n', 'B= ', coef(Model)[varname], sep='')
#          hist(dfbetas(Model)[,varname],xlab=paste('DFBETAS:', varname), main=TheTitle)
#          points(dfbetas(Model)[,varname],rep(0,length(dfbetas(Model)[,varname])),pch = "|", col="blue")
#          abline(v=c(-2, 2), col='red')
#          if (!is.null(ID))   {Indices = identify(dfbetas(Model)[,varname],rep(0,length(dfbetas(Model)[,varname])),labels=ID)}
#      }
#      
#      par(ask=FALSE)
#      if (is.null(Term)){
#         avPlots(Model,intercept=TRUE,id=list(method=list(abs(residuals(Model, type="pearson")), "x"), n=5, cex=1, col='red', location="lr"))
#      }else {
#         avPlots(Model, terms = Term, id=list(method=list(abs(residuals(Model, type="pearson")), "x"), n=5, cex=1, col='red', location="lr"))
#         Values = dfbetas(Model)[Indices, Term]
#      }
#      
#      if (is.null(Term)) {
#        Indices = NULL  #dont return indices if multiple dfbetas were reviewed
#        Values = NULL
#      }
#    },
# 
#    INFLUENCEPLOT =
#    {
#       par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
#       TheTitle = paste('Influence Bubble plot', '\nModel: ', Model$call[2], sep='')
#       plot(hatvalues(Model),rstudent(Model), type='n', xlab='Hat Values', ylab='Studentized Residuals', main=TheTitle )
#       cooksize = 10*sqrt(cooks.distance(Model))/max(cooks.distance(Model))
#       points(hatvalues(Model), rstudent(Model),cex=cooksize)
# 
#       N=length(rstudent(Model))
#       k= length(coef(Model))-1
#       TCut <- qt(p = .025/N, df = N-k-2, lower.tail = FALSE) #Bonferroni corrected t cut for studentized residuals
#       abline(h=c(-1,0, 1) * TCut,col='red', lty=c(1,2,1))
#       abline(v=c(1,2,3) * mean(hatvalues(Model)),col='red', lty=c(2,1,1))
#       if (!is.null(ID)) {
#         Indices = identify(hatvalues(Model),rstudent(Model),labels=ID)
#         Values = c(hatvalues(Model)[Indices], rstudent(Model)[Indices])
#       }
#    },
# 
#    COVRATIO =
#    {
#       #Diagnositics for SE inflation
#       N=length(covratio(Model))
#       k= length(coef(Model))-1
#       par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
#       TheTitle = paste('Model: ', Model$call[2], '\n', 'abs((3*P)/N)-1 cut-off in red', sep='')
#       hist(covratio(Model), xlab='CovRatio', main=TheTitle)
#       abline(v=abs((3*(k+1)/N)-1), col="red")
#       points(covratio(Model),rep(0,length(covratio(Model))),pch = "|", col="blue")
#       if (!is.null(ID)) {
#         Indices = identify(covratio(Model),rep(0,length(covratio(Model))),labels=ID)
#         Values = covratio(Model)[Indices]
#       }
#    },
# 
#    {print('Valid options for type: hatvalues, residuals, cooksd, dfbetas, influenceplot, covratio, univariate')}     #OTHERWISE
# )#end switch
# 
# 
# #Handle return values
# Rownames = row.names(Model$model)[Indices]
# Cases = list(Rownames = Rownames, Values = Values)
# return (Cases)
# }
