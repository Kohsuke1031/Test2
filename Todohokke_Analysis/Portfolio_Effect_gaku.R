##   gaku' s functin note 
##   functions modified from
#  https://seananderson.ca/
#  https://rdrr.io/github/seananderson/ecofolio/
##   Start Date
##   Last updated




#' Calculate the coefficient of variation
#'
#' @param x A numeric vector
#'
#' @examples 
#' cv(rnorm(20))
#'
#' @export

cv <- function(x){
  sd(x)/abs(mean(x))
}


# pe_avg_cv: Estimate the average-CV portfolio effect

#' Takes a matrix of abundance or biomass data and returns various
#' estimates of the average-CV portfolio effect. Options exist to
#' detrend the time series data.
#' 
#' @references 
#' Doak, D., D. Bigger, E. Harding, M. Marvier, R. O'Malley, and D.
#' Thomson. 1998. The Statistical Inevitability of Stability-Diversity
#' Relationships in Community Ecology. Amer. Nat. 151:264-276.
#' 
#' Tilman, D., C. Lehman, and C. Bristow. 1998. Diversity-Stability
#' Relationships: Statistical Inevitability or Ecological Consequence?
#' Amer. Nat. 151:277-282.
#' 
#' Schindler, D., R. Hilborn, B. Chasco, C. Boatright, T. Quinn, L.
#' Rogers, and M. Webster. 2010. Population diversity and the
#' portfolio effect in an exploited species. Nature 465:609-612. doi:
#' 10.1038/nature09060.
#' 
#' @details This version of the portfolio effect consists of dividing
#' the mean of the coefficient of variations (CV) of all individual
#' subpopulations (assets) by the CV of the combined total population.
#'   
#' @param x A matrix or dataframe of abundance or biomass data. The
#' columns should represent different subpopulations or species. The
#' rows should represent the values through time.
#' @param detrending Character value describing if (and how) the time
#' series should be detrended before estimating the portfolio effect.
#' Defaults to not detrending.
#' @param ci Logical value (defaults to \code{FALSE}). Should a 95\%
#' confidence interval be calculated using a bootstrap procedure?
#' Returns the bias-corrected (bca) version of the bootstrap
#' confidence interval.
#' @param boot_reps Number of bootstrap replicates.
#' @param na.rm A logical value indicating whether \code{NA} values
#' should be row-wise deleted. 
#'   
#' @return A numeric value representing the average-CV portfolio
#' effect. If confidence intervals were requested then a list is
#' returned with the portfolio effect \code{pe} and 95\% bootstrapped
#' confidence interval
#' \code{ci}.
#' 
#' @examples 
#' data(pinkbr)
#' pe_avg_cv(pinkbr[,-1], ci = TRUE)
#' pe_avg_cv(pinkbr[,-1], detrending = "loess_detrended", ci = TRUE)
#' 
#' @export


######
pe_avg_cv_gaku <- function(data, detrending = c("not_detrended", "linear_detrended", 
                                        "loess_detrended"),  
                           ci = FALSE, boot_reps = 500, na.rm = FALSE) { 

##  added by gaku 21 Sep 2020
# Data_FishNames <- x[,1]
#  x <- x[,-1]
#  Data_Year<- x[1,]
#  x <- x[-1,]
#  x <- unname(x)
  
#####
  
  if(!detrending[1] %in% c("not_detrended", "linear_detrended", "loess_detrended")) 
    stop("not a valid detrending type")
  
  if(na.rm) data <- na.omit(data)
  
  total_nas <- sum(is.na(data))
  ifelse(!na.rm & total_nas > 0, return_na <- TRUE, return_na <- FALSE)
  
  if(detrending[1] == "not_detrended") {
    cv_single_asset <- mean(apply(data, 2, cv))
    cv_portfolio <- cv(rowSums(data))
    pe <- cv_single_asset / cv_portfolio
  }
  
  if(detrending[1] == "linear_detrended") {
    
    # single assets:
    data_detrended <- data
    for(i in 1:ncol(data)) data_detrended[,i] <- residuals(lm(data[,i]~c(1:nrow(data))))
    single_asset_means <- apply(data, 2, mean)
    single_asset_sds <- apply(data_detrended, 2, sd)
    cv_single_asset <- mean(single_asset_sds / single_asset_means)
    
    # portfolio:
    sd_portfolio <- sd(residuals(lm(rowSums(data)~c(1:nrow(data)))))
    mean_portfolio <- mean(rowSums(data))
    cv_portfolio <- sd_portfolio / mean_portfolio
    pe <- cv_single_asset / cv_portfolio
  }
  
  if(detrending[1] == "loess_detrended") {
    # single assets:
    data_detrended <- data
    for(i in 1:ncol(data)) data_detrended[,i] <- residuals(loess(data[,i]~c(1:nrow(data))))
    single_asset_means <- apply(data, 2, mean)
    single_asset_sds <- apply(data_detrended, 2, sd)
    cv_single_asset <- mean(single_asset_sds / single_asset_means)
    # portfolio:
    sd_portfolio <- sd(residuals(loess(rowSums(data)~c(1:nrow(data)))))
    mean_portfolio <- mean(rowSums(data))
    cv_portfolio <- sd_portfolio / mean_portfolio
    pe <- cv_single_asset / cv_portfolio
  }
  
  pe_avg_cv_for_boot <- function(data) {
    cv_single_asset <- mean(apply(data, 2, cv))
    cv_portfolio <- cv(rowSums(data))
    pe <- cv_single_asset / cv_portfolio
    pe
  }
  
  if(ci) {
    ## confidence interval calculation
    boot.out <- boot::boot(t(data), function(y, i) pe_avg_cv_for_boot(t(y[i,])), R = boot_reps)
    pe_ci <- boot::boot.ci(boot.out, type = "bca")$bca[c(4,5)]
    out <- list(pe = pe, ci = pe_ci)
  }else{
    out <- pe
  }
  if(return_na) out <- NA
  out
}


#' Estimate the mean-variance portfolio effect
#' 
#' Takes a matrix of abundance or biomass data and returns various estimates of
#' the mean-variance portfolio effect. Options exist to fit various
#' mean-variance models and to detrend the time series data.
#' 
#' @details This version of the portfolio effect consists of dividing the CV of
#' a theoretical single population (single asset system) that has the same
#' overall mean but with the variance scaled according to the mean-variance
#' relationship by the CV of the combined total population. The calculation of
#' the portfolio CV is the same as in \code{\link{pe_avg_cv}} but the
#' calculation of the single asset system CV is different.
#'
#' Currently, confidence intervals can only be returned for
#' \code{linear}, \code{linear_detrended}, and \code{loess_detrended}.
#' Otherwise, the value of \code{ci} will be automatically turned to
#' \code{FALSE}. It is not obvious what a confidence interval should
#' be given that the quadratic term in the quadratic and
#' linear-quadratic averaged versions are bounded at 0.
#'
#' @param x A matrix or dataframe of abundance or biomass data. The columns
#' should represent different subpopulations or species. The rows should
#' represent the values through time.
#' @param type Type of model to fit to the log(variance)-log(mean) data.
#' Options are: \itemize{ 
#' \item \code{linear}: linear regression (the default), 
#' \item \code{linear_robust}: robust linear regression 
#' \item \code{quadratic}: quadratic regression 
#' \item \code{linear_quad_avg}: AICc-weighted model averaging of linear and
#' quadratic regression 
#' \item \code{linear_detrended}: detrend the time series with a linear model
#' before estimating z from a linear regression
#' \item \code{loess_detrended}: detrend the time series with a loess smoother
#' before estimating z from a linear regression
#' }
#' @param ci Logical value describing whether a 95\% confidence interval should
#' be calculated and returned (defaults to \code{TRUE}). 
#' @param na.rm A logical value indicating whether \code{NA} values should be
#' row-wise deleted. 
#'
#' @return A numeric value representing the portfolio effect that
#' takes into account the mean-variance relationship. If confidence
#' intervals were requested then a list is returned with the portfolio
#' effect (\code{pe}) and 95\% confidence interval (\code{ci}).
#'   
#' @references 
#' Anderson, S.C., A.B. Cooper, N.K. Dulvy. 2013. Ecological prophets:
#' Quantifying metapopulation portfolio effects. Methods in Ecology and
#' Evolution. In Press.
#'
#' Doak, D., D. Bigger, E. Harding, M. Marvier, R. O'Malley, and D.
#' Thomson. 1998. The Statistical Inevitability of Stability-Diversity
#' Relationships in Community Ecology. Amer. Nat. 151:264-276.
#' 
#' Tilman, D., C. Lehman, and C. Bristow. 1998. Diversity-Stability
#' Relationships: Statistical Inevitability or Ecological Consequence?
#' Amer. Nat. 151:277-282.
#' 
#' Tilman, D. 1999. The Ecological Consequences of Changes in
#' Biodiversity: A Search for General Principles. Ecology
#' 80:1455-1474.
#' 
#' Taylor, L. 1961. Aggregation, Variance and the Mean. Nature
#' 189:732-735. doi: 10.1038/189732a0.
#' 
#' Taylor, L., I. Woiwod, and J. Perry. 1978. The Density-Dependence
#' of Spatial Behaviour and the Rarity of Randomness. J. Anim. Ecol.
#' 47:383-406.
#' @export
#' @examples
#' data(pinkbr)
#' pe_mv(pinkbr[,-1], ci = TRUE)
#' \dontrun{
#' pe_mv(pinkbr[,-1], type = "quadratic") # same as linear in this case
#' pe_mv(pinkbr[,-1], type = "linear_quad_avg")
#' pe_mv(pinkbr[,-1], type = "linear_robust")
#' pe_mv(pinkbr[,-1], type = "linear_detrended", ci = TRUE)
#' pe_mv(pinkbr[,-1], type = "loess_detrended", ci = TRUE)
#' }
#' @import robustbase
# @importMethodsFrom robustbase predict.lmrob 
# @importClassesFrom robustbase lmrob

pe_mv_gaku <- function(x, type = c("linear", "linear_robust", "quadratic",
                              "linear_quad_avg",  "linear_detrended", "loess_detrended"), ci =
                    FALSE, na.rm = FALSE) {

  
  
  type <- type[1]
  
  if(!type %in% c("linear", "linear_robust", "quadratic",
                  "linear_quad_avg", "linear_detrended", "loess_detrended")){
    stop("not a valid type")
  }
  
  if(!type %in% c("linear", "linear_robust", "linear_detrended",
                  "loess_detrended")){
    if(ci == TRUE){
      warning("Confidence intervals aren't supported for this type of
        mean-variance model. Setting ci = FALSE.")
    }
    ci <- FALSE
  }
  
  if(na.rm) x <- na.omit(x)
  
  total_nas <- sum(is.na(x))
  return_na <- ifelse(!na.rm & total_nas > 0, TRUE, FALSE)
  
  ## first get the means:
  m <- apply(x, 2, mean)
  single_asset_mean <- mean(rowSums(x))
  
  cv_portfolio <- cv(rowSums(x))
  
  ## now detrend if desired:
  if(type == "linear_detrended") {
    ## first get cv of detrended portfolio abundance:
    sd_portfolio <- sd(residuals(lm(rowSums(x)~c(1:nrow(x)))))
    mean_portfolio <- mean(rowSums(x))
    cv_portfolio <- sd_portfolio / mean_portfolio
    ## now detrend:
    x <- apply(x, 2, function(y) residuals(lm(y~c(1:length(y)))))
  }
  if(type == "loess_detrended") {
    ## first get CV of detrended portfolio abundance:
    sd_portfolio <- sd(residuals(loess(rowSums(x)~c(1:nrow(x)))))
    mean_portfolio <- mean(rowSums(x))
    cv_portfolio <- sd_portfolio / mean_portfolio
    ## now detrend:
    x <- apply(x, 2, function(y) residuals(loess(y~c(1:length(y)))))
  }
  
  ## and get the variances for the assets:
  v <- apply(x, 2, var)
  
  log.m <- log(m)
  log.v <- log(v)
  d <- data.frame(log.m = log.m, log.v = log.v, m = m, v = v)
  
  taylor_fit <- switch(type[1], 
                       linear = {
                         lm(log.v ~ log.m, data = d)
                       },
                       linear_robust = {
                         robustbase::lmrob(log.v ~ log.m, data = d)
                       },
                       quadratic = {
                         nls(log.v ~ B0 + B1 * log.m + B2 * I(log.m ^ 2),
                             data = d, start = list(B0 = 0, B1 = 2, B2 = 0), lower =
                               list(B0 = -1e9, B1 = 0, B2 = 0), algorithm = "port")
                       },
                       linear_detrended = {
                         lm(log.v ~ log.m, data = d)
                       }, 
                       loess_detrended = {
                         lm(log.v ~ log.m, data = d)
                       },
                       linear_quad_avg = {
                         linear <- nls(log.v ~ B0 + B1 * log.m, data = d, 
                                       start = list(B0 = 0, B1 = 2), 
                                       lower = list(B0 =-1e9, B1 = 0), 
                                       algorithm = "port")
                         print(linear)
                         
                         quadratic <- nls(log.v ~ B0 + B1 * log.m + B2 * I(log.m ^ 2), data= d, 
                                          start = list(B0 = 0, B1 = 2, B2 = 0), 
                                          lower = list(B0 = -1e9, B1 = 0, B2 = 0),
                                          algorithm = "port")
                         
                        print( quadratic)
                        TEST<- MuMIn::model.avg(list(linear=linear, quad=quadratic), rank = MuMIn::AICc)
                        print(TEST)
                         MuMIn::model.avg(list(linear=linear, quad=quadratic), rank = MuMIn::AICc)
                       }
  )
  
  if(ci) {
    single_asset_variance_predict <- predict(taylor_fit, newdata =
                                               data.frame(log.m = log(single_asset_mean)), se = TRUE)
    single_asset_variance <- exp(single_asset_variance_predict$fit)
  } else {
    single_asset_variance_predict <- predict(taylor_fit, newdata =
                                               data.frame(log.m = log(single_asset_mean)), se = FALSE)
    single_asset_variance <- exp(single_asset_variance_predict)
  }
  
  cv_single_asset <- sqrt(single_asset_variance) / single_asset_mean
  pe <- as.numeric(cv_single_asset / cv_portfolio)
  
  if(ci == TRUE) {
    single_asset_variance_ci <- exp(single_asset_variance_predict$fit
                                    + c(-1.96, 1.96) * single_asset_variance_predict$se.fit)
    cv_single_asset_ci <- sqrt(single_asset_variance_ci) / single_asset_mean
    pe_ci <- as.numeric(cv_single_asset_ci / cv_portfolio)
    pe_ci <- pe_ci[order(pe_ci)] # make sure the lower value is first
    out <- list(pe = pe, ci = pe_ci)
  } else {
    out <- pe
  }
  
  if(return_na) out <- NA
  
  out
}

#' Fit Taylor's power law
#'
#' Fits Taylor's power law to the temporal mean and variance in
#' log-log space and returns the coefficients. The model is fit as
#' \code{log(sigma^2_i) = c + z * log(mu_i) + e_i}, where \code{c}
#' represents a constant, \code{z} represents the parameter of
#' interest (Taylor's power law exponent), \code{i} represents a
#' subpopulation, and \code{e_i} represents independent and
#' distributed residual error with mean zero and an estimated
#' variance.
#'
#' @return
#' A list containing the constant \code{c} value and the exponent
#' \code{z} in Taylor's power law equation. If confidence intervals
#' were requested then the list will also contain \code{ci} with the
#' 95\% confidence intervals on the z value.
#'
#' @param x A matrix or dataframe of abundance or biomass data. The
#' columns should represent different subpopulations or species. The
#' rows should represent the values through time.
#' @param ci A Logical value indicating whether 95\% confidence
#' intervals should be calculated for the z value (the exponent in
#' Taylor's power law).
#' @param na.rm A logical value indicating whether \code{NA} values
#' should be row-wise deleted. 
#'
#' @references
#' Taylor, L. 1961. Aggregation, Variance and the Mean. Nature
#' 189:732-735. doi: 10.1038/189732a0.
#'
#' Taylor, L., I. Woiwod, and J. Perry. 1978. The Density-Dependence
#' of Spatial Behaviour and the Rarity of Randomness. J. Anim. Ecol.
#' 47:383-406.
#'
#' Taylor, L., and I. Woiwod. 1982. Comparative Synoptic Dynamics. I.
#' Relationships Between Inter- and Intra-Specific Spatial and
#' Temporal Variance/Mean Population Parameters. J. Anim. Ecol.
#' 51:879-906.
#' @export
#' @examples
#' data(pinkbr)
#' fit_taylor(pinkbr[,-1])
#' 



fit_taylor_gaku <- function(x, ci = FALSE, na.rm = FALSE){

  
  
  if(na.rm) x <- na.omit(x)
  
  total_nas <- sum(is.na(x))
  ifelse(!na.rm & total_nas > 0, return_na <- TRUE, return_na <- FALSE)
  
  m <- apply(x, 2, mean)
  v <- apply(x, 2, var)
  log.m <- log(m)
  log.v <- log(v)
  fit <- lm(log.v ~ log.m)
  c.value <- as.numeric(coef(fit)[1])
  z.value <- as.numeric(coef(fit)[2])
  if(ci == TRUE) {
    z.se <- summary(fit)$coef[2,2]
    z.l <- z.value - 1.96 * z.se
    z.u <- z.value + 1.96 * z.se
    out <- list(c = c.value, z = z.value, z.l = z.l, z.u = z.u)
  }else{
    out <-  list(c = c.value, z = z.value) 
  }
  if(return_na){
    out[[1]] <- NA
    out[[2]] <- NA
  }
  return(out)
}

#' Plot mean-variance relationship
#'
#' Creates a scatter plot of the time series log(variance) vs.
#' log(mean). Shows a model fit to the mean-variance data and an
#' extrapolation to the size of the metapopulation. The linear version
#' of this model is referred to as Taylor's power law.
#'
#' @param x A matrix or dataframe of abundance or biomass data. The
#' columns should represent different subpopulations or species. The
#' rows should represent the values through time.
#' @param show A vector of character objects indicating which
#' mean-variance models to show.
#' @param col Colour for the mean-variance model fit. A vector of
#' length 3 with the three values corresponding to \code{linear},
#' \code{quadratic},
#' \code{robust}.
#' @param lty Line type for the mean-variance model fit. A vector of
#' length 3 with the three values corresponding to \code{linear},
#' \code{quadratic}, \code{robust}.
#' @param pch_sa Point type for the extrapolated
#' "single-asset" portfolio. A vector of length 3 with the three
#' values corresponding to \code{linear}, \code{quadratic},
#' \code{robust}.
#' @param ci Add a confidence interval around the model fit? Only
#' appears for the linear fit option.
#' @param pch_subpops Point type for the subpopulations.
#' @param pch_port Point type for the portfolio.
#' @param add_z Logical. Add Taylor's power law z value (based on a
#' linear model)?
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param ... Other values to pass to \code{plot}.
#' @references 
#' Anderson, S.C., A.B. Cooper, N.K. Dulvy. 2013. Ecological prophets:
#' Quantifying metapopulation portfolio effects. Methods in Ecology and
#' Evolution. In Press.
#'
#' Doak, D., D. Bigger, E. Harding, M. Marvier, R. O'Malley, and D.
#' Thomson. 1998. The Statistical Inevitability of Stability-Diversity
#' Relationships in Community Ecology. Amer. Nat. 151:264-276.
#' 
#' Tilman, D., C. Lehman, and C. Bristow. 1998. Diversity-Stability
#' Relationships: Statistical Inevitability or Ecological Consequence?
#' Amer. Nat. 151:277-282.
#' 
#' Tilman, D. 1999. The Ecological Consequences of Changes in
#' Biodiversity: A Search for General Principles. Ecology
#' 80:1455-1474.
#' 
#' Taylor, L. 1961. Aggregation, Variance and the Mean. Nature
#' 189:732-735. doi: 10.1038/189732a0.
#' 
#' Taylor, L., I. Woiwod, and J. Perry. 1978. The Density-Dependence
#' of Spatial Behaviour and the Rarity of Randomness. J. Anim. Ecol.
#' 47:383-406.
#' @export
#' @examples
#' data(pinkbr)
#' par(mfrow = c(1,3))
#' plot_mv(pinkbr[,-1], show = "linear")
#' mtext("Linear")
#' plot_mv(pinkbr[,-1], show = "quadratic", add_z = FALSE)
#' mtext("Quadratic")
#' plot_mv(pinkbr[,-1], show = "robust", add_z = FALSE)
#' mtext("Robust linear")
#' @import robustbase
# @importMethodsFrom robustbase predict.lmrob 
# @importClassesFrom robustbase lmrob



plot_mv_gaku <- function(x, show = c("linear", "quadratic", "robust"), col
                    = c("#D95F02", "#1B9E77", "#E7298A"), lty = c(1, 1, 1),
                    pch_sa = c(1, 5, 6), ci = FALSE, pch_subpops = 21,
                    pch_port = 4, add_z = TRUE, xlab = "log(mean)", ylab =
                      "log(variance)", ...) {

  
  ## get mean and variance of portfolio and assets:
  x.long <- reshape::melt.data.frame(x, id.vars = NULL)
  overall.d <- apply(x[,-1], 1, sum)
  mv <- plyr::ddply(x.long, "variable", function(x) {data.frame(m =
                                                                  mean(x$value, na.rm = TRUE), v = var(x$value, na.rm = TRUE))})
  overall.mean <- mean(overall.d, na.rm = TRUE)
  portfolio.var <- var(overall.d, na.rm = TRUE)
  m.t <- lm(log(v) ~ log(m), data = mv)
  overall.variance <- exp(as.numeric(predict(m.t, newdata =
                                               data.frame(m = overall.mean))))
  m.t.quad <- nls(log(v) ~ B0 + B1 * log(m) + B2 * I(log(m) ^ 2), data
                  = mv, start = list(B0 = 0, B1 = 2, B2 = 0), lower = list(B0 =
                                                                             -1e9, B1 = 0, B2 = 0), algorithm = "port")
  overall.variance.quad <- exp(as.numeric(predict(m.t.quad, newdata =
                                                    data.frame(m = overall.mean))))
  d1p <- seq(min(mv$m), max(mv$m), length.out = 200)
  d2p <- seq(max(mv$m), overall.mean, length.out = 200)
  
  ## set up plot:
  with(mv, plot((m), (v), xlim = c((min(m)), (overall.mean)*1.15),
                ylim = c(min((v)), max(overall.variance, portfolio.var,
                                       overall.variance.quad)*1.25), pch = pch_subpops, log = "xy", xlab =
                  xlab, ylab =ylab, bg = "#00000020", col = "#00000070", ...))
  points((overall.mean), (portfolio.var), col = "black", pch = pch_port, lwd
         = 1.2, cex = 1.6)
  ## now add fits and extrapolations as requested:
  if("linear" %in% show) {
    m.t.p <- predict(m.t, newdata = data.frame(m = seq(min(mv$m),
                                                       max(mv$m), length.out = 2)))
    m.t.p2 <- predict(m.t, newdata = data.frame(m = seq(max(mv$m),
                                                        overall.mean, length.out = 2)))
    p1 <- predict(m.t, newdata = data.frame(m = d1p), se = TRUE)
    p2 <- predict(m.t, newdata = data.frame(m = d2p), se = TRUE)
    points((overall.mean), (overall.variance), col = col[1], pch = pch_sa[1],
           lwd = 1.5, cex = 1.4)
    segments((min(mv$m)), exp(m.t.p[1]), (max(mv$m)), exp(m.t.p[2]),
             col = col[1], lty = lty[1])
    segments((max(mv$m)), exp(m.t.p2[1]), (overall.mean),
             exp(m.t.p2[2]), lty = 2,  col = col[1])
    if(ci) {
      polygon(c(d1p, rev(d1p)), c(exp(p1$fit + 1.96*p1$se.fit),
                                  exp(rev(p1$fit - 1.96*p1$se.fit))), border = FALSE, col =
                "#00000020")
      polygon(c(d2p, rev(d2p)), c(exp(p2$fit + 1.96*p2$se.fit),
                                  exp(rev(p2$fit - 1.96*p2$se.fit))), border = FALSE, col =
                "#00000010")
    }
  }
  if("quadratic" %in% show){
    p.quad.1 <- predict(m.t.quad, newdata = data.frame(m = d1p), se = FALSE)
    p.quad.2 <- predict(m.t.quad, newdata = data.frame(m = d2p), se = FALSE)
    points((overall.mean), (overall.variance.quad), col =col[2], pch =
             pch_sa[2], lwd = 1.5, cex = 1.4)
    lines(d1p, exp(as.numeric(p.quad.1)), col = col[2], lwd = lty[2])
    lines(d2p, exp(as.numeric(p.quad.2)), col = col[2], lty = 2, lwd = 1.5)
  }
  if("robust" %in% show) {
    m.t.rob <- lmrob(log(v) ~ log(m), data = mv)
    overall.variance.rob <- exp(as.numeric(predict(m.t.rob, newdata =
                                                     data.frame(m = overall.mean))))
    p.rob.1 <- predict(m.t.rob, newdata = data.frame(m = d1p), se = FALSE)
    p.rob.2 <- predict(m.t.rob, newdata = data.frame(m = d2p), se = FALSE)
    points((overall.mean), (overall.variance.rob), col = col[3], pch =
             pch_sa[3], lwd = 1.5, cex = 1.1)
    lines(d1p, exp(p.rob.1), col = col[3], lty = lty[3])
    lines(d2p, exp(p.rob.2), col = col[3], lty = 2)
  }
  if(add_z) {
    mtext(paste("z =", formatC(round(coef(m.t)[2], 2), digits = 1,
                               format = "f")), side = 1, adj = 0.9, line = -1.2)
  }
}


#' Calculate the Thibaut and Connolly diversity-stability relationship
#' 
#' @param x A matrix or dataframe of abundance or biomass data. The
#'   columns should represent different subpopulations or species. The
#'   rows should represent the values through time.
#' @param synchrony The Loreau and de Mazencourt synchrony index. See
#' \code{\link{synchrony}}.
#' @param z Taylor's power law exponent from variance = c * mean^z.
#' See \code{\link{fit_taylor}}.
#' @param overyielding The overyielding coefficient. This reflects the
#' increase in abundance or biomass with increasing diversity and is
#' relevant to community portfolio effects.
#' @return A list containing the CV of the observed community
#' (portfolio) \code{cv_p} expected CV of the community in monoculture
#' \code{cv_1} and the ratio of the monoculture CV to the observed
#' community or portfolio CV \code{pe}.
#' @examples
#' dat = data.frame(x1 = rnorm(20, 10), x2 = rnorm(20, 10), x3 = rnorm(20,10))
#' thibaut_connolly_dsr(dat, synchrony = 0.7, z = 2, overyielding = 1)
#' @export
#' @references
#' Thibaut, L.M. & Connolly, S.R. (2013). Understanding
#' diversity-stability relationships: towards a unified model of
#' portfolio effects. Ecology Letters, 16, 140-150.

thibaut_connolly_dsr_gaku <- function(x, synchrony, z, overyielding) {
  cv_p <- cv(rowSums(x))
  n <- ncol(x)
  pe_inv <- sqrt(synchrony) * sqrt(n^((2 - z) * overyielding))
  cv1 <- cv_p / pe_inv
  list(cv_p = cv_p, cv_1 = cv1, pe = 1/pe_inv)
}