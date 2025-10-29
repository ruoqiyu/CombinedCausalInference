

# Perform genetic matching
m.out <- matchit(
  A ~ X1 + X2 + X3 + X4 + X5 + X6,
  data = dat.t,
  method = "genetic",
  distance = "mahalanobis",  # or "glm" if you prefer propensity scores
  replace = FALSE,           # ensures pair matching
  ratio = 1#,                  # one-to-one matching
  #pop.size = 2000
)

# View summary of matching
summary(m.out)

# Extract matched data
matched.data <- match.data(m.out)



# Extract treated RCT covariates (X1 to X5)
matched_avgs = apply(matched.data[, c("X1", "X2", "X3", "X4", "X5")],2,
	function(x){ aggregate(x, by=list(matched.data$subclass), mean) })
matched_avgs = sapply(matched_avgs, function(x) x[,2])
X.rct <- data.frame(matched_avgs) #dat.t[dat.t$A == 1, c("X1", "X2", "X3", "X4", "X5")]
n_rct <- nrow(X.rct)


# Target moments from observational data
X.os <- dat.os[dat.os$A==1, c("X1", "X2", "X3", "X4", "X5")]
avg_covariates <- colMeans(X.os)
second_moment <- colMeans(X.os^2)

# Two-way interactions
interaction_terms <- combn(names(X.os), 2, function(pair) {
  X.os[[pair[1]]] * X.os[[pair[2]]]
})
interaction_avg <- colMeans(interaction_terms)

# Create interaction terms for RCT data
interaction_rct <- combn(names(X.rct), 2, function(pair) {
  X.rct[[pair[1]]] * X.rct[[pair[2]]]
})

# Combine all moment constraints
moment_targets <- c(avg_covariates, second_moment, interaction_avg)
moment_matrix <- cbind(as.matrix(X.rct), X.rct^2, interaction_rct)

# Objective: negative entropy
entropy_obj <- function(w) {
  sum(w * log(w + 1e-20))  # small constant to avoid log(0)
}

# Gradient of the objective
entropy_grad <- function(w) {
  log(w + 1e-20) + 1
}

# Equality constraints: weighted moments match targets
eval_g_eq <- function(w) {
  c(sum(w) - 1, as.vector(t(moment_matrix) %*% w - moment_targets))
}

# Jacobian of constraints
eval_jac_g_eq <- function(w) {
  t(cbind(rep(1, n_rct), moment_matrix))
}

# Initial weights
w0 <- rep(1/n_rct, n_rct)

# Optimization
result <- nloptr(
  x0 = w0,
  eval_f = entropy_obj,
  eval_grad_f = entropy_grad,
  eval_g_eq = eval_g_eq,
  eval_jac_g_eq = eval_jac_g_eq,
  opts = list(
    algorithm = "NLOPT_LD_SLSQP",
    xtol_rel = 1e-6,
    maxeval = 1000
  ),
  lb = rep(0, n_rct),
  ub = rep(1, n_rct)
)

# Extract optimized weights
w.opt <- result$solution
head(w.opt)


# Join X and Y on 'subclass'
joined_data <- merge(matched.data, data.frame(subclass=as.factor(1:n_rct), ebal_weight=w.opt), by = "subclass", all.x = TRUE)

# Reweighted estimate
est_weightingbased = sum((joined_data$Y*joined_data$ebal_weight)[joined_data$A==1]) - sum((joined_data$Y*joined_data$ebal_weight)[joined_data$A==0])

##################################################

# Create combined dataset with indicator for RCT inclusion
dat.t$in_rct <- 1
dat.os$in_rct <- 0

# Combine datasets
dat.all <- rbind(dat.t[, c("X1", "X2", "X3", "X4", "X5", "X6", "in_rct")],
                 dat.os[, c("X1", "X2", "X3", "X4", "X5", "X6", "in_rct")])

# Fit random forest to predict inclusion
rf_inclusion <- randomForest(as.factor(in_rct) ~ X1 + X2 + X3 + X4 + X5 + X6,
                             data = dat.all,
                             ntree = 500,
                             importance = TRUE)

# Predict probabilities of inclusion

# Predict on RCT covariates (X1 to X6)
X.rct <- dat.t[, c("X1", "X2", "X3", "X4", "X5", "X6")]

# Generate predictions
#pred.rct <- predict(rf.model, newdata = X.rct)

inclusion_probs <- predict(rf_inclusion, newdata = X.rct, type = "prob")[,2]

# Fit random forest to predict treatment in OS
rf_treatment_os <- randomForest(as.factor(A) ~ X1 + X2 + X3 + X4 + X5,
                                data = dat.os,
                                ntree = 500,
                                importance = TRUE)

# Predict probabilities of treatment
treatment_probs_os <- predict(rf_treatment_os, newdata = X.rct, type = "prob")[,2]

entire_nums_wgts = treatment_probs_os*(1-inclusion_probs)/inclusion_probs

est_psbased = 2/( sum(entire_nums_wgts) )* (sum((entire_nums_wgts*dat.t$Y)[dat.t$A==1]) - sum((entire_nums_wgts*dat.t$Y)[dat.t$A==0]))

