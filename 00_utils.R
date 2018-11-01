## Mikael Poul Johannesson
## August 2017

## Div utils functions

str_wrap <- function(string, width = 30) {
  paste0(strwrap(string, width), sep="", collapse="\n")
}


theme_descr <- function(...) {
  theme(text = element_text(size = 10,
                            colour = "black"),
        axis.text = element_text(size = 9,
                                 colour = "black"),
        axis.line = element_line(),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(1, "mm"),
        plot.margin = unit(c(1, 5, 1, 1), "mm"),
        panel.spacing.y = unit(2.5, "mm"),
        panel.spacing.x = unit(7.5, "mm"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 9, angle = 0, face = "bold.italic"),
        strip.text.y = element_text(size = 9, angle = 0, face = "bold.italic"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        ...)
}


geom_errorbar2 <- function(...) {
  p1 <- geom_errorbarh(aes(xmin = estimate - (2 * std.error),
                     xmax = estimate + (2 * std.error)),
                 size = .25, height = 0)
  p2 <- geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 size = .5, height = 0)
  p3 <- geom_point(size = 1)
  c(p1, p2, p3)
}



ggsave2 <- function(filename, ...) {
  for (format in c(".pdf", ".png")) ggsave(filename = paste0("output/", filename, format), ...)
}



## Functions for estimating difference in means
diff_effect <- function(data, post, ...,
                        subgroup = NULL,
                        subset = NULL,
                        cluster = NULL) {

  treat <- vapply(lazyeval::lazy_dots(...), function(x)
    as.character(x$expr), character(1))
  post <- deparse(substitute(post))

  out <- diff_effect_(data, post, treat, subgroup, subset, cluster)
  return(out)
}

diff_effect_ <- function(data, post, treat, subgroup, subset, cluster) {

  for (var in c(treat, subgroup))
    if (!is.factor(data[[var]])) data[[var]] <- factor(data[[var]])

  if (!is.null(subgroup)) {
    subgroup_values <- lapply(subgroup, function(x) levels(factor(data[[x]])))
    subgroup_values <- c(subgroup_values, list(treat))
    names(subgroup_values) <- c(subgroup, "treat")
    grid <- expand.grid(subgroup_values)
  } else {
    grid <- data_frame(treat = treat)
  }

  res <- lapply(1:nrow(grid), function(i) {

    component <- as.character(grid$treat[i])
    data_inner <- data

    if (!is.null(subgroup)) {
      for (j in 1:(ncol(grid) - 1)) {
        data_inner <- data_inner[as.character(data_inner[[names(grid)[j]]]) == as.character(grid[i, j]), ]
        if (nrow(data_inner) == 0) {
          warning(paste0("No rows in subgroup: ",
                         paste0(sapply(names(grid), function(x) paste0(x, "==", as.character(grid[[x]][i]))),
                                collapse = ", ")))
          return(NULL)
        }

      }
    }

    fit <- estimator_regression(formula = paste0(post, " ~ ", component),
                                data = data_inner,
                                cluster = cluster)

    baseline <- data_frame(term = levels(data[[component]])[1],
                           estimate = 0,
                           std.error = 0)

    estimate <- fit %>%
      filter(term != "(Intercept)") %>%
      bind_rows(baseline) %>%
      mutate(term = gsub(paste0("^", component), "", term),
             term = factor(term, levels = levels(factor(data_inner[[component]])))) %>%
      arrange(term) %>%
      mutate(term = as.character(term),
             treatment = component) %>%
      rename(value = term)

    if (!is.null(subgroup)) {
      for (j in 1:(ncol(grid) - 1)) {
        estimate[[names(grid)[j]]] <- as.character(grid[i, j])
      }
    }

    return(estimate)
  })

  res <- bind_rows(res) %>%
    mutate(treatment = factor(treatment, levels = unique(treatment)),
           value = factor(value, levels = unique(value)),
           value_order = row_number())
  return(res)
}

diff_diff_effect <- function(data, post, diff, ...,
                             subgroup = NULL,
                             subset = NULL,
                             cluster = NULL) {

  treat <- vapply(lazyeval::lazy_dots(...), function(x)
    as.character(x$expr), character(1))
  post <- deparse(substitute(post))

  out <- diff_diff_effect_(data, post, treat, diff, subgroup, subset, cluster)
  return(out)
}

diff_diff_effect_ <- function(data, post,
                              treat, diff,
                              subgroup, subset,
                              cluster) {

  for (var in c(treat, subgroup, diff))
    if (!is.factor(data[[var]])) data[[var]] <- factor(data[[var]])

  if (!is.null(subgroup)) {
    subgroup_values <- lapply(subgroup, function(x) levels(factor(data[[x]])))
    subgroup_values <- c(subgroup_values, list(treat))
    names(subgroup_values) <- c(subgroup, "treat")
    grid <- expand.grid(subgroup_values)
  } else {
    grid <- data_frame(treat = treat)
  }

  res <- lapply(1:nrow(grid), function(i) {

    component <- as.character(grid$treat[i])
    data_inner <- data

    if (!is.null(subgroup)) {
      for (j in 1:(ncol(grid) - 1)) {
        data_inner <- data_inner[as.character(data_inner[[names(grid)[j]]]) == as.character(grid[i, j]), ]
        if (nrow(data_inner) == 0) {
          warning(paste0("No rows in subgroup: ",
                         paste0(sapply(names(grid), function(x) paste0(x, "==", as.character(grid[[x]][i]))),
                                collapse = ", ")))
          return(NULL)
        }

      }
    }

    res_inner <- lapply(diff, function(diff_component) {

      formula <- paste0(post, " ~ ",
                        component, " + ",
                        diff_component, " + ",
                        component, ":", diff_component)

      fit <- estimator_regression(formula = formula,
                                  data = data_inner,
                                  cluster = cluster)

      baseline <- data_frame(term = levels(data_inner[[component]])[1],
                             diff_variable = diff_component,
                             diff_value = levels(data_inner[[diff_component]])[-1],
                             estimate = 0,
                             std.error = 0)

      estimate <- fit %>%
        filter(grepl("\\:", term)) %>%
        mutate(diff_variable = diff_component,
               diff_value = gsub("^.*\\:(.*)$", "\\1", term),
               diff_value = gsub(paste0("^", diff_component), "", diff_value),
               term = gsub("^(.*)\\:.*$", "\\1", term),
               term = gsub(paste0("^", component), "", term)) %>%
        bind_rows(baseline) %>%
        arrange(term) %>%
        mutate(term = as.character(term),
               treatment = component) %>%
        rename(value = term)

      if (!is.null(subgroup)) {
        for (j in 1:(ncol(grid) - 1)) {
          estimate[[names(grid)[j]]] <- as.character(grid[i, j])
        }
      }

      return(estimate)

    })
    res_inner <- bind_rows(res_inner)
    return(res_inner)

    })
  res <- bind_rows(res) %>%
    mutate(treatment = factor(treatment, levels = unique(treatment)),
           value = factor(value, levels = unique(value)),
           value_order = row_number())
  return(res)
}


estimator_mean <- function(post, treat, data,
                           subgroup = NULL) {

  data <- data[, c(post, treat, subgroup)]
  data <- na.omit(data)

  group_vars <- c(treat, subgroup)

  estimate <- data %>%
    rename_("post" = post) %>%
    gather_("treatment", "value", treat) %>%
    group_by_(.dots = c("treatment", "value", subgroup)) %>%
    summarize(estimate = mean(post),
              std.error = sd(post) / sqrt(n()),
              n.obs = n()) %>%
    ungroup()

  return(estimate)
}

estimator_regression <- function(formula, data,
                                 cluster = NULL) {

  if (is.character(formula)) formula <- as.formula(formula)

  all_vars <- all.vars(formula)
  if (!is.null(cluster)) all_vars <- c(all_vars, cluster)
  data <- data[, all_vars]
  data <- na.omit(data)


  fit <- lm(formula, data)

  if (!is.null(cluster)) {

    require(sandwich)
    require(lmtest)

    cl <- data[[cluster]]
    M <- length(unique(cl))
    N <- length(cl)

    dfc <- (M / (M - 1)) * ((N - 1) / (N - fit$rank))
    u <- apply(estfun(fit), 2, function(x) tapply(x, cl, sum))
    vcov_cl <- dfc * sandwich(fit, meat = crossprod(u) / N)

    out <- coeftest(fit, vcov_cl) %>%
      broom::tidy()
  } else {

    out <- broom::tidy(fit)
  }

  ## if (robust == FALSE & is.null(cluster)) {
  ##   se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(fit)) / (n - k))))
  ##   res <- cbind(coef(fit), se)
  ## }

  ## if (robust == TRUE) {
  ##   u <- matrix(resid(fit))
  ##   meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
  ##   dfc <- n / (n - k)
  ##   se <- sqrt(dfc * diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
  ##   res <- cbind(coef(fit), se)
  ## }

  ## if (!is.null(cluster)) {
  ##   clus <- cbind(X, data[[cluster]], resid(fit))
  ##   colnames(clus)[(dim(clus)[2] - 1):dim(clus)[2]] <- c(cluster, "resid")
  ##   m <- dim(table(clus[, cluster]))
  ##   dfc <- (m / (m - 1)) * ((n - 1) / (n - k))
  ##   uclust  <- apply(resid(fit) * X, 2, function(x) tapply(x, clus[, cluster], sum))
  ##   se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X))) * dfc)
  ##   res <- cbind(coef(fit), se)
  ## }

  ## res <- cbind(res, res[, 1] / res[, 2], (1 - pnorm(abs(res[, 1] / res[, 2]))) * 2)

  ## out <- as_data_frame(res)
  ## names(out) <- c("estimate", "std.error", "t.value", "p.value")
  ## out$term <- rownames(res)
  ## out <- out[, c(5, 1:4)]

  return(out)
}

