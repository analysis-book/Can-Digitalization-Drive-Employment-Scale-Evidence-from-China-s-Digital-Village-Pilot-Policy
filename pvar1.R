library(panelvar)
library(plm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel) 
library(readxl)
library(vars)
library(tsDyn)
library(IRdisplay)
library(knitr)

data <- read_excel("merged_data38.xlsx")
data <- data[order(data$CO_ID, data$YEAR), ]
data$CO_ID <- as.character(data$CO_ID)
data$YEAR <- as.integer(data$YEAR)

data$`Industrial Structure Upgrading`<- as.numeric(data$`Industrial Structure Upgrading`)
data$`Non-Agricultural Employment Scale`<- as.numeric(data$`Non-Agricultural Employment Scale`)
data$`Agricultural Technology Levels`<- as.numeric(data$`Agricultural Technology Levels`)



data <- data[, c("CO_ID", "YEAR", "Non-Agricultural Employment Scale","Industrial Structure Upgrading", "Agricultural Technology Levels")]

data <- pdata.frame(data,index=c("CO_ID", "YEAR"))
vars <- c("Industrial.Structure.Upgrading", "Agricultural.Technology.Levels", "Non.Agricultural.Employment.Scale")

unit_root_tests <- lapply(vars, function(var) {
  purtest(
    object = data[[var]],
    exo = "intercept",  
    test = "hadri",      # IPS
    lags = 1,       # AIC
  )
})
names(unit_root_tests) <- vars


lapply(names(unit_root_tests), function(var) {
  print(unit_root_tests[[var]])
})


results_list <- list()


for (var in vars) {
  test_types <- c("levinlin", "ips", "madwu", "Pm", "hadri")

  var_results <- list()
  
  for (test in test_types) {
    test_result <- purtest(
      object = data[[var]],
      exo = "intercept",  
      test = test,        
      lags = 1           
    )
    

    stat_value <- test_result$statistic$statistic
    p_value <- test_result$statistic$p.value
    

    significance <- case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
    
    stat_with_sig <- sprintf("%.3f%s", stat_value, significance)
    
    conclusion <- ifelse(p_value < 0.05, 
                         "Stationary (rejection of the unit root)", 
                         "Non-stationary (with a unit root)")
    
    var_results[[test]] <- data.frame(
      Variable = var,
      Test = test,
      Statistic = stat_with_sig,
      P.Value = sprintf("%.4f", p_value),
      Conclusion = conclusion,
      stringsAsFactors = FALSE
    )
  }
  
  results_list[[var]] <- bind_rows(var_results)
}

final_results <- bind_rows(results_list)

signif_note <- " *** p<0.01, ** p<0.05, * p<0.1"

kable(final_results, 
      caption = paste("Fisher test", signif_note),
      col.names = c("Variable", "Testing method", "Statistic", "P-value", "Conclusion"),
      align = c("l", "l", "r", "r", "l")) 


svar_model <- pvargmm(
  dependent_vars = vars,  
  lags = 1,               
  data = data,
  panel_identifier = c("CO_ID", "YEAR"),  
  transformation = "fd",  
  steps = "twostep",     
  system_instruments = TRUE,  
  collapse = TRUE         
)
stability_info <- stability(svar_model)
print(stability_info)
plot(stability_info)

summary(svar_model)

coef_matrices <- coef(svar_model)[,1:3] 


k <- 3
p<-1

companion_matrix <- coef_matrices


eigenvalues <- eigen(companion_matrix)$values

modulus <- Mod(eigenvalues)

eigen_df <- data.frame(
  Real = Re(eigenvalues),
  Imaginary = Im(eigenvalues),
  Modulus = modulus
) %>% arrange(desc(Modulus))  

eigen_plot <- ggplot(eigen_df, aes(x = Real, y = Imaginary)) +
  geom_point(aes(size = Modulus), color = "blue", alpha = 0.7) +
  geom_text_repel(aes(label = round(Modulus, 3)), size = 4, max.overlaps = 20) +

  annotate("path",
           x = cos(seq(0, 2*pi, length.out = 100)),
           y = sin(seq(0, 2*pi, length.out = 100)),
           color = "red", size = 1) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(aes(x = 0, y = 0), color = "black", size = 2) +

  xlim(-1.5, 1.5) +
  ylim(-1.5, 1.5) +

  coord_fixed() +

  labs(title = "PVAR",
       x = "real part",
       y = "inscriber",
       caption = "The red circle indicates the boundary of the unit circle") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(eigen_plot)

all_inside_unit_circle <- all(modulus < 1)
max_modulus <- max(modulus)

irf_results <- girf(svar_model,n.ahead = 5,ma_approx_steps=5)

coef_matrix <- coef(svar_model)[,1:3]

library(ggplot2)
library(patchwork)
set.seed(123)

monte_carlo_girf <- function(svar_model, n.ahead, ma_approx_steps, n_mc = 1000) {
  coef_matrix <- coef(svar_model)[, 1:3]
  sigma_matrix <- vcov(svar_model)
  var_names <- rownames(coef_matrix)
  #GIRF
  girf_array <- array(0, dim = c(n.ahead, length(var_names), n_mc))
  for (i in 1:n_mc) {
    coef_sample <- coef_matrix + 
      matrix(rnorm(9, mean = 0, sd = sqrt(diag(sigma_matrix))), nrow = 3)
    shock_sample <- rnorm(3, mean = 0, sd = sqrt(diag(sigma_matrix)))
    temp_model <- list(
      coefficients = coef_sample,
      sigma = sigma_matrix
    )
    class(temp_model) <- "pvar"
    # GIRF
    girf_array[, , i] <- custom_girf(temp_model, n.ahead, ma_approx_steps, shock_sample)
  }
  mean_girf <- apply(girf_array, c(1, 2), mean)
  lower_girf <- apply(girf_array, c(1, 2), quantile, probs = 0.025)
  upper_girf <- apply(girf_array, c(1, 2), quantile, probs = 0.975)
  dimnames(mean_girf) <- list(NULL, var_names)
  dimnames(lower_girf) <- list(NULL, var_names)
  dimnames(upper_girf) <- list(NULL, var_names)
  return(list(mean = mean_girf, lower = lower_girf, upper = upper_girf))
}

# GIRF
custom_girf <- function(model, n.ahead, ma_approx_steps, shock) {
  irf <- matrix(0, nrow = n.ahead, ncol = 3)
  irf[1, ] <- shock
  for (i in 2:n.ahead) {
    irf[i, ] <- model$coefficients %*% irf[i-1, ]
  }
  return(irf)
}

girf_results <- monte_carlo_girf(
  svar_model = svar_model,  
  n.ahead = 5,
  ma_approx_steps = 5,
  n_mc = 1000
)
plot_irf_matrix_clean <- function(results) {
  raw_vars <- colnames(results$mean)
  clean_vars <- gsub("^fd_", "", raw_vars)
  
  # 生成所有组合的子图
  plot_list <- list()
  
  for (i in seq_along(raw_vars)) {
    for (j in seq_along(raw_vars)) {
      df <- data.frame(
        Period = 1:nrow(results$mean),
        Mean = results$mean[, raw_vars[i]],  
        Lower = results$lower[, raw_vars[i]],
        Upper = results$upper[, raw_vars[i]]
      )
      
      y_limits <- c(
        min(df$Lower) - 0.1 * abs(min(df$Lower)),
        max(df$Upper) + 0.1 * abs(max(df$Upper))
      )
      
      p <- ggplot(df, aes(x = Period)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.7) +
        geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#4E79A7", alpha = 0.15) +
        geom_line(aes(y = Lower), color = "#4E79A7", linetype = "solid", size = 0.8, alpha = 0.8) +
        geom_line(aes(y = Upper), color = "#4E79A7", linetype = "solid", size = 0.8, alpha = 0.8) +
        geom_line(aes(y = Mean), color = "#E15759", size = 1.5) +
        geom_point(aes(y = Mean), color = "#E15759", size = 3, shape = 19) +
        geom_text(
          aes(y = Mean, label = sprintf("%.4f", Mean)),
          vjust = ifelse(df$Mean > 0, -1.5, 1.5),
          size = 3.5, color = "#E15759", fontface = "bold"
        ) +
        labs(
          title = paste("Response:", clean_vars[i], "\nShock:", clean_vars[j]),
          x = ifelse(i == length(raw_vars), "Period", ""),
          y = ifelse(j == 1, "Response", "")
        ) +
        scale_x_continuous(breaks = 1:nrow(df)) +
        coord_cartesian(ylim = y_limits) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          panel.grid.major = element_line(color = "grey92"),
          panel.background = element_rect(
            fill = ifelse(i == j, "#F7F7F7", "white"), 
            color = NA
          ),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      plot_list[[paste(i, j)]] <- p
    }
  }
  
  matrix_plot <- wrap_plots(plot_list, ncol = 3, nrow = 3) +
    plot_annotation(
      
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
      )
    )
  
  return(matrix_plot)
}

irf_matrix_clean <- plot_irf_matrix_clean(girf_results)
print(irf_matrix_clean)
ggsave("irf_matrix_upgradingtructure.png", plot = irf_matrix_clean, width = 19, height = 15, dpi = 500)

library(panelvar)
library(ggplot2)
library(patchwork)
library(abind)

monte_carlo_fevd <- function(pvar_model, n.ahead = 10, n_mc = 1000) {
  coef_matrix <- coef(pvar_model)[, 1:3]
  sigma_matrix <- vcov(pvar_model)
  var_names <- rownames(coef_matrix)
  fevd_array <- array(0, dim = c(n.ahead, length(var_names), length(var_names), n_mc))
  for (i in 1:n_mc) {
    coef_sample <- coef_matrix + 
      matrix(rnorm(9, mean = 0, sd = sqrt(diag(sigma_matrix))), nrow = 3)
    temp_model <- list(
      coefficients = coef_sample,
      sigma = sigma_matrix,
      lags = 1
    )
    class(temp_model) <- "pvar"
    fevd_result <- compute_fevd(temp_model, n.ahead)
    fevd_array[, , , i] <- fevd_result
  }
  mean_fevd <- apply(fevd_array, c(1, 2, 3), mean)
  lower_fevd <- apply(fevd_array, c(1, 2, 3), quantile, probs = 0.025)
  upper_fevd <- apply(fevd_array, c(1, 2, 3), quantile, probs = 0.975)
  
  dimnames(mean_fevd) <- list(NULL, var_names, var_names)
  dimnames(lower_fevd) <- list(NULL, var_names, var_names)
  dimnames(upper_fevd) <- list(NULL, var_names, var_names)
  
  return(list(mean = mean_fevd, lower = lower_fevd, upper = upper_fevd))
}

compute_fevd <- function(model, n.ahead) {
  A <- model$coefficients
  Sigma <- model$sigma
  n_vars <- ncol(A)
  fevd <- array(0, dim = c(n.ahead, n_vars, n_vars))
  
  Phi <- array(0, dim = c(n.ahead, n_vars, n_vars))
  Phi[1, , ] <- diag(n_vars)  # Phi0 = I
  
  for (h in 2:n.ahead) {
    Phi[h, , ] <- Phi[h-1, , ] %*% A
  }
  
  for (h in 1:n.ahead) {
    cum_effect <- matrix(0, n_vars, n_vars)
    for (j in 1:h) {
      cum_effect <- cum_effect + Phi[j, , ] %*% Sigma %*% t(Phi[j, , ])
    }
    
    for (i in 1:n_vars) {
      for (j in 1:n_vars) {
        num <- 0
        for (k in 1:h) {
          num <- num + (Phi[k, i, j] * sqrt(Sigma[j, j]))^2
        }
        denom <- cum_effect[i, i]
        fevd[h, i, j] <- num / denom
      }
    }

    for (i in 1:n_vars) {
      fevd[h, i, ] <- fevd[h, i, ] / sum(fevd[h, i, ]) * 100
    }
  }
  
  return(fevd)
}

plot_fevd_matrix_transposed <- function(fevd_results) {
  raw_vars <- dimnames(fevd_results$mean)[[2]]
  clean_vars <- gsub("^fd_", "", raw_vars) %>%
    gsub("\\.", " ", .)
  plot_list <- list()
  for (shock_idx in seq_along(raw_vars)) {
    for (response_idx in seq_along(raw_vars)) {
      df <- data.frame(
        Horizon = 1:dim(fevd_results$mean)[1],
        Mean = fevd_results$mean[, response_idx, shock_idx],
        Lower = fevd_results$lower[, response_idx, shock_idx],
        Upper = fevd_results$upper[, response_idx, shock_idx]
      )
      p <- ggplot(df, aes(x = Horizon, y = Mean)) +
        geom_ribbon(aes(ymin = pmax(Lower, 0), ymax = pmin(Upper, 100)), 
                    fill = "#4E79A7", alpha = 0.2) +
        geom_line(color = "#E15759", size = 1.2) +
        geom_point(color = "#E15759", size = 2.5) +
        scale_y_continuous(limits = c(0, 100), 
                           labels = scales::percent_format(scale = 1)) +
        labs(
          title = paste("Shock:", clean_vars[shock_idx], "\nResponse:", clean_vars[response_idx]),
          x = ifelse(shock_idx == length(raw_vars), "Period", ""),
          y = ifelse(response_idx == 1, "Contribution percentage", "")
        ) +
        scale_x_continuous(breaks = unique(df$Horizon)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          panel.grid.major = element_line(color = "grey92"),
          panel.background = element_rect(
            fill = ifelse(shock_idx == response_idx, "#F7F7F7", "white"), 
            color = NA
          ),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      last_point <- df[nrow(df), ]
      p <- p + 
        geom_text(
          data = last_point,
          aes(label = sprintf("%.1f%%", Mean)),
          vjust = -1, size = 3.0, color = "#E15759"
        )
      
      plot_list[[paste(shock_idx, response_idx)]] <- p
    }
  }
  
  matrix_plot <- wrap_plots(plot_list, ncol = length(raw_vars), nrow = length(raw_vars)) +
    plot_annotation(
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
      )
    )
  
  return(matrix_plot)
}

fevd_results <- monte_carlo_fevd(
  pvar_model = svar_model, 
  n.ahead = 5,             
  n_mc = 10000              
)


fevd_matrix_plot <- plot_fevd_matrix_transposed(fevd_results)

ggsave("fevd_matrix.png", plot = fevd_matrix_plot, width = 20, height = 15, dpi = 500)
