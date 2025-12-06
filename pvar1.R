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
setwd(dir ="E:/研究生论文/数字乡村试点政策对脱贫地区就业规模的影响研究/数据")

data <- read_excel("merged_data30.xlsx")
data <- data[order(data$地区名称, data$YEAR), ]

# 构建面板数据
# 确保 id 和 year 是因子或整数类型
data$地区名称 <- as.character(data$地区名称)
data$YEAR <- as.integer(data$YEAR)
# 确保变量为数值类型
data$就业规模 <- as.numeric(data$就业规模)
data$`Industrial Structure Upgrading`<- as.numeric(data$`Industrial Structure Upgrading`)
data$`Non-Agricultural Employment Scale`<- as.numeric(data$`Non-Agricultural Employment Scale`)
#data$数字乡村指数 <- as.numeric(data$数字乡村指数)
data$`Agricultural Technology Levels`<- as.numeric(data$`Agricultural Technology Levels`)
data$泰尔系数 <- as.numeric(data$泰尔系数)
data$基尼系数 <- as.numeric(data$基尼系数)
data$阿特金森指数 <- as.numeric(data$阿特金森指数)
data$产业结构升级2 <- as.numeric(data$产业结构升级2)
data$县域数字化 <- as.numeric(data$县域数字化)
data$产业发展7 <- as.numeric(data$产业发展7)
data$digitization_level<- as.numeric(data$digitization_level)
# 提取所需列
data <- data[, c("地区名称", "YEAR", "就业规模","Non-Agricultural Employment Scale","digitization_level", "did", "县域数字化","泰尔系数","基尼系数","阿特金森指数","Industrial Structure Upgrading","产业发展7", "Agricultural Technology Levels")]
data <- data[, c("地区名称", "YEAR", "Non-Agricultural Employment Scale","Industrial Structure Upgrading", "Agricultural Technology Levels")]

data <- pdata.frame(data,index=c("地区名称", "YEAR"))
vars <- c("Industrial.Structure.Upgrading", "Agricultural.Technology.Levels", "Non.Agricultural.Employment.Scale")

## 2.2 单位根检验（确保变量平稳）
#vars <- c("x1", "x2", "x3")  # 待检验的变量
unit_root_tests <- lapply(vars, function(var) {
  purtest(
    object = data[[var]],
    exo = "intercept",  # 含截距项（根据变量趋势可改为"trend"）
    test = "hadri",      # IPS检验（适合异质性面板）
    lags = 1,       # AIC准则选滞后阶数
  )
})
names(unit_root_tests) <- vars

# 打印单位根检验结果
cat("\n单位根检验结果：\n")
lapply(names(unit_root_tests), function(var) {
  cat("\n", var, "的hadri检验：\n", sep = "")
  print(unit_root_tests[[var]])
})


# 初始化结果存储列表
results_list <- list()

# 循环遍历每个变量
for (var in vars) {
  # 循环遍历每种检验方法
  test_types <- c("levinlin", "ips", "madwu", "Pm", "hadri")
  
  # 存储当前变量的所有检验结果
  var_results <- list()
  
  for (test in test_types) {
    # 执行面板单位根检验
    test_result <- purtest(
      object = data[[var]],
      exo = "intercept",  # 含截距项 (根据情况可改为"trend")
      test = test,        # 当前检验方法
      lags = 1            # 滞后阶数 (推荐使用"AIC"自动选择)
    )
    
    # 提取检验结果
    stat_value <- test_result$statistic$statistic
    p_value <- test_result$statistic$p.value
    
    # 添加显著性星号
    significance <- case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
    
    # 组合统计量和显著性
    stat_with_sig <- sprintf("%.3f%s", stat_value, significance)
    
    # 添加单个方法的结论
    conclusion <- ifelse(p_value < 0.05, 
                         "平稳 (拒绝单位根)", 
                         "不平稳 (存在单位根)")
    
    # 存储结果
    var_results[[test]] <- data.frame(
      Variable = var,
      Test = test,
      Statistic = stat_with_sig,
      P.Value = sprintf("%.4f", p_value),
      Conclusion = conclusion,
      stringsAsFactors = FALSE
    )
  }
  
  # 合并当前变量的所有检验结果
  results_list[[var]] <- bind_rows(var_results)
}

# 合并所有结果
final_results <- bind_rows(results_list)

# 添加显著性说明
signif_note <- "显著性标记: *** p<0.01, ** p<0.05, * p<0.1"

# 打印美观的对比表格
kable(final_results, 
      caption = paste("面板单位根检验结果比较 (Fisher型检验)\n", signif_note),
      col.names = c("变量", "检验方法", "统计量", "P值", "结论"),
      align = c("l", "l", "r", "r", "l")) 

# 3. 面板SVAR模型估计
## 3.1 模型设定（x1、x2、x3均为内生变量）
svar_model <- pvargmm(
  dependent_vars = vars,  # 内生变量：x1、x2、x3
  lags = 1,               # 滞后阶数（8年数据适合1阶）
  data = data,
  panel_identifier = c("地区名称", "YEAR"),  # 面板标识
  transformation = "fd",  # 一阶差分（进一步确保平稳性）
  steps = "twostep",      # 两步GMM估计
  system_instruments = TRUE,  # 系统GMM（利用更多工具变量）
  collapse = TRUE         # 压缩工具变量矩阵（适合大截面）
)
stability_info <- stability(svar_model)
print(stability_info)
plot(stability_info)
# 查看模型估计结果
cat("\n面板SVAR模型估计结果：\n")
summary(svar_model)

coef_matrices <- coef(svar_model)[,1:3] 

# 确定变量数量
k <- 3
p<-1
# 创建空的动态矩阵 (kp × kp)
companion_matrix <- coef_matrices

# 3. 计算特征值
eigenvalues <- eigen(companion_matrix)$values

# 计算特征值的模（距离原点的距离）
modulus <- Mod(eigenvalues)

# 创建特征值数据框
eigen_df <- data.frame(
  Real = Re(eigenvalues),
  Imaginary = Im(eigenvalues),
  Modulus = modulus
) %>% arrange(desc(Modulus))  # 按模大小排序

# 4. 绘制特征值单位圆图
eigen_plot <- ggplot(eigen_df, aes(x = Real, y = Imaginary)) +
  geom_point(aes(size = Modulus), color = "blue", alpha = 0.7) +
  geom_text_repel(aes(label = round(Modulus, 3)), size = 4, max.overlaps = 20) +
  # 绘制单位圆
  annotate("path",
           x = cos(seq(0, 2*pi, length.out = 100)),
           y = sin(seq(0, 2*pi, length.out = 100)),
           color = "red", size = 1) +
  # 添加坐标轴和原点
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(aes(x = 0, y = 0), color = "black", size = 2) +
  # 设置坐标轴范围
  xlim(-1.5, 1.5) +
  ylim(-1.5, 1.5) +
  # 设置坐标轴比例相等
  coord_fixed() +
  # 添加标题和标签
  labs(title = "PVAR模型稳定性检验 (p=1)：特征值单位圆图",
       x = "实部",
       y = "虚部",
       caption = "红色圆圈表示单位圆边界") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# 显示图形
print(eigen_plot)

# 5. 检查所有特征值是否在单位圆内
all_inside_unit_circle <- all(modulus < 1)
max_modulus <- max(modulus)

cat("\n===== PVAR模型稳定性检验结果 (p=1) =====\n")
cat("变量数量: k =", k, "\n")
cat("特征值总数: ", length(eigenvalues), "\n")
cat("最大特征值模: ", round(max_modulus, 4), "\n")


# 4. 脉冲响应分析（IRF）：分析变量间的动态影响
## 4.1 计算所有变量间的脉冲响应
irf_results <- girf(svar_model,n.ahead = 5,ma_approx_steps=5)

coef_matrix <- coef(svar_model)[,1:3]

library(ggplot2)
library(patchwork)
set.seed(123)
# 蒙特卡洛抽样函数
monte_carlo_girf <- function(svar_model, n.ahead, ma_approx_steps, n_mc = 1000) {
  # 正确提取系数矩阵 (3x3)
  coef_matrix <- coef(svar_model)[, 1:3]
  
  # 提取残差协方差矩阵 (3x3)
  sigma_matrix <- vcov(svar_model)
  
  # 获取变量名
  var_names <- rownames(coef_matrix)
  
  # 存储GIRF结果
  girf_array <- array(0, dim = c(n.ahead, length(var_names), n_mc))
  
  # 蒙特卡洛模拟
  for (i in 1:n_mc) {
    # 从渐近分布中抽样系数
    coef_sample <- coef_matrix + 
      matrix(rnorm(9, mean = 0, sd = sqrt(diag(sigma_matrix))), nrow = 3)
    
    # 从残差分布中抽样冲击
    shock_sample <- rnorm(3, mean = 0, sd = sqrt(diag(sigma_matrix)))
    
    # 创建临时PVAR模型对象
    temp_model <- list(
      coefficients = coef_sample,
      sigma = sigma_matrix
    )
    class(temp_model) <- "pvar"
    
    # 计算GIRF (需要自定义GIRF计算函数)
    girf_array[, , i] <- custom_girf(temp_model, n.ahead, ma_approx_steps, shock_sample)
  }
  
  # 计算统计量
  mean_girf <- apply(girf_array, c(1, 2), mean)
  lower_girf <- apply(girf_array, c(1, 2), quantile, probs = 0.025)
  upper_girf <- apply(girf_array, c(1, 2), quantile, probs = 0.975)
  
  dimnames(mean_girf) <- list(NULL, var_names)
  dimnames(lower_girf) <- list(NULL, var_names)
  dimnames(upper_girf) <- list(NULL, var_names)
  
  return(list(mean = mean_girf, lower = lower_girf, upper = upper_girf))
}

# 自定义GIRF计算函数
custom_girf <- function(model, n.ahead, ma_approx_steps, shock) {
  # 简化实现 - 实际应用中需要完整的GIRF计算逻辑
  # 这里使用近似线性响应作为示例
  irf <- matrix(0, nrow = n.ahead, ncol = 3)
  irf[1, ] <- shock
  for (i in 2:n.ahead) {
    irf[i, ] <- model$coefficients %*% irf[i-1, ]
  }
  return(irf)
}

# 使用示例
girf_results <- monte_carlo_girf(
  svar_model = svar_model,  # 已估计的PVAR模型
  n.ahead = 5,
  ma_approx_steps = 5,
  n_mc = 1000
)
plot_irf_matrix_clean <- function(results) {
  # 获取并清洗变量名
  raw_vars <- colnames(results$mean)
  clean_vars <- gsub("^fd_", "", raw_vars)
  
  # 生成所有组合的子图
  plot_list <- list()
  
  for (i in seq_along(raw_vars)) {
    for (j in seq_along(raw_vars)) {
      # 准备数据（修正此处语法错误）
      df <- data.frame(
        Period = 1:nrow(results$mean),
        Mean = results$mean[, raw_vars[i]],  # 移除了多余的"]"
        Lower = results$lower[, raw_vars[i]],
        Upper = results$upper[, raw_vars[i]]
      )
      
      # 计算y轴范围（当前子图）
      y_limits <- c(
        min(df$Lower) - 0.1 * abs(min(df$Lower)),
        max(df$Upper) + 0.1 * abs(max(df$Upper))
      )
      
      # 创建子图
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
  
  # 组合成3×3矩阵
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
ggsave("irf_matrix_产业结构升级.png", plot = irf_matrix_clean, width = 19, height = 15, dpi = 500)
# 显示矩阵图（建议在宽屏设备或保存为图片查看）

#5.方差分解图
library(panelvar)
library(ggplot2)
library(patchwork)
library(abind)

# 蒙特卡洛方差分解函数
monte_carlo_fevd <- function(pvar_model, n.ahead = 10, n_mc = 1000) {
  # 提取模型信息
  coef_matrix <- coef(pvar_model)[, 1:3]
  sigma_matrix <- vcov(pvar_model)
  var_names <- rownames(coef_matrix)
  
  # 存储FEVD结果
  fevd_array <- array(0, dim = c(n.ahead, length(var_names), length(var_names), n_mc))
  
  # 蒙特卡洛模拟
  for (i in 1:n_mc) {
    # 从渐近分布中抽样系数
    coef_sample <- coef_matrix + 
      matrix(rnorm(9, mean = 0, sd = sqrt(diag(sigma_matrix))), nrow = 3)
    
    # 创建临时PVAR模型对象
    temp_model <- list(
      coefficients = coef_sample,
      sigma = sigma_matrix,
      lags = 1
    )
    class(temp_model) <- "pvar"
    
    # 计算方差分解 (FEVD)
    fevd_result <- compute_fevd(temp_model, n.ahead)
    fevd_array[, , , i] <- fevd_result
  }
  
  # 计算统计量
  mean_fevd <- apply(fevd_array, c(1, 2, 3), mean)
  lower_fevd <- apply(fevd_array, c(1, 2, 3), quantile, probs = 0.025)
  upper_fevd <- apply(fevd_array, c(1, 2, 3), quantile, probs = 0.975)
  
  dimnames(mean_fevd) <- list(NULL, var_names, var_names)
  dimnames(lower_fevd) <- list(NULL, var_names, var_names)
  dimnames(upper_fevd) <- list(NULL, var_names, var_names)
  
  return(list(mean = mean_fevd, lower = lower_fevd, upper = upper_fevd))
}

# 自定义方差分解计算函数
compute_fevd <- function(model, n.ahead) {
  # 提取系数矩阵和协方差矩阵
  A <- model$coefficients
  Sigma <- model$sigma
  n_vars <- ncol(A)
  
  # 初始化FEVD矩阵
  fevd <- array(0, dim = c(n.ahead, n_vars, n_vars))
  
  # 计算移动平均系数
  Phi <- array(0, dim = c(n.ahead, n_vars, n_vars))
  Phi[1, , ] <- diag(n_vars)  # Phi0 = I
  
  # 递归计算Phi矩阵
  for (h in 2:n.ahead) {
    Phi[h, , ] <- Phi[h-1, , ] %*% A
  }
  
  # 计算方差分解
  for (h in 1:n.ahead) {
    # 计算累计冲击
    cum_effect <- matrix(0, n_vars, n_vars)
    for (j in 1:h) {
      cum_effect <- cum_effect + Phi[j, , ] %*% Sigma %*% t(Phi[j, , ])
    }
    
    # 计算每个冲击的贡献
    for (i in 1:n_vars) {
      for (j in 1:n_vars) {
        # 计算冲击j对变量i的贡献
        num <- 0
        for (k in 1:h) {
          num <- num + (Phi[k, i, j] * sqrt(Sigma[j, j]))^2
        }
        denom <- cum_effect[i, i]
        fevd[h, i, j] <- num / denom
      }
    }
    
    # 标准化为百分比
    for (i in 1:n_vars) {
      fevd[h, i, ] <- fevd[h, i, ] / sum(fevd[h, i, ]) * 100
    }
  }
  
  return(fevd)
}

# 方差分解绘图函数
plot_fevd_matrix_transposed <- function(fevd_results) {
  # 获取并清洗变量名
  raw_vars <- dimnames(fevd_results$mean)[[2]]
  clean_vars <- gsub("^fd_", "", raw_vars) %>%
    gsub("\\.", " ", .)
  
  # 生成所有组合的子图
  plot_list <- list()
  
  # 转置循环顺序：先冲击变量（行），再响应变量（列）
  for (shock_idx in seq_along(raw_vars)) {
    for (response_idx in seq_along(raw_vars)) {
      # 准备数据
      df <- data.frame(
        Horizon = 1:dim(fevd_results$mean)[1],
        Mean = fevd_results$mean[, response_idx, shock_idx],
        Lower = fevd_results$lower[, response_idx, shock_idx],
        Upper = fevd_results$upper[, response_idx, shock_idx]
      )
      
      # 创建子图
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
      
      # 添加贡献值标签（仅最后时期）
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
  
  # 组合成3×3矩阵（行=冲击，列=响应）
  matrix_plot <- wrap_plots(plot_list, ncol = length(raw_vars), nrow = length(raw_vars)) +
    plot_annotation(
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
      )
    )
  
  return(matrix_plot)
}
# ================== 使用示例 ==================

# 假设 pvar_model 是已估计的PVAR模型
# 进行蒙特卡洛方差分解
fevd_results <- monte_carlo_fevd(
  pvar_model = svar_model,  # 替换为您的PVAR模型
  n.ahead = 5,             # 预测期数
  n_mc = 10000                # 蒙特卡洛抽样次数（建议至少500次）
)

# 生成3×3矩阵图
fevd_matrix_plot <- plot_fevd_matrix_transposed(fevd_results)

# 显示矩阵图（建议在宽屏设备查看）
print(fevd_matrix_plot)
ggsave("fevd_matrix_产业结构升级1.png", plot = fevd_matrix_plot, width = 20, height = 15, dpi = 500)
