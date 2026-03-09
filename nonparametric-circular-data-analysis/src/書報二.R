
library(circular)

##  desert ants, Set 1
data(fisherB10)
angles_deg <- fisherB10$set1

theta <- circular(
  angles_deg,
  units    = "degrees",
  template = "geographics",   
  modulo   = "2pi"
)

n <- length(theta)


C <- sum(cos(theta))
S <- sum(sin(theta))
R <- sqrt(C^2 + S^2)
Rbar <- R / n

mu_hat   <- mean.circular(theta)
mu_hat_d <- conversion.circular(mu_hat, type = "angles", units = "degrees")

med_hat   <- median.circular(theta)
med_hat_d <- conversion.circular(med_hat, type = "angles", units = "degrees")

C2   <- mean(cos(2 * theta))
S2   <- mean(sin(2 * theta))
rho2 <- sqrt(C2^2 + S2^2)


S_hat <- (1 - rho2) / (2 * Rbar^2)

cat("n        =", n, "\n")
cat("Mean dir =", round(mu_hat_d, 1), "degrees\n")
cat("Median   =", round(med_hat_d, 1), "degrees\n")
cat("Rbar     =", round(Rbar, 3), "\n")
cat("Disp S   =", round(S_hat, 3), "\n")



old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))

deg2rad <- function(x) x * pi / 180
mean_rad_plot <- deg2rad(90 - mu_hat_d)   
med_rad_plot  <- deg2rad(90 - med_hat_d)

plot(
  theta,
  stack  = TRUE,
  shrink = 1.1,
  pch    = 16,
  col    = "grey30",
  main   = "Desert ants (Set 1)\nRaw data on circle"
)

rho_line <- 1.0  


arrows(
  x0 = 0, y0 = 0,
  x1 = rho_line * cos(mean_rad_plot),
  y1 = rho_line * sin(mean_rad_plot),
  lwd = 2, col = "red"
)

arrows(
  x0 = 0, y0 = 0,
  x1 = rho_line * cos(med_rad_plot),
  y1 = rho_line * sin(med_rad_plot),
  lwd = 2, col = "blue", lty = 2
)

legend(
  "topleft",
  legend = c(
    paste0("Mean = ",   round(mu_hat_d, 1), "°"),
    paste0("Median = ", round(med_hat_d, 1), "°")
  ),
  col = c("red", "blue"),
  lty = c(1, 2),
  bty = "n",
  cex = 0.8
)


rose.diag(
  theta,
  bins   = 12,
  col    = "grey80",
  border = "grey40",
  main   = "Desert ants (Set 1)\nRose diagram"
)

usr   <- par("usr")
r_max <- min(abs(usr[1:2]), abs(usr[3:4]))


lines(
  x = c(0, r_max * cos(mean_rad_plot)),
  y = c(0, r_max * sin(mean_rad_plot)),
  lwd = 2, col = "red"
)


lines(
  x = c(0, r_max * cos(med_rad_plot)),
  y = c(0, r_max * sin(med_rad_plot)),
  lwd = 2, col = "blue", lty = 2
)

legend(
  "topleft",
  legend = c(
    paste0("Mean = ",   round(mu_hat_d, 1), "°"),
    paste0("Median = ", round(med_hat_d, 1), "°")
  ),
  col = c("red", "blue"),
  lty = c(1, 2),
  bty = "n",
  cex = 0.8
)

par(old_par)






# ===========






library(circular)

data(fisherB10)
angles_deg <- fisherB10$set1     

theta <- circular(
  angles_deg,
  units    = "degrees",
  template = "geographics",      
  modulo   = "2pi"
)
n <- length(theta)



med_hat   <- median.circular(theta)
med_hat_d <- conversion.circular(med_hat, type = "angles", units = "degrees")


m <- 2

CI_level <- sum(dbinom(m:(n-m), size = n, prob = 0.5))

cat("n        =", n, "\n")
cat("Median   =", round(med_hat_d, 1), "degrees\n")
cat("m        =", m, "\n")
cat("Exact CI level ≈", round(CI_level, 4), " (約 ",
    round(100 * CI_level, 1), "%)\n\n")



rel <- ((angles_deg - med_hat_d + 180) %% 360) - 180


left  <- sort(rel[rel < 0], decreasing = TRUE)
right <- sort(rel[rel > 0], decreasing = FALSE)

if (length(left) < m || length(right) < m) {
  stop("這個 m 使得左右某一邊樣本數不足")
}

L_rel <- left[m]       
U_rel <- right[m]     


theta_L <- (med_hat_d + L_rel + 360) %% 360
theta_U <- (med_hat_d + U_rel + 360) %% 360

cat("theta_(L_m) =", round(theta_L, 1), "degrees\n")
cat("theta_(U_m) =", round(theta_U, 1), "degrees\n")


old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 1))
deg2rad <- function(x) x * pi / 180
to_plot_rad <- function(deg_geo) deg2rad(90 - deg_geo)

med_rad_plot   <- to_plot_rad(med_hat_d)
thetaL_rad_plot <- to_plot_rad(theta_L)
thetaU_rad_plot <- to_plot_rad(theta_U)

plot(
  theta,
  stack  = TRUE,
  shrink = 1.1,
  pch    = 16,
  col    = "grey30",
  main   = "Desert ants (Set 1)\nMedian CI on circle"
)

ang_seq_deg  <- seq(theta_L, theta_U, length.out = 200)
ang_seq_rad  <- to_plot_rad(ang_seq_deg)

r_ci <- 1.0 
x_ci <- c(0, r_ci * cos(ang_seq_rad), 0)
y_ci <- c(0, r_ci * sin(ang_seq_rad), 0)

polygon(x_ci, y_ci,
        col = "grey85", border = NA)

arrows(
  x0 = 0, y0 = 0,
  x1 = r_ci * cos(med_rad_plot),
  y1 = r_ci * sin(med_rad_plot),
  lwd = 2, col = "blue", lty = 2
)

legend(
  "topleft",
  legend = c(
    bquote("Median" == .(round(med_hat_d, 1))*"°"),
    bquote("CI level " %~~% .(round(100 * CI_level, 1))*"%"),
    bquote("(" *theta[.(paste0("L[", m, "]"))]*","*
             theta[.(paste0("U[", m, "]"))]* ")")
  ),
  bty = "n", col = c("blue", NA, NA),
  lty = c(2, 0, 0), cex = 0.8
)

par(old_par)







# =========












library(circular)

data(fisherB10)
angles_deg <- fisherB10$set1       

theta <- circular(
  angles_deg,
  units    = "degrees",
  template = "geographics",        
  modulo   = "2pi"
)
n <- length(theta)

med_hat   <- median.circular(theta)
med_hat_d <- conversion.circular(med_hat, type = "angles", units = "degrees")


m <- 2
CI_level <- sum(dbinom(m:(n - m), size = n, prob = 0.5))

rel <- ((angles_deg - med_hat_d + 180) %% 360) - 180

left  <- sort(rel[rel <  0], decreasing = TRUE)  
right <- sort(rel[rel >  0], decreasing = FALSE) 

if (length(left) < m || length(right) < m) {
  stop("這個 m 使得左右某一邊樣本數不足")
}

L_rel <- left[m]
U_rel <- right[m]

theta_L <- (med_hat_d + L_rel + 360) %% 360   # theta_(L_m)
theta_U <- (med_hat_d + U_rel + 360) %% 360   # theta_(U_m)

cat("n        =", n, "\n")
cat("Median   =", round(med_hat_d, 1), " degrees\n")
cat("CI level ≈", round(100 * CI_level, 1), "%\n")
cat("theta_(L_", m, ") =", round(theta_L, 1), " degrees\n", sep = "")
cat("theta_(U_", m, ") =", round(theta_U, 1), " degrees\n\n", sep = "")


mu0_deg <- 45

mu0_rel <- ((mu0_deg - med_hat_d + 180) %% 360) - 180
inside_CI <- (L_rel < mu0_rel) && (mu0_rel < U_rel)

cat("Hypothesized median mu0 =", mu0_deg, " degrees\n")
cat("mu0 is", ifelse(inside_CI, "INSIDE", "OUTSIDE"),
    "the CI (L_rel, U_rel) = (",
    round(L_rel, 1), ",", round(U_rel, 1), ")\n\n")

old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 1))

deg2rad <- function(x) x * pi / 180
to_plot_rad <- function(deg_geo) deg2rad(90 - deg_geo)

med_rad_plot   <- to_plot_rad(med_hat_d)
thetaL_rad_plot <- to_plot_rad(theta_L)
thetaU_rad_plot <- to_plot_rad(theta_U)
mu0_rad_plot   <- to_plot_rad(mu0_deg)

plot(
  theta,
  stack  = FALSE,
  shrink = 1.1,
  pch    = 16,
  col    = "grey30",
  main   = "Desert ants (Set 1)\nMedian CI and test of hypothesized median"
)

r_ci <- 1.0


ang_seq_deg <- seq(theta_L, theta_U, length.out = 200)
ang_seq_rad <- to_plot_rad(ang_seq_deg)

x_ci <- c(0, r_ci * cos(ang_seq_rad), 0)
y_ci <- c(0, r_ci * sin(ang_seq_rad), 0)

polygon(x_ci, y_ci, col = "grey85", border = NA)

arrows(
  x0 = 0, y0 = 0,
  x1 = r_ci * cos(med_rad_plot),
  y1 = r_ci * sin(med_rad_plot),
  lwd = 2, col = "blue", lty = 2
)

arrows(
  x0 = 0, y0 = 0,
  x1 = r_ci * cos(mu0_rad_plot),
  y1 = r_ci * sin(mu0_rad_plot),
  lwd = 2, col = "red"
)

legend(
  "topleft",
  legend = c(
    bquote("Sample median" == .(round(med_hat_d, 1))*"°"),
    bquote("CI level " %~~% .(round(100 * CI_level, 1))*"%"),
    bquote("H[0]:  "~tilde(mu)==.(mu0_deg)*"°"),
    if (inside_CI) "Fail to reject H[0]" else "Reject H[0]"
  ),
  col = c("blue", NA, "red", NA),
  lty = c(2, 0, 1, 0),
  bty = "n",
  cex = 0.8
)

par(old_par)









# =============











library(circular)

data(fisherB10)
angles_deg <- fisherB10$set1  

theta_circ <- circular(
  angles_deg,
  units    = "degrees",
  template = "geographics",    
  modulo   = "2pi"
)
n <- length(theta_circ)

theta_rad <- angles_deg * pi / 180


C <- sum(cos(theta_rad))
S <- sum(sin(theta_rad))
R <- sqrt(C^2 + S^2)
Rbar <- R / n

mean_hat_rad <- atan2(S, C)              
mean_hat_d   <- (mean_hat_rad * 180 / pi) %% 360
if (mean_hat_d > 180) mean_hat_d <- mean_hat_d - 360  

phi_rad <- theta_rad - mean_hat_rad
rho2 <- mean(cos(2 * phi_rad))

delta_hat <- (1 - rho2) / (2 * Rbar^2)


sigma_hat2 <- delta_hat / n
sigma_hat  <- sqrt(sigma_hat2)


alpha    <- 0.05
z_alpha2 <- qnorm(1 - alpha/2)


asin_arg      <- z_alpha2 * sigma_hat
asin_arg_safe <- pmin(pmax(asin_arg, -1), 1)

halfwidth_rad <- asin(asin_arg_safe)         
halfwidth_deg <- halfwidth_rad * 180 / pi     

ci_analytic_L <- mean_hat_d - halfwidth_deg
ci_analytic_U <- mean_hat_d + halfwidth_deg

cat("Sample mean direction =", round(mean_hat_d, 2), "deg\n")
cat("Rbar                  =", round(Rbar, 3), "\n")
cat("rho2 (central)        =", round(rho2, 3), "\n")
cat("delta-hat             =", round(delta_hat, 3), "\n")
cat("sigma-hat             =", round(sigma_hat, 3), "\n")
cat("Analytic 95% CI       = (",
    round(ci_analytic_L, 2), ", ",
    round(ci_analytic_U, 2), ") deg\n\n", sep = "")


set.seed(123)  

B <- 2000  
boot_means_deg <- numeric(B)

for (b in 1:B) {
  idx_b     <- sample.int(n, size = n, replace = TRUE)
  angles_b  <- angles_deg[idx_b]
  theta_b_c <- circular(
    angles_b,
    units    = "degrees",
    template = "geographics",
    modulo   = "2pi"
  )
  m_b     <- mean.circular(theta_b_c)
  m_b_deg <- conversion.circular(m_b, type = "angles", units = "degrees")
  boot_means_deg[b] <- as.numeric(m_b_deg)
}


unwrap_around <- function(x_deg, center_deg) {
  ((x_deg - center_deg + 180) %% 360) - 180 + center_deg
}
boot_means_unwrapped <- unwrap_around(boot_means_deg, mean_hat_d)

ci_boot_L <- quantile(boot_means_unwrapped, probs = alpha/2)
ci_boot_U <- quantile(boot_means_unwrapped, probs = 1 - alpha/2)

cat("Bootstrap 95% CI       = (",
    round(ci_boot_L, 2), ", ",
    round(ci_boot_U, 2), ") deg\n\n", sep = "")


old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))


deg2rad      <- function(x) x * pi / 180
to_plot_rad  <- function(deg_geo) deg2rad(90 - deg_geo)

mean_rad_plot      <- to_plot_rad(mean_hat_d)
bootL_rad_plot     <- to_plot_rad(ci_boot_L)
bootU_rad_plot     <- to_plot_rad(ci_boot_U)
analyticL_rad_plot <- to_plot_rad(ci_analytic_L)
analyticU_rad_plot <- to_plot_rad(ci_analytic_U)


hist(
  boot_means_unwrapped,
  breaks = 15,
  main   = "Bootstrap means of direction\n(Desert ants, Set 1)",
  xlab   = expression(bar(theta)^"* (degrees)"),
  col    = "grey85",
  border = "grey40"
)
abline(v = mean_hat_d, col = "red",  lwd = 2)
abline(v = ci_boot_L,  col = "black", lwd = 2, lty = 2)
abline(v = ci_boot_U,  col = "black", lwd = 2, lty = 2)

legend(
  "topright",
  legend = c(
    bquote("Sample mean = " ~ .(round(mean_hat_d, 1))*degree),
    bquote("Boot 95% CI = (" ~ .(round(ci_boot_L, 1))*","~
             .(round(ci_boot_U, 1))*")"*degree)
  ),
  bty = "n", cex = 0.8
)

plot(
  theta_circ,
  stack  = FALSE,
  shrink = 1.1,
  pch    = 16,
  col    = "grey30",
  main   = "Desert ants (Set 1)\nMean direction: bootstrap vs analytic CI"
)

r_ci <- 1.0  # 半徑


ang_seq_boot_deg <- seq(ci_boot_L, ci_boot_U, length.out = 200)
ang_seq_boot_rad <- to_plot_rad(ang_seq_boot_deg)

x_boot <- c(0, r_ci * cos(ang_seq_boot_rad), 0)
y_boot <- c(0, r_ci * sin(ang_seq_boot_rad), 0)
polygon(x_boot, y_boot, col = "grey70", border = NA)
）
ang_seq_an_deg <- seq(ci_analytic_L, ci_analytic_U, length.out = 200)
ang_seq_an_rad <- to_plot_rad(ang_seq_an_deg)

r_an <- 1.1
x_an <- c(0, r_an * cos(ang_seq_an_rad), 0)
y_an <- c(0, r_an * sin(ang_seq_an_rad), 0)
polygon(x_an, y_an, col = rgb(0.9, 0.9, 0.9, 0.7), border = NA)

arrows(
  x0 = 0, y0 = 0,
  x1 = r_ci * cos(mean_rad_plot),
  y1 = r_ci * sin(mean_rad_plot),
  lwd = 2, col = "red"
)

legend(
  "topleft",
  legend = c(
    bquote("Mean = " ~ .(round(mean_hat_d, 1))*degree),
    bquote("Boot 95% CI"),
    bquote("Analytic 95% CI")
  ),
  fill   = c(NA, "grey70", rgb(0.9, 0.9, 0.9, 0.7)),
  border = c(NA, NA, NA),
  lty    = c(1, NA, NA),
  col    = c("red", NA, NA),
  bty    = "n",
  cex    = 0.8
)

par(old_par)






# =============








library(circular)

data(fisherB10)
angles_deg <- fisherB10$set1          
n <- length(angles_deg)

# ---------- 小工具 ----------
deg2rad <- function(x) x * pi/180
rad2deg <- function(x) x * 180/pi
wrap_pi  <- function(a) ((a + pi) %% (2*pi)) - pi   
unwrap_around_deg <- function(x_deg, center_deg) {  
  ((x_deg - center_deg + 180) %% 360) - 180 + center_deg
}


th  <- deg2rad(angles_deg)

C <- mean(cos(th)); S <- mean(sin(th))
mu_hat <- atan2(S, C)             
Rbar   <- sqrt(C^2 + S^2)         

phi    <- wrap_pi(th - mu_hat)
rho2   <- mean(cos(2*phi))           
delta_hat <- (1 - rho2) / (2 * Rbar^2)
sigma_hat <- sqrt(delta_hat / n)  

cat("n =", n,
    "\nSample mean =", round(rad2deg(mu_hat), 2), "deg",
    "\nRbar =", round(Rbar, 3),
    "\nrho2 =", round(rho2, 3),
    "\ndelta-hat =", round(delta_hat, 3),
    "\nsigma-hat =", round(sigma_hat, 3), "\n\n")


mu0_deg <- 20
mu0     <- deg2rad(mu0_deg)


Z_obs <- sin(wrap_pi(mu_hat - mu0)) / sigma_hat
cat("Analytic Z (reference) =", round(Z_obs, 3), "\n\n")

set.seed(123)
B <- 2000
boot_means_deg <- numeric(B)

for (b in 1:B) {
  th_b <- sample(th, n, replace = TRUE)
  mu_b <- atan2(mean(sin(th_b)), mean(cos(th_b)))
  boot_means_deg[b] <- rad2deg(mu_b)
}
mu_hat_deg <- rad2deg(mu_hat)
boot_means_unwrapped <- unwrap_around_deg(boot_means_deg, mu_hat_deg)

alpha <- 0.05
ci_boot_L <- as.numeric(quantile(boot_means_unwrapped, probs = alpha/2))
ci_boot_U <- as.numeric(quantile(boot_means_unwrapped, probs = 1 - alpha/2))

inside_CI <- (ci_boot_L <= mu0_deg) && (mu0_deg <= ci_boot_U)

cat(sprintf("Bootstrap 95%% CI for mu = (%.1f, %.1f) deg\n", ci_boot_L, ci_boot_U))
cat("CI decision:", ifelse(inside_CI, "Fail to reject H0", "Reject H0"), "\n\n")

resid0 <- wrap_pi(th - mu_hat)   

Zb <- numeric(B)
set.seed(123)
for (b in 1:B) {
  th_b <- wrap_pi(sample(resid0, n, replace = TRUE) + mu0)
  
  Cb <- mean(cos(th_b)); Sb <- mean(sin(th_b))
  mu_b <- atan2(Sb, Cb)
  Rbar_b <- sqrt(Cb^2 + Sb^2)
  
  phi_b  <- wrap_pi(th_b - mu_b)    
  rho2_b <- mean(cos(2*phi_b))
  delta_b <- (1 - rho2_b) / (2 * Rbar_b^2)
  sigma_b <- sqrt(delta_b / n)
  
  Zb[b] <- sin(wrap_pi(mu_b - mu0)) / sigma_b
}

p_boot <- mean(abs(Zb) >= abs(Z_obs))
cat("Bootstrap (H0-centered) two-sided p-value =", round(p_boot, 4), "\n")
cat("p-value decision:", ifelse(p_boot < alpha, "Reject H0", "Fail to reject H0"), "\n\n")

theta_circ <- circular(angles_deg, units="degrees",
                       template="geographics", modulo="2pi")

to_plot_rad <- function(deg_geo) deg2rad(90 - deg_geo)
mean_rad_plot  <- to_plot_rad(mu_hat_deg)
bootL_rad_plot <- to_plot_rad(ci_boot_L)
bootU_rad_plot <- to_plot_rad(ci_boot_U)
mu0_rad_plot   <- to_plot_rad(mu0_deg)

old_par <- par(no.readonly = TRUE); par(mfrow = c(1, 2))


hist(
  boot_means_unwrapped, breaks = 15,
  main   = "Bootstrap means of direction\n(Desert ants, Set 1)",
  xlab   = expression(bar(theta)^"* (degrees)"),
  col    = "grey85", border = "grey40"
)
abline(v = mu_hat_deg, col = "red",  lwd = 2)
abline(v = mu0_deg,   col = "blue", lwd = 2, lty = 2)
abline(v = ci_boot_L, col = "black", lwd = 2, lty = 3)
abline(v = ci_boot_U, col = "black", lwd = 2, lty = 3)
legend("topright",
       legend = c(
         bquote("Sample mean = " ~ .(round(mu_hat_deg, 1))*degree),
         bquote(mu[0] == .(mu0_deg)*degree),
         bquote("Boot 95% CI = (" ~ .(round(ci_boot_L,1))*","~.(round(ci_boot_U,1))*")"*degree),
         bquote("p[boot] under H[0] " %~~% .(round(p_boot, 3)))
       ),
       col = c("red","blue","black", NA), lty = c(1,2,3,0), bty = "n", cex = 0.8
)


plot(theta_circ, stack=FALSE, shrink=1.1, pch=16, col="grey30",
     main="Desert ants (Set 1)\nTesting a specified mean direction")
r_ci <- 1.0
ang_seq_boot_deg <- seq(ci_boot_L, ci_boot_U, length.out = 200)
ang_seq_boot_rad <- to_plot_rad(ang_seq_boot_deg)
polygon(c(0, r_ci*cos(ang_seq_boot_rad), 0),
        c(0, r_ci*sin(ang_seq_boot_rad), 0),
        col="grey80", border=NA)
arrows(0,0, r_ci*cos(mean_rad_plot), r_ci*sin(mean_rad_plot), lwd=2, col="red")      # sample mean
arrows(0,0, r_ci*cos(mu0_rad_plot),   r_ci*sin(mu0_rad_plot),   lwd=2, col="blue", lty=2) # mu0
legend("topleft",
       legend = c(
         bquote(bar(theta) == .(round(mu_hat_deg, 1))*degree),
         bquote(mu[0] == .(mu0_deg)*degree),
         bquote("Boot 95% CI"),
         if (p_boot < alpha) "Reject H[0]" else "Fail to reject H[0]"
       ),
       col=c("red","blue",NA,NA), lty=c(1,2,0,0), fill=c(NA,NA,"grey80",NA),
       border=c(NA,NA,NA,NA), bty="n", cex=0.8
)

par(old_par)








# ==================










library(circular)

data(fisherB10)
angles_deg <- fisherB10$set1   

theta_circ <- circular(
  angles_deg,
  units    = "degrees",
  template = "geographics",    
  modulo   = "2pi"
)
n <- length(theta_circ)

med_hat_c   <- median.circular(theta_circ)
med_hat_deg <- as.numeric(
  conversion.circular(med_hat_c, type = "angles", units = "degrees")
)


phi_deg <- ((angles_deg - med_hat_deg + 180) %% 360) - 180

phi_nz   <- phi_deg[phi_deg != 0]
n_prime  <- length(phi_nz)

cat("Sample median direction =", round(med_hat_deg, 2), "deg\n")
cat("n' (nonzero deviations) =", n_prime, "\n\n")


abs_phi <- abs(phi_nz)

ranks <- rank(abs_phi, ties.method = "average")

w_plus <- sum(ranks[phi_nz > 0])

T_total <- n_prime * (n_prime + 1) / 2
w_minus <- T_total - w_plus

cat("phi (deg, nonzero)  =", round(phi_nz, 1), "\n")
cat("abs(phi)            =", round(abs_phi, 1), "\n")
cat("ranks(|phi|)        =", ranks, "\n")
cat("w_n^+ (sum ranks φ>0) =", w_plus, "\n")
cat("w_n^- (sum ranks φ<0) =", w_minus, "\n\n")



alpha <- 0.05

w_test <- wilcox.test(
  x           = phi_nz,
  mu          = 0,
  alternative = "two.sided",
  exact       = TRUE,
  conf.int    = FALSE
)

p_exact <- w_test$p.value

cat("Exact Wilcoxon p-value (two-sided) =", round(p_exact, 4), "\n\n")


crit_upper <- qsignrank(1 - alpha/2, n_prime)
crit_lower <- T_total - crit_upper

cat("Critical region (two-sided, alpha = 0.05):\n")
cat(" Reject H0 if w_n^+ >= ", crit_upper,
    " or w_n^+ <=", crit_lower, "\n", sep = "")
cat("Here w_n^+ =", w_plus,
    ifelse(w_plus >= crit_upper | w_plus <= crit_lower,
           "→ Reject H0\n\n", "→ Fail to reject H0\n\n"))


mu_w    <- T_total / 4                    
sigma_w <- sqrt(n_prime * (n_prime + 1) * (2 * n_prime + 1) / 24)
w_std   <- (w_plus - mu_w - 0.5) / sigma_w 

p_norm <- 2 * (1 - pnorm(abs(w_std)))

cat("Normal-approx z for w^+ =", round(w_std, 3), "\n")
cat("Normal-approx p-value   =", round(p_norm, 4), "\n\n")


old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))


cols <- ifelse(phi_nz > 0, "red", "blue")

plot(
  abs_phi, phi_nz,
  xlab = expression("|" * phi[i] * "|" ~ "(absolute deviation, degrees)"),
  ylab = expression(phi[i] ~ "(signed deviation, degrees)"),
  pch  = 19,
  col  = cols,
  main = "Signed deviations from sample median\n(Wilcoxon signed ranks)"
)


text(
  abs_phi, phi_nz,
  labels = round(ranks, 1),
  pos    = 3,
  cex    = 0.8
)

abline(h = 0, lty = 2)


legend(
  "topleft",
  legend = c(
    expression(phi[i] > 0 ~ "(前方)"),
    expression(phi[i] < 0 ~ "(後方)")
  ),
  col = c("red", "blue"),
  pch = 19,
  bty = "n",
  cex = 0.8
)


legend(
  "bottomright",
  legend = c(
    bquote(w[n]^"+" == .(round(w_plus, 1))),
    bquote(p[exact] %~~% .(round(p_exact, 3)))
  ),
  bty = "n",
  cex = 0.8
)

deg2rad     <- function(x) x * pi / 180
to_plot_rad <- function(deg_geo) deg2rad(90 - deg_geo)

med_rad_plot <- to_plot_rad(med_hat_deg)


phi_all  <- ((angles_deg - med_hat_deg + 180) %% 360) - 180
cols_all <- ifelse(phi_all > 0, "red",
                   ifelse(phi_all < 0, "blue", "darkgrey"))

plot(
  theta_circ,
  stack  = FALSE,
  shrink = 1.1,
  pch    = 16,
  col    = cols_all,
  main   = "Desert ants (Set 1)\nSymmetry about sample median?"
)

r_med <- 1.0
arrows(
  x0 = 0, y0 = 0,
  x1 = r_med * cos(med_rad_plot),
  y1 = r_med * sin(med_rad_plot),
  lwd = 2, col = "black"
)

legend(
  "topleft",
  legend = c(
    bquote("Median =" ~ .(round(med_hat_deg, 1))*degree),
    expression(phi > 0 ~ "(紅：median 前方)"),
    expression(phi < 0 ~ "(藍：median 後方)")
  ),
  col   = c("black", "red", "blue"),
  pch   = c(NA, 16, 16),
  lty   = c(1, NA, NA),
  bty   = "n",
  cex   = 0.8
)

par(old_par)
