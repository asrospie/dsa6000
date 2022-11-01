


estimate_fish <- function(df, x) {
  mu_salmon <- mean(df[which(df$species == 'Salmon'), 'length'])
  mu_seabass <- mean(df[which(df$species == 'Seabass'), 'length'])
  mu_other <- mean(df[which(df$species == 'Other'), 'length'])
  
  sigma_salmon <- var(df$length)
  sigma_seabass <- var(df$length)
  sigma_other <- var(df$length)
  
  pi_salmon <- sum(df$species == 'Salmon') / nrow(df)
  pi_seabass <- sum(df$species == 'Seabass') / nrow(df)
  pi_other <- sum(df$species == 'Other') / nrow(df)
  
  k_salmon <- x * (mu_salmon / (sigma_salmon^2)) - (mu_salmon^2 / (2 * sigma_salmon^2)) + log(pi_salmon, base = 10)
  k_seabass <- x * (mu_seabass / (sigma_seabass^2)) - (mu_seabass^2 / (2 * sigma_seabass^2)) + log(pi_seabass, base = 10)
  k_other <- x * (mu_other / (sigma_other^2)) - (mu_other^2 / (2 * sigma_other^2)) + log(pi_other, base = 10)
  
  return(c(k_salmon, k_seabass, k_other))

}

estimate_fish(fish, 15)
