# install.packages('ICC', 'dplyr', 'ggplot2')
library(ICC)
library(dplyr)
library(ggplot2)

# Carregando os dados gerados pelo script trata_dados.R
experimental_data = read.csv("data/experimental_data.csv")
results = read.csv("data/results.csv")

# Analisando se há consenso entre as notas dos participantes para a completude do abstract
icc_analysis <- ICCest(add_abstract_id, mean_completeness_specific_judge, experimental_data)

print("Resultados da análise de ICC (Consenso)")
cat("MSBA =", icc_analysis$vara, '\n')
cat("MSWA =", icc_analysis$varw, '\n')
cat("ICC =", icc_analysis$ICC, '\n')

# Separando as amostras
IST_1 <- results %>% filter(Treatment == "IST-1")
IST_2 <- results %>% filter(Treatment == "IST-2")
JSS_1 <- results %>% filter(Treatment == "JSS-1")
JSS_2 <- results %>% filter(Treatment == "JSS-2")

# Gera os gráficos de densidade de kernel para completude
png(file="plots/kernel_density_completeness.png", width=1200, height=700)
    par(mfrow=c(2,2))
    hist(JSS_1$MedianCompleteness, prob = TRUE, main = "(a)JSS Period 1", xlab = "Abstract completeness score")
    lines(density(JSS_1$MedianCompleteness), col = "black")
    hist(JSS_2$MedianCompleteness, prob = TRUE, main = "(b)JSS Period 2", xlab = "Abstract completeness score")
    lines(density(JSS_2$MedianCompleteness), col = "black")
    hist(IST_1$MedianCompleteness, prob = TRUE, main = "(c)IST Period 1", xlab = "Abstract completeness score")
    lines(density(IST_1$MedianCompleteness), col = "black")
    hist(IST_2$MedianCompleteness, prob = TRUE, main = "(d)IST Period 2", xlab = "Abstract completeness score")
    lines(density(IST_2$MedianCompleteness), col = "black")
dev.off()

# Gera os gráficos de densidade de kernel para clareza
png(file="plots/kernel_density_clarity.png", width=1200, height=700)
    par(mfrow=c(2,2))
    hist(JSS_1$MedianClarity, prob = TRUE, main = "(a)JSS Period 1", xlab = "Abstract clarity score")
    lines(density(JSS_1$MedianClarity), col = "black")
    hist(JSS_2$MedianClarity, prob = TRUE, main = "(b)JSS Period 2", xlab = "Abstract clarity score")
    lines(density(JSS_2$MedianClarity), col = "black")
    hist(IST_1$MedianClarity, prob = TRUE, main = "(c)IST Period 1", xlab = "Abstract clarity score")
    lines(density(IST_1$MedianClarity), col = "black")
    hist(IST_2$MedianClarity, prob = TRUE, main = "(d)IST Period 2", xlab = "Abstract clarity score")
    lines(density(IST_2$MedianClarity), col = "black")
dev.off()

# Gera o scatterplot para a relação entre completude e clareza
png(file="plots/scatter_plot.png")
    scatter_data <- rbind(IST_1, IST_2, JSS_1, JSS_2)
    scatter_plot <- ggplot(scatter_data, aes(x = MedianClarity, y = MeanCompleteness), xlab("Abstract clarity"), ylab("Abstract completeness"))
    scatter_plot + geom_point(aes(shape = factor(Treatment))) + xlab("Abstract clarity") + ylab("Abstract completeness") + labs(shape="Abstract type")
dev.off()
