# install.packages('ICC', 'dplyr', 'ggplot2')
library(dplyr)
library(effsize)
library(ICC)
library(ggplot2)
library(reproducer)

# Carregando os dados disponibilizados pelos autores dos arquivos
# Dados brutos
experimental_data = KitchenhamMadeyskiBudgen16.SubjectData
# Dados tratados
results = KitchenhamMadeyskiBudgen16.DiffInDiffData

# Testatando o calculo da completude por juiz (media das respostas)
experimental_data$mean_completeness_specific_judge <- rowMeans(experimental_data[,13:20], na.rm=TRUE)
if (identical(round(experimental_data$mean_completeness_specific_judge, 7), round(experimental_data$MeanScore, 7))) {
   print('Função de média correta!')
}else {
   print('Função de média incorreta!')
   stop('Algum valor está incorreto: \n')
   round(experimental_data$mean_completeness_specific_judge, 7) == round(experimental_data$MeanScore, 7)
}
sites = unique(experimental_data$Site)

## Analisando se há consenso entre as notas dos participantes para a completude do abstract
for (site in sites) {
    icc_analysis <- ICCest(AbstractID, MeanScore, experimental_data %>% filter(Site == site))
    cat("Resultados da análise de ICC (Consenso) da instituição", site, "\n")
    cat("MSBA =", icc_analysis$vara, '\n')
    cat("MSWA =", icc_analysis$varw, '\n')
    cat("ICC =", icc_analysis$ICC, '\n')
}

# Separando as amostras
IST_1 <- results %>% filter(Treatment == "IB1")
IST_2 <- results %>% filter(Treatment == "IB2")
JSS_1 <- results %>% filter(Treatment == "JB1")
JSS_2 <- results %>% filter(Treatment == "JB2")

# Gera os gráficos de densidade de kernel para completude
png(file="plots_original_data/kernel_density_completeness.png", width=1200, height=700)
    par(mfrow=c(2,2))
    hist(JSS_1$MedianCompleteness, prob = TRUE, main = "(a)JSS Period 1", xlab = "Abstract completeness score", ylim = c(0,5), xlim = c(0.2,1))
    lines(density(JSS_1$MedianCompleteness), col = "black")
    hist(JSS_2$MedianCompleteness, prob = TRUE, main = "(b)JSS Period 2", xlab = "Abstract completeness score", ylim = c(0,5), xlim = c(0.2,1))
    lines(density(JSS_2$MedianCompleteness), col = "black")
    hist(IST_1$MedianCompleteness, prob = TRUE, main = "(c)IST Period 1", xlab = "Abstract completeness score", ylim = c(0,5), xlim = c(0.2,1))
    lines(density(IST_1$MedianCompleteness), col = "black")
    hist(IST_2$MedianCompleteness, prob = TRUE, main = "(d)IST Period 2", xlab = "Abstract completeness score", ylim = c(0,5), xlim = c(0.2,1), breaks = 5)
    lines(density(IST_2$MedianCompleteness), col = "black")
dev.off()

# Gera os gráficos de densidade de kernel para clareza
png(file="plots_original_data/kernel_density_clarity.png", width=1200, height=700)
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
png(file="plots_original_data/scatter_plot_complete.png")
    scatter_data <- rbind(IST_1, IST_2, JSS_1, JSS_2)
    scatter_plot <- ggplot(scatter_data, aes(x = MedianClarity, y = MedianCompleteness), xlab("Abstract clarity"), ylab("Abstract completeness"))
    scatter_plot + geom_point(aes(shape = factor(Treatment))) + xlab("Abstract clarity") + ylab("Abstract completeness") + labs(shape="Abstract type")
dev.off()

png(file="plots_original_data/scatter_plot_paper.png")
    scatter_data <- rbind(IST_1, IST_2, JSS_1, JSS_2)
    scatter_plot <- ggplot(scatter_data, aes(x = MedianClarity, y = MedianCompleteness),
                    xlab("Abstract clarity"), ylab("Abstract completeness"))
    scatter_plot + geom_point(aes(shape = factor(Treatment))) + xlab("Abstract clarity") +
                   ylab("Abstract completeness") + labs(shape="Abstract type") + xlim(c(3,8))
dev.off()

# Executa o teste Cliff's para verificar a influência do journal na completude
cliff_journal_completeness <- cliff.delta(results$MedianCompleteness~results$Journal, return.dm=T)

print("Resultados do teste Cliff's para influência do journal na completude")
cat("Delta =", cliff_journal_completeness$estimate, '\n')
cat("Confidence Interval =", cliff_journal_completeness$conf.int, '\n')

# Executa o teste Cliff's para verificar a influência do journal na clareza 
cliff_journal_clarity <- cliff.delta(results$MedianClarity~results$Journal, return.dm=T)

print("Resultados do teste Cliff's para influência do journal na clareza")
cat("Delta =", cliff_journal_clarity$estimate, '\n')
cat("Confidence Interval =", cliff_journal_clarity$conf.int, '\n')

# Executa o teste Cliff's para verificar a influência do periodo na completude
# Por padrão, o R ordena os levels, neste caso 1 e 2, por ordem crescente.
# Mas o artigo quer comparar o período 2 com o 1 e não o 1 com o 2. 
# Por isso, declaramos exeplicitamente a ordem dos fatores
cliff_period_completeness <- cliff.delta(results$MedianCompleteness~factor(results$Timeperiod, levels = c(2, 1)), return.dm=T)

print("Resultados do teste Cliff's para influência do período na completude")
cat("Delta =", cliff_period_completeness$estimate, '\n')
cat("Confidence Interval =", cliff_period_completeness$conf.int, '\n')

# Executa o teste Cliff's para verificar a influência do periodo na clareza
cliff_period_clarity <- cliff.delta(results$MedianClarity~factor(results$Timeperiod, levels = c(2, 1)), return.dm=T)

print("Resultados do teste Cliff's para influência do período na clarity")
cat("Delta =", cliff_period_clarity$estimate, '\n')
cat("Confidence Interval =", cliff_period_clarity$conf.int, '\n')
