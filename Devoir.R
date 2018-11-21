### Nicholas langevin 
##  Processus stochastique 
#   Devoir  

# Importation des packages utiles au programme.
library(dplyr)
library(xtable)
library(ggplot2)
library(actuar)


# Choisir pour quel processus on fait les simulations, i = 1,2.
i = 1

alpha = c(3,2)
beta = c(0.5, 1500)
lambda = alpha[1] * beta[1]
 
set.seed(2545)
nsim = 1e5
 
### Calcul du processus de poisson ordinaire ###
# Initialisation des listes contenants les réponses pour le processus i.
N_t = list()
S_t = list()
x_t = list()
Pr_n = list()
esp_n = list()
var_n = list()
esp_x = list()
var_x = list()
esp_s = list()
var_s = list()
 
# Effectue les simulations pour les différents t 
# Le programme est batie de façon à simulé un processus à la fois.
for(t in 1:5){
    N_t[[t]] = numeric(nsim)
    S_t[[t]] = numeric(nsim)
    x_t[[t]] = numeric()
    for(j in 1:nsim){           +       
            # Simule n_t selon le processus choisi (i = 1,2) 
            if(i == 1){
                N_t[[t]][j] = rpois(1, lambda * t) 
            }else if(i == 2){
                N_t[[t]][j] = rpois(1, rgamma(1, alpha[1], beta[1]) * t)  
            }else{
                stop("Choisir un i valide")
            }
           
            X = rgamma(N_t[[t]][j], alpha[2], scale = beta[2])
            x_t[[t]] = c(x_t[[t]], X)
                       
            S_t[[t]][j] = sum( X )
    }
                 
            # A) P(N_i(t) = n) , n = 0,1,...,10
            n = 0:10
            Pr_n[[t]] = sapply(n, function(j) sum(N_t[[t]] == j) / nsim)
                   
            # B)
            esp_n[[t]] = mean(N_t[[t]])
            esp_x[[t]] = mean(x_t[[t]])
                         
            # C)
            var_n[[t]] = var(N_t[[t]])
            var_x[[t]] = var(x_t[[t]])
                             
            # D)
            esp_s[[t]] = mean(S_t[[t]])
            var_s[[t]] = var(S_t[[t]])
}

### Calcules des valeurs théoriques pour le processus i = 1 ### 
# Valeurs théoriques pour Pr_n pour le processus i=1
prob.n_t_1 <- function(lambda, t, n){
    (lambda * t)^n * exp(- lambda * t) / factorial(n)
}
theorique.Pr_n_1 = lapply(1:5, function(t) sapply(0:10, prob.n_t_1, lambda = lambda, t = t ))
print(xtable(data.frame(t_1 = theorique.Pr_n_1[[1]],
                        t_2 = theorique.Pr_n_1[[2]],
                        t_3 = theorique.Pr_n_1[[3]],
                        t_4 = theorique.Pr_n_1[[4]],
                        t_5 = theorique.Pr_n_1[[5]]), digits = 4))

# Valeur théorique pour N(t) pour le processus i=1
theorique.esp_n_1 = sapply(1:5, function(t) lambda * t)
theorique.var_n_1 = sapply(1:5, function(t) lambda * t)

# Valeur théorique pour X
theorique.esp_x = alpha[2] * beta[2]
theorique.var_x = alpha[2] * beta[2]^2

# Valeur théorique pour S(t) pour le processus i=1
theorique.esp_s_1 = theorique.esp_n_1 * theorique.esp_x
theorique.var_s_1 = theorique.esp_n_1 * theorique.var_x + theorique.esp_x^2 * theorique.var_n_1
    
### Output des valeurs de simulations et des valeur théorique en tableau latex pour le rapport ###
choix_output_simulation = c("esp_n", "var_n", "esp_x", "var_x", "esp_s", "var_s", "Pr_n")
choix_output_theorique = c("theorique.esp_n", "theorique.esp_s_1", "theorique.var_s_1")
esp_x = mean(unlist(esp_x)) # E[X] ne dépend pas de t, on prend la moyenne des 5 simulation pour plus de précision.
var_x = mean(unlist(var_x)) # Var(X) ne dépend pas de t, on prend la moyenne des 5 simulation pour plus de précision.

out = "theorique.var_s_1"
print(xtable(data.frame(t_1 = eval(parse(text = out))[[1]],
                        t_2 = eval(parse(text = out))[[2]],
                        t_3 = eval(parse(text = out))[[3]],
                        t_4 = eval(parse(text = out))[[4]],
                        t_5 = eval(parse(text = out))[[5]]), digits = 0))

### 2.2 ###
empirique.Fn.S_1_data = data.frame(sapply(1:5, function(t) S_t[[t]]))

# Préparation du data pour un ggplots 
empirique.Fn.S_1 = data.frame(Légende = c(rep("t=1", nrow(empirique.Fn.S_1_data)),
                                         rep("t=2", nrow(empirique.Fn.S_1_data)),
                                         rep("t=3", nrow(empirique.Fn.S_1_data)),
                                         rep("t=4", nrow(empirique.Fn.S_1_data)),
                                         rep("t=5", nrow(empirique.Fn.S_1_data))),
                              value = c(empirique.Fn.S_1_data$X1,
                                        empirique.Fn.S_1_data$X2,
                                        empirique.Fn.S_1_data$X3,
                                        empirique.Fn.S_1_data$X4,
                                        empirique.Fn.S_1_data$X5))


ggplot(empirique.Fn.S_1, aes(x = value, color = Légende))+
                        stat_ecdf(geom = "step")+
                        theme_classic()+
                        theme(legend.position=c(0.95,0.3))+
                        scale_x_continuous(expand = c(0, 0))+ 
                        scale_y_continuous(expand = c(0, 0))+
                        scale_color_manual(values=c("red", "blue", "green", "brown", "black"))+
                        #ggtitle(expression(paste("Fonction de répartition de ",S[1](t)) ))+
                        labs(y=expression(F[S[1](t)](s)), x="s")
ggsave("Graphiques/empirique_Fn_S_1.png", width = 5.5, height = 5)                                            



kappa = c(0.95,0.975,0.99)
# Deux facon de calculée la VaR. La 2e est utilisé.
VaR.S_t = lapply(kappa, function(k) sapply(1:5, function(t) sort(S_t[[t]])[nsim*k]))
VaR.S_t_v2 = sapply(1:5, function(t)  quantile(S_t[[t]], probs = kappa))
xtable(data.frame(VaR.S_t_v2))

# # Deux facon de calculée la TVaR. La 2e est utilisé.
TVaR.S_t = lapply(kappa, function(k) sapply(1:5, function(t) mean(sort(S_t[[t]])[(nsim*k+1):nsim])))
TVaR.S_t_v2 = lapply(kappa, function(k) sapply(1:5, function(t) mean(S_t[[t]][S_t[[t]] > quantile(S_t[[t]], probs = k)])))
xtable(data.frame(alpha = kappa, rbind(TVaR.S_t_v2[[1]], TVaR.S_t_v2[[2]], TVaR.S_t_v2[[3]])))

TVaR.S <- function(kappa, t){
    mean(S_t[[t]][S_t[[t]] > quantile(S_t[[t]], probs = kappa)])
}

# Graphique de la densité des coûts pour pouvoir interprété la VaR et la TVaR
ggplot(empirique.Fn.S_1, aes(x = value, fill = Légende))+
    geom_density(alpha = 0.5)+
    theme_classic()+
    theme(legend.position=c(0.95,0.3))+
    scale_fill_manual(values=c("red", "blue", "green", "brown", "black"))+
    scale_x_continuous(expand = c(0, 0))+ 
    scale_y_continuous(expand = c(0, 0))+
    labs(y="Densité", x="s")
         
ggsave("Graphiques/densite_Fn_S_1.png", width = 5.5, height = 5)                                            




































