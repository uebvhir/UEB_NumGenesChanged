################################################################################
####################
##### geneSelectable
##### Autora: Miriam Mota
##### Modificaciones: Ricardo Gonzalo
####################
##### topTab: Toptable en format '.csv'
##### adj0: p.valor adjustat pel qual filtrar (de normal, 0.01)
##### adj1: segon p.valor adjustat pel qual filtrar (de normal, 0.05)
##### adj2: tercer p.valor adjustat pel qual filtrar (de normal, 0.25)
##### P1: p.valor pel qual filtrar (de normal, 0.01)
##### P2: segon p.valor pel qual filtrar (de normal, 0.05)
################################################################################
genesSelectable <- function (topTab, adj0, adj1, adj2, adj3, P1, P2,FC=0)
{
  upBelowB <- sum(topTab$B > 0  & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowB <- sum(topTab$B > 0  & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowAdj0 <- sum(topTab$adj.P.Val < adj0 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowAdj0 <- sum(topTab$adj.P.Val < adj0 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowAdj1 <- sum(topTab$adj.P.Val < adj1 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowAdj1 <- sum(topTab$adj.P.Val < adj1 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowAdj2 <- sum(topTab$adj.P.Val < adj2 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowAdj2 <- sum(topTab$adj.P.Val < adj2 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowAdj3 <- sum(topTab$adj.P.Val < adj3 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowAdj3 <- sum(topTab$adj.P.Val < adj3 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowP1 <- sum(topTab$P.Value < P1 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowP1 <- sum (topTab$P.Value < P1 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  upBelowP2 <- sum(topTab$P.Value < P2 & topTab$t > 0 & abs(topTab$logFC) > FC)
  downBelowP2 <- sum(topTab$P.Value < P2 & topTab$t < 0 & abs(topTab$logFC) > FC)
  
  return(c(upReg_B = upBelowB,downReg_B = downBelowB,
           upRegAdj0.01 = upBelowAdj0, downRegAdj0.01 = downBelowAdj0,
           upRegAdj0.05 = upBelowAdj1, downRegAdj0.05 = downBelowAdj1,
           upRegAdj0.15 = upBelowAdj2, downRegAdj0.15 = downBelowAdj2,
           upRegAdj0.25 = upBelowAdj2, downRegAdj0.25 = downBelowAdj2,
           upRegP0.01 = upBelowP1, downRegP0.01 = downBelowP1,
           upRegP0.05 = upBelowP2, downRegP0.05 = downBelowP2))   
}