library(ggplot)
library(latex2exp)

# final state equation derivation https://courses.helsinki.fi/sites/default/files/course-material/4670641/final_size_derivation.pdf

a <- data.frame(s_inf=seq(0, 1, 0.01))

df <- data.frame(s_inf = rbind(a, a, a, a, a), R0_ = c(rep(1.5, 101), rep(2, 101), rep(2.5, 101), rep(3, 101), rep(4, 101)))

df$R0 <- as.factor(df$R0_)


# English
text = TeX("Total percentage of infected\ncould be calculated as a root\nof equation")
text2 = TeX("$s_{\\infty} = $\\frac{-log(s_{\\infty})}{R_0} .$")
text3 = TeX("Roots could be find \nvisually as intersection \nwith y=x line")

ggplot(df, aes(x = s_inf, color = R0)) + 
  geom_abline(slope = 1, intercept = 0, size = 1.5) + 
  geom_line(aes(y = -log(s_inf)/R0_, color = R0), size = 1.5) + 
  xlab(TeX("s_\\infty")) + 
  ylab(TeX("$\\frac{-\\log(s_{\\infty})}{R0}$")) + 
  guides(color=guide_legend(title = TeX("R_0"))) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size = 15)) + 
  ylim(-0.21, 1.7) + 
  #geom_segment(xend = 0.485, yend = -0.02, x = 0.485, y = 0.48, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.42, yend = 0, x = 0.42, y = 0.42, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.385, yend = -0.04, x = 0.385, y = 0.385, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.345, yend = -0.02, x = 0.345, y = 0.35, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.3, yend = 0, x = 0.3, y = 0.29, arrow=arrow(), size = 1.2, col = "darkblue") + 
  geom_segment(xend = 0.485, yend = -0, x = 0.485, y = 0.48, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.42, yend = 0, x = 0.42, y = 0.42, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.385, yend = -0, x = 0.385, y = 0.385, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.345, yend = -0, x = 0.345, y = 0.35, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.3, yend = 0, x = 0.3, y = 0.29, arrow=arrow(), size = 1.2, col = "darkblue") + 
  geom_abline(slope = 0, intercept = 0) + 
  #geom_text(aes(x=0.15, y=-0.14, label = "Celkové\n procento\n nakažených:"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.15, y=-0.1, label = "Total\n infected:"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.485, y=-0.14, label = "~51%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.42, y=-0.07, label = "~58%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.385, y=-0.21, label = "~61%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.345, y=-0.14, label = "~65%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.3, y=-0.07, label = "~70%"), col = "darkred", size = 3.5) +
  annotate("text", label = text, x = 0.6, y = 1.5, col = "black", size = 4) + 
  annotate("text", label = text2, x = 0.7, y = 1.4, col = "black", size = 4) + 
  annotate("text", label = text3, x = 0.55, y = 1, col = "black", size = 4) + 
  ggtitle('Where Merkel took "up to 70 % could became infected"?')
  
# Czech
text = TeX("Celkový podíl nakažených \nlze spočítat jako\nkořen rovnice")
text2 = TeX("$s_{\\infty} = $\\frac{-log(s_{\\infty})}{R_0} .$")
text3 = TeX("Kořeny lze najít vizuálně \njako body protínající čáru y=x")

ggplot(df, aes(x = s_inf, color = R0)) + 
  geom_abline(slope = 1, intercept = 0, size = 1.5) + 
  geom_line(aes(y = -log(s_inf)/R0_, color = R0), size = 1.5) + 
  xlab(TeX("s_\\infty")) + 
  ylab(TeX("$\\frac{-\\log(s_{\\infty})}{R0}$")) + 
  guides(color=guide_legend(title = TeX("R_0"))) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size = 15)) + 
  ylim(-0.21, 1.7) + 
  #geom_segment(xend = 0.485, yend = -0.02, x = 0.485, y = 0.48, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.42, yend = 0, x = 0.42, y = 0.42, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.385, yend = -0.04, x = 0.385, y = 0.385, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.345, yend = -0.02, x = 0.345, y = 0.35, arrow=arrow(), size = 1.2, col = "darkblue") +
  #geom_segment(xend = 0.3, yend = 0, x = 0.3, y = 0.29, arrow=arrow(), size = 1.2, col = "darkblue") + 
  geom_segment(xend = 0.485, yend = -0, x = 0.485, y = 0.48, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.42, yend = 0, x = 0.42, y = 0.42, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.385, yend = -0, x = 0.385, y = 0.385, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.345, yend = -0, x = 0.345, y = 0.35, arrow=arrow(), size = 1.2, col = "darkblue") +
  geom_segment(xend = 0.3, yend = 0, x = 0.3, y = 0.29, arrow=arrow(), size = 1.2, col = "darkblue") + 
  geom_abline(slope = 0, intercept = 0) + 
  geom_text(aes(x=0.15, y=-0.14, label = "Celkové\n procento\n nakažených:"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.485, y=-0.14, label = "~51%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.42, y=-0.07, label = "~58%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.385, y=-0.21, label = "~61%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.345, y=-0.14, label = "~65%"), col = "darkred", size = 3.5) +
  geom_text(aes(x=0.3, y=-0.07, label = "~70%"), col = "darkred", size = 3.5) +
  annotate("text", label = text, x = 0.6, y = 1.5, col = "black", size = 4) + 
  annotate("text", label = text2, x = 0.7, y = 1.4, col = "black", size = 4) + 
  annotate("text", label = text3, x = 0.63, y = 1.15, col = "black", size = 4) + 
  ggtitle('Kde Merkelová vzala, že "novým typem koronaviru by \nse v Německu mohlo nakazit 60 až 70 procent obyvatel"?') 
  

