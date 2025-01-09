# Graphical representation:
plot1 <- ggplot(data, aes(x=femalematedbefore, y=total.courtship.duration, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Vierge","Fécondée")) +
  ylab("Durée totale de la parade (s)") +
  theme_light(base_size = 20) +
  theme(legend.position="none")

plot2 <- ggplot(data, aes(x=femalematedbefore, y=number.of.courtships, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("Statut de la femelle") +
  scale_x_discrete(labels = c("Vierge","Fécondée")) +
  ylab("Nombre de tentatives du mâle") +
  theme_light(base_size = 20) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,12),breaks = c(0,3,6,9,12))

plot3 <- ggplot(data, aes(x=femalematedbefore, y=mean.courtship.length, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Vierge","Fécondée")) +
  ylab("Durée moyenne des tentatives (s)") +
  theme_light(base_size = 20) +
  theme(legend.position="none")

library(gridExtra)
grid.arrange(plot1,plot2,plot3,ncol=3)
