#Visualization

library(ggplot2)


#Plot full Time Series
ggplot(DT_final, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")



#Daily Mean absolute Deviation
Med_Mad <- DT_final[, .(Med = median(value), Mad = mad(value)),
                  by = (seq(nrow(DT_final)) - 1) %% 48]
ggplot(Med_Mad, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")




#Weekly Mean Absolute Deviation
Med_Mad_Week <- DT_final[, .(Med = median(value), Mad = mad(value)),
                       by = (seq(nrow(DT_final)) - 1) %% (48*7)]
ggplot(Med_Mad_Week, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")
















