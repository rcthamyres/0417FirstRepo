Health_stat %>% filter(`Disease Name` == "Cancer", Country == "USA")  %>% group_by(Year) %>% summarise(avg_treatment_cost = mean(`Average Treatment Cost (USD)`, na.rm = TRUE)) %>% ggplot(aes(x = Year, y = avg_treatment_cost)) + geom_line(color="purple") + geom_point() + labs(x = "Year", y = "Average Treatment Cost (USD)", title = "Average Cancer Treatment Cost in the USA Over the Years")+ geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linetype = "longdash",linewidth=.6)+
  theme(text = element_text(family="Helvetica",size=16), 
        panel.grid.major= element_line(color="grey", linetype="dotted"), 
        panel.grid.minor = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect("white"), 
        plot.title= element_text(face="bold", hjust = 0.5,margin = margin(b = 12)),
        axis.title = element_text(face="bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave(filename = "/Users/thamyresreginacosta/Documents/uw-madison/J677/cancer_cost_usa.png",dpi=600,width=16)
Health_stat %>% filter(`Disease Name` == "Cancer", Country == "USA") %>% 
  group_by(Year) %>% 
  summarise(`Average Treatment Cost (USD)` = mean(`Average Treatment Cost (USD)`, na.rm = TRUE),`Recovery Rate (%)` = mean(`Recovery Rate (%)`, na.rm = TRUE)) %>%
  pivot_longer(cols = c(`Average Treatment Cost (USD)`, `Recovery Rate (%)`),names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(x = Year, y = Value)) + geom_line(color="purple") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linetype = "longdash",linewidth=.6) + facet_wrap(~ Metric, scales = "free_y", ncol = 1) + labs(title = "Comparison Between Average Treatment Cost and Recovery Rate",x = "Year",y = NULL) + 
  theme(text = element_text(family="Helvetica",size=13), 
        panel.grid.major= element_line(color="grey", linetype="dotted"), 
        panel.grid.minor = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect("white"), 
        plot.title= element_text(face="bold", hjust = 0.5,margin = margin(b = 12)),
        axis.title = element_text(face="bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(face = "bold",size=11),
        strip.background = element_blank())
ggsave(filename = "/Users/thamyresreginacosta/Documents/uw-madison/J677/cancer_costandrecovery_usa.png",dpi=600)
Health_stat %>% filter(`Disease Name` == "Cancer", Country == "USA") %>% group_by(Year) %>% summarise(`Recovery Rate (%)` = mean(`Recovery Rate (%)`, na.rm = TRUE),`Per Capita Income (USD)` = mean(`Per Capita Income (USD)`, na.rm = TRUE)) %>% pivot_longer(cols = c(`Recovery Rate (%)`, `Per Capita Income (USD)`), names_to = "Metric", values_to = "Value") %>% ggplot(aes(x = Year, y = Value)) + geom_line(color="purple") + geom_point(size=1) + geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linetype = "longdash") + facet_wrap(~ Metric, scales = "free_y", ncol = 1) + labs(title = "Relationship Between Recovery Rate and Income",x = "Year",y = NULL) +
  theme(text = element_text(family="Helvetica",size=13), 
        panel.grid.major= element_line(color="grey", linetype="dotted"), 
        panel.grid.minor = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect("white"), 
        plot.title= element_text(face="bold", hjust = 0.5,margin = margin(b = 12)),
        axis.title = element_text(face="bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(face = "bold",size=11),
        strip.background = element_blank(),
        panel.spacing = unit(1.5, "lines"))
ggsave(filename = "/Users/thamyresreginacosta/Documents/uw-madison/J677/cancer_recoveryandincome_usa.png",dpi=600)
Health_stat %>% filter(`Disease Name` == "Cancer", Country == "USA") %>% group_by(Year, `Age Group`) %>% summarise(avg_recovery = mean(`Recovery Rate (%)`, na.rm = TRUE)) %>% ggplot(aes(x = Year, y = avg_recovery)) + geom_line(color="purple") + geom_point(size=.6) + geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linetype = "longdash") + facet_wrap(~ `Age Group`) + labs(title = "How Recovery Rate Varies by Age Group",x = "Year",y = "Average Recovery Rate (%)") +
  theme(text = element_text(family="Helvetica",size=13), 
        panel.grid.major= element_line(color="grey", linetype="dotted"), 
        panel.grid.minor = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect("white"), 
        plot.title= element_text(face="bold", hjust = 0.5,margin = margin(b = 12)),
        axis.title = element_text(face="bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(face = "bold",size=11),
        strip.background = element_blank(),
        panel.spacing = unit(1.5, "lines"))
ggsave(filename = "/Users/thamyresreginacosta/Documents/uw-madison/J677/cancer_recoveryage_usa.png",dpi=600,width=18)
Health_stat %>% filter(`Disease Name` == "Cancer", Country == "USA") %>% group_by(Year, Gender) %>% summarise(avg_recovery = mean(`Recovery Rate (%)`, na.rm = TRUE)) %>% ggplot(aes(x = Year, y = avg_recovery)) + geom_line(color="purple") + geom_point(size=.4) + geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linetype = "longdash") + facet_wrap(~ Gender) + labs(title = "How Recovery Rate Varies by Gender", x = "Year", y = "Average Recovery Rate (%)") +
  theme(text = element_text(family="Helvetica",size=13), 
        panel.grid.major= element_line(color="grey", linetype="dotted"), 
        panel.grid.minor = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect("white"), 
        plot.title= element_text(face="bold", hjust = 0.5,margin = margin(b = 12)),
        axis.title = element_text(face="bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(face = "bold",size=11),
        strip.background = element_blank(),
        panel.spacing = unit(1.5, "lines"))
ggsave(filename = "/Users/thamyresreginacosta/Documents/uw-madison/J677/cancer_recoverygender_usa.png",dpi=600,width=20)
