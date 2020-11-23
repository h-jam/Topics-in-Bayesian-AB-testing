library(yaml)
library(ggplot2)
library(survival) # to fit survival models
library(survminer) # to plot survival data

# A data set
data <- yaml.load_file("example-3-edm-promotion/example-3-data.yaml")
df <- data.frame(data$data)

# Censoring/conversions in each group
prop.table(table(df$group, df$censored), 1)

# plot density of response times
png("Report/plots/example-3-time-to-event.png", height = 400, width = 600)
df$group <- as.factor(df$group)
levels(df$group) <- c("Group A", "Group B")
ggplot(df, aes(x = time_diff, fill = factor(censored)), col ="white") + 
  geom_histogram(alpha = 0.6) + 
  facet_wrap(~group, nrow = 2, scales = "free")+
  theme_classic() + 
  theme(legend.position="bottom") + 
  scale_fill_manual(name = element_blank(), 
                      labels = c('Event', 'Censored'),
                     values = c('darkblue','green')) + 
  xlab("Time to Complete (Days)") + 
  guides(color = FALSE) + 
  ggtitle("Distribution of Time to Event")
dev.off()

# fit a Kaplan-Meyer estimate
fit1 <- survfit(Surv(time_diff, event)~ group, data = df[df$event == 1,])

png("Report/plots/example-3-KM.png", height = 300, width = 600)
ggsurvplot(fit1, 
           alpha = 0.5,
           palette = c('red','NavyBlue'),
           legend = "bottom",
           xlab = "Time (Days)",
           size = 2,
           legend.title = element_blank(),
           legend.labs = c('Group A', 'Group B'),
           title = "Example 3 Kaplan-Meier Estimate of Survival Function")
dev.off()

# log-rank test
survdiff(Surv(time_diff, event)~ group, data = df)

# fit a cox proportional hazards model
fit2 <- coxph(Surv(time_diff, event)~ group, data = df)
cox.zph(fit2)

plot(cox.zph(fit2))

# forest plot of hazard ratio
ggforest(fit2)

#end
