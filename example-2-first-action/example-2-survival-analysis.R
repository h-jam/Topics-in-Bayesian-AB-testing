library(yaml)
library(ggplot2)
library(survival) # to fit survival models
library(survminer) # to plot survival data

# A data set
data <- yaml.load_file("example-2-first-action/example-2-data.yaml")
df <- data.frame(data$data)

# Censoring/conversions in each group
prop.table(table(df$group, df$is_censored), 1)

# plot density of response times
png("Report/plots/example-2-time-to-event.png", height = 400, width = 600)
df$group <- as.factor(df$group)
levels(df$group) <- c("Group A", "Group B")
ggplot(df, aes(x = time_diff, fill = factor(is_censored)), col ="white") + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) + 
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

# Plot observations against time to complete event + censoring
ggplot(df, aes(x = 1:nrow(df), colour = factor(group))) + 
  geom_linerange(aes(ymin = 0, ymax = time_diff), alpha = 0.5) + 
  geom_point(aes(y = time_diff, shape = factor(is_censored)), size = 2) + 
  scale_y_continuous(breaks = 0:15*2) +
  ylab("Time to Complete (Days)") + 
  xlab("Subject") +
  ggtitle("Time to Event") + 
  scale_shape_manual(name = element_blank(), 
                     labels = c('Event', 'Censored'), 
                     values = c(19, 1)) + 
  scale_colour_manual(name = element_blank(), 
                      labels = c('Control (Post)', 'Treatment (Tour)'),
                      values = c('red','NavyBlue')) + 
  coord_flip() + 
  theme_bw() + theme(legend.position="bottom")

# Estimating survival curves with the Kaplan-Meier method
# need to flip censored and event for survival analysis

# fit a Kaplan-Meyer estimate
fit1 <- survfit(Surv(time_diff, event)~ group, data = df)

png("Report/plots/example-2-KM.png", height = 300, width = 600)
ggsurvplot(fit1, 
           alpha = 0.5,
           palette = c('red','NavyBlue'),
           legend = "bottom",
           xlab = "Time (Days)",
          size = 2,
           legend.title = element_blank(),
           legend.labs = c('Group A', 'Group B'),
           title = "Example 2 Kaplan-Meier Estimate of Survival Function")
dev.off()

# log-rank test
survdiff(Surv(time_diff, event)~ group, data = df)

# fit a cox proportional hazards model
fit2 <- coxph(Surv(time_diff, event)~ group, data = df)
cox.zph(fit2)

plot(cox.zph(fit2))

# forest plot of hazard ratio
ggforest(fit2)
