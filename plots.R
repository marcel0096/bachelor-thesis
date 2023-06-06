# Generate basic plot displaying prices for each hour

df.example <- df.example[df.example$Instance_Type != "c6g.metal", ]

df.base <- df.example[df.example$Instance_Number == 0, ]
df.base <- df.base[rep(seq_len(nrow(df.base)), each = 24), ]
df.base$Instance_Number <- 1:24

df.fluct <- df.example[df.example$Instance_Number != 0, ]
df.fluct$group <- cumsum(df.fluct$Instance_Type != lag(df.fluct$Instance_Type, default = ""))

df.fluct$Total_Costs <- df.fluct$Total_Costs + df.base$Total_Costs


# only connect point with same color
# ggplot() +
#   geom_point(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   geom_line(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type, group = group), alpha = 0.5) +
#   geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
#   scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
#   labs(x = "Hours", y = "Prices")


ggplot(df.fluct, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
  scale_fill_discrete(name = "Fluct") +
  geom_point() +
  geom_segment(df.fluct, mapping = aes(x = lag(Instance_Number), y = lag(Total_Costs), 
                                       xend = Instance_Number, yend = Total_Costs), alpha = 0.5) + 
  geom_line(df.base, mapping = aes(x = Instance_Number, y = Total_Costs, color = Instance_Type)) +
  scale_y_continuous(name = "Prices - base", sec.axis = sec_axis(trans = ~. - df.base[1, "Total_Costs"], name = "Prices - fluct")) +
  scale_x_continuous(breaks = df.fluct$Instance_Number) +
  labs(x = "Hours", y = "Prices", color = "Instances") +
  theme_bw()

# adapt axis
# make exact price appear next to instance

