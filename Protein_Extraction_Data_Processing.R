#---------------Growth Curve From Protein Extraction Data---------------#
#Load data
##Your data should be formatted in two columns in a CSV file
###My first column was called "SampleInfo" and was formatted in the following way: SUBSTRATE_TIMEPOINT_INOCULA_REPLICATE, edit line 11 to contain the variables you have in your "SampleInfo" Column.
###The second column was called "Absorbance".
protExt4 <- as.csv("/home/lgschaer/old/Plastic_Deg/Protein_Extraction/BCA_ProteinExtraction_10132020.csv", row.names = 1, header = TRUE, sep = ",", check.names = TRUE, stringsAsFactors = TRUE)
head(protExt4)

protein_info <- protExt4 %>%
  mutate(SampleInfo = SampleID) %>%
  separate(SampleInfo, into = c("Substrate", "Time_Point", "Inocula", "Replicate"), sep = "_") %>%
  group_by(Substrate,Time_Point, Inocula) %>%
  mutate(estProtein_mL = ((Absorbance - .1158)/0.3641)*2, #convert absorbance to estimated mg protein per mL
         minProt = min(estProtein_mL),
         maxProt = max(estProtein_mL),
         avgProtein_mL = mean(estProtein_mL))
head(protein_info)

colors <- c("Blue", "Red", "Orange", "Purple", "Green", "Magenta")

#plot with ggplot
ggplot(protein_info, mapping = aes(x = factor(Time_Point, levels = c("Blank", "T0", "T1", "T2", "T3", "T4")), 
                                   y = estProtein_mL, color = Substrate), show.legend = TRUE)+
  facet_grid(cols = vars(Substrate), rows = , shrink = TRUE)+
  geom_boxplot(show.legend = TRUE)+
  ggtitle("Protein Extraction") +
  ylab("Estimated Protein (mg/mL)") +
  xlab("Time")+
  scale_color_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 15, angle = 90),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

#TPA only

tpa_only <- protein_info %>%
  filter(Substrate == "TPA")
head(tpa_only)

colors <- c("Blue", "Red", "Orange", "Purple", "Green", "Magenta", "Grey", "lightblue")

#plot with ggplot

ggplot(tpa_only, mapping = aes(x = factor(Time_Point, levels = c("Blank", "T0", "T1", "T2", "T3", "T4")), 
                               y = avgProtein_mL, fill = Inocula), color = "black", show.legend = TRUE)+
  geom_col(aes(x = Time_Point, y = avgProtein_mL, fill = Inocula), color = "black", position = "dodge")+
  geom_errorbar(mapping=aes(ymin=minProt, ymax=maxProt), color = "black", position = "dodge") +
  ggtitle("Protein Extraction") +
  ylab("Estimated Protein (mg/mL)") +
  xlab("Time")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 15, angle = 0),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

#TA only

ta_only <- protein_info %>%
  filter(Substrate == "TA")
head(ta_only)

colors <- c("Blue", "Red", "Orange", "Purple", "Green", "Magenta", "Grey", "lightblue")

#plot with ggplot

ggplot(ta_only, mapping = aes(x = factor(Time_Point, levels = c("Blank", "T0", "T1", "T2", "T3", "T4")), 
                              y = avgProtein_mL, fill = Inocula), color = "black", show.legend = TRUE)+
  geom_col(aes(x = Time_Point, y = avgProtein_mL, fill = Inocula), color = "black", position = "dodge")+
  geom_errorbar(mapping=aes(ymin=minProt, ymax=maxProt), color = "black", position = "dodge") +
  ggtitle("Protein Extraction") +
  ylab("Estimated Protein (mg/mL)") +
  xlab("Time")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 15, angle = 0),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))


#BPA only

bpa_only <- protein_info %>%
  filter(Substrate == "BPA")
head(bpa_only)

colors <- c("Blue", "Red", "Orange", "Purple", "Green", "Magenta", "Grey", "lightblue")

#plot with ggplot

ggplot(bpa_only, mapping = aes(x = factor(Time_Point, levels = c("Blank", "T0", "T1", "T2", "T3", "T4")), 
                               y = avgProtein_mL, fill = Inocula), color = "black", show.legend = TRUE)+
  geom_col(aes(x = Time_Point, y = avgProtein_mL, fill = Inocula), color = "black", position = "dodge")+
  geom_errorbar(mapping=aes(ymin=minProt, ymax=maxProt), color = "black", position = "dodge") +
  ggtitle("Protein Extraction") +
  ylab("Estimated Protein (mg/mL)") +
  xlab("Time")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 15, angle = 0),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))
