require(tidyverse)
require(googlesheets)

# Read Data from Google Sheets
gs_ls()
lithics <- gs_title("lithics_joana")
gs_ws_ls(lithics)
lithics <- gs_read(ss = lithics, ws = "Sheet1")
lithics <- rename(lithics, id = ID)


# Read field data
context <- read.csv("analysis/data/raw_data/context.csv")
xyz <- read.csv("analysis/data/raw_data/xyz.csv")

cols_to_concat <- c("Unit", "ID")

context$ID <- str_squish(context$ID)
xyz$ID <- str_squish(xyz$ID)

context <- context %>%
  unite_(col='id', cols_to_concat, sep="-", remove=FALSE) %>%
  select(id, Spit, Level, Code) %>%
  distinct(id, .keep_all = TRUE)

xyz <- xyz %>%
  rename(Unit = UNIT) %>%
  unite_(col='id', cols_to_concat, sep="-", remove=FALSE) %>%
  filter(Suffix == 0) %>%
  select(id, X, Y, Z) %>%
  distinct(id, .keep_all = TRUE)


########################
# Join field tables
field_data <- full_join(xyz, context, by = "id")

lithic_data <- field_data %>%
  filter(Code == "LITHIC")
write.csv(lithic_data, file = "analysis/data/derived_data/lithic_data.csv")


# Join field and lithic tables

dataset <- inner_join(field_data, lithics, by = "id")
write.csv(dataset, file = "analysis/data/derived_data/dataset.csv")


# Plot classes through stratigraphy using artificial spits

dataset <- read.csv("analysis/data/derived_data/dataset.csv")


# Calulate spit volumes
cols_to_concat <- c("Level", "Spit")

buckets_by_spit <- context %>%
  filter(Level %in% c("4E", "5", "5B", "5C") & Code == "BUCKET" & !Spit %in% c("limp_corte", "profile", " ", NA)) %>%
  unite_(col='depth', cols_to_concat, sep=".", remove=FALSE)

buckets_by_spit$depth <- str_replace(buckets_by_spit$depth, "B", "")
buckets_by_spit$depth <- str_replace(buckets_by_spit$depth, "C", "")

spit_vol <- buckets_by_spit %>%
  group_by(depth) %>%
  count(Code) %>%
  mutate(spit_vol = (n*10)/1000)


#############################
# Calculate average depth of spits

spit_depths <- field_data %>%
  filter(Level %in% c("4E", "5", "5B", "5C") & !Spit %in% c("limp_corte", "LIMP_CORTE", "profile", " ", NA, "0")) %>%
  unite_(col='depth', cols_to_concat, sep=".", remove=FALSE)

spit_depths$depth <- str_replace(spit_depths$depth, "B", "")
spit_depths$depth <- str_replace(spit_depths$depth, "C", "")

spit_aver_depths <- spit_depths %>%
  group_by(depth) %>%
  summarise(avg = mean(Z)) %>%
  mutate_if(is.numeric, format, 3)


#########################
## Data classes per spit

full_data <- read.csv("analysis/data/derived_data/dataset.csv")

cols_to_concat <- c("Level", "Spit")

lithics_by_spit <- full_data %>%
  filter(Level %in% c("4E", "5", "5B", "5C") & !Spit %in% c("limp_corte", "profile", " ", NA)) %>%
  select(id, Z, Spit, Level, Code, Classe, Fragmentacao_Artefacto, N_Esquirolas, Tipo_Retocado) %>%
  unite_(col='depth', cols_to_concat, sep=".", remove=FALSE)

lithics_by_spit$depth <- str_replace(lithics_by_spit$depth, "B", "")
lithics_by_spit$depth <- str_replace(lithics_by_spit$depth, "C", "")


############

cores_by_spit <- lithics_by_spit %>%
  filter(Classe == "Núcleo") %>%
  group_by(depth) %>%
  count(Classe)

###########

debitage_by_spit <- lithics_by_spit %>%
  filter(Classe %in% c("Lasca", "Produto alongado"))

debitage_by_spit$Classe <- str_replace(debitage_by_spit$Classe, "Lasca", "Debitagem")
debitage_by_spit$Classe <- str_replace(debitage_by_spit$Classe, "Produto alongado", "Debitagem")

debitage_prox_by_spit <- lithics_by_spit %>%
  filter(Classe %in% c("Fragmento Lasca", "Fragmento produto alongado") & Fragmentacao_Artefacto == "Proximal")

debitage_prox_by_spit$Classe <- str_replace(debitage_prox_by_spit$Classe, "Fragmento Lasca", "Debitagem")
debitage_prox_by_spit$Classe <- str_replace(debitage_prox_by_spit$Classe, "Fragmento produto alongado", "Debitagem")

debitage_by_spit_all <- bind_rows(debitage_by_spit, debitage_prox_by_spit, id = NULL)

debitage_by_spit_all <- debitage_by_spit_all %>%
  group_by(depth) %>%
  count(Classe)


##################

retouched_tools_by_spit <- lithics_by_spit %>%
  filter(Classe == "Produto retocado") %>%
  group_by(depth) %>%
  count(Classe)


##################

chips_by_spit <- lithics_by_spit %>%
  filter(Classe == "Esquírola") %>%
  select(depth, N_Esquirolas) %>%
  group_by(depth) %>%
  summarise(n = sum(N_Esquirolas))

chips_by_spit$Classe <- rep("Esquírola",nrow(chips_by_spit))
chips_by_spit <- select(chips_by_spit, depth, Classe, n)


##################

burins_by_spit <- lithics_by_spit %>%
  filter(Tipo_Retocado %in% c("Buril", "Buril sobre truncatura"))

burins_by_spit$Tipo_Retocado <- str_replace(burins_by_spit$Tipo_Retocado, "Buril sobre truncatura", "Buril")

burins_by_spit <- burins_by_spit %>%
  group_by(depth) %>%
  count(Tipo_Retocado)

burins_by_spit <- select(burins_by_spit, depth, Classe = Tipo_Retocado, n)


##################

endscrapers_by_spit <- lithics_by_spit %>%
  filter(Tipo_Retocado == "Raspadeira") %>%
  group_by(depth) %>%
  count(Tipo_Retocado)

endscrapers_by_spit <- select(endscrapers_by_spit, depth, Classe = Tipo_Retocado, n)


#########################
# Data classes per cubic meter !!Needs revision!!

cores_by_spit$spit_vol <- spit_vol$spit_vol[match(cores_by_spit$depth, spit_vol$depth)]
cores_by_spit <- cores_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
cores_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(cores_by_spit$depth, spit_aver_depths$depth)]
cores_by_spit$spit_aver_depth <- as.numeric(as.character(cores_by_spit$spit_aver_depth))


debitage_by_spit_all$spit_vol <- spit_vol$spit_vol[match(debitage_by_spit_all$depth, spit_vol$depth)]
debitage_by_spit_all <- debitage_by_spit_all %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
debitage_by_spit_all$spit_aver_depth <- spit_aver_depths$avg[match(debitage_by_spit_all$depth, spit_aver_depths$depth)]
debitage_by_spit_all$spit_aver_depth <- as.numeric(as.character(debitage_by_spit_all$spit_aver_depth))

retouched_tools_by_spit$spit_vol <- spit_vol$spit_vol[match(retouched_tools_by_spit$depth, spit_vol$depth)]
retouched_tools_by_spit <- retouched_tools_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
retouched_tools_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(retouched_tools_by_spit$depth, spit_aver_depths$depth)]
retouched_tools_by_spit$spit_aver_depth <- as.numeric(as.character(retouched_tools_by_spit$spit_aver_depth))


chips_by_spit$spit_vol <- spit_vol$spit_vol[match(chips_by_spit$depth, spit_vol$depth)]
chips_by_spit <- chips_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol) %>%
  filter(!depth %in% c("5.9", "5.10"))
chips_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(chips_by_spit$depth, spit_aver_depths$depth)]
chips_by_spit$spit_aver_depth <- as.numeric(as.character(chips_by_spit$spit_aver_depth))

burins_by_spit$spit_vol <- spit_vol$spit_vol[match(burins_by_spit$depth, spit_vol$depth)]
burins_by_spit <- burins_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
burins_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(burins_by_spit$depth, spit_aver_depths$depth)]
burins_by_spit$spit_aver_depth <- as.numeric(as.character(burins_by_spit$spit_aver_depth))

endscrapers_by_spit$spit_vol <- spit_vol$spit_vol[match(endscrapers_by_spit$depth, spit_vol$depth)]
endscrapers_by_spit <- endscrapers_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
endscrapers_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(endscrapers_by_spit$depth, spit_aver_depths$depth)]
endscrapers_by_spit$spit_aver_depth <- as.numeric(as.character(endscrapers_by_spit$spit_aver_depth))


# Merge tables

artifacts_by_cubic_meter <- bind_rows(chips_by_spit, debitage_by_spit_all)

artifacts_by_cubic_meter <- filter(artifacts_by_cubic_meter, spit_vol > 0.10) # removes low volumetric levels


# Plot

ggplot(artifacts_by_cubic_meter, (aes(spit_aver_depth, artifacts_by_cubic_meter, colour = Classe))) +
  geom_point() +
  stat_smooth(span = 0.5, se = FALSE) +
  xlab("Profundidade (m)") +
  ylab("Número de artefactos por metro cúbico de sedimento (escala logarítmica)") +
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size= 12),
        legend.title = element_text(size = 14)) +
  scale_y_log10()+
  coord_flip() +
  scale_color_manual(values = c("grey50", "grey20")) +
  theme_minimal()


ggplot(artifacts_by_cubic_meter, (aes(spit_aver_depth, artifacts_by_cubic_meter, colour = Classe))) +
  geom_point() +
  geom_line() +
  xlab("Profundidade (m)") +
  ylab("Número de artefactos por metro cúbico de sedimento (escala logarítmica)") +
  theme(axis.text=element_text(size=10)) +
  scale_y_log10() +
  coord_flip()


############################
## Elongated products chart

alongados <- lithics %>%
  filter(Classe == "Produto alongado" & Materia == "Silex") %>%
  select(Materia, Espessura, Largura)

# Histogram
require(plyr)
mu <- ddply(alongados, "Materia", summarise, grp.mean=mean(Largura))

ggplot(alongados, aes(x=Largura, color=Materia, fill=Materia)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Materia),
             linetype="dashed")+
  scale_color_manual(values = "grey50")+
  scale_fill_manual(values = "grey50")+
  labs(x="Largura(mm)", y = "Densidade") +
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size= 12),
        legend.title = element_text(size = 14))


# Scatterplot

alongados_all <- lithics %>%
  filter(Classe == "Produto alongado" & Materia == "Silex") %>%
  select(Materia, Comprimento, Largura)


ggplot(alongados_all,aes(Largura, Comprimento)) +
  geom_point() +
  geom_smooth( method = lm, se = FALSE) +
  scale_color_grey(start=0.8, end=0.2) +
  xlab("Largura (mm)") +
  ylab("Comprimento (mm)") +
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size= 12),
        legend.title = element_text(size = 14)) +
  theme_minimal()

##########################
## Flake mass vs retouched mass

debitage_mass <- lithics %>%
  filter(Classe %in% c("Lasca", "Produto alongado", "Produto retocado") & Materia == "Silex") %>%
  select(Classe, Peso) %>%
  filter(Peso > 0)

ggplot(debitage_mass, aes(Classe, Peso)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_y_log10()


##########################
## Raw Materials

raw_materials <- lithics %>%
  filter(!Classe %in% c("Fragmento", "Esquírola")) %>%
  group_by(Materia) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n)) * 100)

ggplot(raw_materials, aes(x = reorder(Materia, -freq), y = freq)) +
  geom_bar(stat = "identity", width = 0.7,  fill = "grey50") +
  xlab("Matérias-primas") +
  ylab("Percentagem") +
  theme(axis.text=element_text(size=14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size= 14),
        legend.title = element_text(size = 16)) +
  theme_minimal()


##########################
## Cortex

cortex$Cortex = factor(cortex$Cortex, levels=c('0%','1-30%','31-60%','61-99%','100%'))
cortex$Materia = factor(cortex$Materia, levels=c('Silex','Grauvaque','Calcedonia','Dolerito','Outro'))

cortex <- lithics %>%
  filter(Classe %in% c("Núcleo", "Produto alongado", "Lasca", "Produto retocado") & Cortex != "NA") %>%
  mutate(Classe = replace(Classe, Classe == "Produto retocado", "Utensilio retocado")) %>%
  group_by(Materia, Classe, Cortex) %>%
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n)) * 100)

ggplot() +
  geom_bar(aes(y = n, x = Classe, fill = Cortex), data = cortex, stat = "identity", position = "fill") +
  facet_wrap( ~ Materia, ncol = 2, scales='free') +
  xlab("Classes") +
  ylab("Percentagem") +
  theme(axis.text=element_text(size=24),
        axis.title.x = element_text(size = 26),
        axis.title.y = element_text(size = 26),
        legend.text = element_text(size= 24),
        legend.title = element_text(size = 26)) +
  scale_fill_grey(start=0.7, end=0.2) +
  theme_minimal()


###################
## Dolerito

dolerito <- full_data %>%
  filter(Materia == "Dolerito") %>%
  select(Materia, Level, Spit) %>%
  group_by(Level, Spit) %>%
  dplyr::summarise(n = n())
