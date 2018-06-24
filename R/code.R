require(tidyverse)

# Read Data from Google Sheets
require(googlesheets)
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



# Join field tables
field_data <- full_join(xyz, context, by = "id")

# Join field and lithic tables
dataset <- inner_join(field_data, lithics, by = "id")
write.csv(dataset, file = "analysis/data/derived_data/dataset.csv")

# Plot classes through stratigraphy using artificial spits

require(ggplot2)
require(tidyverse)

read.csv("analysis/data/derived_data/dataset.csv")

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



# Data classes per spit

full_data <- read.csv("analysis/data/derived_data/dataset.csv")

cols_to_concat <- c("Level", "Spit")

lithics_by_spit <- full_data %>%
  filter(Level %in% c("4E", "5", "5B", "5C") & !Spit %in% c("limp_corte", "profile", " ", NA)) %>%
  select(id, Z, Spit, Level, Code, Classe, N_Esquirolas) %>%
  unite_(col='depth', cols_to_concat, sep=".", remove=FALSE)

lithics_by_spit$depth <- str_replace(lithics_by_spit$depth, "B", "")
lithics_by_spit$depth <- str_replace(lithics_by_spit$depth, "C", "")

cores_by_spit <- lithics_by_spit %>%
  filter(Classe == "Núcleo") %>%
  group_by(depth) %>%
  count(Classe)

debitage_by_spit <- lithics_by_spit %>%
  filter(Classe %in% c("Lasca", "Produto alongado")) %>%
  group_by(depth) %>%
  count(Classe)

retouched_tools_by_spit <- lithics_by_spit %>%
  filter(Classe == "Produto retocado") %>%
  group_by(depth) %>%
  count(Classe)

chips_by_spit <- lithics_by_spit %>%
  filter(Classe == "Esquírola") %>%
  select(depth, N_Esquirolas) %>%
  group_by(depth) %>%
  summarise(n = sum(N_Esquirolas))

chips_by_spit$Classe <- rep("Esquírola",nrow(chips_by_spit))
chips_by_spit <- select(chips_by_spit, depth, Classe, n)



# Data classes per cubic meter

cores_by_spit$spit_vol <- spit_vol$spit_vol[match(cores_by_spit$depth, spit_vol$depth)]
cores_by_spit <- cores_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
cores_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(cores_by_spit$depth, spit_aver_depths$depth)]
cores_by_spit$spit_aver_depth <- as.numeric(as.character(cores_by_spit$spit_aver_depth))


debitage_by_spit$spit_vol <- spit_vol$spit_vol[match(debitage_by_spit$depth, spit_vol$depth)]
debitage_by_spit <- debitage_by_spit %>%
  mutate(artifacts_by_cubic_meter = n/spit_vol)
debitage_by_spit$spit_aver_depth <- spit_aver_depths$avg[match(debitage_by_spit$depth, spit_aver_depths$depth)]
debitage_by_spit$spit_aver_depth <- as.numeric(as.character(debitage_by_spit$spit_aver_depth))

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


# Merge tables

artifacts_by_cubic_meter <- bind_rows(cores_by_spit, debitage_by_spit, chips_by_spit, retouched_tools_by_spit)

## artifacts_by_cubic_meter <- filter(artifacts_by_cubic_meter, spit_vol > 0.10) # removes low volumetric levels


# Plot

ggplot(artifacts_by_cubic_meter, (aes(spit_aver_depth, artifacts_by_cubic_meter, colour = Classe))) +
  geom_point() +
  stat_smooth(span = 0.5, se = FALSE) +
  xlab("Profundidade (m)") +
  ylab("Número de artefactos por metro cúbico de sedimento (escala logarítmica)") +
  theme(axis.text=element_text(size=10)) +
  scale_y_log10()+
  coord_flip()
