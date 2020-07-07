"Victim Variable:"

"Should have Youth and Adult victim separate and then a cumulative var?"

# 0 = not a victim in past 12 months
# 1 = victim in past 12 months

# 1. You were shot/stabbed
# 2. You got jumped
# 3. You had a knife/gun pulled on you
# 4. You saw someone get shot/stabbed

"Variables included"

# 1. You were shot/stabbed:
table(waves$H1FV3)
table(waves$H1FV4)
table(waves$H2FV3)
table(waves$H2FV4)
table(waves$H3DS18D)
table(waves$H3DS18E)
table(waves$H4DS16)

# 2. You were jumped:
table(waves$H1FV6)
table(waves$H2FV5)
table(waves$H3DS18F)
table(waves$H3DS18G)
table(waves$H4DS18)

# 3. You had a knife/gun pulled on you:
table(waves$H1FV2)
table(waves$H2FV2)
table(waves$H3DS18B)
table(waves$H3DS18C)
table(waves$H4DS15)

# 4. You saw someone get shot/stabbed:
table(waves$H1FV1)
table(waves$H2FV1)
table(waves$H3DS18A)
table(waves$H4DS14)

"Cleaning up each variable:"
# You were shot/stabbed:
waves <- waves %>%
  mutate(JH1FV3 = case_when(
    H1FV3 %in% c(6, 8, 9) ~ NaN,
    H1FV3 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH1FV4 = case_when(
    H1FV4 %in% c(6, 8, 9) ~ NaN,
    H1FV4 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV3 = case_when(
    H2FV3 %in% c(6, 8) ~ NaN,
    H2FV3 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV4 = case_when(
    H2FV4 %in% c(6, 8) ~ NaN,
    H2FV4 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18D = replace(H3DS18D, H3DS18D %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18E = replace(H3DS18E, H3DS18E %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS16 = replace(H4DS16, H4DS16 %in% c(NA, 6, 8), NA))

table(waves$JH1FV3)
table(waves$JH1FV4)
table(waves$JH2FV3)
table(waves$JH2FV4)
table(waves$AH3DS18D)
table(waves$AH3DS18E)
table(waves$AH4DS16)

# 2. You were jumped:

waves <- waves %>%
  mutate(JH1FV6 = case_when(
    H1FV6 %in% c(6, 8, 9) ~ NaN,
    H1FV6 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV5 = case_when(
    H2FV5 %in% c(6, 8) ~ NaN,
    H2FV5 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18F = replace(H3DS18F, H3DS18F %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18G = replace(H3DS18G, H3DS18G %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS18 = replace(H4DS18, H4DS18 %in% c(NA, 6, 8), NA))

table(waves$JH1FV6)
table(waves$JH2FV5)
table(waves$AH3DS18F)
table(waves$AH3DS18G)
table(waves$AH4DS18)

# 3. You had a knife/gun pulled on you:
waves <- waves %>%
  mutate(JH1FV2 = case_when(
    H1FV2 %in% c(6, 8, 9) ~ NaN,
    H1FV2 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV2 = case_when(
    H2FV2 %in% c(6, 8) ~ NaN,
    H2FV2 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18B = replace(H3DS18B, H3DS18B %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH3DS18C = replace(H3DS18C, H3DS18C %in% c(NA, 6, 8, 9), NA)) %>%
  mutate(AH4DS15 = replace(H4DS15, H4DS15 %in% c(NA, 6, 8), NA))

table(waves$JH1FV2)
table(waves$JH2FV2)
table(waves$AH3DS18B)
table(waves$AH3DS18C)
table(waves$AH4DS15)

# 4. You saw someone get shot/stabbed:
waves <- waves %>%
  mutate(JH1FV1 = case_when(
    H1FV1 %in% c(6, 8, 9) ~ NaN,
    H1FV1 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(JH2FV1 = case_when(
    H2FV1 %in% c(6, 8) ~ NaN,
    H2FV1 %in% c(1, 2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH3DS18A = case_when(
    H3DS18A %in% c(6, 8, 9) ~ NaN,
    H3DS18A == 1 ~ 1,
    TRUE ~ 0)) %>%
  mutate(AH4DS14 = replace(H4DS14, H4DS14 %in% c(NA, 6, 8), NA))

table(waves$JH1FV1)
table(waves$JH2FV1)
table(waves$AH3DS18A)
table(waves$AH4DS14)

"Creating the Juvenile Victim Variable:"

table(waves$JH1FV3)
table(waves$JH1FV4)
table(waves$JH2FV3)
table(waves$JH2FV4)
table(waves$JH1FV6)
table(waves$JH2FV5)
table(waves$JH1FV2)
table(waves$JH2FV2)
table(waves$JH1FV1)
table(waves$JH2FV1)

waves <- waves %>%
  mutate(JVictim = case_when(
    JH1FV3 == 1 | JH1FV4 == 1 |
    JH2FV3 == 1 | JH2FV4 == 1 |
    JH1FV6 == 1 | JH2FV5 == 1 |
    JH1FV2 == 1 | JH2FV2 == 1 |
    JH1FV1 == 1 | JH2FV1 == 1 ~ 1,
    TRUE ~ 0))

table(waves$JVictim)

test <-  waves %>%
  select(JH1FV3, JH1FV4, JH2FV3, JH2FV4, JH1FV6,
         JH2FV5, JH1FV2, JH2FV2, JH1FV1, JH2FV1) %>%
  mutate(Total = rowSums(.[1:10],na.rm = TRUE))
table(test$Total)

"Creating the Adult Victim Variable:"

table(waves$AH3DS18D)
table(waves$AH3DS18E)
table(waves$AH4DS16)
table(waves$AH3DS18F)
table(waves$AH3DS18G)
table(waves$AH4DS18)
table(waves$AH3DS18B)
table(waves$AH3DS18C)
table(waves$AH4DS15)
table(waves$AH3DS18A)
table(waves$AH4DS14)

waves <- waves %>%
  mutate(AVictim = case_when(
    AH3DS18D == 1 | AH3DS18E == 1 | AH4DS16 == 1 |
    AH3DS18F == 1 | AH3DS18G == 1 | AH4DS18 == 1 |
    AH3DS18B == 1 | AH3DS18C == 1 | AH4DS15 == 1 |
    AH3DS18A == 1 | AH4DS14 == 1 ~ 1,
    TRUE ~ 0))

table(waves$AVictim)

test <-  waves %>%
  select(AH3DS18D, AH3DS18E, AH4DS16, AH3DS18F, AH3DS18G, AH4DS18,
         AH3DS18B, AH3DS18C, AH4DS15, AH3DS18A, AH4DS14) %>%
  mutate(Total = rowSums(.[1:11], na.rm = TRUE))
table(test$Total)








