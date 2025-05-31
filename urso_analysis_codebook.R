## 1 ── Libraries -------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)      # Excel import
  library(dplyr)       # Data wrangling
  library(stringr)     # String utilities
  library(tableone)    # Baseline table
  library(MatchIt)     # Propensity‑score matching
  library(tidyr)       # Pivoting
  library(writexl)     # Optional XLSX export
})

## 2 ── Load workbook ---------------------------------------------------

xls_file <- "OPV + Urso (4).xlsx"     # adjust path if needed
stopifnot(file.exists(xls_file))

# URSO sheet = "Master"
raw_urso  <- read_excel(xls_file, sheet = "Master", .name_repair = "universal")

# CONTROL sheet = "Control Group"
raw_ctrl  <- read_excel(xls_file, sheet = "Control Group", .name_repair = "universal")

## 3 ── Data cleaning & harmonisation ----------------------------------

#---- 3·1  Keep patient rows only --------------------------------------
urso <- raw_urso  %>% filter(is.numeric(Patient))
ctrl <- raw_ctrl  %>% filter(is.numeric(Patient.))   # note dot from name_repair

#---- 3·2  Rename columns for consistency ------------------------------
rename_vec_urso <- c(
  age             = "Age...Dx",
  sex             = "Sex",
  race            = "Race",
  portal_htn      = "Portal.HTN..n.0.y.1",
  cpsh            = "CSPH..n.0.y.1.specify",
  pvt             = "PVT.n.0.y.1",
  ibd_cvid_celiac = "CVID..IBD..celiac..n.0.y.1",
  autoimmune      = "Rheum.Autoimmune.dz.n.0..y.1",
  hypercoag       = "Hyper.coag.state..n.0.y.1..specify",
  ast_0           = "Pretx.AST",
  alt_0           = "Pretx.ALT",
  alp_0           = "Pretx.AP",
  ggt_0           = "Pretx.GGT",
  ast_6           = "1st.PostTx.AST",
  alt_6           = "1st.PostTx.ALT",
  alp_6           = "1st.PostTx.AP",
  ggt_6           = "1st.PostTx.GGT",
  ast_12          = "Latest.PostTx.AST",
  alt_12          = "Latest.PostTx.ALT",
  alp_12          = "Latest.PostTx.AP",
  ggt_12          = "Latest.PostTx.GGT",
  mgkg            = "Urso.mg.kg...Start",
  interval_days   = "Interval.Time..days.",
  urso_ae_raw     = "Urso.A.e"
)

rename_vec_ctrl <- c(
  age             = "Age",
  sex             = "Sex",
  race            = "Race",
  portal_htn      = "portal.htn.yes.1.no...0",
  cpsh            = "CSPH.yes.1.no.0.specify.",
  pvt             = "PVT.yes.1.no.0",
  ibd_cvid_celiac = "CVID..IBD..Celiac.yes.1.No.0",
  autoimmune      = "Rheum..autoimmune.dz.yes.1.no.0",
  hypercoag       = "hypercoag.state.yes.1.no.0",
  ast_0           = "AST",
  alt_0           = "ALT",
  alp_0           = "ALK.phos",
  ggt_0           = "GGT",
  ast_6           = "AST.1",
  alt_6           = "ALT.1",
  alp_6           = "ALK.phos.1",
  ggt_6           = "GGT.1",
  ast_12          = "AST.2",
  alt_12          = "ALT.2",
  alp_12          = "ALK.phos.2",
  ggt_12          = "GGT.2"
)

urso  <- urso  %>% rename(!!!rename_vec_urso)
ctrl  <- ctrl  %>% rename(!!!rename_vec_ctrl)

#---- 3·3  Helper functions --------------------------------------------
clean_yes_no <- function(x) {
  ifelse(str_detect(tolower(trimws(x)), "^(1|y)"), 1L,
         ifelse(str_detect(tolower(trimws(x)), "^(0|n)"), 0L, NA_integer_))
}
harmonise_race <- function(x) {
  first <- toupper(substr(as.character(x), 1, 1))
  recode(first, W="White", B="Black", H="Hispanic", A="Asian", .default="Other")
}
percent_change <- function(x0, x1) (x1 - x0) / x0 * 100

#---- 3·4  Variable conversion -----------------------------------------
urso <- urso %>%
  mutate(across(c(portal_htn, cpsh, pvt, ibd_cvid_celiac,
                  autoimmune, hypercoag), clean_yes_no),
         race_clean = harmonise_race(race),
         across(matches("^ast_|^alt_|^alp_|^ggt_"), as.numeric),
         delta_ast_6  = ast_6  - ast_0,
         delta_alt_6  = alt_6  - alt_0,
         delta_alp_6  = alp_6  - alp_0,
         pct_ast_6    = percent_change(ast_0, ast_6),
         dose_cat     = case_when(
           !is.na(mgkg) & mgkg < 10        ~ "<10",
           !is.na(mgkg) & mgkg <= 15       ~ "10–15",
           !is.na(mgkg) & mgkg > 15        ~ ">15",
           TRUE                            ~ "Not recorded"
         ),
         ae           = ifelse(
           is.na(urso_ae_raw) |
             str_detect(tolower(urso_ae_raw), "^(n|no|none)"), 0L, 1L))

ctrl <- ctrl %>%
  mutate(across(c(portal_htn, cpsh, pvt, ibd_cvid_celiac,
                  autoimmune, hypercoag), clean_yes_no),
         race_clean = harmonise_race(race),
         across(matches("^ast_|^alt_|^alp_|^ggt_"), as.numeric),
         delta_ast_6  = ast_6  - ast_0,
         delta_alt_6  = alt_6  - alt_0,
         delta_alp_6  = alp_6  - alp_0,
         pct_ast_6    = percent_change(ast_0, ast_6))

## 4 ── Propensity‑score matching --------------------------------------

data_comb <- bind_rows(
  urso  %>% mutate(group = "URSO"),
  ctrl  %>% mutate(group = "CONTROL")
) %>%
  mutate(sex_bin = ifelse(str_detect(toupper(sex), "^M"), 1, 0)) %>%
  drop_na(age, sex_bin)   # PS model cannot handle missing

# Logistic PS model (age + sex)
ps_model <- glm(group == "URSO" ~ age + sex_bin,
                data = data_comb, family = binomial)
data_comb$ps <- predict(ps_model, type = "response")

# 1 : 1 nearest‑neighbour (no replacement)
m.out <- matchit(group ~ age + sex_bin,
                 data = data_comb,
                 method = "nearest",
                 replace = FALSE, ratio = 1)

matched <- match.data(m.out)
urso_m  <- matched %>% filter(group == "URSO")
ctrl_m  <- matched %>% filter(group == "CONTROL")

## 5 ── Table 1 : Baseline characteristics -----------------------------

vars_cat <- c("sex", "race_clean", "portal_htn", "cpsh", "pvt",
              "ibd_cvid_celiac", "autoimmune", "hypercoag")
vars_cont <- c("age", "ast_0", "alt_0", "alp_0", "ggt_0")

tbl_pre  <- CreateTableOne(vars = c(vars_cat, vars_cont),
                           strata = "group", data = data_comb,
                           factorVars = vars_cat)
tbl_post <- CreateTableOne(vars = c(vars_cat, vars_cont),
                           strata = "group", data = matched,
                           factorVars = vars_cat)

write.csv(print(tbl_pre, quote = FALSE, noSpaces = TRUE),
          "./output/Table1_pre_match.csv", row.names = FALSE)
write.csv(print(tbl_post, quote = FALSE, noSpaces = TRUE),
          "./output/Table1_post_match.csv", row.names = FALSE)

## 6 ── Table 2 : Temporal enzyme trajectory ---------------------------

temporal_long <- data_comb %>%
  select(group, starts_with("ast_"), starts_with("alt_"),
         starts_with("alp_"), starts_with("ggt_")) %>%
  pivot_longer(-group,
               names_to = c("enzyme", "month"),
               names_pattern = "(ast|alt|alp|ggt)_(0|6|12)",
               values_to = "value") %>%
  mutate(month = recode(month, `0`="baseline", `6`="six", `12`="twelve"))

summary_temporal <- temporal_long %>%
  group_by(group, enzyme, month) %>%
  summarise(n      = sum(!is.na(value)),
            median = median(value, na.rm = TRUE),
            q1     = quantile(value, .25, na.rm = TRUE),
            q3     = quantile(value, .75, na.rm = TRUE), .groups = "drop")

write.csv(summary_temporal,
          "./output/Table2_temporal.csv", row.names = FALSE)

## 7 ── Table 4 : Sub‑group analysis (AST Δ at 6 mo) -------------------

subgroups <- list(
  `Sex: Male`          = list(col = "sex"        , filter = ~ str_detect(toupper(sex), "^M")),
  `Age ≥ 50 years`     = list(col = "age"        , filter = ~ age >= 50),
  `Portal HTN`         = list(col = "portal_htn" , filter = ~ portal_htn == 1),
  `Auto‑immune`        = list(col = "autoimmune" , filter = ~ autoimmune == 1),
  `Hyper‑coagulable`   = list(col = "hypercoag"  , filter = ~ hypercoag == 1)
)

build_sub_table <- function(df_u, df_c, tag){
  bind_rows(lapply(names(subgroups), function(lbl){
    f <- subgroups[[lbl]]$filter
    u <- df_u %>% filter(!!f(.)) %>% pull(delta_ast_6) %>% na.omit()
    c <- df_c %>% filter(!!f(.)) %>% pull(delta_ast_6) %>% na.omit()
    tibble(
      subgroup = lbl,
      group    = tag,
      n        = length(u),
      median   = median(u),
      q1       = quantile(u,.25),
      q3       = quantile(u,.75),
      control_n= length(c),
      control_median = median(c),
      control_q1 = quantile(c,.25),
      control_q3 = quantile(c,.75),
      p_value = ifelse(length(u) & length(c),
                       wilcox.test(u, c)$p.value, NA_real_)
    )
  }))
}

tbl4_pre  <- build_sub_table(urso,    ctrl,    "URSO")
tbl4_post <- build_sub_table(urso_m,  ctrl_m,  "URSO")

write.csv(bind_rows(pre = tbl4_pre, post = tbl4_post, .id = "match_status"),
          "./output/Table4_subgroups.csv", row.names = FALSE)

## 8 ── Table 6 : Dose–response summary --------------------------------

dose_summary <- urso %>%
  group_by(dose_cat) %>%
  summarise(n            = n(),
            median_mgkg  = median(mgkg, na.rm = TRUE),
            median_days  = median(interval_days, na.rm = TRUE),
            pct_change   = median(pct_ast_6, na.rm = TRUE),
            q1_change    = quantile(pct_ast_6, .25, na.rm = TRUE),
            q3_change    = quantile(pct_ast_6, .75, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(duration_months = median_days / 30.44,
         pct_IQR         = sprintf("%.1f %% [%.1f %% – %.1f %%]",
                                   pct_change, q1_change, q3_change)) %>%
  select(dose_cat, n, median_mgkg,
         median_days, duration_months, pct_IQR)

write.csv(dose_summary, "./output/Table6_dose_response.csv", row.names = FALSE)

message("All tables written to './output/'.  Script completed successfully.")
