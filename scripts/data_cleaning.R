ucsb_catch <- vroom('C:/Users/pokkd/Downloads/baseball_internship/Catchers/app/BigWest_21_25.csv')

ucsb_catch_red <- ucsb_catch %>% select(Date, CatcherId, Catcher, PitchNo, Pitcher, PitcherId, PitcherTeam, 
                                        Batter, PitchCall, PlateLocHeight, PlateLocSide) %>% 
  filter(PitchCall %in% c("StrikeCalled", "BallCalled")) %>% group_by(CatcherId) # takes away some pitches

ucsb_catch_red$PlateLocHeight <- as.numeric(ucsb_catch_red$PlateLocHeight)
ucsb_catch_red$PlateLocSide <- as.numeric(ucsb_catch_red$PlateLocSide)

#print(ucsb_catch_red %>% count(Catcher), n=100)

ucsb_catch_red <- ucsb_catch_red %>%
  filter(!is.na(PlateLocSide) & !is.na(PlateLocHeight))

#ucsb_catch_red %>% count(Catcher) # takes away some more pitches, still more are lost after, maybe the join?

hexdata_all <- ucsb_catch_red %>%
  mutate(HexBinX = round(PlateLocSide, 1),
         HexBinY = round(PlateLocHeight, 1)) %>%
  group_by(HexBinX, HexBinY) %>%
  summarise(
    TotalPitches = n(),
    Strikes = sum(PitchCall == "StrikeCalled")
  ) %>%
  mutate(StrikePercentage = Strikes / TotalPitches * 100)


ucsb_catch_red <- ucsb_catch_red %>%
  mutate(HexBinX = round(PlateLocSide, 1),
         HexBinY = round(PlateLocHeight, 1)) %>%
  left_join(hexdata_all %>% select(HexBinX, HexBinY, StrikePercentage),
            by = c("HexBinX", "HexBinY"))

print(ucsb_catch_red %>% count(Catcher), n=100)

write.csv(ucsb_catch_red, "ucsb_catch_red.csv", row.names = FALSE)
