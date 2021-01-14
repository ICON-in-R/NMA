
if (binData & !medData) {
  if (!RANDOM) {
    modelResults <- 
      NMA(
      winSource = here::here("raw_data", "SurvWoodsFEa_bin.txt"), # model
      
      # data
      dataFunc = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,        
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      
      # parameters
      effectParam = "beta",
      modelParams = NA, # 
      
      
      folder = endpoint,
      label = label,               #analysis name
      endpoint = endpoint,        # PFS,OS
      random = RANDOM,   # logical; is random effects model? for fixed effects RANDOM=FALSE, for random effects RANDOM=TRUE
      binData = binData,   # logical; availability of binary endpoint data
      medData = medData,   # logical; availability of median endpoint data
      refTx = REFTX,          # reference treatment name
      decEff = decEff,   #logical
      lg = FALSE)
    
    
my_nma <- NMA6$new()

my_nma$data(subData,
            subDataBin,
            subDataMed)    

my_nma$model(endpoint = "PFS",
             medData = TRUE,
             binData = TRUE)

my_nma$params(effectParam = "beta",
              modelParams = "sd")    

my_nma$analysis(decEff,
                endpoint,
                label,
                refTx,
                lg,
                folder)

res <- run_model(my_nma)

