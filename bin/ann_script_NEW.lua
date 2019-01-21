
-- Function sleep
function sleep(n) os.execute("sleep " .. tonumber(n)); end

-- Adapted from mesothelioma_ann_script_KhalidAbdulla.lua

-- function applyModel
function applyModel(testPerceptron, dataset_patient_profile)

    local correctPredictions = 0
    local atleastOneTrue = false
    local atleastOneFalse = false
    local predictionTestVect = {}
    local truthVect = {}

    for i=1,#dataset_patient_profile do
      local current_label = dataset_patient_profile[i][2][1]
      local prediction = testPerceptron:forward(dataset_patient_profile[i][1])[1]

      prediction = (prediction+1)/2      
      predictionTestVect[i] = prediction
      truthVect[i] = current_label      

      local labelResult = false      
      if current_label >= THRESHOLD and prediction >= THRESHOLD  then
	labelResult = true
      elseif current_label < THRESHOLD and prediction < THRESHOLD  then
	labelResult = true
      end
            
      if labelResult==true then correctPredictions = correctPredictions + 1; end      
      if prediction>=THRESHOLD then
	atleastOneTrue = true
      else
	atleastOneFalse = true
      end
    end

    print("\nCorrect predictions = "..round(correctPredictions*100/#dataset_patient_profile,2).."%")

    if atleastOneTrue==false then print("ATTENTION: all the predictions are FALSE") end
    if atleastOneFalse==false then print("ATTENTION: all the predictions are TRUE") end

   require './metrics_ROC_AUC_computer.lua'
   metrics_ROC_AUC_computer(predictionTestVect, truthVect)

    local printValues = false
    local output_confusion_matrix = confusion_matrix(predictionTestVect, truthVect, THRESHOLD, printValues)

    return {output_confusion_matrix[4], output_confusion_matrix[1]}; -- accuracy
end



-- add comma to separate thousands
function comma_value(amount)
  local formatted = amount
  while true do  
    formatted, k = string.gsub(formatted, "^(-?%d+)(%d%d%d)", '%1,%2')
    if (k==0) then
      break
    end
  end
  return formatted
end

-- function that computes the confusion matrix
function confusion_matrix(predictionTestVect, truthVect, threshold, printValues)

  local tp = 0
  local tn = 0
  local fp = 0
  local fn = 0
  local MatthewsCC = -2
  local accuracy = -2
  local arrayFPindices = {}
  local arrayFPvalues = {}
  local arrayTPvalues = {}
  local areaRoc = 0

  local fpRateVett = {}
  local tpRateVett = {}
  local precisionVett = {}
  local recallVett = {}

  for i=1,#predictionTestVect do

    if printValues == true then
      io.write("predictionTestVect["..i.."] = ".. round(predictionTestVect[i],4).."\ttruthVect["..i.."] = "..truthVect[i].." ");
      io.flush();
    end

    if predictionTestVect[i] >= threshold and truthVect[i] >= threshold then
      tp = tp + 1
      arrayTPvalues[#arrayTPvalues+1] = predictionTestVect[i]
      if printValues == true then print(" TP ") end
    elseif  predictionTestVect[i] < threshold and truthVect[i] >= threshold then
      fn = fn + 1
      if printValues == true then print(" FN ") end
    elseif  predictionTestVect[i] >= threshold and truthVect[i] < threshold then
      fp = fp + 1
      if printValues == true then print(" FP ") end
      arrayFPindices[#arrayFPindices+1] = i;
      arrayFPvalues[#arrayFPvalues+1] = predictionTestVect[i]  
    elseif  predictionTestVect[i] < threshold and truthVect[i] < threshold then
      tn = tn + 1
      if printValues == true then print(" TN ") end
    end
  end

    print("TOTAL:")
    print(" FN = "..comma_value(fn).." / "..comma_value(tonumber(fn+tp)).."\t (truth == 1) & (prediction < threshold)");
    print(" TP = "..comma_value(tp).." / "..comma_value(tonumber(fn+tp)).."\t (truth == 1) & (prediction >= threshold)\n");

    print(" FP = "..comma_value(fp).." / "..comma_value(tonumber(fp+tn)).."\t (truth == 0) & (prediction >= threshold)");
    print(" TN = "..comma_value(tn).." / "..comma_value(tonumber(fp+tn)).."\t (truth == 0) & (prediction < threshold)\n");

  local continueLabel = true
  local signedMCC = -2

    if continueLabel then
      upperMCC = (tp*tn) - (fp*fn)
      innerSquare = (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
      lowerMCC = math.sqrt(innerSquare)

      MatthewsCC = -2
      if lowerMCC>0 then MatthewsCC = upperMCC/lowerMCC end
      signedMCC = MatthewsCC
      print("signedMCC = "..signedMCC)

      if MatthewsCC > -2 then print("\n::::\tMatthews correlation coefficient = "..signedMCC.."\t::::\n");
      else print("Matthews correlation coefficient = NOT computable");  end

      accuracy = (tp + tn)/(tp + tn +fn + fp)
      print("accuracy = "..round(accuracy,2).. " = (tp + tn) / (tp + tn +fn + fp) \t  \t [worst = -1, best =  +1]");

      local f1_score = -2
      if (tp+fp+fn)>0 then   
    f1_score = (2*tp) / (2*tp+fp+fn)
    print("f1_score = "..round(f1_score,2).." = (2*tp) / (2*tp+fp+fn) \t [worst = 0, best = 1]");
      else
    print("f1_score CANNOT be computed because (tp+fp+fn)==0")    
      end
      
     local tp_rate = -2
      if (tp+fn)>0 then   
	tp_rate = (tp) / (tp+fn)
	print("TP rate = "..round(tp_rate,2).." = TP rate = tp / (tp+fn) \t [worst = 0, best = 1]");
      else
	print("TP rate CANNOT be computed because (tp+fn)==0")    
      end
      
     local tn_rate = -2
      if (tn+fp)>0 then   
	tn_rate = (tn) / (tn+fp)
	print("TN rate = "..round(tn_rate,2).." = TN rate = tn / (tn+fp) \t [worst = 0, best = 1]");
      else
	print("TN rate CANNOT be computed because (tn+fp)==0")    
      end

   print("\nMCC\taccuracy\tF1_score\tTP_rate\tTN_rate")
    io.write("+"..round(signedMCC,2).."\t")
    io.write(round(accuracy,2).."\t\t"..round(f1_score,2))
    io.write("\t\t"..round(tp_rate,2).."\t"..round(tn_rate,2).."\n")
    io.flush()
           
    end
    


    return {accuracy, arrayFPindices, arrayFPvalues, MatthewsCC};
end


-- Permutations
-- tab = {1,2,3,4,5,6,7,8,9,10}
-- permute(tab, 10, 10)
function permute(tab, n, count)
      n = n or #tab
      for i = 1, count or n do
        local j = math.random(i, n)
        tab[i], tab[j] = tab[j], tab[i]
      end
      return tab
end

-- round a real value
function round(num, idp)
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end



-- ##############################

local profile_vett = {}
local csv = require("csv")
local fileName = "../data/dataset_edited_without_time_NORM.csv"

local mkdirCommand = "mkdir -p models"
io.write("mkdirCommand: "..mkdirCommand.." \n")
io.flush()
local mkdirCommandRes = sys.execute(mkdirCommand)


print("Readin' "..tostring(fileName))
local f = csv.open(fileName)
local column_names = {}

local j = 0
for fields in f:lines() do

  if j>0 then
    profile_vett[j] = {}
      for i, v in ipairs(fields) do 
    profile_vett[j][i] = tonumber(v);
      end
    j = j + 1
  else
    for i, v in ipairs(fields) do 
    column_names[i] = v
     end
    j = j + 1
  end
end

OPTIM_PACKAGE = true
local output_number = 1
THRESHOLD = 0.5 -- ORIGINAL
DROPOUT_FLAG = false
MOMENTUM_ALPHA = 0.5
MAX_MSE = 4

-- CHANGE: increased learn_rate to 0.01, reduced hidden units to 50, turned momentum on, increased iterations to 200
LEARN_RATE = 0.01
-- local hidden_units = 50
MOMENTUM = true
ITERATIONS = 200
-------------------------------------

local hiddenUnitVect = {}
-- {5, 10, 15, 20,  25,  30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120,  125,  130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225,  230, 235, 240, 245, 250,  255, 260, 265, 270, 275, 280, 285, 290, 295, 300}

local maxHUs = 500
local step =10
for m=5,maxHUs,step do
 hiddenUnitVect[#hiddenUnitVect+1] = m
end

local hiddenLayerVect = {1,2,3}

local profile_vett_data = {}
local label_vett = {}


-- OPTIMIZATION LOOPS  
local MCC_vect = {}  
local hus_vect = {}
local hl_vect = {}

for i=1,#profile_vett do
  profile_vett_data[i] = {}

  for j=1,#(profile_vett[1]) do  
    if j<#(profile_vett[1]) then
      profile_vett_data[i][j] = profile_vett[i][j]
    else
      label_vett[i] = profile_vett[i][j]
    end    
  end
end

print("Number of value profiles (rows) = "..#profile_vett_data);
print("Number features (columns) = "..#(profile_vett_data[1]));
print("Number of targets (rows) = "..#label_vett);

table_row_outcome = label_vett
table_rows_vett = profile_vett

-- ########################################################

-- START

-- Seed random number generator
-- torch.manualSeed(0)

local indexVect = {}; 
for i=1, #table_rows_vett do indexVect[i] = i;  end
permutedIndexVect = permute(indexVect, #indexVect, #indexVect);

-- CHANGE: increase test_set to 50%
-- TEST_SET_PERC = 50
-- -------------------------
-- 
-- local test_set_size = round((TEST_SET_PERC*#table_rows_vett)/100)
-- 
-- print("training_set_size = "..(#table_rows_vett-test_set_size).." elements");
-- print("test_set_size = "..test_set_size.." elements\n");
-- 
-- local train_table_row_profile = {}
-- local test_table_row_profile = {}
-- local original_test_indexes = {}
-- 
-- for i=1,#table_rows_vett do
--   if i<=(tonumber(#table_rows_vett)-test_set_size) then
--     train_table_row_profile[#train_table_row_profile+1] = {torch.Tensor(table_rows_vett[permutedIndexVect[i]]), torch.Tensor{table_row_outcome[permutedIndexVect[i]]}}
--   else
-- 
--     original_test_indexes[#original_test_indexes+1] = permutedIndexVect[i];
--     test_table_row_profile[#test_table_row_profile+1] = {torch.Tensor(table_rows_vett[permutedIndexVect[i]]), torch.Tensor{table_row_outcome[permutedIndexVect[i]]}}
--   end
-- end

---------------------------
TRAINING_SET_PERC = 60
VALIDATION_SET_PERC = 20
TEST_SET_PERC = 100 - TRAINING_SET_PERC - VALIDATION_SET_PERC

print("\nTRAINING_SET_PERC = "..TRAINING_SET_PERC)
print("VALIDATION_SET_PERC = "..VALIDATION_SET_PERC)
print("TEST_SET_PERC = "..TEST_SET_PERC.."\n")

local training_set_size = round((TRAINING_SET_PERC*(#table_rows_vett)/100))
local validation_set_size = round((VALIDATION_SET_PERC*(#table_rows_vett)/100))
TEST_SET_SIZE = (#table_rows_vett) - validation_set_size - training_set_size

print("training_set_size = "..training_set_size.." elements");
print("validation_set_size = "..validation_set_size.." elements");
print("TEST_SET_SIZE = "..TEST_SET_SIZE.." elements\n");


train_table_row_profile = {}
validation_table_row_profile = {}
test_table_row_profile = {}
modelFileVect = {}

local original_validation_indexes = {}

local train_poss = 0
local train_negs = 0
local val_poss = 0
local val_negs = 0
local test_poss = 0
local test_negs = 0

for i=1,#table_rows_vett do
    
  if i<=(tonumber(#table_rows_vett-TEST_SET_SIZE)-validation_set_size) then
    train_table_row_profile[#train_table_row_profile+1] = {torch.Tensor(table_rows_vett[permutedIndexVect[i]]), torch.Tensor{table_row_outcome[permutedIndexVect[i]]}}
    
    if ( train_table_row_profile[#train_table_row_profile][2][1]==1) then
        train_poss = train_poss + 1
    else   
        train_negs = train_negs + 1
    end
  
  elseif i> (#table_rows_vett-TEST_SET_SIZE-validation_set_size) and i <= (#table_rows_vett-TEST_SET_SIZE) then
      
    original_validation_indexes[#original_validation_indexes+1] = permutedIndexVect[i];
    -- print("original_validation_indexes =".. permutedIndexVect[i]);
    
    validation_table_row_profile[#validation_table_row_profile+1] = {torch.Tensor(table_rows_vett[permutedIndexVect[i]]), torch.Tensor{table_row_outcome[permutedIndexVect[i]]}}
    
     if (validation_table_row_profile[#validation_table_row_profile][2][1]==1) then
        val_poss = val_poss + 1
    else   
        val_negs = val_negs + 1
    end
   
  else
    
    test_table_row_profile[#test_table_row_profile+1] = {torch.Tensor(table_rows_vett[permutedIndexVect[i]]), torch.Tensor{table_row_outcome[permutedIndexVect[i]]}}
    
    if (test_table_row_profile[#test_table_row_profile][2][1]==1) then
        test_poss = test_poss + 1
    else   
        test_negs = test_negs + 1
    end
    
  end
end

print("\ntrain_poss =".. train_poss)
print("train_negs =".. train_negs.."\n")
print("val_poss =".. val_poss)
print("val_negs =".. val_negs.."\n")
print("test_poss =".. test_poss)
print("test_negs =".. test_negs.."\n")


---------------------------

require 'nn'

-- OPTIMIZATION LOOPS  
local MCC_vect = {}  

for a=1,#hiddenUnitVect do
  for b=1,#hiddenLayerVect do
      
    local hidden_units = hiddenUnitVect[a]
    local hidden_layers = hiddenLayerVect[b]
    print("\n\n\n> > > > > > > > > New hyper-parameter configuration > > > > > > > > > ")
    print("hidden_units = "..hidden_units.."\t  hidden_layers = "..hidden_layers)
      
    local perceptron = nn.Sequential()
    input_number = #table_rows_vett[1]

    perceptron:add(nn.Linear(input_number, hidden_units))
    perceptron:add(nn.Sigmoid())
    if DROPOUT_FLAG==true then perceptron:add(nn.Dropout()) end

    for w=1,hidden_layers do
    perceptron:add(nn.Linear(hidden_units, hidden_units))
    perceptron:add(nn.Sigmoid())
    if DROPOUT_FLAG==true then perceptron:add(nn.Dropout()) end
    end
    perceptron:add(nn.Linear(hidden_units, output_number))


    function train_table_row_profile:size() return #train_table_row_profile end 
    function test_table_row_profile:size() return #test_table_row_profile end


    local criterion = nn.MSECriterion()  
    local lossSum = 0
    local error_progress = 0

      require 'optim'
      local params, gradParams = perceptron:getParameters()     
      local optimState = nil

      if MOMENTUM==true then 
    optimState = {learningRate = LEARN_RATE}
      else 
    optimState = {learningRate = LEARN_RATE,
              momentum = MOMENTUM_ALPHA }
      end

      local total_runs = ITERATIONS*#train_table_row_profile

      local loopIterations = 1
      for epoch=1,ITERATIONS do
            for k=1,#train_table_row_profile do

            -- Function feval 
            local function feval(params)
            gradParams:zero()

            local thisProfile = train_table_row_profile[k][1]
            local thisLabel = train_table_row_profile[k][2]
                
            local thisPrediction = perceptron:forward(thisProfile)
            local loss = criterion:forward(thisPrediction, thisLabel)
            
            -- print("thisPrediction = "..round(thisPrediction[1],2).." thisLabel = "..thisLabel[1])
            -- sleep(2)

            lossSum = lossSum + loss
            error_progress = lossSum*100 / (loopIterations*MAX_MSE)

    --         if ((loopIterations*100/total_runs)*10)%10==0 then
    --           io.write("completion: ", round((loopIterations*100/total_runs),2).."%" )
    --           io.write(" (epoch="..epoch..")(element="..k..") loss = "..round(loss,2).." ")      
    --           io.write("\terror progress = "..round(error_progress,5).."%\n")
    --         end

            local dloss_doutput = criterion:backward(thisPrediction, thisLabel)

            perceptron:backward(thisProfile, dloss_doutput)

            return loss,gradParams
        end
      optim.sgd(feval, params, optimState)
      loopIterations = loopIterations+1
    end     
   end
   
   print("### applyModel(perceptron, validation_table_row_profile)")     
   MCC_vect[#MCC_vect+1] = applyModel(perceptron, validation_table_row_profile)[1]
   hus_vect[#hus_vect+1] = hidden_units
   hl_vect[#hl_vect+1] = hidden_layers
   
    local modelFile = "./models/model_hus"..hidden_units.."_hl"..hidden_layers.."_time"..tostring(os.time());
     torch.save(tostring(modelFile), perceptron);     
     print("Saved model file: "..tostring(modelFile));
     modelFileVect[#modelFileVect+1] = modelFile;
   
  end
end

io.write("#MCC_vect = "..#MCC_vect.."\n")
io.flush()

local maxMCC = -1
local maxMCCpos = -1
for k=1,#MCC_vect do
      io.write("@ @ @ @ @ @ @ MCC_vect["..k.."] ="..round(MCC_vect[k],4))
      io.write(" hidden units = "..hus_vect[k].." ")
      io.write(" hidden layers = "..hl_vect[k].." ")      
      io.write(" @ @ @ @ @ @ @\n")
      io.flush()
      
      if MCC_vect[k]>=maxMCC then 
	  maxMCC = MCC_vect[k]
	  maxMCCpos = k
      end
end




local modelFileToLoad = tostring(modelFileVect[maxMCCpos])
io.write("\n\nmodelFileToLoad ="..modelFileToLoad)
io.write("\n top model:  units layers = ".. hus_vect[maxMCCpos])
io.write(" hidden layers  = ".. hl_vect[maxMCCpos].."\n")
io.flush()

local loadedModel = torch.load(modelFileToLoad)

print("\n\n### applyModel(loadedModel, test_table_row_profile)")
local executeTestOutput = applyModel(loadedModel, test_table_row_profile)

local lastMCC = executeTestOutput[1]
local lastAccuracy = executeTestOutput[2]

print("':':':':' lastMCC = "..round(lastMCC,3).."  lastAccuracy = "..round(lastAccuracy,3).." ':':':':'\n\n\n")

for i=1,#modelFileVect do
  local command = "rm "..tostring(modelFileVect[i])
  io.write("command: "..command.." \n")
  local res = sys.execute(command)
  -- print("command response: "..res)
end
