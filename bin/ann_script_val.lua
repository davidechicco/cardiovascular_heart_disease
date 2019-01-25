

print('\n\n @ @ @ @ @ @ START @ @ @ @ @ @ @ ');
print('file: mesothelioma_ann_script_start.lua');
print('author Davide Chicco <davide.chicco@gmail.com>');
print(os.date("%c", os.time()));

MAX_MSE = 4

local timeStart = os.time()

-- createPerceptron
function createPerceptron(this_input_number, this_hidden_units, this_hidden_layers, this_output_number)


  perceptron = nn.Sequential()  

  perceptron:add(nn.Linear(this_input_number, this_hidden_units))
  perceptron:add(nn.Sigmoid())
  if DROPOUT_FLAG==true then perceptron:add(nn.Dropout()) end

  for w=1,this_hidden_layers do
    perceptron:add(nn.Linear(this_hidden_units, this_hidden_units))
    perceptron:add(nn.Sigmoid())
    if DROPOUT_FLAG==true then perceptron:add(nn.Dropout()) end
  end
  perceptron:add(nn.Linear(this_hidden_units, this_output_number))


  -- perceptronUpper:add(nn.Linear(input_number, hiddenUnits))
  -- perceptronUpper:add(nn.ReLU())
  -- perceptronUpper:add(nn.Dropout()) 
  --
  -- for w=1, hiddenLayers do
  --   perceptronUpper:add(nn.Linear(hiddenUnits,hiddenUnits))
  --   perceptronUpper:add(nn.ReLU())
  --   perceptronUpper:add(nn.Dropout())
  -- end
  -- perceptronUpper:add(nn.Linear(hiddenUnits,output_layer_number))


  if XAVIER_INITIALIZATION==true then 
    print("XAVIER_INITIALIZATION = "..tostring(XAVIER_INITIALIZATION))
    perceptron = require("../../Neuroblastoma/src/lib/weight-init.lua")(perceptron,  'xavier') -- XAVIER
  end

  return perceptron;
end


-- function executeTest
function executeTest(testPerceptron, dataset_patient_profile)

    local correctPredictions = 0
    local atleastOneTrue = false
    local atleastOneFalse = false
    local predictionTestVect = {}
    local truthVect = {}

    for i=1,#dataset_patient_profile do
      local current_label = dataset_patient_profile[i][2][1]
      local prediction = testPerceptron:forward(dataset_patient_profile[i][1])[1]

      -- prediction = (prediction+1)/2      
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


-- Function sleep
function sleep(n) os.execute("sleep " .. tonumber(n)); end

  -- Function table.contains
function table.contains(table, element)  
  local count = 1
  for _, value in pairs(table) do
   -- print("value: "..tostring(value).." element: "..tostring(element));
    if tostring(value) == tostring(element) or value==element then
      return {true,count}
    end
    count = count + 1
  end
  return {false,-1}
end

-- Function that prints 
function printTime(timeStart, stringToPrint)
	timeEnd = os.time();
	duration = timeEnd - timeStart;
	print('\nduration '..stringToPrint.. ': '.. comma_value(tonumber(duration)).. ' seconds');
	io.flush();
	print('duration '..stringToPrint.. ': '..string.format("%.2d days, %.2d hours, %.2d minutes, %.2d seconds", (duration/(60*60))/24, duration/(60*60)%24, duration/60%60, duration%60)) 
	io.flush();
	
    return duration;
end



-- Function that reads a value and returns the string of the signed value
function signedValueFunction(value)
  
      local value = tonumber(value);
      --print("value = "..value);
      local charPlus = ""
      if tonumber(value) >= 0 then charPlus = "+"; end
      local outputString = charPlus..""..tostring(round(value,2));
      --print("outputString = "..outputString);
      
      return tostring(outputString);
end
       

-- from sam_lie
-- Compatible with Lua 5.0 and 5.1.
-- Disclaimer : use at own risk especially for hedge fund reports :-)

---============================================================
-- add comma to separate thousands
-- 
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

    
    if continueLabel then
      upperMCC = (tp*tn) - (fp*fn)
      innerSquare = (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
      lowerMCC = math.sqrt(innerSquare)
      
      MatthewsCC = -2
      if lowerMCC>0 then MatthewsCC = upperMCC/lowerMCC end
      local signedMCC = signedValueFunction(MatthewsCC);
      print("signedMCC = "..signedMCC);
      
      if MatthewsCC > -2 then print("\n::::\tMatthews correlation coefficient = "..signedMCC.."\t::::\n");
      else print("Matthews correlation coefficient = NOT computable");	end
      
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
	
      
      local totalRate = 0
      if MatthewsCC > -2 and f1_score > -2 then 
	totalRate = MatthewsCC + accuracy + f1_score 
	-- print("total rate = "..round(totalRate,2).." in [-1, +3] that is "..round((totalRate+1)*100/4,2).."% of possible correctness");
      end
      
--       local numberOfPredictedOnes = tp + fp;
--       print("numberOfPredictedOnes = (TP + FP) = "..comma_value(numberOfPredictedOnes).." = "..round(numberOfPredictedOnes*100/(tp + tn + fn + fp),2).."%");
--       
--       io.write("\nDiagnosis: ");
--       if (fn >= tp and (fn+tp)>0) then print("too many FN false negatives"); end
--       if (fp >= tn and (fp+tn)>0) then print("too many FP false positives"); end
--       
--       
--       if (tn > (10*fp) and tp > (10*fn)) then print("Excellent ! ! !");
--       elseif (tn > (5*fp) and tp > (5*fn)) then print("Very good ! !"); 
--       elseif (tn > (2*fp) and tp > (2*fn)) then print("Good !"); 
--       elseif (tn >= fp and tp >= fn) then print("Alright"); 
--       else print("Baaaad"); end
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



-- ##############################3

local profile_vett = {}


local csv = require("csv")
local fileName = tostring("../data/dataset_edited_without_time_NORM.csv")

-- local fileName = tostring(arg[1])
--"../data/MesotheliomaDataSet_DicleUniversity_NORMALIZED.csv" 
-- MesotheliomaDataSet_original_COL_NORM.csv


print("Readin' "..tostring(fileName));

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
NORMALIZATION = false
MAX_VALUE = 1
local output_number = 1
THRESHOLD = 0.5 -- ORIGINAL
-- THRESHOLD = 0.1529
XAVIER_INITIALIZATION = false
DROPOUT_FLAG = false 
MOMENTUM_ALPHA = 0.5

MOMENTUM = false
LEARN_RATE = 0.01 
ITERATIONS = 500

-- LEARN_RATE = 0.01
-- local hidden_units = 50
-- MOMENTUM = false
-- ITERATIONS = 200

-- local hidden_layers = 1 -- best is 1
local hiddenUnitVect = {5, 10, 25,50,75,100,125,150,175,200,225,250,275,300}
-- local hiddenUnitVect = {5, 10, 25, 35, 50, 75, 100}
-- local hiddenLayerVect = {1}
 local hiddenLayerVect = {1} --,2,3,4,5}

local max_values = {}


print("#max_values = "..#max_values);
print("NORMALIZATION = "..tostring(NORMALIZATION));

-- filePointer = io.open("normalized_data_file.csv", "w")

local profile_vett_data = {}
local label_vett = {}

for i=1,#profile_vett do
  profile_vett_data[i] = {}
--   io.write("#"..i.."# ")
--   io.flush()
  for j=1,#(profile_vett[1]) do
    
    if NORMALIZATION==true then MAX_VALUE = max_values[j] end;
  
    if j<#(profile_vett[1]) then
      profile_vett_data[i][j] = (profile_vett[i][j]/MAX_VALUE)
      -- io.write("profile_vett_data["..i.."]["..j.."] = "..profile_vett_data[i][j].." ")
      -- filePointer:write(round(profile_vett_data[i][j],2)..",")
      -- io.flush()
    else
      label_vett[i] = profile_vett[i][j]
      -- filePointer:write(round(label_vett[i],2)..",")
      -- io.flush()
    end    
  end
   -- filePointer:write("\n")
--   io.flush()
end

print("Number of value profiles (rows) = "..#profile_vett_data);
print("Number features (columns) = "..#(profile_vett_data[1]));
print("Number of data instances (rows) = "..#label_vett);

if NORMALIZATION==true and #max_values ~= #(profile_vett_data[1]) then
  print("Error: different number of max_values and features. The program will stop");
  os.exit();
end

local patient_outcome = label_vett
local patients_vett = profile_vett_data

-- filePointer:close()

-- os.exit()

-- ########################################################



-- START

local timeStart = os.time();

local indexVect = {}; 
for i=1, #patients_vett do indexVect[i] = i;  end
permutedIndexVect = permute(indexVect, #indexVect, #indexVect);

-- -- VALIDATION_SET_PERC = 20
-- -- TEST_SET_SIZE = 65
-- -- 
-- -- local validation_set_size = 65
-- -- round((VALIDATION_SET_PERC*(#patients_vett-TEST_SET_SIZE))/100)
-- -- 
-- -- print("training_set_size = "..((#patients_vett-TEST_SET_SIZE)-validation_set_size).." elements");
-- -- print("validation_set_size = "..validation_set_size.." elements\n");
-- -- print("TEST_SET_SIZE = "..TEST_SET_SIZE.." elements\n");



TRAINING_SET_PERC = 60
VALIDATION_SET_PERC = 20
TEST_SET_PERC = 20

local training_set_size = round((TRAINING_SET_PERC*(#patients_vett)/100))
local validation_set_size = round((VALIDATION_SET_PERC*(#patients_vett)/100))
TEST_SET_SIZE = (#patients_vett) - training_set_size - validation_set_size

print("training_set_size = "..training_set_size.." elements");
print("validation_set_size = "..validation_set_size.." elements\n");
print("TEST_SET_SIZE = "..TEST_SET_SIZE.." elements\n");





print("#patients_vett = "..#patients_vett);

-- os.exit()

train_patient_profile = {}
validation_patient_profile = {}
test_patient_profile = {}
modelFileVect = {}

local original_validation_indexes = {}

for i=1,#patients_vett do
    
  if i<=(tonumber(#patients_vett-TEST_SET_SIZE)-validation_set_size) then
    train_patient_profile[#train_patient_profile+1] = {torch.Tensor(patients_vett[permutedIndexVect[i]]), torch.Tensor{patient_outcome[permutedIndexVect[i]]}}
    
    --print("training outcome["..#train_patient_profile.."] = "..train_patient_profile[#train_patient_profile][2][1]);
  
  elseif i> (#patients_vett-TEST_SET_SIZE-validation_set_size) and i <= (#patients_vett-TEST_SET_SIZE) then
      
    original_validation_indexes[#original_validation_indexes+1] = permutedIndexVect[i];
    -- print("original_validation_indexes =".. permutedIndexVect[i]);
    
    validation_patient_profile[#validation_patient_profile+1] = {torch.Tensor(patients_vett[permutedIndexVect[i]]), torch.Tensor{patient_outcome[permutedIndexVect[i]]}}
    --print("validation outcome["..#validation_patient_profile.."] = "..validation_patient_profile[#validation_patient_profile][2][1]);
   
  else
    
    test_patient_profile[#test_patient_profile+1] = {torch.Tensor(patients_vett[permutedIndexVect[i]]), torch.Tensor{patient_outcome[permutedIndexVect[i]]}}
    
  end
end

require 'nn'
input_number = (#(train_patient_profile[1][1]))[1]




function train_patient_profile:size() return #train_patient_profile end 
function validation_patient_profile:size() return #validation_patient_profile end 
  
-- local fileName = "../results/positive_error_progress"..tostring(os.time())..".csv"
-- local filePointer = io.open(fileName, "w")  
-- local printError = false
  
-- OPTIMIZATION LOOPS  
local MCC_vect = {}  
local hus_vect = {}
local hl_vect = {}

for b=1,#hiddenLayerVect do
   for a=1,#hiddenUnitVect do

    
    local hidden_units = hiddenUnitVect[a]
    local hidden_layers = hiddenLayerVect[b]
    print("\n\n\n$$$ hidden_units = "..hidden_units.."\t hidden_layers = "..hidden_layers.." $$$")
    
    local perceptron = createPerceptron(input_number, hidden_units, hidden_layers, output_number)


    local criterion = nn.MSECriterion()  
    local lossSum = 0
    local positiveLossSum = 0
    local error_progress = 0
    local numberOfOnes = 0
    local positiveErrorProgress = 0


    if OPTIM_PACKAGE == false then

      myTrainer = nn.StochasticGradient(perceptron, criterion)
      myTrainer.learningRate = LEARN_RATE
      myTrainer.maxIteration = ITERATIONS
      myTrainer:train(train_patient_profile)
      
    else
      
      require 'optim'
      local params, gradParams = perceptron:getParameters()     
      local optimState = nil
	
      if MOMENTUM==true then 
	optimState = {learningRate = LEARN_RATE}
      else 
	optimState = {learningRate = LEARN_RATE,
			  momentum = MOMENTUM_ALPHA }
      end
      
      local total_runs = ITERATIONS*#train_patient_profile
      local loopIterations = 1
      for epoch=1,ITERATIONS do
	for k=1,#train_patient_profile do
	  
	    -- Function feval 
	    local function feval(params)
		gradParams:zero()
		
		local thisProfile = train_patient_profile[k][1]
		local thisLabel = train_patient_profile[k][2]

		local thisPrediction = perceptron:forward(thisProfile)		
		
		
		-- thisPrediction = (thisPrediction+1)/2 -- [-1,+1] -> [0,1]
		
		local loss = criterion:forward(thisPrediction, thisLabel)
		
		-- if hidden_units == 100 then
		--   print("thisPrediction = "..round(thisPrediction[1],2).." thisLabel = "..thisLabel[1].."\tloss = "..loss)
		-- end
		
		lossSum = lossSum + loss
		error_progress = lossSum*100 / (loopIterations*MAX_MSE)
		
		--print("thisLabel[1] = "..thisLabel[1].." positiveLossSum = "..positiveLossSum.." numberOfOnes = "..numberOfOnes);
		
		if thisLabel[1]==1 then
		  positiveLossSum = positiveLossSum + loss
		  numberOfOnes = numberOfOnes + 1		  
		end
		if (numberOfOnes > 0 ) then 
		  positiveErrorProgress = positiveLossSum*100 / (numberOfOnes*MAX_MSE) 
		end
		
		if ((loopIterations*100/total_runs)*10)%100==0 then
		  io.write("completion: ", round((loopIterations*100/total_runs),2).."%" )
		  io.write(" (epoch="..epoch..")(element="..k..") loss = "..round(loss,3).." ")      
		  io.write("\terror progress = "..round(error_progress,5).."%\n")
		end
-- 		if printError== true then
-- 		  filePointer:write(loopIterations..","..positiveErrorProgress.."\n")
-- 		end

		local dloss_doutput = criterion:backward(thisPrediction, thisLabel)
		
		perceptron:backward(thisProfile, dloss_doutput)

		return loss,gradParams
	    end
	  optim.sgd(feval, params, optimState)
	  loopIterations = loopIterations+1
	end     
      end

    end
    
   print("\n\n### executeTest(perceptron, validation_patient_profile)")     
   MCC_vect[#MCC_vect+1] = executeTest(perceptron, validation_patient_profile)[1]
   hus_vect[#hus_vect+1] = hidden_units
   hl_vect[#hl_vect+1] = hidden_layers
    
     local modelFile = "./models/model_hus"..hidden_units.."_hl"..hidden_layers.."_time"..tostring(os.time());
     torch.save(tostring(modelFile), perceptron);     
     print("Saved model file: "..tostring(modelFile));
     modelFileVect[#modelFileVect+1] = modelFile;

  end
end

local maxMCC = -1
local maxMCCpos = -1
for k=1,#MCC_vect do
      io.write("@ @ @ @ @ @ @ MCC_vect["..k.."] ="..round(MCC_vect[k],3))
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
print("\n\nmodelFileToLoad ="..modelFileToLoad)

local loadedModel = torch.load(modelFileToLoad)

print("\n\n### executeTest(loadedModel, test_patient_profile)")
local executeTestOutput = executeTest(loadedModel, test_patient_profile)

local lastMCC = executeTestOutput[1]
local lastAccuracy = executeTestOutput[2]

print("':':':':' lastMCC = "..round(lastMCC,3).."  lastAccuracy = "..round(lastAccuracy,3).." ':':':':'")

for i=1,#modelFileVect do
  local command = "rm "..tostring(modelFileVect[i])
  io.write("command: "..command.." \n")
  local res = sys.execute(command)
  -- print("command response: "..res)
end


-- filePointer:close()

printTime(timeStart, " complete execution")
