
globalMinFPplusFN_vector = {}

-- function that checks if all the elements of a vector are +1
function checkAllOnes(vector)
  
  local dime = #vector
  local total = 0
  local flag = true
  
  for i=1,dime do
    if round(vector[i],0)~=1 then flag = false end
    total = total + vector[i]
  end
  
  --print("vector average="..round((total/dime),2));
  
  return flag;
end

-- function that checks if all the elements of a vector are 0
function checkAllZeros(vector)
  
  local dime = #vector
  local total = 0
  local flag = true
  
  for i=1,dime do
    if round(vector[i],0)~=0 then flag = false end
    total = total + vector[i]
  end
  
  --print("vector average="..round((total/dime),2));
  
  return flag;
end

-- function that creates the ROC area under the curve
function metrics_ROC_AUC_computer(completePredValueVector, truthVector)

	-- printVector(completePredValueVector, "completePredValueVector");
	-- printVector(truthVector, "truthVector");
	-- os.exit();
	
	
	if checkAllZeros(truthVector)==true then
	  
	  local successRate=0;	  
	  print("ATTENTION: all the ground-truth values area 0.0\t The metrics ROC area will be the success rate");	  
	  
	  local countZeros = 0
	  for u=1,#completePredValueVector do
	    if(completePredValueVector[u]<0.5) then countZeros = countZeros + 1; end
	  end
	  successRate = round(countZeros*100/#completePredValueVector, 3);	
	  
	  return successRate;	  
	end
	
  
	local timeNewAreaStart0 = os.time();
	
	local tp_rate = {}
	local fp_rate = {}
	local precision_vect = {}
	local recall_vect = {}

	
	ROC = require './metrics/roc_thresholds.lua';
	local newVect = fromZeroOneToMinusOnePlusOne(truthVector)
	local roc_points = torch.Tensor(#completePredValueVector, 2)
	local precision_recall_points = torch.Tensor(#completePredValueVector, 2)
	--print("#completePredValueVector="..comma_value(#completePredValueVector).."\t#newVect="..comma_value(#newVect));
	local roc_thresholds_output = roc_thresholds(torch.DoubleTensor(completePredValueVector), torch.IntTensor(newVect))
	
	local splits = roc_thresholds_output[1]
	local thisThreshold = roc_thresholds_output[2]
	globalMinFPplusFN_vector[#globalMinFPplusFN_vector+1] = thisThreshold
	
	--print("th.\tTNs\tFNs\tTPs\tFPs\t#\tTPrate\tFPrate");
	  for i = 1, #splits do
		  thresholds = splits[i][1]
		  tn = splits[i][2]
		  fn = splits[i][3]
		  tp = splits[i][4]
		  fp = splits[i][5]
		  
		  tp_rate[i] = 0
		  if ((tp+fn)~=0) then tp_rate[i] = tp / (tp+fn) end
		  fp_rate[i] = 0
		  if ((fp+tn)~=0) then fp_rate[i] = fp / (fp+tn) end
		  
		  roc_points[i][1] = tp_rate[i]
		  roc_points[i][2] = fp_rate[i]
		  
		  precision_vect[i] = 0
		  if ((tp+fp)~=0) then precision_vect[i] = tp/(tp+fp) end
		  recall_vect[i]= tp_rate[i] -- = tp / (tp+fn)
		  
		  --io.write(round(precision_vect[i],3).." "..round(recall_vect[i],3).."\n");
		  --io.flush();
		  
		  -- print(thresholds.."\t"..tn.."\t"..fn.."\t"..tp.."\t"..fp.."\t#\t"..tp_rate[i].."\t"..fp_rate[i])
	  end

	local area_roc = round(areaNew(tp_rate,fp_rate)*100,2);
	print("metrics area_roc = "..area_roc.."%");	

 	if area_roc < 0 then io.stderr:write('ERROR: AUC < 0%, problem ongoing'); return; end
 	if area_roc > 100 then io.stderr:write('ERROR: AUC > 100%, problem ongoing'); return; end	
	
	-- print("#splits= "..#splits.." #precision_vect= "..#precision_vect.." #recall_vect= "..#recall_vect);
	
	
	-- printVector(precision_vect, "precision_vect");
	-- printVector(recall_vect, "recall_vect");
	
	require './sort_two_arrays_from_first.lua';
	sortedPrecisionVett, sortedRecallVett = sort_two_arrays_from_first(precision_vect, recall_vect, #precision_vect)
	
	-- printVector(sortedPrecisionVett, "sortedPrecisionVett");
	-- printVector(sortedRecallVett, "sortedRecallVett");
	
	local area_precision_recall = round((areaNew(sortedPrecisionVett, sortedRecallVett)-1)*100, 2) ; -- UNDERSTAND WHY -1 ???
	print("(beta) metrics area_precision_recall = "..area_precision_recall.."%");	

 	if area_precision_recall < 0 then io.stderr:write('ERROR: PrecisionRecallArea < 0%, problem ongoing'); return; end
 	if area_precision_recall > 100 then io.stderr:write('ERROR: PrecisionRecallArea > 100%, problem ongoing;'); return; end
	

-- 	timeNewAreaFinish = os.time();
-- 	durationNewAreaTotal = timeNewAreaFinish - timeNewAreaStart;
-- 	print('\ntotal duration of the new area_roc metrics ROC_AUC_computer function: '.. tonumber(durationNewAreaTotal).. ' seconds');
-- 	io.flush();
-- 	print('total duration of the new area_roc metrics ROC_AUC_computer function: '..string.format("%.2d hours, %.2d minutes, %.2d seconds", durationNewAreaTotal/(60*60), durationNewAreaTotal/60%60, durationNewAreaTotal%60));
-- 	io.flush();
	

	return area_roc;
end


-- Function that reads a vector and replace all the occurrences of 0's to occurrences of -1's
function fromZeroOneToMinusOnePlusOne(vector)
  
  newVector = {}
  
  for i=1,#vector do 
      newVector[i] = vector[i]
      if (vector[i] == 0) then
	newVector[i] = -1
      end
  end
  
  return newVector;
end
