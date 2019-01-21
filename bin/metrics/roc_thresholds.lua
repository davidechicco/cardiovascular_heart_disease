require 'torch'

-- rounds a real number num to the number having idp values after the dot
function round(num, idp)
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end



function getIndex(tensorChosen, element, size)

    vectorBis = {}
      for i = 1,size do
	vectorBis[i] = tensorChosen[i];
      end 

      for k, v in pairs(vectorBis) 
	do if(element == v) then return k; end
      end  
end

function roc_thresholds(responses, labels)

	-- assertions about the data format expected
	assert(responses:size():size() == 1, "responses should be a 1D vector")
	assert(labels:size():size() == 1 , "labels should be a 1D vector")

	-- print("(#responses)[1]="..(#responses)[1].."\t ##labels="..##labels);
	-- io.flush();
	
   	-- assuming labels {-1, 1}
   	local npositives = torch.sum(torch.eq(labels,  1))
   	local nnegatives = torch.sum(torch.eq(labels, -1))
   	local nsamples = npositives + nnegatives

   	assert(nsamples == responses:size()[1], "labels should contain only -1 or 1 values")
   	
   	-- sort by response value
   	local responses_sorted, indexes_sorted = torch.sort(responses)
   	local labels_sorted = labels:index(1, indexes_sorted)

	local true_negatives = 0
	local false_negatives = 0   	
	local true_positives = 0
	local false_positives = 0   	

   	local epsilon = 0.01

	-- a split threshold divides the data as follows:
   	-- 	if response[i] <= split: classify sample[i] as belonging to the negative class
   	-- 	if response[i] > split:  classify sample[i] as belonging to the positive class 

	-- Base case, where the split threshold is a bit lower than the minimum response for any sample.
	-- Here, all samples are classified as belonging to the positive class, therefore we have
	-- zero true negatives and zero false negatives (all are either true positives or false positives)
	local split = responses[1]-epsilon
	local splits = {}
	local splitsExtended = {}
	local FPplusFN = {}

	-- we are going to start moving through the samples and increasing the split threshold
   	local i = 0
   	while i<=nsamples do
		-- if a set of samples have *exactly* this response, we can't distinguish between them.
		-- Therefore, all samples with that response will be classified as negatives (since response == split)
		-- and depending on their true label, we need to increase either the TN or the FN counters
		while i+1 <= nsamples and responses_sorted[i+1] == split do
			if labels_sorted[i+1] == -1 then
				true_negatives = true_negatives + 1
			else
				false_negatives = false_negatives + 1
			end
			
			if i>=1 and i<= nsamples then
			  FPplusFN[i] =  false_positives + false_negatives
-- 			  io.write("\nresponses_sorted["..i.."]="..responses_sorted[i]);				  
-- 			  io.write(" FPplusFN["..i.."]="..(false_positives + false_negatives))	
-- 			  io.write(" false_positives="..false_positives);
-- 			  io.write(" false_negatives="..false_negatives.."\n");
-- 			  io.flush()		
			end
			
			i = i+1
		end
		-- now that we dealt with the "degenerate" situation of having multiple samples with exactly the same response
		-- coinciding with the current threshold, lets store this threshold and the current TN and FN
		splits[#splits+1] = {split, true_negatives, false_negatives}	
		
		
		true_positives = npositives - false_negatives
		false_positives = nnegatives - true_negatives 
		splitsExtended[#splitsExtended+1] = {split, true_negatives, false_negatives, true_positives, false_positives, false_positive_rate, true_positive_rate}	
		
		if i>=1 and i<= nsamples then
			  FPplusFN[i] = false_positives + false_negatives
		end
		
		
		
		-- We can now move on
		i = i + 1
		if i<=nsamples and labels_sorted[i] == 1 then
			false_negatives = false_negatives + 1
			
			if i>=1 and i<= nsamples then
			  	FPplusFN[i] =  false_positives + false_negatives
			end
			
		else
			true_negatives = true_negatives + 1
			-- while we see only negative examples we can keep increasing the threshold, because there is no point in picking 
			-- a threshold if we can pick a higher one that will increase the amount of true negatives (therefore decreasing the 
			-- false positives), without causing any additional false negative. 
			while i+1 <= nsamples and labels_sorted[i+1] == -1 do
				true_negatives = true_negatives + 1
				
				if i>=1 and i<= nsamples then   
				  FPplusFN[i] = false_positives + false_negatives
				end				
				
				i = i+1	
			end
		end
		
		if i>=1 and i<= nsamples then
			FPplusFN[i] = false_positives + false_negatives	
		end
		
		-- new "insteresting" split threshold  
		if i<=nsamples then
			split = responses_sorted[i]
		end
   	end

   	-- we are now done, lets return the table with all the tuples of {thresholds, true negatives, false negatives}
   	-- {{split_1, TN_1, FN_1},   ... , {split_k, TN_k, FP_k}}
		
	--for i=1,#FPplusFN do print("FPplusFN["..i.."]="..FPplusFN[i]); end
	
	
	
-- 	-- print("(#responses_sorted)[1]="..(#responses_sorted)[1]);
	print("#FPplusFN="..#FPplusFN)
	table.sort(FPplusFN)
	minError = FPplusFN[1];
	minErrorIndex = getIndex(FPplusFN, minError, #FPplusFN);
 	globalThreshold =  responses_sorted[minErrorIndex]
	print("minError="..minError.."\tminErrorIndex="..minErrorIndex .."\tglobalThreshold ="..globalThreshold);
	
	-- if (#responses_sorted)[1] ~= #FPplusFN then print("((#responses_sorted)[1] ~= #FPplusFN) Error: not all the FPplusFN values have been computed"); os.exit(); end
	
	
	globalMinFPplusFN = globalThreshold

   	return {splitsExtended, globalMinFPplusFN}
end

function areaNew(newFPrateArray, newTPrateArray)

	local area = 0.0 

	for i=1,#newFPrateArray do
	    if i>=2 then
		area = area + (newFPrateArray[i] - newFPrateArray[i-1])*((newTPrateArray[i]+newTPrateArray[i-1])/2.0)
	    end
	end
	area =tonumber(1+area)
	--print("areaNew="..area);
	return area
end

-- #######################################

--[[
resp = torch.DoubleTensor { -0.9, -0.8, -0.8, -0.5, -0.1, 0.0, 0.2, 0.2, 0.51, 0.74, 0.89}
labels = torch.IntTensor  {   -1,   -1,    1,   -1,   -1,   1,   1,  -1,   -1,    1,    1}

tp_rate = {}
fp_rate = {}


local roc_points = torch.Tensor((#resp)[1], 2)
splits = roc_thresholds(resp, labels)
print("th.\tTNs\tFNs\tTPs\tFPs\t#\tTPrate\tFPrate");
for i = 1, #splits do
	thresholds = splits[i][1]
	tn = splits[i][2]
	fn = splits[i][3]
	tp = splits[i][4]
	fp = splits[i][5]
	tp_rate[i] = round(tp / (tp+fn),2)
	fp_rate[i] = round(fp / (fp+tn),2)
	roc_points[i][1] = tp_rate[i]
	roc_points[i][2] = fp_rate[i]
	print(thresholds.."\t"..tn.."\t"..fn.."\t"..tp.."\t"..fp.."\t#\t"..tp_rate[i].."\t"..fp_rate[i])
end

areaNew(tp_rate,fp_rate)

]]
