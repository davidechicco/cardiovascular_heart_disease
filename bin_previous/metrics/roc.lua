local roc = {}


function roc.splits(responses, labels, neglabel, poslabel)
  	
   	local nsamples = responses:size()[1]
   
   	-- sort by response value
   	local responses_sorted, indexes_sorted = torch.sort(responses)
   	local labels_sorted = labels:index(1, indexes_sorted)

	local true_negatives = 0
	local false_negatives = 0   	

   	local epsilon = 0.01

	-- a threshold divides the data as follows:
   	-- 	if response[i] <= threshold: classify sample[i] as belonging to the negative class
   	-- 	if response[i] > threshold:  classify sample[i] as belonging to the positive class 

	-- Base case, where the threshold is a bit lower than the minimum response for any sample.
	-- Here, all samples are classified as belonging to the positive class, therefore we have
	-- zero true negatives and zero false negatives (all are either true positives or false positives)
	local threshold = responses[1]-epsilon
	local splits = {}

	-- we are going to start moving through the samples and increasing the threshold
   	local i = 0
   	while i<=nsamples do
		-- if a set of samples have *exactly* this response, we can't distinguish between them.
		-- Therefore, all samples with that response will be classified as negatives (since response == threshold)
		-- and depending on their true label, we need to increase either the TN or the FN counters
		while i+1 <= nsamples and responses_sorted[i+1] == threshold do
			if labels_sorted[i+1] == neglabel then
				true_negatives = true_negatives + 1
			else
				false_negatives = false_negatives + 1
			end
			i = i+1
		end
		-- now that we dealt with the "degenerate" situation of having multiple samples with exactly the same response
		-- coinciding with the current threshold, lets store this threshold and the current TN and FN
		splits[#splits+1] = {threshold = threshold, true_negatives = true_negatives, false_negatives = false_negatives}	

		-- We can now move on
		i = i + 1
		if i<=nsamples and labels_sorted[i] == poslabel then
			false_negatives = false_negatives + 1
		else
			true_negatives = true_negatives + 1
			-- while we see only negative examples we can keep increasing the threshold, because there is no point in picking 
			-- a threshold if we can pick a higher one that will increase the amount of true negatives (therefore decreasing the 
			-- false positives), without causing any additional false negative. 
			while i+1 <= nsamples and labels_sorted[i+1] == neglabel do
				true_negatives = true_negatives + 1
				i = i+1	
			end
		end
		
		-- new "interesting" threshold  
		if i<=nsamples then
			threshold = responses_sorted[i]
		end
   	end

   	-- we are now done, lets return the table with all the tuples of {thresholds, true negatives, false negatives}
   	-- {{threshold_1, TN_1, FN_1},   ... , {threshold_k, TN_k, FP_k}}

   	return splits
end



function roc.points(responses, labels, neglabel, poslabel)

        -- default values for arguments
        poslabel = poslabel or 1
        neglabel = neglabel or -1

	-- assertions about the data format expected
	assert(responses:size():size() == 1, "responses should be a 1D vector")
	assert(labels:size():size() == 1 , "labels should be a 1D vector")

	-- avoid degenerate class definitions
	assert(poslabel ~= neglabel, "positive and negative class can't have the same label")

	-- assuming labels { neglabel, poslabel }
	local npositives = torch.sum(torch.eq(labels, poslabel))
	local nnegatives = torch.sum(torch.eq(labels, neglabel))
	local nsamples = npositives + nnegatives

	assert(nsamples == responses:size()[1], "labels should contain only " .. neglabel .. " or " .. poslabel .. " values")

	local splits = roc.splits(responses, labels, neglabel, poslabel)

   	local roc_points = torch.Tensor(#splits, 2)
   	local thresholds = torch.Tensor(#splits, 1)

   	for i=1,#splits do
   		local false_positives = nnegatives - splits[i].true_negatives
		local true_positives = npositives - splits[i].false_negatives 
		local false_positive_rate = 1.0*false_positives/nnegatives
		local true_positive_rate = 1.0*true_positives/npositives
		roc_points[#splits - i + 1][1] = false_positive_rate
		roc_points[#splits - i + 1][2] = true_positive_rate	
		thresholds[#splits - i + 1][1] = splits[i].threshold
   	end

   	return roc_points, thresholds
end

function roc.area(roc_points)

	local area = 0.0 
	local npoints = roc_points:size()[1]

	for i=1,npoints-1 do
		local width = (roc_points[i+1][1] - roc_points[i][1])
		local avg_height = (roc_points[i][2]+roc_points[i+1][2])/2.0
		area = area + width*avg_height
	end

	return area
end

   
return roc
