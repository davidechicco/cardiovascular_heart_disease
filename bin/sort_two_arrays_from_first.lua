


-- let's order ACrate and APrate
function sort_two_arrays_from_first(tens1aux, tens2aux, size)

tens1 = {}
tens2  = {}

	-- make a copy of ACrate, to sort
	tens1Sorted = {}
	tens2reordered ={}
	for q=1, size do
	  tens1Sorted[q] = tens1aux[q];
	  tens2reordered[q] = -1;
	end
	table.sort(tens1Sorted);

	for i=1, size do
	    for j=1, size do
	      if tens1aux[i]== tens1Sorted[j] then 
		   -- print('The element '.. tens1aux[i] ..' was '..i..'th in the former table and now it is ' .. j ..'th \n');
		  tens2reordered[j] = tens2aux[i];
	      end
	      -- print('j '..j .. ' i '..i);
	    end
	end

for p=1,size do
  tens1[p] = tens1Sorted[p]
  tens2[p] = tens2reordered[p]
  --print(tens1[p] .. ' '.. tens2[p]);
end

return tens1, tens2

end