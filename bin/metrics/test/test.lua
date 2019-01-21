require 'torch'
metrics = require 'metrics'
gfx = require 'gfx.js'

resp = torch.DoubleTensor { -0.9, -0.8, -0.8, -0.5, -0.1, 0.0, 0.2, 0.2, 0.51, 0.74, 0.89}
labels = torch.IntTensor  {   -1,   -1,    1,   -1,   -1,   1,   1,  -1,   -1,    1,    1}

roc_points, thresholds = metrics.roc.points(resp, labels)
area = metrics.roc.area(roc_points)

assert(area >=0.7 and area <= 0.75, "unexpected area under ROC")

print(roc_points)
print(thresholds)
print(area)

gfx.chart(roc_points)


resp = torch.load('resp_1.dat')
labels = torch.load('labels.dat')

roc_points, thresholds = metrics.roc.points(resp, labels)
area = metrics.roc.area(roc_points)

assert(area >=0.49 and area <= 0.51, "unexpected area under ROC")

--print(roc_points)
print(area)

gfx.chart(roc_points)




