Torch metrics package
====================

This package provides utility functions to evaluate your machine learning models.

#### Disclaimer:

Use at your own risk. The code is not extensively tested, therefore it might contain bugs. 
If you find any, please let me know and I will try to fix it.

#### Installation:

```sh
git clone https://github.com/hpenedones/metrics.git
cd metrics
luarocks make
```

#### Receiver Operator Curves (ROC)

Used to evalute performance of binary classifiers, and their trade-offs in terms of false-positive and false-negative rates.

```lua
require 'torch'
metrics = require 'metrics'
gfx = require 'gfx.js'

resp = torch.DoubleTensor { -0.9, -0.8, -0.8, -0.5, -0.1, 0.0, 0.2, 0.2, 0.51, 0.74, 0.89}
labels = torch.IntTensor  {   -1,   -1,    1,   -1,   -1,   1,   1,  -1,   -1,    1,    1}

roc_points, thresholds = metrics.roc.points(resp, labels)
area = metrics.roc.area(roc_points)

print(roc_points)
print(thresholds)
print(area)

gfx.chart(roc_points)

```

![](https://raw.githubusercontent.com/hpenedones/metrics/master/img/roc1.png)

#### Confusion matrix (TODO)

Used to evaluate performance of multi-class classifiers.
