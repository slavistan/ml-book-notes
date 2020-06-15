---
title: Notes
author: Stan
urlcolor: teal
---

```{.bash .cb.run}
URL="https://raw.githubusercontent.com/ageron/handson-ml2/master/datasets/housing/housing.csv"
mkdir -p _datasets 
curl -so - "$URL" > _datasets/housing.csv
```

```{.python .cb.nb jupyter_kernel=python3}
import pandas as pd

x=25
housing = pd.read_csv("_datasets/housing.csv")
housing.head()
```

```{.python .cb.run}
print("hi")
print(x)
```
