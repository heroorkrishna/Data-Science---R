import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import rcparams
rcparams['figure,figsize']=10,6

#importing the data
dataset=pd.read_csv(filename())

dataset['month']=pd.to_datetime(datase['month'], inferdatetime format=True)
indexeddatetime=dataset.set_indexed(['month'])

from dataetime import datetime
indexeddatetime.head(5) #best of 5 datasets 