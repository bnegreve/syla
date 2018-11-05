#!/usr/bin/env python3

import pandas as pd
import torch.optim
import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
#device = torch.device("cpu")


train_x = np.array(pd.read_csv("data/train_x.csv").values)
train_y = np.array(pd.read_csv("data/train_y.csv").values)
#input_tensor = torch.Tensor (train_x.values, device=device)
#output_tensor = torch.Tensor (train_y.values, device=device)




n_epochs = 100
batch_size = 100

inputsize = 128
hiddensize = 500
outputsize = 32
learning_rate = 0.005


criterion = nn.MSELoss()

class Net (nn.Module):
    def __init__(self):
        super(Net,self).__init__()
        self.fc1 = nn.Linear (inputsize,hiddensize)
        self.fc2 = nn.Linear (hiddensize, hiddensize)
        self.fc3 = nn.Linear (hiddensize, hiddensize)
        self.fc4 = nn.Linear(hiddensize,outputsize)
        
    def forward (self,x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = F.relu(self.fc3(x))
        x = self.fc4(x)
        return x

model = Net().to(device)
optimizer = torch.optim.SGD(model.parameters(),lr=learning_rate)
xlen = len(train_x)
nbatches = (xlen // batch_size) + (0 if xlen % batch_size == 0 else 1)
print("Training set size:", xlen, "batchsize", batch_size, "batch per epoch", nbatches)


for e in range(n_epochs):
    for i in range (nbatches) :
        batch_start = i * batch_size
        batch_end = min(batch_start + batch_size, xlen)        

        x = torch.Tensor(train_x[batch_start:batch_end], device=device).cuda()
        y = torch.Tensor(train_y[batch_start:batch_end], device=device).cuda()

        optimizer.zero_grad()

        yhat = model(x)
        loss = criterion(yhat, y)
        lossval = loss.item()
        print("epoch", e, "batch", i, "(size:", batch_end - batch_start, "loss:", lossval)
        
        loss.backward()
        optimizer.step()






print ("############# TESTING ##################")


def test(xtest_file, ytest_file):
    test_x = np.array(pd.read_csv(xtest_file).values)
    test_y = np.array(pd.read_csv(ytest_file).values)
    test_loss = 0.0

    xlen = len(test_x)

    for i in range (xlen) :        
        x = torch.Tensor(test_x[i], device=device).cuda()
        y = torch.Tensor(test_y[i], device=device).cuda()
        yhat = model(x)
        test_loss += criterion(yhat, y).item()

    print("Test loss : ", test_loss / float(xlen))
        

test("data/test_x.csv", "data/test_y.csv")
       
