#!/usr/bin/env python3

import pandas as pd
import torch.optim
import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import argparse


device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
#device = torch.device("cpu")

parser = argparse.ArgumentParser(description='Description')

parser.add_argument('-l', action="store", type=float, default=0.005,
                    dest='learning_rate',help='learning rate')
parser.add_argument('-e', action="store", type=int, default=10,
                    dest='epochs',help='number of epochs')

args = parser.parse_args()

train_x = np.array(pd.read_csv("data/train_x.csv", dtype=np.float32).values)
train_y = np.array(pd.read_csv("data/train_y.csv").values, dtype=np.float32)



#input_tensor = torch.Tensor (train_x.values, device=device)
#output_tensor = torch.Tensor (train_y.values, device=device)




n_epochs = args.epochs
batch_size = 100

inputsize = 128
hiddensize = 500
outputsize = 32
learning_rate = args.learning_rate


criterion = nn.KLDivLoss()

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
        x = F.log_softmax(x, dim=1)
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

        x = torch.tensor(train_x[batch_start:batch_end], device=device)
        y = torch.tensor(train_y[batch_start:batch_end]) # TODO do not bring this back on the cpu
        y = F.softmax(y, dim=1).to(device)

        optimizer.zero_grad()

        yhat = model(x)
        loss = criterion(yhat, y)
        lossval = loss.item()
        if i % 100 == 0: 
            print("epoch", e, "batch", i, "(size:", batch_end - batch_start, "loss:", lossval)
        
        loss.backward()
        optimizer.step()






print ("############# TESTING ##################")


def test(xtest_file, ytest_file):
    test_x = np.array(pd.read_csv(xtest_file).values, dtype=np.float32)
    test_y = np.array(pd.read_csv(ytest_file).values, dtype=np.float32)
    test_loss = 0.0

    xlen = len(test_x)

    with torch.no_grad():
        for i in range (xlen) :        
            x = torch.tensor([test_x[i]], device=device)
            y = torch.tensor([test_y[i]])
            y = F.softmax(y,dim=1).to(device)
            yhat = model(x)
            test_loss += criterion(yhat, y).item()

    print("Test loss : ", test_loss / float(xlen))
        

test("data/test_x.csv", "data/test_y.csv")
       
