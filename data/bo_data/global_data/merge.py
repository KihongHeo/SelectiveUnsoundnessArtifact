#!/usr/bin/python
import glob
import re

for label in glob.glob("*.label"):
    name = re.split(r'.label',label)[0]
    data = name + ".feat"
    print(name)
    print(data)
    df = open(data, 'r')
    lf = open(label, 'r')
    data = df.readlines()
    label = lf.readlines()
    s = ""
    for data, label in zip(data,label):
        data = re.split(r' |\t|\n',data)
        data = filter(lambda x: x.strip() != ':', data)
        data = filter(lambda x: x.strip() != '', data)
        print data
        label = re.split(r' : ',label)
        label = filter(lambda x: x.strip() != '', label)
        label = label[1]
        for col in data:
            s = s + col + "\t"
        s = s + " " + label
        f = open(name+".tr", 'w')
        f.write(s)
        f.close()
