#!/usr/bin/python
import sys, glob, getopt
import os, re, random
import argparse
import datetime
import subprocess
from sklearn import svm, tree
import numpy as np 

bo_pgms = [
       "crackaddr", "recipient", "mime1", "mime2", "prescan", "tTflag", "dns",
       "nxt", "sig", "iquery", "ns",
       "mapped", "obo", "realpath",
       "polymorph","ncompress", "129.compress", "spell", "man", "bzip2", "gzip", "bc", "sed"
       ]
format_pgms = ["mp3","ghost","uni2ascii","pal", "shntool", "sdop", "latex", "rrd", "daemon","rplay", "rptp",
               "urjtag", "a2ps", "dico", "ddico"]

bug_info = {
# buffer overrun bugs
  'crackaddr' : 
    ["crackaddr-bad.c:197","crackaddr-bad.c:215","crackaddr-bad.c:248","crackaddr-bad.c:252","crackaddr-bad.c:303",
     "crackaddr-bad.c:305","crackaddr-bad.c:317","crackaddr-bad.c:333","crackaddr-bad.c:335","crackaddr-bad.c:345",
     "crackaddr-bad.c:351","crackaddr-bad.c:355","crackaddr-bad.c:362","crackaddr-bad.c:375","crackaddr-bad.c:411",
     "crackaddr-bad.c:426","crackaddr-bad.c:428","crackaddr-bad.c:437","crackaddr-bad.c:442","crackaddr-bad.c:445",
     "crackaddr-bad.c:473","crackaddr-bad.c:483","crackaddr-bad.c:485","crackaddr-bad.c:487","crackaddr-bad.c:496",
     "crackaddr-bad.c:499","crackaddr-bad.c:502","crackaddr-bad.c:504"],
  'recipient' : ["util-bad.c:176","util-bad.c:183"],
  "mime1"     : ["mime1-bad.c:239","mime1-bad.c:254","mime1-bad.c:266"], 
  "mime2"     : ["mime2-bad.c:203","mime2-bad.c:216","mime2-bad.c:216","mime2-bad.c:230","mime2-bad.c:234",
                 "mime2-bad.c:234","mime2-bad.c:246","mime2-bad.c:250","mime2-bad.c:250","mime2-bad.c:265"], 
  "prescan"   : ["prescan-overflow-bad.c:399", "prescan-overflow-bad.c:420", "prescan-overflow-bad.c:519"], 
  "tTflag"    : ["tTflag-bad.c:170"], 
  "dns"       : ["txt-dns-file-bad.c:328", "txt-dns-file-bad.c:330"],
  "nxt"       : ["nxt-bad.c:411"], 
  "sig"       : ["sig-bad.c:561"], 
  "iquery"    : ["iquery-bad.c:135"], 
  "ns"        : ["ns-lookup-bad.c:146", "ns-lookup-bad.c:164"],
  "mapped"    : ["mapped-path-bad.c:107", "mapped-path-bad.c:146", "mapped-path-bad.c:149", "mapped-path-bad.c:169"],
  "obo"       : ["realpath-bad.c:475"],
  "realpath"  : ["realpath-2.4.2-bad.c:192", "realpath-2.4.2-bad.c:262", "realpath-2.4.2-bad.c:266", "realpath-2.4.2-bad.c:281",
                 "realpath-2.4.2-bad.c:291", "realpath-2.4.2-bad.c:298", "realpath-2.4.2-bad.c:303", "realpath-2.4.2-bad.c:322",
                 "realpath-2.4.2-bad.c:331", "realpath-2.4.2-bad.c:338", "realpath-2.4.2-bad.c:343", "realpath-2.4.2-bad.c:356", 
                 "realpath-2.4.2-bad.c:359", "realpath-2.4.2-bad.c:364", "realpath-2.4.2-bad.c:376", "realpath-2.4.2-bad.c:383", 
                 "realpath-2.4.2-bad.c:390", "realpath-2.4.2-bad.c:397", "realpath-2.4.2-bad.c:402", "realpath-2.4.2-bad.c:410", 
                 "realpath-2.4.2-bad.c:416", "realpath-2.4.2-bad.c:425", "realpath-2.4.2-bad.c:432", "realpath-2.4.2-bad.c:437"],
  "polymorph"      : ["polymorph.c:118", "polymorph.c:192", "polymorph.c:193", "polymorph.c:193", "polymorph.c:196", 
                 "polymorph.c:198", "polymorph.c:200", "polymorph.c:209", "polymorph.c:276", "polymorph.c:229"],
  "ncompress"     : ["compress42.c:887", "compress42.c:908", "compress42.c:976", "compress42.c:981", "compress42.c:1003",
                 "compress42.c:1004", "compress42.c:1025", "compress42.c:1026", "compress42.c:1046", "compress42.c:1112",
                 "compress42.c:1113", "compress42.c:1138"],
  "spell"     : ["str.c:225"],
  "man"       : ["man.c:174", "man.c:175", "man.c:503", "man.c:996", "man.c:1002", "manfile.c:244"],
  "bzip2"     : ["bzip2.c:1077", "bzip2.c:1078", "bzip2.c:1082"],
  "129.compress"       : ["harness.c:26", "harness.c:29", "harness.c:196", "harness.c:198", "harness.c:204", 
                 "harness.c:204", "compress95.c:1192"], 
  "gzip"      : ["gzip.c:515", "gzip.c:1005", "gzip.c:1053", "gzip.c:1056", "gzip.c:1070", 
                 "gzip.c:1133", "gzip.c:1434", "gzip.c:1439", "gzip.c:1467", "gzip.c:1502",
                 "gzip.c:1504", "gzip.c:1685", "gzip.c:1688"], 
  "bc"        : ["storage.c:177", "util.c:577"],
  "sed"       : ["regex_internal.c:674"],
# format string bugs
  "mp3"       : ["mp3rename.c:554<-argv@mp3rename.c:25"],
  "uni2ascii" : ["uni2ascii.c:3111<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3120<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3130<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3148<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3152<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3165<-argv@uni2ascii.c:2722",
    "uni2ascii.c:3167<-argv@uni2ascii.c:2722"],
  "ghost"    : [ "genconf.c:773<-argv@genconf.c:338", "genconf.c:865<-argv@genconf.c:338"],
  "latex"    : [ "main.c:873<-_IO_getc@parser.c:438", "main.c:873<-_IO_getc@parser.c:449"],
  "daemon"   : [ "prog.c:1502<-argv@daemon.c:3299"],
  "rplay"    : [],
  "rptp"     : [ "rptp.c:320<-recv@rptp.c:358", "rptp.c:320<-fread@rptp.c:687", "rptp.c:261<-argv@rptp.c:153"], 
  "a2ps"     : [
   "dstring.c:326<-getenv@userdata.c:62",
   "dstring.c:326<-getenv@userdata.c:70",
   "dstring.c:326<-getenv@userdata.c:72",
   "dstring.c:326<-getenv@metaseq.c:538",
   "dstring.c:326<-getenv@metaseq.c:568",
   "dstring.c:326<-argv@main.c:928"],
  "dico"     : [ "diag.c:80<-getenv@connect.c:135"],
  "ddico"    : [ "main.c:1291<-argv@main.c:1355"],
  "gnuplot"  : [ "mouse.c:439<-fgets@misc.c:197"],
  "urjtag" : [
   "prototype.c:174<-fgets@jtag.c:226",
   "prototype.c:179<-fgets@jtag.c:226",
   "prototype.c:191<-fgets@jtag.c:226",
   "prototype.c:202<-fgets@jtag.c:226",
   "prototype.c:207<-fgets@jtag.c:226",
   "prototype.c:219<-fgets@jtag.c:226",

   "prototype.c:174<-fgetc@parse.c:130",
   "prototype.c:179<-fgetc@parse.c:130",
   "prototype.c:191<-fgetc@parse.c:130",
   "prototype.c:202<-fgetc@parse.c:130",
   "prototype.c:207<-fgetc@parse.c:130",
   "prototype.c:219<-fgetc@parse.c:130"],

  "pal" : [ "input.c:466<-argv@main.c:703", "input.c:466<-fgets@input.c:633", "input.c:621<-argv@main.c:703"],

  "shntool" : ["core_mode.c:766<-argv@core_shntool.c:357"],
  "rrd" : [ "rrd_info.c:28<-argv@rrd_tool.c:400" ],
  "sdop" : [ "write.c:1347<-fgets@read.c:772",
   "number.c:123<-fgets@read.c:772",
   "number.c:127<-fgets@read.c:772" ,
   "number.c:131<-fgets@read.c:772",
   "number.c:142<-fgets@read.c:772",
   "number.c:166<-fgets@read.c:772",
   "number.c:170<-fgets@read.c:772",
   "number.c:174<-fgets@read.c:772",
   "number.c:185<-fgets@read.c:772",
   "number.c:209<-fgets@read.c:772",
   "number.c:213<-fgets@read.c:772",
   "number.c:217<-fgets@read.c:772",
   "number.c:228<-fgets@read.c:772",
   "number.c:123<-fgets@read.c:320",
   "number.c:123<-fgets@read.c:351",
   "number.c:123<-fgets@read.c:394",
   "number.c:123<-fgets@read.c:553",
   "number.c:127<-fgets@read.c:320",
   "number.c:127<-fgets@read.c:351",
   "number.c:127<-fgets@read.c:394",
   "number.c:127<-fgets@read.c:553",
   "number.c:131<-fgets@read.c:320",
   "number.c:131<-fgets@read.c:351",
   "number.c:131<-fgets@read.c:394",
   "number.c:131<-fgets@read.c:553",
   "number.c:142<-fgets@read.c:320",
   "number.c:142<-fgets@read.c:351",
   "number.c:142<-fgets@read.c:394",
   "number.c:142<-fgets@read.c:553",
   "number.c:166<-fgets@read.c:320",
   "number.c:166<-fgets@read.c:351",
   "number.c:166<-fgets@read.c:394",
   "number.c:166<-fgets@read.c:553",
   "number.c:170<-fgets@read.c:320",
   "number.c:170<-fgets@read.c:351",
   "number.c:170<-fgets@read.c:394",
   "number.c:170<-fgets@read.c:553",
   "number.c:174<-fgets@read.c:320",
   "number.c:174<-fgets@read.c:351",
   "number.c:174<-fgets@read.c:394",
   "number.c:174<-fgets@read.c:553",
   "number.c:185<-fgets@read.c:320",
   "number.c:185<-fgets@read.c:351",
   "number.c:185<-fgets@read.c:394",
   "number.c:185<-fgets@read.c:553",
   "number.c:209<-fgets@read.c:320",
   "number.c:209<-fgets@read.c:351",
   "number.c:209<-fgets@read.c:394",
   "number.c:209<-fgets@read.c:553",
   "number.c:213<-fgets@read.c:320",
   "number.c:213<-fgets@read.c:351",
   "number.c:213<-fgets@read.c:394",
   "number.c:213<-fgets@read.c:553",
   "number.c:217<-fgets@read.c:320",
   "number.c:217<-fgets@read.c:351",
   "number.c:217<-fgets@read.c:394",
   "number.c:217<-fgets@read.c:553",
   "number.c:228<-fgets@read.c:320",
   "number.c:228<-fgets@read.c:351",
   "number.c:228<-fgets@read.c:394",
   "number.c:228<-fgets@read.c:553",
   "write.c:1347<-fgets@read.c:320",
   "write.c:1347<-fgets@read.c:351",
   "write.c:1347<-fgets@read.c:394",
   "write.c:1347<-fgets@read.c:553"]
}

def get_bug_info(pgm,output):
  bugs = bug_info[pgm]
  m = re.search(r"#unproven[ |\t]+: ([0-9]+)",output)
  num_of_alarms = int(m.group(1))
  num_of_bugs = 0
  for bug in bugs:
    if output.find(bug) > 0:
      num_of_bugs = num_of_bugs + 1
  return (num_of_alarms, num_of_bugs,  num_of_alarms - num_of_bugs)

def run(target,pgm,tunable_loop,tunable_lib,tunable_global,verbose,name):
  loop_param = ""
  lib_param = ""
  global_param = ""
  for loopid in tunable_loop:
    loop_param = loop_param + "-unsound_loop " + loopid + " "
  for libid in tunable_lib:
    lib_param = lib_param + "-unsound_lib " + libid + " "
  for loc in tunable_global:
    if loc == "all":
      global_param = "-unsound_global_all"
    else:
      global_param = global_param + "-unsound_global \"" + loc + "\" "

  if target == "bo":
    cmd = ("./bo_analyzer/main.native benchmarks/"+ target + "/" + pgm + "*.c " + loop_param + " " + lib_param + " " + global_param)
  elif target == "format":
    cmd = ("./format_analyzer/Main.native benchmarks/"+ target + "/" + pgm + "*.c " + lib_param)

  if verbose == True:
    print("== tunable loops ==")
    print(tunable_loop)
    print("== tunable libs ==")
    print(tunable_lib)
    print cmd

    output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, shell=True)
    (alarms,bugs,false) = get_bug_info(pgm,output)
    print "#Alarm : " + str(alarms)
    print "#Bug   : " + str(bugs)
    print "#False : " + str(false)
    return (bugs,false)
  else:
    output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, shell=True)
    (alarms,bugs,false) = get_bug_info(pgm,output)
    with open("results/"+pgm+"."+name,'w') as f:
      f.write(output)
    return (bugs,false)

def doSoundAnalysis(target,pgm):
  return run(target,pgm,[],[],[],False,"sound")

rand_feat_loop = []
rand_feat_lib = []
rand_feat_global = []

def shuffleFeature():
    global rand_feat_loop
    global rand_feat_lib
    global rand_feat_global
    rand_feat_loop = range(0,22)
    random.shuffle(rand_feat_loop)
    rand_feat_loop = rand_feat_loop[0:11]
    rand_feat_lib = range(0,15)
    random.shuffle(rand_feat_lib)
    rand_feat_lib = rand_feat_lib[0:8]
    rand_feat_global = range(0,30)
    random.shuffle(rand_feat_global)
    rand_feat_global = rand_feat_lib[0:15]

def getFeatures(args,param,tokens):
    name = tokens[0]
    tokens = tokens[1:]
    if args.feat == "syntactic" and param == "loop":
        tokens = tokens[0:13] + [tokens[len(tokens) - 1]]
    elif args.feat == "semantic" and param == "loop":
        tokens = tokens[13:]
    elif args.feat == "random" and param == "loop":
        new_tokens = []
        for i in rand_feat_loop:
            new_tokens.append(tokens[i])
        tokens = new_tokens + [tokens[len(tokens) - 1]]
    elif args.feat == "syntactic" and param == "lib":
        tokens = tokens[0:6] + [tokens[len(tokens) - 1]]
    elif args.feat == "semantic" and param == "lib":
        tokens = tokens[6:]
    elif args.feat == "random" and param == "lib":
        new_tokens = []
        for i in rand_feat_lib:
            new_tokens.append(tokens[i])
        tokens = new_tokens + [tokens[len(tokens) - 1]]
    elif args.feat == "syntactic" and param == "global":
        tokens = [tokens[0]] + tokens[25:28] + [tokens[len(tokens) - 1]]
    elif args.feat == "semantic" and param == "global":
        tokens = tokens[1:25] + tokens[28:]
    elif args.feat == "random" and param == "global":
        new_tokens = []
        for i in rand_feat_global:
            new_tokens.append(tokens[i])
        tokens = new_tokens + [tokens[len(tokens) - 1]]
    else:
        tokens = tokens
    return [name] + tokens

def mkTrSet(args,fnames,param):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  trset = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    tokens = getFeatures(args,param,tokens)
    if int(tokens[len(tokens)-1]) == 1:   
      tokens = tokens[1:len(tokens)-1]
      for b in tokens:
        feat.append(float(b))
      trset.append(feat)
  return trset

def mkLabeledTrSet(fnames):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  trset = []
  label = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    label.append(int(tokens[len(tokens)-1]))
    tokens = tokens[1:len(tokens)-1]
    for b in tokens:
      feat.append(float(b))
    trset.append(feat)
  return (trset,label)

def mkTestSet(args,fnames,param):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  testset = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    tokens = getFeatures(args,param,tokens)
    feat.append(tokens[0])
    tokens = tokens[1:len(tokens)] # all features
    for b in tokens:
      feat.append(float(b))
    testset.append(feat) 
  return testset

def mkOracle(fnames):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  oracle = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    if tokens[len(tokens) - 1] == "1":
        oracle.append(tokens[0])
  return oracle

def doOCSVM(target,trset,testset,param=""):
  # fit the model
    if target == "bo": 
        clf = svm.OneClassSVM(nu=0.1, kernel="rbf", gamma=0.1)
    else:
        clf = svm.OneClassSVM(nu=0.2, kernel="rbf", gamma=0.1)
    clf.fit(trset)
    tunable = []

    for feat in testset:
        result = clf.predict(np.array(feat[1:len(feat)-1]).reshape(1,-1))
        if result[0] == 1:
            tunable.append(feat[0])
    return tunable

def doDT(target,trset,label,unsound):
    clf = tree.DecisionTreeClassifier(criterion='entropy')
    clf.fit(trset,label)
    print("target: %s-%s: " % (target, unsound))
    print(clf.feature_importances_)
#    tree.export_graphviz(clf, out_file='tree_'+unsound+'.dot')


def doRandom1(testset):
    tunable = []
    for feat in testset:
        if random.getrandbits(1) == 0:
            tunable.append(feat[0])
    return tunable

def doRandom2(tunable, testset):
    testset = random.sample(testset,len(tunable))
    tunable = []
    for feat in testset:
        tunable.append(feat[0])
    return tunable

def doSVC(target,trset,label,testset,param=""):
  # fit the model
    clf = svm.SVC(kernel="rbf", class_weight="balanced")
    clf.fit(trset,label)
    tunable = []

    for feat in testset:
        result = clf.predict(np.array(feat[1:len(feat)-1]).reshape(1,-1))
        if result[0] == 1:
            tunable.append(feat[0])
    return tunable

def doIG(args):
    target = args.target
    if target == "bo":
        pgms = bo_pgms
        f_train = "cv/"+target+"_cv/train_all"
        loop_training = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
        lib_training = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
        global_training = map(lambda x: "data/"+target+"_data/global_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
        (loop_trset, loop_label) = mkLabeledTrSet(loop_training)
        (lib_trset, lib_label) = mkLabeledTrSet(lib_training)
        (global_trset, global_label) = mkLabeledTrSet(global_training)
        doDT(target, loop_trset, loop_label, 'loop')
        doDT(target, lib_trset, lib_label, 'lib')
        doDT(target, global_trset, global_label, 'global')
    else:
        pgms = format_pgms

def doSelectiveAnalysis(args,pgm,f_train,f_test):
    target = args.target
    if target == "bo":
        loop_training = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
        loop_test = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
        loop_testset = mkTestSet(args,loop_test,"loop")

        global_training = map(lambda x: "data/"+target+"_data/global_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
        global_test = map(lambda x: "data/"+target+"_data/global_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
        global_testset = mkTestSet(args,global_test,"global")

        if args.clf == "ocsvm":
            loop_trset = mkTrSet(args,loop_training,"loop")
            if 'loop' in args.unsound:
                tunable_loop = doOCSVM(target,loop_trset,loop_testset)
            else:
                tunable_loop = []
            global_trset = mkTrSet(args,global_training,"global")
            if 'global' in args.unsound:
                tunable_global = doOCSVM(target,global_trset,global_testset,param="global")
            else:
                tunable_global = []
        elif args.clf == "svc":
            (loop_trset,loop_label) = mkLabeledTrSet(loop_training)
            tunable_loop = doSVC(target,loop_trset,loop_label,loop_testset)
            (global_trset,global_label) = mkLabeledTrSet(global_training)
            tunable_global = doSVC(target,global_trset,global_label,global_testset,param="global")
        elif args.clf == "rand1":
            tunable_loop = doRandom1(loop_testset)
            tunable_global = doRandom1(global_testset)
        elif args.clf == "rand2":
            loop_trset = mkTrSet(args,loop_training,"loop")
            tunable_loop = doOCSVM(target,loop_trset,loop_testset)
            global_trset = mkTrSet(args,global_training,"global")
            tunable_global = doOCSVM(target,global_trset,global_testset,param="global")
            tunable_loop = doRandom2(tunable_loop,loop_testset)
            tunable_global = doRandom2(tunable_global,global_testset)
    else:
        tunable_loop = []
        tunable_global = []

    lib_training = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
    lib_test = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
    lib_testset = mkTestSet(args,lib_test,"lib")
    if args.clf == "ocsvm":
        lib_trset = mkTrSet(args,lib_training,"lib")
        if 'lib' in args.unsound:
            tunable_lib = doOCSVM(target,lib_trset,lib_testset)
        else:
            tunable_lib = []
    elif args.clf == "svc":
        (lib_trset,lib_label) = mkLabeledTrSet(lib_training)
        tunable_lib = doSVC(target,lib_trset,lib_label,lib_testset)
    elif args.clf == "rand1":
        tunable_lib = doRandom1(lib_testset)
    elif args.clf == "rand2":
        lib_trset = mkTrSet(args,lib_training,"lib")
        tunable_lib = doOCSVM(target,lib_trset,lib_testset)
        tunable_lib = doRandom2(tunable_lib,lib_testset)
    return run(target,pgm,tunable_loop,tunable_lib,tunable_global,False,"selective")

def rand1(args):
    pgms = bo_pgms
    for p in pgms:
        train = "cv/"+args.target+"_cv/train_"+p
        test = "cv/"+args.target+"_cv/test_"+p
        results = []
        for i in range(0,10):
            (selective_true, selective_false) = doSelectiveAnalysis(args,p,train,test)
            results.append((selective_true, selective_false))
        print p
        print results

def randFeature(args):
    pgms = bo_pgms
    avg_true = 0.0
    avg_false = 0.0
    for p in pgms:
        train = "cv/"+args.target+"_cv/train_"+p
        test = "cv/"+args.target+"_cv/test_"+p
        results = []
        total_true = 0
        total_false = 0
        for i in range(0,10):
            shuffleFeature()
            (selective_true, selective_false) = doSelectiveAnalysis(args,p,train,test)
            total_true += selective_true
            total_false += selective_false
            results.append((selective_true, selective_false))
        print p
        print results
        print "total true: %d, total false: %d" % (total_true, total_false)
#        print "avg true: %f, avg false: %f" % (float(total_true) / 10, float(total_false) / 10)
        avg_true += float(total_true) / 10
        avg_false += float(total_false) / 10
    print "avg true: %f, avg false: %f" % (avg_true, avg_false)


def doSelectiveAnalysisOracle(target,pgm,f_train,f_test):
  if target == "bo":
    loop_training = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
    loop_test = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
    loop_trset = mkTrSet(loop_training)
    loop_testset = mkTestSet(loop_test)
    tunable_loop = doOCSVM(target,loop_trset,loop_testset)

    global_test = map(lambda x: "data/"+target+"_data/global_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
    tunable_global = mkOracle(global_test)
  else:
    tunable_loop = []
  lib_training = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
  lib_test = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  lib_trset = mkTrSet(lib_training)
  lib_testset = mkTestSet(lib_test)
  tunable_lib = doOCSVM(target,lib_trset,lib_testset)
  return run(target,pgm,tunable_loop,tunable_lib,tunable_global,False,"oracle")

def doUnsoundAnalysis(args,pgm,f_test):
  target = args.target
  if target == "bo":
    loop_test = map(lambda x: "data/"+target+"_data/loop_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
    loop_testset = mkTestSet(args,loop_test,"loop")
    tunable_loop = [ x[0] for x in loop_testset ]
    global_test = map(lambda x: "data/"+target+"_data/global_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
    global_testset = mkTestSet(args,global_test,"global")
    tunable_global = [ x[0] for x in global_testset ]
  else:
    tunable_loop = []
    tunable_global = ["-1"]

  lib_test = map(lambda x: "data/"+target+"_data/lib_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  lib_testset = mkTestSet(args,lib_test,"lib")
  tunable_lib = [ x[0] for x in lib_testset ]
  return run(target,pgm,tunable_loop,tunable_lib,tunable_global,False,"unsound")

def leave_one_out(args):
  target = args.target
  if target == "bo":
    pgms = bo_pgms
  else:
    pgms = format_pgms
  total_bugs = 0
  total_sound_true = 0
  total_sound_false = 0
  total_selective_true = 0
  total_selective_false = 0
  total_unsound_true = 0
  total_unsound_false = 0
  print "Target : %s" % target
  print "Leave-one-out Cross Validation"
  print "-----------------------------------------------------------------"
  print "%20s %5s %10s %13s %10s" % ("", "", "Sound", "Selective", "Unsound")
  print "%20s %5s %5s %5s %5s %5s %5s %5s" % ("Program", "Bug", "T", "F", "T", "F", "T", "F")
  print "-----------------------------------------------------------------"
  for p in pgms:
    sys.stdout.flush()
    train = "cv/"+target+"_cv/train_"+p
    test = "cv/"+target+"_cv/test_"+p
    (sound_true, sound_false) = doSoundAnalysis(target,p)
    (selective_true, selective_false) = doSelectiveAnalysis(args,p,train,test)
    (unsound_true, unsound_false) = doUnsoundAnalysis(args,p,test)
    print "%20s %5d %5d %5d %5d %5d %5d %5d" % (p, len(bug_info[p]), sound_true, sound_false, selective_true, selective_false, unsound_true, unsound_false)
    total_bugs += len(bug_info[p])
    total_sound_true += sound_true
    total_sound_false += sound_false
    total_selective_true += selective_true
    total_selective_false += selective_false
    total_unsound_true += unsound_true
    total_unsound_false += unsound_false
  print "-----------------------------------------------------------------"
  print "                     %5s %5s %5s %5s %5s %5s %5s" % (total_bugs, total_sound_true, total_sound_false, total_selective_true, total_selective_false, total_unsound_true, total_unsound_false, )
  print "-----------------------------------------------------------------"


def cv_sound(args,f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (sound_true, sound_false) = doSoundAnalysis(args.target,p)
    total_true += sound_true
    total_false += sound_false
  return (total_true, total_false)

def cv_selective(args,f_train, f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (true, false) = doSelectiveAnalysis(args,p,f_train,f_test)
    total_true += true
    total_false += false
  return (total_true, total_false)
 
def cv_unsound(args,f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (true, false) = doUnsoundAnalysis(args.target,p,f_test)
    total_true += true
    total_false += false
  return (total_true, total_false)
 

def k_fold(args,k):
  total_bugs = 0
  total_sound_true = 0
  total_sound_false = 0
  total_selective_true = 0
  total_selective_false = 0
  total_unsound_true = 0
  total_unsound_false = 0
  print "Target : %s" % args.target
  print "%d - fold Cross Validation" % k
  print "-----------------------------------------------------------------"
  print "%5s %10s %13s %10s" % ("", "Sound", "Selective", "Unsound")
  print "%5s %5s %5s %5s %5s %5s %5s" % ("Trial", "T", "F", "T", "F", "T", "F")
  print "-----------------------------------------------------------------"
  for i in range(0,10):
    sys.stdout.flush()
    train = "cv/"+args.target+"_cv/train"+str(k)+"_"+str(i)
    test = "cv/"+args.target+"_cv/test"+str(k)+"_"+str(i)
    (sound_true, sound_false) = cv_sound(args,test)
    (selective_true, selective_false) = cv_selective(args,train,test)
    (unsound_true, unsound_false) = cv_unsound(args,test)
    print "%5d %5d %5d %5d %5d %5d %5d" % (i, sound_true, sound_false, selective_true, selective_false, unsound_true, unsound_false)
    total_sound_true += sound_true
    total_sound_false += sound_false
    total_selective_true += selective_true
    total_selective_false += selective_false
    total_unsound_true += unsound_true
    total_unsound_false += unsound_false
  print "-----------------------------------------------------------------"
  print "      %5s %5s %5s %5s %5s %5s" % (total_sound_true, total_sound_false, total_selective_true, total_selective_false, total_unsound_true, total_unsound_false, )
  print "-----------------------------------------------------------------"

def global_extract():
    target = "bo"
    directory = "data/bo_data/global_data"
    for p in bo_pgms:
        print p
        cmd = "~/project/sparrow/bin/sparrow benchmarks/"+ target + "/" + p + "*.c "
        output = subprocess.check_output(cmd, shell=True)
        with open(directory + "/" + p + ".feat", 'w') as f:
               f.write(output)


def global_labeling():
    target = "bo"
    directory = "global_data"
    for p in bo_pgms:
        print p
        (sound_true, sound_false) = doSoundAnalysis(target,p)
        f = open(directory + "/"+p+".data")
        fw = open(directory + "/"+p+".label", 'w')
        ft = open(directory + "/"+p+".temp", 'w')
        lines = f.readlines()
        trset = []
        temp_line = "sound true : " + str(sound_true) + ", sound false : " + str(sound_false) + "\n"
        ft.write(temp_line)
        for line in lines:
            col = re.split(r' |\t|\n',line)
            col = filter(lambda x: x.strip() != '', col)[0]
            (selective_true, selective_false) = run(target,p,[],[],[],[col],False,"selective")
            temp_line = "selective true : " + str(selective_true) + ", selective false : " + str(selective_false) + "\n"
            ft.write(temp_line)
            if (sound_false > selective_false) and (sound_true == selective_true):
                line = col + " : 1\n"
                fw.write(line)
            else:
                line = col + " : 0\n"
                fw.write(line)
        f.close()
        fw.close()

def main(argv):
    parser = argparse.ArgumentParser(description='Selectively Unsound Static Analysis Experiments')
    parser.add_argument('--target')
    parser.add_argument('--cv', default='leave-one-out')
    parser.add_argument('--clf', default='ocsvm')
    parser.add_argument('--feat', default='all')
    parser.add_argument('--unsound', action='append')
    # for random features
    # ./experiment.py --target bo --cv rand-feature --feat random
    args = parser.parse_args()

    if args.cv == 'leave-one-out':
        leave_one_out(args)
    elif args.cv == 'two-fold':
        k_fold(args,55)
    elif args.cv == 'three-fold':
        k_fold(args,73)
    elif args.cv == 'label':
        global_labeling()
    elif args.cv == 'extract':
        global_extract()
    elif args.cv == 'rand1':
        rand1(args)
    elif args.cv == 'rand-feature':
        randFeature(args)
    elif args.cv == 'ig':
        doIG(args)
    else:
        print 'Invalid Argument: ' + args.cv
        parser.print_help()
        sys.exit(0)
   
if __name__ == "__main__":
  main(sys.argv[1:])
