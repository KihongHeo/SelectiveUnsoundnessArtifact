#!/usr/bin/python
import sys, glob, getopt
import os, re, random
import argparse
import datetime
import subprocess
from sklearn import svm
import numpy as np 

pgms = [
       "crackaddr", "recipient", "mime1", "mime2", "prescan", "tTflag", "dns",
       "nxt", "sig", "iquery", "ns",
       "mapped", "obo", "realpath",
       "polymorph","ncompress", "129.compress", "spell", "man", "bzip2", "gzip", "bc", "sed"
       ]

bug_info = {
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
  "realpath"  : ["realpath-2.4.2-bad.c:192", "realpath-2.4.2-bad.c:262", "realpath-2.4.2-bad.c:266", "realpath-2.4.2-bad.c:281", #"realpath-2.4.2-bad.c:286",
                 "realpath-2.4.2-bad.c:291", "realpath-2.4.2-bad.c:298", "realpath-2.4.2-bad.c:303", "realpath-2.4.2-bad.c:322", #"realpath-2.4.2-bad.c:327",
                 "realpath-2.4.2-bad.c:331", "realpath-2.4.2-bad.c:338", "realpath-2.4.2-bad.c:343", "realpath-2.4.2-bad.c:356", "realpath-2.4.2-bad.c:359",
                 "realpath-2.4.2-bad.c:364", "realpath-2.4.2-bad.c:376", "realpath-2.4.2-bad.c:383", "realpath-2.4.2-bad.c:390", "realpath-2.4.2-bad.c:397",
                 "realpath-2.4.2-bad.c:402", "realpath-2.4.2-bad.c:410", "realpath-2.4.2-bad.c:416", "realpath-2.4.2-bad.c:425", #"realpath-2.4.2-bad.c:421",
                 "realpath-2.4.2-bad.c:432", "realpath-2.4.2-bad.c:437"],
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
  "sed"       : ["regex_internal.c:674"]
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

def run(pgm,tunable_loop,tunable_lib,verbose):
  loop_param = ""
  lib_param = ""
  for loopid in tunable_loop:
    loop_param = loop_param + "-unsound_loop " + loopid + " "
  for libid in tunable_lib:
    lib_param = lib_param + "-unsound_lib " + libid + " "

  cmd = ("./bo_analyzer benchmarks/"+ pgm + "*.c " + loop_param + " " + lib_param)
  
  if verbose == True:
    print("== tunable loops ==")
    print(tunable_loop)
    print("== tunable libs ==")
    print(tunable_lib)

    output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, shell=True)
    (alarms,bugs,false) = get_bug_info(pgm,output)
    print "#Alarm : " + str(alarms)
    print "#Bug   : " + str(bugs)
    print "#False : " + str(false)
    return (bugs,false)
  else:
    output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, shell=True)
    (alarms,bugs,false) = get_bug_info(pgm,output)
    return (bugs,false)

def doSoundAnalysis(pgm):
  return run(pgm,[],[],False)

def mkTrSet(fnames):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  trset = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    if int(tokens[len(tokens)-1]) == 1:   
      tokens = tokens[1:len(tokens)-1]
      for b in tokens:
        feat.append(float(b))
      trset.append(feat)
  return trset


def mkTestSet(fnames):
  lines = []
  for fname in fnames:
    f = open(fname,"r")
    lines = lines + f.readlines()
  testset = []
  for line in lines:
    feat = []
    tokens = re.split(r' |\t|\n',line)
    tokens = filter(lambda x: x.strip() != '', tokens)
    feat.append(tokens[0])
    tokens = tokens[1:len(tokens)] # all features
    for b in tokens:
      feat.append(float(b))
    testset.append(feat)
  return testset

def doOCSVM(trset,testset):
  # fit the model
  clf = svm.OneClassSVM(nu=0.1, kernel="rbf", gamma=0.1)
  clf.fit(trset)
  tunable = []

  haru = 0
  inter = 0
  for feat in testset:
    result = clf.predict(np.array(feat[1:len(feat)-1]).reshape(1,-1))
    if result[0] == 1:
      tunable.append(feat[0])
  return tunable

def doSelectiveAnalysis(pgm,f_train,f_test):
  loop_training = map(lambda x: "data/loop_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
  loop_test = map(lambda x: "data/loop_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  lib_training = map(lambda x: "data/lib_data/"+x.strip("\n")+".tr", open(f_train, 'r').readlines())
  lib_test = map(lambda x: "data/lib_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  loop_trset = mkTrSet(loop_training)
  loop_testset = mkTestSet(loop_test)
  lib_trset = mkTrSet(lib_training)
  lib_testset = mkTestSet(lib_test)
  tunable_loop = doOCSVM(loop_trset,loop_testset)
  tunable_lib = doOCSVM(lib_trset,lib_testset)
  return run(pgm,tunable_loop,tunable_lib,False)

def doUnsoundAnalysis(pgm,f_test):
  loop_test = map(lambda x: "data/loop_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  lib_test = map(lambda x: "data/lib_data/"+x.strip("\n")+".tr", open(f_test, 'r').readlines())
  loop_testset = mkTestSet(loop_test)
  lib_testset = mkTestSet(lib_test)
  tunable_loop = [ x[0] for x in loop_testset ]
  tunable_lib = [ x[0] for x in lib_testset ]
  return run(pgm,tunable_loop,tunable_lib,False)

def leave_one_out():
  total_bugs = 0
  total_sound_true = 0
  total_sound_false = 0
  total_selective_true = 0
  total_selective_false = 0
  total_unsound_true = 0
  total_unsound_false = 0
  print "-----------------------------------------------------------------"
  print "%20s %5s %10s %13s %10s" % ("", "", "Sound", "Selective", "Unsound")
  print "%20s %5s %5s %5s %5s %5s %5s %5s" % ("Program", "Bug", "T", "F", "T", "F", "T", "F")
  print "-----------------------------------------------------------------"
  for p in pgms:
    sys.stdout.flush()
    train = "cv/train_"+p
    test = "cv/test_"+p
    (sound_true, sound_false) = doSoundAnalysis(p)
    (selective_true, selective_false) = doSelectiveAnalysis(p,train,test)
    (unsound_true, unsound_false) = doUnsoundAnalysis(p,test)
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


def cv_sound(f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (sound_true, sound_false) = doSoundAnalysis(p)
    total_true += sound_true
    total_false += sound_false
  return (total_true, total_false)

def cv_selective(f_train, f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (true, false) = doSelectiveAnalysis(p,f_train,f_test)
    total_true += true
    total_false += false
  return (total_true, total_false)
 
def cv_unsound(f_test):
  pgms = map (lambda x: x.strip("\n"), open (f_test, 'r').readlines())
  total_true = 0
  total_false = 0
  for p in pgms:
    (true, false) = doUnsoundAnalysis(p,f_test)
    total_true += true
    total_false += false
  return (total_true, total_false)
 

def k_fold(k):
  total_bugs = 0
  total_sound_true = 0
  total_sound_false = 0
  total_selective_true = 0
  total_selective_false = 0
  total_unsound_true = 0
  total_unsound_false = 0
  print "%d - fold Cross Validation" % k
  print "-----------------------------------------------------------------"
  print "%5s %10s %13s %10s" % ("", "Sound", "Selective", "Unsound")
  print "%5s %5s %5s %5s %5s %5s %5s" % ("Trial", "T", "F", "T", "F", "T", "F")
  print "-----------------------------------------------------------------"
  for i in range(0,10):
    sys.stdout.flush()
    train = "cv/train"+str(k)+"_"+str(i)
    test = "cv/test"+str(k)+"_"+str(i)
    (sound_true, sound_false) = cv_sound(test)
    (selective_true, selective_false) = cv_selective(train,test)
    (unsound_true, unsound_false) = cv_unsound(test)
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

def main(argv):
  parser = argparse.ArgumentParser(description='ICSE 2017 Experiments -- Interval Analysis')
  parser.add_argument('--cv')
  args = parser.parse_args()

  if args.cv == 'leave-one-out':
    leave_one_out()
  elif args.cv == 'two-fold':
    k_fold(55)
  elif args.cv == 'three-fold':
    k_fold(73)
  else:
    print 'Invalid Argument: ' + args.cv
    parser.print_help()
    sys.exit(0)
   
if __name__ == "__main__":
  main(sys.argv[1:])
