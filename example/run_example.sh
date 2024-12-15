#!/bin/bash
export MBUFR_TABLES=../bufrtables
BIN=../bin/
#--------------------------------------------
# Encode daycli message im BUFR from text file
#----------------------------------------------
${BIN}daycli_encoder -i DAYCLI_82191.txt -o DAYCLI_82191.bufr

#-------------------------------------------------------------
# Decode DAYCLI message from BUFR to text file (using bufrdump)
#--------------------------------------------------------------
${BIN}/bufrdump -i DAYCLI_82191.bufr -o DAYCLI_82191.bufr.txt

#-------------------------------------------------------------
# Decode DAYCLI message from BUFR to csv file (using bufr2csv)
#--------------------------------------------------------------
${BIN}/bufr2csv DAYCLI_82191.bufr 0 0
