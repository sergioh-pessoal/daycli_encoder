#!/bin/bash
export MBUFR_TABLES=../bufrtables
#--------------------------------------------
# Encode daycli message im BUFR from text file
#----------------------------------------------
../bin/daycli_encoder -i DAYCLI_82191.txt -o DAYCLI_82191.bufr 

#-------------------------------------------------------------
# Decode DAYCLI message from BUFR to text file (using bufrdump)
#--------------------------------------------------------------
../bin/bufrdump -i DAYCLI_82191.bufr -o DAYCLI_82191.bufr.txt

#-------------------------------------------------------------
# Decode DAYCLI message from BUFR to csv file (using bufr2csv)
#--------------------------------------------------------------
../bin/bufr2csv DAYCLI_82191.bufr 0 0
