#####################################################################
# This makefile compiles INPE-MBUFRTOOLS + daycli_encoder  
#####################################################################

#------------------------------------------------------
# Include compilation directives for installed compiler
#-----------------------------------------------------
 include makefile_gfortran 
 #include makefile_g95
 #include makefile_ifort 

#--------------------------------------------------------------------
# DIRECTORIES
#--------------------------------------------------------------------
DIRSHARED=./shared
DIRBIN=../bin
DIRTOOLS=./tools
SRC=./daycli_encoder

#--------------
# MAIN PROGRAM 
#--------------
MAIN          =$(SRC)/daycli_encoder.f90

# ----------------------
# MBUFRTOOLS programs 
#----------------------
BUFRDUMPF     = $(DIRTOOLS)/bufrdump.f90
BUFRTIMEF     = $(DIRTOOLS)/bufrtime.f90
BUFRLISTF     = $(DIRTOOLS)/bufrcontent.f90
BUFRGENF      = $(DIRTOOLS)/bufrgen.f90
BUFRQCF       = $(DIRTOOLS)/bufrqc.f90
BUFRASCIIF    = $(DIRTOOLS)/bufr2csv.f90
BUFRSPLITF    = $(DIRTOOLS)/bufrsplit.f90

#--------------------------------------------------------------------
# SHARED MODULES  
#----------------------------------------------------------------------
STRINGFLIBF   = $(DIRSHARED)/f90lib/stringflib.f90
DATELIBF      = $(DIRSHARED)/f90lib/datelib.f90
MBUFRF        = $(DIRSHARED)/mbufr-adt/mbufr.f90
MTEMPLATESF   = $(DIRSHARED)/mbufr-adt/mformats.f90
MCODESFLAGS   = $(DIRSHARED)/mbufr-adt/mcodesflags.f90
MGRADSOF      = $(DIRSHARED)/grdlib/mgrads_obs.f90

#---------------------------
# EXEC
#--------------------------
EXEC         =  $(DIRBIN)/daycli_encoder
BUFRLIST     =  $(DIRBIN)/bufrcontent
BUFRGEN      =  $(DIRBIN)/bufrgen
BUFRDUMP     =  $(DIRBIN)/bufrdump
BUFRTIME     =  $(DIRBIN)/bufrtime
BUFRASCII    =  $(DIRBIN)/bufr2csv
BUFRSPLIT    =  $(DIRBIN)/bufrsplit

#-------------
# COMPILATION
#-------------

all:$(EXEC)  $(BUFRDUMP) $(BUFRGEN) $(BUFRLIST) $(BUFRTIME) $(BUFRSPLIT) $(BUFRASCII)  

$(EXEC) : $(MAIN) mbufr.o stringflib.o 
	mkdir -p $(DIRBIN)
	$(F90)  -o $@ $(MAIN) mbufr.o stringflib.o 
#-------------
# Basic tools 
#-------------
$(BUFRDUMP) : $(BUFRDUMPF) mbufr.o stringflib.o mcodesflags.o
	$(F90)  -o $@ $(BUFRDUMPF) mbufr.o stringflib.o mcodesflags.o
$(BUFRLIST) : $(BUFRLISTF) mbufr.o stringflib.o mcodesflags.o
	$(F90)  -o $@ $(BUFRLISTF) mbufr.o stringflib.o mcodesflags.o
$(BUFRTIME) : $(BUFRTIMEF) mbufr.o datelib.o stringflib.o
	$(F90)  -o $@ $(BUFRTIMEF) mbufr.o datelib.o stringflib.o
$(BUFRGEN) : $(BUFRGENF) mbufr.o stringflib.o
	$(F90)  -o $@ $(BUFRGENF) mbufr.o stringflib.o
$(BUFRASCII) : $(BUFRASCIIF) mbufr.o
	$(F90) -o $@ $(BUFRASCIIF) mbufr.o
$(BUFRSPLIT) : $(BUFRSPLITF) mbufr.o stringflib.o datelib.o
	$(F90) -o $@ $(BUFRSPLITF) mbufr.o stringflib.o datelib.o
#---------
# Modules 
#---------
mbufr.o   : $(MBUFRF)
	$(F90) -c $(MBUFRF)
stringflib.o   : $(STRINGFLIBF)
	$(F90) -c $(STRINGFLIBF)
datelib.o   : $(DATELIBF) stringflib.o
	$(F90) -c $(DATELIBF)
mformats.o : $(MTEMPLATESF) mbufr.o
	$(F90) -c $(MTEMPLATESF)
mcodesflags.o : $(MCODESFLAGS) stringflib.o mbufr.o
	$(F90) -c $(MCODESFLAGS)

clean:
	rm *.o  *.mod
