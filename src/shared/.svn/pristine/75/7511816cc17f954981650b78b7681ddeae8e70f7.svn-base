#!/bin/bash
#
# Este script corresponde ao DPS03. Vide partes do sistema. Vide tambem opcoes de executao
#
#-------------------------
# PARTES DO SISTEMA
#------------------------
# DPS01 ou scp - Copia dados BUFR das area remota e coloca num diretorio local (nao submeter no cray)    
# DPS02 -        Separa os dados por tipo e janela de tempo (bufrsplit) 
# DPS03 -        Roda o bufrextractor para fazer a extracao dos dados de interesse
#                conforme  tipo e gera as saidas me formato grads, text, etc. 
#
#--------------------------------
#  OPCOES DE  EXECUCAO DO DPS03
# -------------------------------	
	op1="yes"   #ANDAR
	op2a="yes"  #RASON
	op2b="yes"  #RASON TAC
	op2c="no"  #RASON ASSIMILA
	op3="no"   #HDS
	NP=7  # Numero de processadores
#------------------------------
cd $HOME/run/v.test/scripts/atena
source ./config.sh
dh=`date +%d%H`
SCNAME='run06h'
echo '+----------------------------------------------------+'
echo '|                     run_DPS03.sh                   |' 
echo '|                   Extracao de dados                |'    
echo '+----------------------------------------------------+'
echo '|WORK_HOME  ='$WORK_HOME
echo '|SUBMIT_HOME='$SUBMIT_HOME
echo '|RUNDIR     ='$RUNDIR
echo '|LOGDIR     ='$LOGDIR
echo '|DIRIN      ='$DIRIN 
cd $RUNDIR
	ee=$1
	echo '+----------------------------------------------------+'
	echo '|                     DPS03.sh                       |' 
	echo '+----------------------------------------------------+'
	if [ -z "$ee" ]; then 
		echo "| Inform date: (yyyymmddhh) or 'run'"
		#ee="yesterday"
		exit
	fi
	echo '| Start from :'` pwd -P` 
	echo '+----------------------------------------------------+'
	echo $SCNAME' '$ee
	ls -ltr ../shared/get_date.sh
	source ../shared/get_date.sh $ee
	yy=$yy0
	mm=$mm0
	dd=$dd0
	hh=$hh0
	echo $SCNAME'DATE='$yy$mm$dd$hh
#---------------------	
# OPCAO 1 - ANDAR 
#---------------------
if [ "$op1" == "yes" ] ; then
	localdir=$DIRIN/datain/brutos
        dataout=$DIROUT/dataout
	origin=$localdir/$yy$mm/$dd/DPS02
       	destination=$dataout/DPS03
       	mkdir -p $destination
        FILEIN=$origin/'X_B004_T'$yy$mm$dd$hh.bufr
        FILEOUT=$destination/'B004_T'$yy$mm$dd$hh	 
	OP='ANDAR'
        EXTRACTORCFG="mformat_ANDAR.cfg"	
 	flog=$LOGDIR/andar.log
        ymdh=$yy$mm$dd$hh
	mpirun -np $NP $BINDIR/bufrextractor -p $EXTRACTORCFG -i $FILEIN -o $FILEOUT -v 0 -d $ymdh
	

fi
#----------------------------	
# OPCAO 2 - RADIOSSONDA BUFR
#----------------------------

if [ "$op2a" == "yes" ] ; then
	#
	# BRUTOS
	#
	echo "---- BRUTOS ----"
	localdir=$DIRIN/datain/brutos
        dataout=$DIROUT/dataout
	origin=$localdir/$yy$mm/$dd/DPS02
       	destination=$dataout/DPS03/$yy$mm/$dd
       	mkdir -p $destination
        FILEIN=$origin/'X_B002_T'$yy$mm$dd$hh.bufr
        FILEOUT=$destination/'B002_T'$yy$mm$dd$hh	 
	OP='RASON'
        EXTRACTORCFG="mformat_RASON.cfg"	
 	flog=$LOGDIR/rason.log
        ymdh=$yy$mm$dd$hh
  	echo $BINDIR'/bufrextractor -p '$EXTRACTORCFG' -i '$FILEIN' -o '$FILEOUT' -v 0 -d '$ymdh
	mpirun -np $NP $BINDIR/bufrextractor -p $EXTRACTORCFG -i $FILEIN -o $FILEOUT -v 0 -d $ymdh
	cat $FILEOUT.pre_* > $FILEOUT.pre
	rm $FILEOUT.pre_*
	dcrason -o $FILEOUT -d $yy$mm
fi
 if [ "$op2b" == "yes" ] ; then
	#
	# TAC
	#	
	echo "---- TAC ----"
	localdir=$DIRIN/datain/bdados_tac
        dataout=$DIROUT/dataout
	origin=$localdir/$yy$mm/$dd/DPS02_tac
       	destination=$dataout/DPS03/$yy$mm/$dd
       	mkdir -p $destination
        FILEIN=$origin/'C_C046B002_T'$yy$mm$dd$hh.bufr
        FILEOUT=$destination/'TAC_B002_T'$yy$mm$dd$hh	 
	OP='RASON'
        EXTRACTORCFG="mformat_RASON.cfg"	
 	flog=$LOGDIR/rason.log
        ymdh=$yy$mm$dd$hh
  	echo $BINDIR'/bufrextractor -p '$EXTRACTORCFG' -i '$FILEIN' -o '$FILEOUT' -v 0 -d '$ymdh
	mpirun -np $NP $BINDIR/bufrextractor -p $EXTRACTORCFG -i $FILEIN -o $FILEOUT -v 0 -d $ymdh
	cat $FILEOUT.pre_* > $FILEOUT.pre
	rm $FILEOUT.pre_*
	dcrason -o $FILEOUT -d $yy$mm
	
fi
 if [ "$op2c" == "yes" ] ; then
	#
	# ASSIMILA
	#	
	echo "---- temppilot ----"
	localdir=$DIRIN/datain/assimila
        dataout=$DIROUT/dataout
	origin=$localdir/$yy$mm/$dd/DPS02
       	destination=$dataout/DPS03/$yy$mm/$dd
       	mkdir -p $destination
	FILEIN=$origin/'DAS_B002_T'$yy$mm$dd$hh.bufr
        FILEOUT=$destination/'DAS_B002_T'$yy$mm$dd$hh
        OP='RASON'
        EXTRACTORCFG="mformat_RASON.cfg"	
 	flog=$LOGDIR/rason.log
        ymdh=$yy$mm$dd$hh
  	echo $BINDIR'/bufrextractor -p '$EXTRACTORCFG' -i '$FILEIN' -o '$FILEOUT' -v 0 -d '$ymdh
	mpirun -np $NP $BINDIR/bufrextractor -p $EXTRACTORCFG -i $FILEIN -o $FILEOUT -v 0 -d $ymdh
	cat $FILEOUT.pre_* > $FILEOUT.pre
	rm $FILEOUT.pre_*
	dcrason -o $FILEOUT -d $yy$mm
	
fi
