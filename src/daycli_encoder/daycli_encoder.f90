program daycli_encoder
 use stringflib
 use mbufr
 implicit none
 
 type time_slot
	real hour
	real minute
	real second
	real dt
 end type
 type(time_slot)                  ::TIME_TX
 type(time_slot)                  ::TIME_TN
 type(time_slot)                  ::TIME_TM
 type(time_slot)                  ::TIME_RR
 type(time_slot)                  ::TIME_DS
 type(time_slot)                  ::TIME_TSD
 real,parameter                   ::null_val =-99999999   ! Undefined value used in input namelist
 real                             ::undefval              ! Undefined value used by mbufr module
 character(len=1024)              ::filein
 character(len=1024)              ::fileout
 character(len=255)               ::line
 character(len=1),dimension(10)   ::namearg ! Nome dos argumentos!
 character(len=1024),dimension(10)::arg     ! argumentos 
 integer                          ::nargss
 integer                          ::l,i,j,s,i2,k
 integer                          ::c          !Number of character
 character(len=20),dimension(20)  ::substrings
 integer                          ::nelements
 integer                          ::WIGOS1,WIGOS2,WIGOS3,WMO1,WMO2
 character(len=17)                ::WIGOS4
 real(kind=realk),dimension(31)   ::RR,TN,TX,TM
 real(kind=realk),dimension(31)   ::HNEIGEF    !Death of Fresh Snow
 real(kind=realk),dimension(31)   ::NEIGETOT06 !Total Snow Death
 real(kind=realk),dimension(31)   ::QRR,QTN,QTX,QTM,QHNEIGEF,QNEIGETOT06
 character(len=10),dimension(31)  ::POSTE,DATE
 character(len=60)                ::auxc
 character(len=10),dimension(13)  ::cn
 character(len=22)                ::header !Telecomunication header (T1T2A1A1ii_cccc_YYGGgg(_BBB)
 integer                          ::nnn    !Sequence Number for header 
 real(kind=realk)                 ::LAT,LON,HT
 character(len=2)                 ::SM_TEMP, SM_PREC
 real,parameter                   ::FieldSig=5
 real,parameter                   ::maximum=2
 real,parameter                   ::minimum=3
 real,parameter                   ::mean=4
 integer                          ::day
 integer                          ::nday
 real(kind=realk)                 ::LATITUDE=null_val
 real(kind=realk)                 ::LONGITUDE=null_val
 character(len=60)                ::WIGOS=' '
 integer                          ::WMO=null_val
 real(kind=realk)                 ::HTEMP=null_val
 real(kind=realk)                 ::HA=null_val                 !HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL
 real(kind=realk)                 ::SMC_TEMP=null_val           !SITING AND MEASUREMENT QUALITY CLASSFICATION FOR TEMPERATURE (CODE TABLE) 22=B5
 real(kind=realk)                 ::SMC_PREC=null_val           !SITING AND MEASUREMENT QUALITY CLASSIFICATION FOR PRECIPITAION (CCITTIA5)
 real(kind=realk)                 ::METHOD_TM=null_val         !Method used to calculate the average daily temperature 
 integer                          ::YEAR=null_val
 integer                          ::MONTH=null_val
 real(kind=realk)                 ::HOUR=null_val              !Maximum Temperature - Hour
 real(kind=realk)                 ::MINUTE=null_val            !Maximum Temperature - Hour
 real(kind=realk)                 ::SECOND=null_val            !Maximum Temperature - Hour
 real(kind=realk)                 ::DT=null_val               !Time period or displacement
 real(kind=realk)                 ::CK                        !Use 0 or +273.16 to convert temperature from C to K
 integer                          ::X1,X2
 REAL(kind=realk)                 ::CENTER=255
 real(kind=realk)                 ::SUBCENTER=0 
 !{ Declaracao de variaveis para MBUFR-ADT
   type(sec1type)::sec1
   type(sec3type)::sec3
   type(sec4type)::sec4
   integer       ::err
!}
NAMELIST /SECTION1/CENTER,SUBCENTER
NAMELIST /STATION_ID/LATITUDE,LONGITUDE, WIGOS,WMO,HTEMP,HA,SMC_TEMP,SMC_PREC,METHOD_TM
NAMELIST /STIME_TX/HOUR,MINUTE,SECOND,DT
NAMELIST /STIME_TN/HOUR,MINUTE,SECOND,DT
NAMELIST /STIME_TM/HOUR,MINUTE,SECOND,DT
NAMELIST /STIME_RR/HOUR,MINUTE,SECOND,DT
NAMELIST /STIME_DS/HOUR,MINUTE,SECOND,DT
NAMELIST /STIME_TSD/HOUR,MINUTE,SECOND,DT
!---------
! WELCOME
!---------
   CK=0
   X1=0
   X2=0
   header=""
   nnn=0
   call getarg2(namearg,arg,nargss)                                         
   do i=1, nargss 
                                                          
      if (namearg(i)=="i") then 
        filein=arg(i)
        x1=1
      end if

      if (namearg(i)=="o") then 
        fileout=arg(i)
        x2=1
      end if
      if (namearg(i)=="h") then 
        header=arg(i)
        ![.471...INMX11.EUMP.272231...]

      end if
      if (namearg(i)=="s") then 
        nnn=val(arg(i))
      end if
   end do
   
 if ((x1*X2)==0) then
  print *,"+---------------------------------------------------------------------+"
  print *,"| INPE DAYCLI_ENCODER: Encode DAYCLI messages in FM94-BUFR (3-07-075) |"
  print *,"| Autor: sergio.ferreira@inpe.br - Version 1.1 2022                   |"
  print *,"| Include MBUFR-ADT module ",MBUFR_VERSION,"                  |"
  print *,"+---------------------------------------------------------------------+"
  print *,"| use daycli_encoder -i <infile> -o <outfile>  {-s nnn -h header}     |"
  print *,"|   infile:= input text file name                                     |"
  print *,"|   outfile:= outpur BUFR file name (DAYCLI)                          |"
  print *,"|                                                                     |"
  print *,"|   Optional: Inclusion of Abbreviation header                        |"
  print *,"|   -s nnn    := sequence number of Abbreviation header               |"
  print *,'|   -h header := "T1T2A1A2ii cccc YYGGgg ( BBB)"                      |'
  print *,"+---------------------------------------------------------------------+"
   
  
	stop
   end if
 !-----------------------------------------------------------------------------------
 ! gets the undefined value used by mbufr and sets stringflib to use the same value
 !-----------------------------------------------------------------------------------
  undefval=undef()
  call init_stringflib(undefval)
  
 
!----------------------------------------
! READ NAMELIST STATION_ID and TIME_SLOT
!----------------------------------------

 open (1,file=filein,status='old')
	READ  (1, SECTION1)
	if (SUBCENTER<0) SUBCENTER=255
	READ  (1, STATION_ID)
	
	READ  (1, STIME_TX)
	TIME_TX%HOUR=HOUR
	TIME_TX%MINUTE=MINUTE
	TIME_TX%SECOND=SECOND
	TIME_TX%DT=DT
	
	READ  (1, STIME_TN)
	TIME_TN%HOUR=HOUR
	TIME_TN%MINUTE=MINUTE
	TIME_TN%SECOND=SECOND
	TIME_TN%DT=DT
	
	READ  (1, STIME_TM)
	TIME_TM%HOUR=HOUR
	TIME_TM%MINUTE=MINUTE
	TIME_TM%SECOND=SECOND
	TIME_TM%DT=DT
	
	READ  (1, STIME_RR)
	TIME_RR%HOUR=HOUR
	TIME_RR%MINUTE=MINUTE
	TIME_RR%SECOND=SECOND
	TIME_RR%DT=DT
	
	READ  (1, STIME_DS)
	TIME_DS%HOUR=HOUR
	TIME_DS%MINUTE=MINUTE
	TIME_DS%SECOND=SECOND
	TIME_DS%DT=DT
	
	READ  (1, STIME_TSD)
	TIME_TSD%HOUR=HOUR
	TIME_TSD%MINUTE=MINUTE
	TIME_TSD%SECOND=SECOND
	TIME_TSD%DT=DT
	
 close(1)

print *, "NAMELIST: STATION_ID and TIME_SLOT (Ok)"


!---------------------------------
! WMO STATION NUMBER AND WIGOS ID
!---------------------------------
 
 if (WMO>0) then 
	WMO1=int(WMO/1000)
	WMO2=WMO-WMO1*1000
 else
	WMO1=undefval
	WMO2=undefval
 end if
 WIGOS1=null_val
 WIGOS2=null_val
 WIGOS3=null_val
 WIGOS4=""
 if (len_trim(WIGOS)>0) then
	call split(WIGOS,"-",substrings, nelements)
	if (nelements/=4) print *,"Error in WIGOS ID"
	WIGOS1= val(substrings(1))  !*001125-WIGOS IDENTIFIER SERIES (NUMERIC)
	WIGOS2= val(substrings(2))  ! 001126-WIGOS ISSUER OF IDENTIFIER (NUMERIC)
	WIGOS3= val(substrings(3))  ! 001127-WIGOS ISSUE NUMBER (NUMERIC)
	WIGOS4=trim(substrings(4))! 001128-WIGOS LOCAL IDENTIFIER (CHARACTER) (CCITTIA5)
 end if

 
write(*,'(" WIGOS=[",i2.2,"-",i5.5,"-",i5.5,"-",a16,"]")')WIGOS1,WIGOS2,WIGOS3,WIGOS4
 print *,'LATITUDE =',LATITUDE
 print *,'HTEMP    =',HTEMP
 print *,'HA       =',HA
 print *,'SMC_TEMP =',SMC_TEMP
 print *,'SMC_PREC =',SMC_PREC
 print *,'METHOD_TM=',METHOD_TM
 write (*,'(" WMO      = ",i2,i3)')WMO1,WMO2
 
 
 cn(1)="date"
 cn(2)="rr(mm)"
 cn(3)="qrr"
 cn(4)="ds"
 cn(5)="qds"
 cn(6)="tsd"
 cn(7)="qtsd"
 cn(8)="tn(K)"
 cn(9)="qtn"
 cn(10)="tx(K)"
 cn(11)="qtx"
 cn(12)="tm(K)"
 cn(13)="qtm"

 !-------------------
 ! Init Variables 
 !------------------
 
 RR(:)=null_val
 tN(:)=null_val
 tX(:)=null_val
 TM(:)=null_val
 HNEIGEF(:)   =null_val
 NEIGETOT06(:)=null_val
 QRR(i)=null_val
 qtN(i)=null_val
 qtX(i)=null_val
 qTM(i)=null_val
 qHNEIGEF(i)=null_val
 qNEIGETOT06(i)=null_val
			
 
 
LAT=LATITUDE 
LON=LONGITUDE 


HT=HTEMP                  !Highr of temperature sensor

!----------
! READ DATA 
!----------

	open (1,file=filein,status='old')
 5	read(1,'(a)',end=999) line
	if (index(line,"DATA_SECTION")==0) then
		goto 5
	end if 
	 
	i=0
 10	read(1,'(a)',end=999)line
		if (index(line,"/")==0) then 
		l=index(line,"#")+index(line,"!")+index(ucases(line),"DATE")
		if ((l==0).and.(len_trim(line)>0)) then 
			i=i+1
			!line=replace(line,",",".")
			call split(line,",",.false.,substrings, nelements)
			
			if (nelements<13) then 
			        print *,"Error reading the line =",i
				print *,line 
				print *,"Error: nelements=",nelements
				stop
			end if
			
			read(substrings(1),'(i4,i2,i2)')YEAR,MONTH,DAY
			if (i/=day) then 
				print *,"Error: Error in day sequence "
				print *,"The spected value = ",i
				print *,"The provided value =",day
				stop
			end if 
	
			RR (i)=val(substrings(2))
			QRR(i)=val(substrings(3))
			HNEIGEF(i)=val(substrings(4))
			qHNEIGEF(i)=val(substrings(5))
			NEIGETOT06(i)=val(substrings(6))
			qNEIGETOT06(i)=val(substrings(7))
			
			tN(i)=val(substrings(8))
			qtN(i)=val(substrings(9))
			tX(i)=val(substrings(10))
			qtX(i)=val(substrings(11))
			TM(i)=val(substrings(12))
			qTM(i)=val(substrings(13))
			
			
			
			!------------------------------------------------
			! Consistence of temperature (Must be in Kelvin)
			! ----------------------------------------------
			if ((tn(i)<0).and.(tn(i)>-3000)) then 
				print *,"Error 1 in values of temperature"
				write (*,'(" -> tn=",f5.2," tm=",f5.2," tx=",f5.2)')tn(i),tm(i),tx(i)
				do k=1,13
					print *,cn(k),"=",val(substrings(k))
				end do
				stop
			end if

			
			!------------------------------------------------
			! Consistence of temperature 
			! ----------------------------------------------
			if ((tn(i)>-3000).and.(tm(i)>-3000).and.(tx(i)>-3000)) then
			if (int(tn(i))>int(tx(i)).or.int(tn(i))>int(tm(i)).or.int(tx(i))<int(tm(i))) then
				print *,"Error 2 in values of temperature"
				write (*,'(" -> tn=",f7.2," tm=",f7.2," tx=",f7.2)')tn(i),tm(i),tx(i)
				print *," -> ",trim(line)
				
				print *,""
				print *,"Other values:"
			
				do k=1,13
					print *,cn(k),"=",val(substrings(k))
				end do
				stop
			end if 
			end if 
			!print *,i," Quality flag=",qrr(i),qtn(i),qtx(i),qtm(i)
			!print *,nelements,trim(line)
		end if 
		
		goto 10
		end if
 999    close(1)
	nday=i
	
	if (len_trim(header)>0) write(*,'(1x,"HEADER = ",i3.3," ",a)')nnn,trim(header)
	
    !--------------------------
	! INITIALIZE  MBUFR MODULE
	!--------------------------
	call INIT_MBUFR(3,.true.)
	call open_mbufr(2,fileout)
	if (len_trim(header)>0) call write_header(2,nnn,header)
	
	!----------------
	! SECTION 1 DATA
	!---------------
	if (SUBCENTER<0) SUBCENTER=255
	sec1%NumMasterTable=0
	sec1%center        =CENTER
	sec1%subcenter     =SUBCENTER
	sec1%update        =0
	sec1%btype         =0
	sec1%Intbsubtype   =21
	sec1%bsubtype      =0
	sec1%VerMasterTable=38
	sec1%VerLocalTable =0
	sec1%year          =YEAR
	sec1%month         =MONTH
	sec1%day           =0
	sec1%hour          =0
	sec1%minute        =0
	sec1%second        =0
			
	!----------------		
	! SECTION 3 DATA
	!---------------		
	sec3%nsubsets=nday
	sec3%ndesc=100
	sec3%is_cpk=0
	allocate(sec3%d(sec3%ndesc))
	j=0
	j=j+1;sec3%d(j)=307075 !<First-order statistics (code table) Set as missing value
	sec3%ndesc=j
	!print *," :DAYCLI2BUFR:sec3ndesc=",j
	
	
	!
	! SECTION 4
	!
	sec4%nvars=100
	allocate(sec4%r(sec4%nvars,sec3%nsubsets))
	allocate(sec4%c(sec4%nvars,sec3%nsubsets))
	sec4%r(:,:)=0
	sec4%c(:,:)=0
	
	do day=1,nday
	s=day
        j=0 	
	j=j+1;sec4%r(j,s)= WIGOS1 !#     1) *001125-WIGOS IDENTIFIER SERIES (NUMERIC)
        j=j+1;sec4%r(j,s)= WIGOS2 !#     2)  001126-WIGOS ISSUER OF IDENTIFIER (NUMERIC)
        j=j+1;sec4%r(j,s)= WIGOS3 !#     3)  001127-WIGOS ISSUE NUMBER (NUMERIC)
	
	auxc=WIGOS4               !# 4-19)  001128-WIGOS LOCAL IDENTIFIER (CHARACTER) (CCITTIA5)
	c=0
	do i2=1,16
		j=j+1
		c=c+1
		sec4%r(j,s)=ichar(auxc(i2:i2))
		sec4%c(j,s)=c
	end do
	
	j=j+1;sec4%r(j,s)= WMO1     !#    20) *001001-WMO BLOCK NUMBER (NUMERIC)
        j=j+1;sec4%r(j,s)= WMO2     !#    21)  001002-WMO STATION NUMBER (NUMERIC)
	j=j+1;sec4%r(j,s)= LAT      !#    24) *005001-LATITUDE (HIGH ACCURACY) (DEG)
        j=j+1;sec4%r(j,s)= LON      !#    25)  006001-LONGITUDE (HIGH ACCURACY) (DEG)
	j=j+1;sec4%r(j,s)= HA       !#    26)  007030-HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3) (M)
	j=j+1;sec4%r(j,s)= SMC_TEMP !#    27) B5=25 008095-SITING AND MEASUREMENT QUALITY CLASSFICATION FOR TEMPERATURE (CODE TABLE) 22=B5
	j=j+1;sec4%r(j,s)= SMC_PREC !#    28)"B3"=12 008096-SITING AND MEASUREMENT QUALITY CLASSIFICATION FOR PRECIPITAION (CCITTIA5)
	j=j+1;sec4%r(j,s)= METHOD_TM!#    29 Method used to calculate the average daily temperature  (8)
	j=j+1;sec4%r(j,s)= YEAR     !#    30)  004001-YEAR (A)
        j=j+1;sec4%r(j,s)= MONTH    !#    31)  004002-MONTH (MON)
	j=j+1;sec4%r(j,s)= day      !#    32)  004003-DAY (D)

	
	!(precipitation)
	!---------------
	j=j+1;sec4%r(j,s)= TIME_RR%DT              !#    33) 0-04-023 time perid or displacement (DAY)
	j=j+1;sec4%r(j,s)= TIME_RR%HOUR            !#    34) 0-04-004hour
	j=j+1;sec4%r(j,s)= TIME_RR%MINUTE          !#    35) Minute 
	j=j+1;sec4%r(j,s)= TIME_RR%SECOND          !#    36) Second
	j=j+1;sec4%r(j,s)= 0                  !#    37) 2-04-004
        j=j+1;sec4%r(j,s)= FieldSig           !#    38) 031021
	j=j+1;sec4%r(j,s)= QRR(day)           !# ...39) Assoaciated field 
	j=j+1;sec4%r(j,s)= RR(day)            !#    40) Total Accumulated Precipitation 
	j=j+1;sec4%r(j,s)=  0                 !#    41) 204000

	!Depth of fresh snow
	!----------------
	j=j+1;sec4%r(j,s)= TIME_DS%DT              !#    42)time perid or displacement
	j=j+1;sec4%r(j,s)= TIME_DS%HOUR            !#    43) hour
	j=j+1;sec4%r(j,s)= TIME_DS%MINUTE          !#    44) minute
	j=j+1;sec4%r(j,s)= TIME_DS%SECOND          !#    45) seconde
	j=j+1;sec4%r(j,s)= 0                  !#    46) 2-04-004
        j=j+1;sec4%r(j,s)= FieldSig           !#    47) 031021
	j=j+1;sec4%r(j,s)= qHNEIGEF(day)      !#    57) Assoaciated field 
	j=j+1;sec4%r(j,s)= HNEIGEF(DAY)    !#    58 [0-13-013! Total Snow Depht
	j=j+1;sec4%r(j,s)=  0                 !#    50) 20400
	
	!Total Snow Depth
	!------------------
	j=j+1;sec4%r(j,s)= TIME_TSD%DT             !#    51)time perid or displacement
	j=j+1;sec4%r(j,s)= TIME_TSD%HOUR           !#    52)hour
	j=j+1;sec4%r(j,s)= TIME_TSD%MINUTE         !#    53) minute
	j=j+1;sec4%r(j,s)= TIME_TSD%SECOND         !#    54) seconde
	j=j+1;sec4%r(j,s)=  0                 !#    55) 2-04-004
        j=j+1;sec4%r(j,s)= FieldSig           !#    56) 031021
	j=j+1;sec4%r(j,s)= qNEIGETOT06(day)   !#    48)
	j=j+1;sec4%r(j,s)= NEIGETOT06(day)    !#    49) [0-13-013! Total Snow Depht
	j=j+1;sec4%r(j,s)=  0                 !#    59)  20400
	
	j=j+1;sec4%r(j,s)=  HT                !#    60)  007032-HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) (M)
	 !{ Loop tx,tn,tt	
	 !{
	 j=j+1;sec4%r(j,s)= TIME_TX%DT             !#    61) time perid or displacement
	 j=j+1;sec4%r(j,s)= TIME_TX%HOUR           !#    62) #hour
         j=j+1;sec4%r(j,s)= TIME_TX%MINUTE         !#    63) minute
	 j=j+1;sec4%r(j,s)= TIME_TX%SECOND         !#    64) seconde
	 j=j+1;sec4%r(j,s)= maximum           !#    65) 008023-FIRST-ORDER STATISTICS (CODE TABLE)
	 j=j+1;sec4%r(j,s)=  0                !#    66) 204012
         j=j+1;sec4%r(j,s)=FieldSig           !#    67) 031021
	 j=j+1;sec4%r(j,s)=qtX(day)           !#    68)
         j=j+1;sec4%r(j,s)= TX(DAY)+CK        !#    69) 012101-TEMPERATURE/AIR TEMPERATURE (K)
	 j=j+1;sec4%r(j,s)=  0                !#    70) 20400
	 !}
	 !{
	 j=j+1;sec4%r(j,s)=TIME_TN%DT              !#    71)time perid or displacement
	 j=j+1;sec4%r(j,s)=TIME_TN%HOUR            !#  ..72)hour
         j=j+1;sec4%r(j,s)=TIME_TN%MINUTE          !#    73) minute
	 j=j+1;sec4%r(j,s)=TIME_TN%SECOND          !#    74) seconde
	 j=j+1;sec4%r(j,s)= minimum           !#    75)  008023-FIRST-ORDER STATISTICS (CODE TABLE)
	 j=j+1;sec4%r(j,s)=  0                !#    76)  204012
         j=j+1;sec4%r(j,s)=FieldSig           !#    77)  031021
	 j=j+1;sec4%r(j,s)=qtn(day)           !#    78)
         j=j+1;sec4%r(j,s)= TN(DAY)+CK        !#    79)  012101-TEMPERATURE/AIR TEMPERATURE (K)
	 j=j+1;sec4%r(j,s)=  0                !#    80)  20400
	 !}
	 !{
	  j=j+1;sec4%r(j,s)=TIME_TM%DT             !#    81) time perid or displacement
	  j=j+1;sec4%r(j,s)=TIME_TM%HOUR           !#    82) 026004-TIME IN UTC TO COMPUTE THE AVERAGE DAILY TEMPERATURE (H)
	  J=J+1;sec4%r(j,s)=TIME_TM%MINUTE         !#    83) minute
	  j=j+1;sec4%r(j,s)=TIME_TM%SECOND         !#    84) seconde
	  j=j+1;sec4%r(j,s)= mean             !#    85)  008023-FIRST-ORDER STATISTICS (CODE TABLE)
	  j=j+1;sec4%r(j,s)=  0               !#    86)  204012
          j=j+1;sec4%r(j,s)=FieldSig          !#    87)  031021
	  j=j+1;sec4%r(j,s)=qtm(day)          !#    88)
	  j=j+1;sec4%r(j,s)= TM(DAY)+CK       !#    89)  012101-TEMPERATURE/AIR TEMPERATURE (K)
	  j=j+1;sec4%r(j,s)=  0               !#    90)  20400
	 !}
	!}
	j=j+1;sec4%r(j,s)= undef()            !#    91) 008023-FIRST-ORDER STATISTICS (CODE TABLE)
	

	
	sec4%nvars=j
	end do
	!print *," :DAYCLI2BUFR:Nvars=",sec4%nvars
	call write_mbufr(2,sec1,sec3,sec4)
	
	if (len_trim(header)>0) call write_end_of_message(2)
	call close_mbufr(2)
        
    
    
    
	print *," :DAYCLI2BUFR: Done"


 
 
	stop
end program 
