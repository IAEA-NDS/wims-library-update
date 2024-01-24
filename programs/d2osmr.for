       PROGRAM D2OSMR
C-Title  : D2OSMR Program
C-Purpose: Summarize WLUP D2O Benchmarks
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2000)
C-M
C-M  Manual for Program D2OSMR
C-M  =========================
C-M  A summary of results for the WLUP heavy water reactor
C-M  benchmarks is compiled from partial outputs. The output
C-M  list files are processed:
C-M    Ex.1   ZED-2  Task-1 (2 cases)
C-M                  Task-2 (2 cases)
C-M                  Task-3 (1 case)
C-M                  Task-4 (4 cases)
C-M    Ex.2   DCA    Task-1 (6 cases)
C-M
C-M  The list files are those produced by analysing the WIMS-D
C-M  outputs with post-processing codes developed within the scope
C-M  of the WIMS-D Library Update Project (IAEA-CRP 1999-2000) by
C-M  Francisco Leszczynski.
C-M
C-M  Instructions:
C-M  The filenames for the output report file and the 5 list files
C-M  for the benchmarks described above are entered in response to
C-M  the prompt from the keyboard. Individual files may be omitted
C-M  from the analysis by entering "-" for the filename.
C-M
C-
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(16)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'E1T1we6.lis'
     2    ,     'E1T2we6.lis'
     3    ,     'E1T3we6.lis'
     4    ,     'E1T4we6.lis'
     1    ,     'E2T1we6.lis'
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'D2OSMR.LST'/
C*
      DATA NAME/' ZED2T1D2O'
     1         ,' ZED2T1Air'
     2         ,' ZED2T2p24'
     2         ,' ZED2T2p40'
     3         ,' ZED2T3p24'
     4         ,' ZED2T4D22'
     4         ,' ZED2T4A22'
     4         ,' ZED2T4D28'
     4         ,' ZED2T4A28'
     1         ,'  DCAT1D22'
     1         ,'  DCAT1A22'
     1         ,'  DCAT1H22'
     1         ,'  DCAT1D25'
     1         ,'  DCAT1A25'
     1         ,'  DCAT1H25'
     4         ,'          '/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,691) ' D2OSMR - Summarize WLUP D2O Benchmarks '
      WRITE(LTT,691) ' ====================================== '
      WRITE(LTT,691)
C*
C* Define the output file
      WRITE(LTT,691) ' Default output filename              : ',FLNO
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNO=FLNM
C*
C* Define the output file
      IC=1
   11 WRITE(LTT,691) ' Default ZED-2 Task 1 Benchmark file  : ',FLNI(IC)
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
      IC=2
   12 WRITE(LTT,691) ' Default ZED-2 Task-2 Benchmark file  : ',FLNI(IC)
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=12)
        CLOSE(UNIT=LIN)
      END IF
      IC=3
   13 WRITE(LTT,691) ' Default ZED-2 Task 3 Benchmark file  : ',FLNI(IC)
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=13)
        CLOSE(UNIT=LIN)
      END IF
      IC=4
   14 WRITE(LTT,691) ' Default ZED-2 Task 4 Benchmark file  : ',FLNI(IC)
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=14)
        CLOSE(UNIT=LIN)
      END IF
      IC=5
   15 WRITE(LTT,691) ' Default DCA Task 1 Benchmark file    : ',FLNI(IC)
      WRITE(LTT,691) '$          Enter new name to redefine : '
      READ (LKB,691) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=15)
        CLOSE(UNIT=LIN)
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,691) ' D2OSMR - Summarize WLUP D2O Benchmarks '
      WRITE(LOU,691) ' ====================================== '
      WRITE(LOU,691)
C*
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              2 cases : ',FLNI(1)
      IF(FLNI(2).NE.'-')
     1WRITE(LOU,691) '                              2 cases : ',FLNI(2)
      IF(FLNI(3).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(3)
      IF(FLNI(4).NE.'-')
     1WRITE(LOU,691) '                              4 cases : ',FLNI(4)
      IF(FLNI(5).NE.'-')
     1WRITE(LOU,691) '                              6 cases : ',FLNI(5)
      WRITE(LOU,691)
C*
      WRITE(LOU,691) '   LATTICE      K-eff         Del       '
     1              ,'  ConvR          CuA          LuR       '
      WRITE(LOU,691) ' ======================================='
     1              ,'====================================    '
C* Initialize
      SRKE=0
      SDKE=0
      ADKE=0
      NDKE=0
C*
      SRRH=0
      SDRH=0
      ADRH=0
      NDRH=0
C*
      SRCR=0
      SDCR=0
      ADCR=0
      NDCR=0
C*
      SRCU=0
      SDCU=0
      ADCU=0
      NDCU=0
C*
      SRUL=0
      SDUL=0
      ADUL=0
      NDUL=0
 
C*
C* Begin processing the ZED-2 Task-1 Benchmark
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=100)
C* Multiplication factor for D2O and Air channel
      AK0=1
      RO0=0
   20 READ (LIN,692,END=100) REC
      IF(REC(1:14).NE.'COOLANT METHOD') GO TO 20
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:20),'(F9.0)') RO1
      AK1=1/(1-RO1/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:20),'(F9.0)') RO2
      AK2=1/(1-RO2/100000)
      RO1=RO1/1000
      RO2=RO2/1000
C* Fast fission factor (assembly average) from DSN calculation
   22 READ (LIN,692) REC
      IF(REC(1: 8).NE.'Coolant ') GO TO 22
      READ (LIN,692) REC
      READ (REC(40:47),'(F8.0)' ) RRH1
      READ (LIN,692) REC
      READ (REC(40:47),'(F8.0)' ) RRH2
   23 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 23
      READ (REC( 8:11),'(F4.0)' ) DRH0
   24 READ (LIN,692) REC
      IF(REC(1:14).NE.'Coolant Method') GO TO 24
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(51:56),'(F6.0)') DRH1
      RH1=RRH1*(1+DRH1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(51:56),'(F8.0)') DRH2
      RH2=RRH2*(1+DRH2/100)
c
CD       PRINT *,'DEL',RRH1,RRH2,DRH0,RH1,DRH1,RH2,DRH2
c
C* Initial conversion factor (assembly average) from DSN calculation
   25 READ (LIN,692) REC
      IF(REC(1: 8).NE.'Coolant ') GO TO 25
      READ (LIN,692) REC
      READ (REC(40:47),'(F9.0)' ) RCR1
      READ (LIN,692) REC
      READ (REC(40:47),'(F9.0)' ) RCR2
   26 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 26
      READ (REC( 8:12),'(F5.0)' ) DCR0
   27 READ (LIN,692) REC
      IF(REC(1:14).NE.'Coolant Method') GO TO 27
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(55:61),'(F7.0)') DCR1
      CR1=RCR1*(1+DCR1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(55:61),'(F8.0)') DCR2
      CR2=RCR2*(1+DCR2/100)
c
CD       PRINT *,'CRR',RCR1,RCR2,DCR0,CR1,DCR1,CR2,DCR2
c
C* Fission rate (not processed)
   28 READ (LIN,692) REC
      IF(REC(1: 8).NE.'Coolant ') GO TO 28
      READ (LIN,692) REC
      READ (REC(40:47),'(F9.0)' ) RFR1
      READ (LIN,692) REC
      READ (REC(40:47),'(F9.0)' ) RFR2
   29 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 29
      READ (REC( 8:11),'(F4.0)' ) DFR0
   30 READ (LIN,692) REC
      IF(REC(1:14).NE.'Coolant Method') GO TO 30
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (LIN,692) REC
C      READ (LIN,692)
C* Copper Activity (overall average) from DSN calculation
   31 READ (LIN,692) REC
      IF(REC(1: 8).NE.'Coolant ') GO TO 31
      READ (LIN,692) REC
      READ (REC( 8:42),'(5F7.0)') RCUA,RCUB,RCUC,RCUD,RCUM
      RCU1=(RCUA+RCUB+RCUC+RCUD+RCUM)/5
      READ (LIN,692) REC
      READ (REC( 8:42),'(5F7.0)') RCUA,RCUB,RCUC,RCUD,RCUM
      RCU2=(RCUA+RCUB+RCUC+RCUD+RCUM)/5
   32 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 32
      READ (REC( 8:11),'(F4.0)' ) DCU0
   33 READ (LIN,692) REC
      IF(REC(1:14).NE.'Coolant Method') GO TO 33
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:61),'(5(3X,F7.0))') CUA,CUB,CUC,CUD,CUM
      DCU1=(CUA+CUB+CUC+CUD+CUM)/5
      CU1 =RCU1*(1+DCU1/100)
      DCU1=SQRT((CUA*CUA+CUB*CUB+CUC*CUC+CUD*CUD+CUM*CUM)/5)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:61),'(5(3X,F7.0))') CUA,CUB,CUC,CUD,CUM
      DCU2=(CUA+CUB+CUC+CUD+CUM)/5
      CU2 =RCU2*(1+DCU2/100)
      DCU2=SQRT((CUA*CUA+CUB*CUB+CUC*CUC+CUD*CUD+CUM*CUM)/5)
c
CD       PRINT *,'CUA',RCU1,RCU2,DCU0,CU1,DCU1,CU2,DCU2
c
C* Lutetium/Manganese ratio (overall average) from DSN calculation
   34 READ (LIN,692) REC
      IF(REC(1: 8).NE.'Coolant ') GO TO 34
      READ (LIN,692) REC
      READ (REC(36:42),'(F9.0)' ) RUL1
      READ (LIN,692) REC
      READ (REC(36:42),'(F9.0)' ) RUL2
   35 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 35
      READ (REC( 8:11),'(F4.0)' ) DUL0
   36 READ (LIN,692) REC
      IF(REC(1:14).NE.'Coolant Method') GO TO 36
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:61),'(5(3X,F7.0))') ULA,ULB,ULC,ULD,DUL1
      UL1 =RUL1*(1+DUL1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(12:61),'(5(3X,F7.0))') ULA,ULB,ULC,ULD,DUL2
      UL2 =RUL2*(1+DUL2/100)
c
CD       PRINT *,'LUR',RUL1,RUL2,DUL0,UL1,DUL1,UL2,DUL2
c
C*
      SRKE=SRKE+2*RO0
      SDKE=SDKE+RO1*RO1+RO2*RO2
      ADKE=ADKE+RO1+RO2
      NDKE=NDKE+2
C*
      SRRH=SRRH+2*DRH0
      SDRH=SDRH+DRH1*DRH1+DRH2*DRH2
      ADRH=ADRH+DRH1+DRH2
      NDRH=NDRH+2
C*
      SRCR=SRCR+2*DCR0
      SDCR=SDCR+DCR1*DCR1+DCR2*DCR2
      ADCR=ADCR+DCR1+DCR2
      NDCR=NDCR+2
C*
      SRCU=SRCU+2*DCU0
      SDCU=SDCU+DCU1*DCU1+DCU2*DCU2
      ADCU=ADCU+DCU1+DCU2
      NDCU=NDCU+2
C*
      SRUL=SRUL+2*DUL0
      SDUL=SDUL+DUL1*DUL1+DUL2*DUL2
      ADUL=ADUL+DUL1+DUL2
      NDUL=NDUL+2
C* Print the resutls for this benchmark
      WRITE(LOU,601) NAME(1)
     1              ,AK0,RO0,RRH1,DRH0,RCR1,DCR0,RCU1,DCU0,RUL1,DUL0
      WRITE(LOU,601)'          '
     1              ,AK1,RO1, RH1,DRH1, CR1,DCR1, CU1,DCU1, UL1,DUL1
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(2)
     1              ,AK0,RO0,RRH2,DRH0,RCR2,DCR0,RCU2,DCU0,RUL2,DUL0
      WRITE(LOU,601)'          '
     1              ,AK2,RO2, RH2,DRH2, CR2,DCR2, CU2,DCU2, UL2,DUL2
      WRITE(LOU,691)
C*
      CLOSE(UNIT=LIN)
C*
C* Begin processing the ZED-2 Task-2 Benchmark
  100 IC=2
      OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=200)
C* Multiplication factor for D2O and Air channel
      AK0=1
      RO0=0
  120 READ (LIN,692,END=200) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 120
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:22),'(F9.0)') RO1
      AK1=1/(1-RO1/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:22),'(F9.0)') RO2
      AK2=1/(1-RO2/100000)
c
CD       PRINT *,'AKE',AK0,AK0,R00,AK1,RO1,AK2,RO2
c
      RO1=RO1/1000
      RO2=RO2/1000
C* Fast fission factor (assembly average) from DSN calculation
  122 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 122
      READ (LIN,692) REC
      READ (REC( 5:17),'(F13.0)' ) RRH1
      READ (LIN,692) REC
      READ (REC( 5:17),'(F13.0)' ) RRH2
  123 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 123
      READ (REC( 8:11),'(F4.0)' ) DRH0
  124 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 124
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:23),'(F10.0)') DRH1
      RH1=RRH1*(1+DRH1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:23),'(F10.0)') DRH2
      RH2=RRH2*(1+DRH2/100)
c
CD       PRINT *,'DEL',RRH1,RRH2,DRH0,RH1,DRH1,RH2,DRH2
c
C* Initial conversion factor (assembly average) from DSN calculation
  125 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 125
      READ (LIN,692) REC
      READ (REC( 5:17),'(F13.0)' ) RCR1
      READ (LIN,692) REC
      READ (REC( 5:17),'(F13.0)' ) RCR2
  126 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 126
      READ (REC( 8:12),'(F5.0)' ) DCR0
  127 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 127
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:23),'(F10.0)') DCR1
      CR1=RCR1*(1+DCR1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(14:23),'(F10.0)') DCR2
      CR2=RCR2*(1+DCR2/100)
c
CD       PRINT *,'CRR',RCR1,RCR2,DCR0,CR1,DCR1,CR2,DCR2
c
C*
      SRKE=SRKE+2*RO0
      SDKE=SDKE+RO1*RO1+RO2*RO2
      ADKE=ADKE+RO1+RO2
      NDKE=NDKE+2
C*
      SRRH=SRRH+2*DRH0
      SDRH=SDRH+DRH1*DRH1+DRH2*DRH2
      ADRH=ADRH+DRH1+DRH2
      NDRH=NDRH+2
C*
      SRCR=SRCR+2*DCR0
      SDCR=SDCR+DCR1*DCR1+DCR2*DCR2
      ADCR=ADCR+DCR1+DCR2
      NDCR=NDCR+2
C* Print the resutls for this benchmark
      WRITE(LOU,601) NAME(3)
     1              ,AK0,RO0,RRH1,DRH0,RCR1,DCR0
      WRITE(LOU,601)'          '
     1              ,AK1,RO1, RH1,DRH1, CR1,DCR1
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(4)
     1              ,AK0,RO0,RRH2,DRH0,RCR2,DCR0
      WRITE(LOU,601)'          '
     1              ,AK2,RO2, RH2,DRH2, CR2,DCR2
      WRITE(LOU,691)
C*
      CLOSE(UNIT=LIN)
C*
C* Begin processing the ZED-2 Task-3 Benchmark
  200 IC=3
      OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=300)
C* Multiplication factor for D2O and Air channel
      AK0=1
      RO0=0
  220 READ (LIN,692,END=300) REC
      IF(REC(1: 6).NE.'METHOD') GO TO 220
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC( 5:12),'(F8.0)') RO1
      AK1=1/(1-RO1/100000)
c
CD      PRINT *,'AKE',AK0,AK0,R00,AK1,RO1,AK2,RO2
c
      RO1=RO1/1000
C*
      SRKE=SRKE+RO0**2
      SDKE=SDKE+RO1*RO1
      ADKE=ADKE+RO1
      NDKE=NDKE+1
C* Print the resutls for this benchmark
      WRITE(LOU,601) NAME(5)
     1              ,AK0,RO0
      WRITE(LOU,601)'          '
     1              ,AK1,RO1
      WRITE(LOU,691)
C*
      CLOSE(UNIT=LIN)
C*
C* Begin processing the ZED-2 Task-4 Benchmark
  300 IC=4
      OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=400)
C* Multiplication factor for D2O and Air channel
      AK0=1
      RO0=0
  320 READ (LIN,692,END=400) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 320
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:30),'(F9.0)') RO1
      AK1=1/(1-RO1/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:30),'(F9.0)') RO2
      AK2=1/(1-RO2/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:30),'(F9.0)') RO3
      AK3=1/(1-RO3/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:30),'(F9.0)') RO4
      AK4=1/(1-RO4/100000)
c
CD      PRINT *,'AKE',AK0,AK0,R00,AK1,RO1,AK2,RO2,AK3,RO3,AK4,RO4
c
      RO1=RO1/1000
      RO2=RO2/1000
      RO3=RO3/1000
      RO4=RO4/1000
C* Fast fission factor (assembly average) from DSN calculation
  322 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 322
      READ (LIN,692) REC
      READ (REC(16:26),'(F11.0)' ) RRH1
      READ (LIN,692) REC
      READ (REC(16:26),'(F11.0)' ) RRH2
      READ (LIN,692) REC
      READ (REC(16:26),'(F11.0)' ) RRH3
      READ (LIN,692) REC
      READ (REC(16:26),'(F11.0)' ) RRH4
  323 READ (LIN,692) REC
      IF(REC(1: 7).NE.'(Dexp.=' ) GO TO 323
      READ (REC( 8:12),'(F5.0)' ) DRH0
  324 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 324
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') DRH1
      RH1=RRH1*(1+DRH1/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') DRH2
      RH2=RRH2*(1+DRH2/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') DRH3
      RH3=RRH3*(1+DRH3/100)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') DRH4
      RH4=RRH4*(1+DRH4/100)
c
CD    PRINT *,'DEL',RRH1,RRH2,RRH3,RRH4,DRH0
c    1             ,RH1,DRH1,RH2,DRH2,RH3,DRH3,RH4,DRH4
c
C*
      SRKE=SRKE+4*RO0
      SDKE=SDKE+RO1*RO1+RO2*RO2+RO3*RO3+RO4*RO4
      ADKE=ADKE+RO1+RO2+RO3+RO4
      NDKE=NDKE+4
C*
      SRRH=SRRH+4*DRH0
      SDRH=SDRH+DRH1*DRH1+DRH2*DRH2+DRH3*DRH3+DRH4*DRH4
      ADRH=ADRH+DRH1+DRH2+DRH3+DRH4
      NDRH=NDRH+4
C* Print the results for this benchmark
      WRITE(LOU,601) NAME(6)
     1              ,AK0,RO0,RRH1,DRH0
      IF(RO1.LT.-.99) RO1=ABS(RO1)
      WRITE(LOU,601)'          '
     1              ,AK1,RO1, RH1,DRH1
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(7)
     1              ,AK0,RO0,RRH2,DRH0
      IF(RO2.LT.-.99) RO2=ABS(RO2)
      WRITE(LOU,601)'          '
     1              ,AK2,RO2, RH2,DRH2
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(8)
     1              ,AK0,RO0,RRH3,DRH0
      IF(RO3.LT.-.99) RO3=ABS(RO3)
      WRITE(LOU,601)'          '
     1              ,AK3,RO3, RH3,DRH3
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(9)
     1              ,AK0,RO0,RRH4,DRH0
      IF(RO4.LT.-.99) RO4=ABS(RO4)
      WRITE(LOU,601)'          '
     1              ,AK4,RO4, RH4,DRH4
      WRITE(LOU,691)
C*
      CLOSE(UNIT=LIN)
C*
C* Begin processing the DCA Task-1 Benchmark
  400 IC=5
      OPEN(UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=500)
C* Multiplication factor for D2O and Air channel
      AK0=1
      RO0=0
  420 READ (LIN,692) REC
      IF(REC(1: 5).NE.'PITCH') GO TO 420
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:39),'(F8.0)') RO1
      AK1=1/(1-RO1/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') RO2
      AK2=1/(1-RO2/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') RO3
      AK3=1/(1-RO3/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') RO4
      AK4=1/(1-RO4/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') RO5
      AK5=1/(1-RO5/100000)
      READ (LIN,692) REC
C      READ (LIN,692)
      READ (REC(22:29),'(F8.0)') RO6
      AK6=1/(1-RO6/100000)
c
CD      PRINT *,'AKE',AK0,AK0,R00,AK1,RO1,AK2,RO2
c    1               ,AK3,RO3,AK4,RO4,AK5,RO5,AK6,RO6
c
      RO1=RO1/1000
      RO2=RO2/1000
      RO3=RO3/1000
      RO4=RO4/1000
      RO5=RO5/1000
      RO6=RO6/1000
C*
      SRKE=SRKE+6*RO0
      SDKE=SDKE+RO1*RO1+RO2*RO2+RO3*RO3+RO4*RO4+RO5*RO5+RO6*RO6
      ADKE=ADKE+RO1+RO2+RO3+RO4+RO5+RO6
      NDKE=NDKE+6
C* Print the resutls for this benchmark
      WRITE(LOU,601) NAME(10)
     1              ,AK0,RO0
      IF(RO1.LT.-.99) RO1=ABS(RO1)
      WRITE(LOU,601)'          '
     1              ,AK1,RO1
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(11)
     1              ,AK0,RO0
      IF(RO2.LT.-.99) RO2=ABS(RO2)
      WRITE(LOU,601)'          '
     1              ,AK2,RO2
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(12)
     1              ,AK0,RO0
      IF(RO3.LT.-.99) RO3=ABS(RO3)
      WRITE(LOU,601)'          '
     1              ,AK3,RO3
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(13)
     1              ,AK0,RO0
      IF(RO4.LT.-.99) RO4=ABS(RO4)
      WRITE(LOU,601)'          '
     1              ,AK4,RO4
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(14)
     1              ,AK0,RO0
      IF(RO4.LT.-.99) RO4=ABS(RO5)
      WRITE(LOU,601)'          '
     1              ,AK5,RO5
      WRITE(LOU,691)
      WRITE(LOU,601) NAME(15)
     1              ,AK0,RO0
      IF(RO4.LT.-.99) RO6=ABS(RO6)
      WRITE(LOU,601)'          '
     1              ,AK6,RO6
      WRITE(LOU,691)
C*
      CLOSE(UNIT=LIN)
C*
  500 WRITE(LOU,691) ' ======================================='
     1              ,'====================================    '
C*
      IF(NDKE.GT.0) THEN
        SRKE=     SRKE/NDKE
        SDKE=SQRT(SDKE/NDKE)
        ADKE=     ADKE/NDKE
      END IF
      IF(NDRH.GT.0) THEN
        SRRH=     SRRH/NDRH
        SDRH=SQRT(SDRH/NDRH)
        ADRH=     ADRH/NDRH
      END IF
      IF(NDCR.GT.0) THEN
        SRCR=     SRCR/NDCR
        SDCR=SQRT(SDCR/NDCR)
        ADCR=     ADCR/NDCR
      END IF
      IF(NDCU.GT.0) THEN
        SRCU=     SRCU/NDCU
        SDCU=SQRT(SDCU/NDCU)
        ADCU=     ADCU/NDCU
      END IF
      IF(NDUL.GT.0) THEN
        SRUL=     SRUL/NDUL
        SDUL=SQRT(SDUL/NDUL)
        ADUL=     ADUL/NDUL
      END IF
C*
      WRITE(LOU,602) ' Average  '
     1              ,     SRKE,     SRRH,     SRCR,     SRCU,     SRUL
      WRITE(LOU,603) ' Dif/St.Dv'
     1              ,ADKE,SDKE,ADRH,SDRH,ADCR,SDCR,ADCU,SDCU,ADUL,SDUL
C* End of file processing
   80 STOP 'D2OSMR Completed'
C*
  601 FORMAT(A10,F8.5,'(',F4.2,')'
     2          ,F7.4,'(',F4.1,')'
     3          ,F7.4,'(',F4.1,')'
     4          ,F7.3,'(',F4.2,')'
     5          ,F7.3,'(',F4.2,')' )
  602 FORMAT(A10,8X,'(',F4.2,')'
     2          ,7X,'(',F4.1,')'
     3          ,7X,'(',F4.1,')'
     4          ,7X,'(',F4.2,')'
     5          ,7X,'(',F4.2,')' )
  603 FORMAT(A10,F8.2,'(',F4.2,')'
     2          ,F7.1,'(',F4.1,')'
     3          ,F7.1,'(',F4.1,')'
     4          ,F7.2,'(',F4.2,')'
     5          ,F7.2,'(',F4.2,')' )
  691 FORMAT(2A40)
  692 FORMAT(A120)
      END
