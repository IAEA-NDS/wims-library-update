      PROGRAM WEDKIN
C-Title  : WEDKIN Program
C-Purpose: Extract k-inf from WIMS output as a function of burnup
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia
C-Version: 2003 Original code
C-M
C-M  Manual for Program WEDKIN
C-M  =========================
C-M  A WIMS output with burnu is processed. The k-inf values as a
C-M  function of burnup are tabulated in PLOTTAB curves format.
C-M    The k-eff values are read from leakage edit of Chain 14
C-M  and the burnup is estimated from the burnup step Chain2 output
C-M  that follows.
C-M
C-M  Files used:
C-M    3  WIMS output file to be processed (requested from input),
C-M    4  Tabulated k-eff output file in plottab format,
C-M    5  Keyboard input,
C-M    6  Diagnostics output.
C-
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLWI,FLCU
      DATA BLNK/'                                        '/
     3     FLWI/'WIMSD.OUT'/
     4     FLCU/'WEDKIN.CUR'/
      DATA LWI,LCU,LKB,LTT/ 3, 4, 5, 6 /
C*
      WRITE(LTT,940)
      WRITE(LTT,940) ' WEDKIN - Tabulate k-eff from WIMS    '
      WRITE(LTT,940) ' =================================    '
      WRITE(LTT,940)
C*
C* Select the output option
      WRITE(LTT,940) ' Select the output option:            '
      WRITE(LTT,940) '   0  Unnormalised k-inf '
      WRITE(LTT,940) '   1  Unnormalised k-eff '
      WRITE(LTT,940) '  10  k-inf normalised to first point '
      WRITE(LTT,940) '$ 11  k-eff normalised to first point '
      READ (LKB,940) FLNM
      IF(FLNM.NE.BLNK) READ(FLNM,*) IOP
      IOK=IOP-10*(IOP/10)
C* Specify the output curve filename
C...  WRITE(LTT,940) '$Enter output curve filename        : '
C...  READ (LKB,940) FLNM
C...  IF(FLNM.NE.BLNK) FLCU=FLNM
      OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
C* Specify the WIMS library name
      GO TO 12
   11 WRITE(LTT,940) ' ERROR - non existent file requested  ',FLWI
   12 WRITE(LTT,940) '$Enter WIMS file to be processed    : '
      READ (LKB,940,END=90) FLNM
      IF(FLNM.EQ.BLNK) GO TO 90
      FLWI=FLNM
      OPEN (UNIT=LWI,FILE=FLWI,STATUS='OLD',ERR=11)
C*
      IKI=0
      NBU=0
C*
C* Begin processing the WIMS output
C* Find k-inf in the leakage edit
C"172 GROUPS.....  k-infinity  1.309005E+00   k-effective  1.004001E+00"
   20 DO WHILE (REC(13:23).NE.'GROUPS.....')
        READ (LWI,980,END=80) REC
      END DO
C* Choose k-inf or k-eff
      IF(IOK.EQ.0) THEN
        READ (REC(36:49), * ) AKE
      ELSE
        READ (REC(64:77), * ) AKE
      END IF
      READ (LWI,980) REC
      IKI=1
C* Find burnup in the Chain 15 output
      DO WHILE(REC(1:4).NE.'0t= ')
        READ (LWI,980,END=80) REC
      END DO
C* Check that k-eff was read
      IF(IKI.NE.1) GO TO 20
C"0t=     0.000 days, Irrad=     0.000 MWd/te"
      READ (REC(27:36), * ) BRN
      NBU=NBU+1
      IF(NBU.EQ.1) THEN
        WRITE(LCU,940) FLWI
        AK0=AKE
      END IF
      IF(IOP.GE.10)  AKE=AKE/AK0
      WRITE(LCU,911) BRN,AKE
      IKI=0
      GO TO 20
C*
   80 IF(NBU.GT.0) THEN
        WRITE(LCU,940)
      ELSE
        WRITE(LTT,940) ' WARNING - No burnup steps found on     ',FLWI
      END IF
      CLOSE(UNIT=LWI)
      GO TO 12
C*
   90 CLOSE(UNIT=LCU)
      STOP 'WEDKIN - Completed'
C*
  911 FORMAT(F11.1,F11.5)
  940 FORMAT(2A40)
  980 FORMAT(A120)
      END
