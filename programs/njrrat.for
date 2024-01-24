      PROGRAM NJRRAT
C-Title  : NJRRAT Program
C-Purpose: Calculate Reaction Rate Ratios from NJOY Output
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (1998)
C-Version:
C-V  00/04 Implement search for isotope production.
C-V  00/08 Fix minor bugs (A.Trkov).
C-V  01/04 Increase max.No. of groups (A.Trkov)
C-M
C-M  Manual for Program NJRRAT
C-M  =========================
C-M
C-M  The GROUPR printout from the NJOY output is processed. The
C-M  flux values are read from MF 3, MT 1. The group constants for
C-M  the specified reactions are read. The total reaction rate is
C-M  calculated and the ratio printed.
C-M
C-M  Instructions
C-M  ------------
C-M  The following are read from the keyboard:
C-M  - NJOY output filename to be processed
C-M  - Numerator MT number
C-M  - Denominator MT number
C-M
C-M  To allow calculation of isotope production ratios, instead
C-M  of the MT number the reaction string as it appears on the
C-M  NJOY output may be specified
C-M
C-M  Example: "(n,g) 61148m1 production"
C-M
C-M  The calculated reaction rate ratio is printed to the terminal
C-M  screen and to the NJRRAT.LOG file.
C-
      CHARACTER*40  SRCH,FLNI,FLNO,MTX(2)
      CHARACTER*132 REC
      PARAMETER    (MXG=1000)
      DIMENSION     FLX(MXG),XSR(MXG,2),FF(10),MTN(2),SS(2)
C*
      DATA LIN,LOU,LKB,LTT/ 1, 2, 5, 6 /
      DATA FLNI/'AM1.OUT'/
     1     FLNO/'NJRRAT.LOG'/
C*
      DO 2 I=1,2
      DO 2 J=1,MXG
      XSR(J,I)=0.
      FLX(J)  =0.
    2 CONTINUE
C*
      WRITE(LTT,91) ' NJRRAT - Calculate Reaction Rate Ratios'
      WRITE(LTT,91) ' ======================================='
      WRITE(LTT,91)
   12 WRITE(LTT,91) ' Enter NJOY Outputfile to be processed: '
      READ (LKB,91) FLNI
      OPEN (UNIT=LIN,FILE=FLNI,STATUS='OLD',ERR=12)
C*
      MTN(1)=0
      WRITE(LTT,91) '          Enter Numerator   MT number : '
      READ (LKB,91) MTX(1)
      READ(MTX(1),93,ERR=14) MTN(1)
      WRITE(MTX(1),'('' for mf  3 and mt'',I3)') MTN(1)
   14 MTN(2)=0
      WRITE(LTT,91) '          Enter Denominator MT number : '
      READ (LKB,91) MTX(2)
      READ(MTX(2),93,ERR=15) MTN(2)
      WRITE(MTX(2),'('' for mf  3 and mt'',I3)') MTN(2)
C*
C* Define the first cross section
   15 IS1=1
      IS2=2
      IF(MTN(1).GE.1. AND. MTN(2).GE.1) THEN
        IF(MTN(1).GT.MTN(2)) THEN
          SRCH  =MTX(1)
          MTX(1)=MTX(2)
          MTX(2)=SRCH
          MT    =MTN(1)
          MTN(1)=MTN(2)
          MTN(2)=MT
          IS1=2
          IS2=1
        END IF
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,91) ' NJRRAT - Calculate Reaction Rate Ratios'
      WRITE(LOU,91) ' ======================================='
      WRITE(LOU,91)
      WRITE(LOU,91) ' NJOY Output file processed           : ',FLNI
      WRITE(LOU,91)
C* Clear the array
      DO 18 I=1,MXG
      XSR(I,1)=0.
      XSR(I,2)=0.
   18 CONTINUE
C*
C* Find the GROUPR output
   20 READ (LIN,92,END=60) REC
      IF(REC(1:10).NE.' groupr...') go to 20
C* Read the flux values from the total cross section
   22 READ (LIN,92,END=60) REC
      IF(REC(1:20).NE.' for mf  3 and mt  1') go to 22
      READ (LIN,92,END=82)
      READ (LIN,92,END=82)
      READ (LIN,92,END=82) REC
      READ (LIN,92,END=82)
      REC(14:24)='  999999999'
C* Determine the number of Sigma-zero values
      READ(REC,95) (FF(J),J=1,10)
      NS=11
   26 NS=NS-1
      IF(FF(NS).LE.0) GO TO 26
      IF(NS.GT.1) THEN
        WRITE(LTT,97) ' Number of Sigma-0 values is          : ',NS
        DO 28 J=1,NS
          WRITE(LTT,99) J,NINT(FF(J))
   28   CONTINUE
        WRITE(LTT,91) ' By default the last value is chosen    '
        WRITE(LTT,91) '             Choose index to redefine : '
        READ (LKB,93) IDM
        IF(IDM.GT.0 .AND.IDM.LT.NS) NS=IDM
      END IF
 
      NG=0
   29 READ (LIN,92,END=60) REC
      IF(REC(1:10).EQ.'          ') GO TO 30
      READ (REC,94,ERR=82) JG,LG
      IF(JG.GT.MXG) STOP 'NJRRAT ERROR - MXG Limit exceeded'
      READ (LIN,92,END=60) REC
      READ (REC,95,ERR=82) FF
C* Take the value of the last (or selected) Sigma-zero
      IF(LG.EQ.0) THEN
        IF(JG.NE.NG+1) THEN
C...      STOP 'NJRRAT ERROR - Incorrect group sequence'
          WRITE(LTT,97) ' NJRRAT WARNING - Bad sequence at group ',JG
        END IF
        FLX(JG)=FF(NS)
        NG=JG
      END IF
      GO TO 29
C*
   30 IX=0
   32 IX=IX+1
      JS=1
      IF(MTN(IX).EQ.2 .OR. MTN(IX).EQ.18 .OR. MTN(IX).EQ.102) JS=NS
C* Read the self-shielded cross sections
   40 READ (LIN,92,END=60) REC
      IF(MTN(IX).GE.1) THEN
        IF(REC( 1:20).NE.MTX(IX)( 1:20)) GO TO 40
      ELSE
        IF(REC(22:35).NE.MTX(IX)( 1:14)) GO TO 40
      END IF
      READ (LIN,92,END=82)
      READ (LIN,92,END=82)
      READ (LIN,92,END=82)
      READ (LIN,92,END=82)
 
   42 READ (LIN,92,END=82) REC
      IF(REC(1:20).EQ.'                    ') GO TO 44
      READ (REC,96,ERR=82) JG,(FF(J),J=1,JS)
      XSR(JG,IX)=FF(JS)
      IF(JG.LT.NG) GO TO 42
   44 IF(IX.LT. 2) GO TO 32
C*
   60 IF(NG.LT.1) GO TO 86
      S1=0.
      S2=0.
      DO 62 IG=1,NG
      SS(1)=SS(1)+FLX(IG)*XSR(IG,1)
      SS(2)=SS(2)+FLX(IG)*XSR(IG,2)
   62 CONTINUE
      IF(SS(1).LE.0 .OR. SS(2).LE.0) GO TO 84
      RAT=SS(IS1)/SS(IS2)
      WRITE(LTT,98) MTX(IS1),MTX(IS2),RAT
      WRITE(LOU,98) MTX(IS1),MTX(IS2),RAT
      STOP 'NJRRAT Completed'
c*
   82 STOP 'NJRRAT ERROR - Reading NJOY output file'
   84 STOP 'NJRRAT ERROR - Zero reaction rate'
   86 STOP 'NJRRAT ERROR - No data found'
C*
   91 FORMAT(2A40)
   92 FORMAT(A132)
   93 FORMAT(BN,I10)
   94 FORMAT(I4,I7)
   95 FORMAT(13X,10F11.0)
   96 FORMAT(I4,2X,10F11.0)
   97 FORMAT(A40,I6)
   98 FORMAT(1X,A20,' / ',A20,' Ratio =',G14.4)
   99 FORMAT(I6,I12)
      END
