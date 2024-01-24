      PROGRAM SMRDI2
C-Title  : SMRDI2 Program
C-Purpose: Tabulate and compare benchmark summary results
C-         for BNL-Th benchmarks
C-         =SMRDIF with modified output title
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia.
C-Version: 1998
C-V 01/05  F.Leszczynski, A.Trkov:
C-V      - Allow for more benchmarks.
C-V      - Add instructions in the comments.
C-M
C-M  Manual for Program SMRDIF
C-M  =========================
C-M  The program reads the K-eff and four spectral indices from a
C-M  reference file and from the WEDOG3 output, which summarizes the
C-M  results of a WIMS-D calculation for BNL-Th benchmarks
C-M
C-M  The file containing reference results may contain up to MXBN
C-M  records, each corresponding to one benchmark lattice. The value
C-M  of MXBN is set in the PARAMETER statement in the code.
C-M  The format of the file containing reference results is:
C-M  Column  Description:
C-M    1-10  Lattice identification string.
C-M   11-18  Reference multiplication factor.
C-M   19-23  Uncertainty in the multiplication factor [pcm].
C-M   24-29  Rho28
C-M   30-35  Uncertainty in Rho28
C-M   36-43  Del25
C-M   44-50  Uncertainty in Del25
C-M   51-58  Del28
C-M   59-65  Uncertainty in Del28
C-M   66-72  C*
C-M   73-78  Uncertainty in C*
C-M
C-M  The number of records and the sequence in the WEDOG2 output
C-M  file must correspond to the sequence in the reference file. No
C-M  checking for consistency is done in the code.
C-M
C-M  Instructions:
C-M  The filenames are entered in response to the prompts
C-M  -  Reference results filename.
C-M  -  WEDOG2 output analysing WIMS-D results.
C-M  -  Output summary file.
C-M  -  String to flag printout differences in percent. If blank
C-M     is specified, the absolute differences are printed.
C-
      PARAMETER     (MXBN=100)
      CHARACTER*40   FLNM,FLNR,FLNC
      CHARACTER*10   BLN,LBL
      CHARACTER*6    HRJ,HR(5),HC(5)
      DIMENSION      PR(5),PC(5),ER(5),EC(5,MXBN)
     1              ,ND(5),RV(5),AV(5),AE(5)
      DATA ND,RV,AV /5*0, 5*0., 5*0./
      DATA BLN/'          '/
C* Define the filenames
      WRITE(6,91)
      WRITE(6,91) ' SMRDI2 - Compare lattice spectr.indices'
      WRITE(6,91) ' ====================================   '
      WRITE(6,91)
      WRITE(6,91) '$Enter the reference filename         : '
      READ (5,91) FLNM
      FLNR=FLNM
      OPEN (UNIT=1,FILE=FLNM,STATUS='OLD')
      WRITE(6,91) '$Enter the compared  filename         : '
      READ (5,91) FLNM
      FLNC=FLNM
      OPEN (UNIT=2,FILE=FLNM,STATUS='OLD')
      WRITE(6,91) '$Enter the output filename            : '
      READ (5,91) FLNM
      OPEN (UNIT=8,FILE=FLNM,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')
      WRITE(6,91) '$Enter non-blank to print % diff.     : '
      READ (5,91) FLNM
C* Write the header to output
      WRITE(8,91)
      WRITE(8,91) ' SMRDIF - Compare lattice spectr.indices'
      WRITE(8,91) ' ====================================   '
      WRITE(8,91)
      WRITE(8,91) ' Reference file                       : ',FLNR
      WRITE(8,91) ' Compared  file                       : ',FLNC
      WRITE(8,91)
      WRITE(8,91) '   LATTICE      K-eff       Rho28       '
     1           ,'  Del25          Del28         ConvR    '
      WRITE(8,91) '  ======================================'
     1           ,'================================        '
      NE=0
C* Read a line from reference and compared data base
   20 READ (1,92,END=80) LBL,PR(1),IKR,(PR(J),ER(J),J=2,5)
      READ (2,94,END=80)     PC(1),    (PC(J)      ,J=2,5)
      NE=NE+1
C* Calculate differences in K-eff and 4 spectral indices
      ER(1   )=IKR
      EC(1,NE)=0.
      IF(PR(1).NE.0) EC(1,NE)=1.E5*ALOG(PC(1)/PR(1))
      IKC=EC(1,NE)
      ND(1) =ND(1)+1
      DO 22 J=2,5
      EC(J,NE)=0.
      IF(PR(J).EQ.0) GO TO 22
      EC(J,NE)=PC(J)-PR(J)
      IF(EC(J,NE).EQ.0) EC(J,NE)=1.E-30
      ND(J) =ND(J)+1
   22 CONTINUE
C* Case: Print absolute differences
      IF(FLNM.NE.'                                        ') GO TO 30
      WRITE(8,92) LBL,PR(1),IKR,(PR(J),ER(J   ),J=2,5)
      WRITE(8,92) BLN,PC(1),IKC,(PC(J),EC(J,NE),J=2,5)
      WRITE(8,92)
      GO TO 20
C* Case: Print relative differences
   30 ERJ=0.
C*        multiplication factor
      IF(PR(1).NE.0) ERJ=100.*IKR*1E-5/PR(1)
      RV(1)=RV(1)+ERJ*ERJ
      ERJ=ABS(ERJ)
      IF(ERJ  .GE. 9.950) WRITE(HRJ,95) IFIX(ERJ+0.5)
      IF(ERJ  .LT. 9.950) WRITE(HRJ,96) ERJ
      IF(ERJ  .LT. 0.995) WRITE(HRJ,97) ERJ
      HRJ(2:2)='~'
      HR(1)   =HRJ
      ERJ=100.*IKC*1E-5/PC(1)
      EC(1,NE)=ERJ
      AV(1)=AV(1)+ERJ
      IF     (ERJ     .GT. 9999  ) THEN
        WRITE(HRJ,95) 9999
        GO TO 31
      ELSE IF(ERJ     .GT. 9.950 ) THEN
        WRITE(HRJ,95) IFIX(ERJ+0.5)
        GO TO 31
      ELSE IF(ERJ     .GT. 0     ) THEN
        WRITE(HRJ,97) ERJ
        GO TO 31
      ELSE IF(ERJ     .EQ. 0     ) THEN
        HRJ     ='      '
        GO TO 31
      ELSE IF(ERJ     .GT. -.995 ) THEN
        WRITE(HRJ,97) ABS(ERJ)
        HRJ(2:2)='-'
        GO TO 31
      ELSE IF(ERJ     .GT. -9.95 ) THEN
        WRITE(HRJ,96) ERJ
        GO TO 31
      ELSE IF(ERJ     .GT. -999.5) THEN
        WRITE(HRJ,95) IFIX(ERJ-0.5)
        GO TO 31
      ELSE
        WRITE(HRJ,95) -999
      END IF
   31 HC(1)   =HRJ
C*        spectral indices
      DO 34 J=2,5
      ERJ=0.
      IF(PR(J).NE.0) ERJ=ABS(100.*ER(J)/PR(J))
      RV(J)=RV(J)+ERJ*ERJ
      IF(ERJ  .GE. 9.950) WRITE(HRJ,95) IFIX(ERJ+0.5)
      IF(ERJ  .LT. 9.950) WRITE(HRJ,96) ERJ
      IF(ERJ  .LT. 0.995) WRITE(HRJ,97) ERJ
      HRJ(2:2)='~'
      IF(ER(J).EQ. 0    ) HRJ     ='      '
      HR(J)=HRJ
      ERJ=0.
      IF(PR(J).NE.0) ERJ=100.*EC(J,NE)/PR(J)
      EC(J,NE)=ERJ
      AV(J)=AV(J)+ERJ
      IF     (ERJ     .GT. 9999  ) THEN
        WRITE(HRJ,95) 9999
        GO TO 33
      ELSE IF(ERJ     .GT. 9.950 ) THEN
        WRITE(HRJ,95) IFIX(ERJ+0.5)
        GO TO 33
      ELSE IF(ERJ     .GT. 0     ) THEN
        WRITE(HRJ,97) ERJ
        GO TO 33
      ELSE IF(ERJ     .EQ. 0     ) THEN
        HRJ     ='      '
        GO TO 33
      ELSE IF(ERJ     .GT. -.995 ) THEN
        WRITE(HRJ,97) ABS(ERJ)
        HRJ(2:2)='-'
        GO TO 33
      ELSE IF(ERJ     .GT. -9.95 ) THEN
        WRITE(HRJ,96) ERJ
        GO TO 33
      ELSE IF(ERJ     .GT. -999.5) THEN
        WRITE(HRJ,95) IFIX(ERJ-0.5)
        GO TO 33
      ELSE
        WRITE(HRJ,95) -999
      END IF
   33 HC(J)=HRJ
   34 CONTINUE
C* Write the relative differences for the current lattice
      WRITE(8,93) LBL,(PR(J),HR(J),J=1,5)
      WRITE(8,93) BLN,(PC(J),HC(J),J=1,5)
      WRITE(8,93)
      GO TO 20
C* Input exhausted - print average if rel.differences requested
   80 IF(FLNM.EQ.'                                        ') GO TO 90
      DO 86 J=1,5
      IF(ND(J).GT.0) RV(J)=SQRT(RV(J)/ND(J))
      IF(ND(J).GT.0) AV(J)=     AV(J)/ND(J)
      AE(J)=0.
      IF(ND(J).EQ.0) GO TO 86
      DO 82 I=1,NE
      IF(EC(J,I).NE.0) AE(J)=AE(J)+(EC(J,I)-AV(J))**2
   82 CONTINUE
      IF(ND(J).GT.0) AE(J)=SQRT(AE(J)/ND(J))
   86 CONTINUE
      WRITE(8,98) '   Average',(RV(J)      ,J=1,5),(AV(J),AE(J),J=1,5)
   90 STOP 'SMRDI2 Completed'
   91 FORMAT(2A40)
   92 FORMAT(A10,F8.5,I5,1X,2F6.3,1X,2F7.4,1X,2F7.4,1X,2F6.3)
   93 FORMAT(A10,F8.5,A6,F7.3,A6,F8.4,A6,F8.4,A6,F7.3,A6)
   94 FORMAT(5F10.0)
   95 FORMAT('(',I4  ,')')
   96 FORMAT('(',F4.1,')')
   97 FORMAT('(',F4.2,')')
   98 FORMAT(2X,78('-')/  A10,F13.2,F13.2,F14.2,F14.2,F13.2 /
     1 11X,2( F6.2,'(~',F4.2,')'),2( F7.2,'(~',F4.2,')')
     1      , F6.2,'(~',F4.2,')' )
      END
