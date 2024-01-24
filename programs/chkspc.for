      PROGRAM CHKSPC
C-Title  : CHKSPC Program
C-Purpose: Check weighting spectra in NJOY inputs
      PARAMETER    (MXPT=200,MXSP=5)
      CHARACTER*80  REC
      CHARACTER*40  FLNJ,FLPT,FLCH
      CHARACTER*6   LBL
      DIMENSION     NP1(MXSP),ENR1(MXSP,MXPT),WSP1(MXSP,MXPT)
     1             ,ENR2(MXPT),WSP2(MXPT)
C*
      DOUBLE PRECISION EG9(70),EG18(173)
C*
      DATA EG9/1.d-5,.005d0,.01d0,.015d0,.02d0,.025d0,.03d0,.035d0,
     1 .042d0,.05d0,.058d0,.067d0,.08d0,.1d0,.14d0,.18d0,.22d0,.25d0,
     2 .28d0,.3d0,.32d0,.35d0,.4d0,.5d0,.625d0,.78d0,.85d0,.91d0,.95d0,
     3 .972d0,.996d0,1.02d0,1.045d0,1.071d0,1.097d0,1.123d0,1.15d0,
     4 1.3d0,1.5d0,2.1d0,2.6d0,3.3d0,4.d0,9.877d0,15.968d0,27.7d0,
     5 48.052d0,75.501d0,148.728d0,367.262d0,906.898d0,1425.1d0,
     6 2239.45d0,3519.1d0,5530.d0,9118.d0,1.503d4,2.478d4,4.085d4,
     7 6.734d4,1.11d5,1.83d5,3.025d5,5.d5,8.21d5,1.353d6,2.231d6,
     8 3.679d6,6.0655d6,1.d7/
      DATA EG18/
     &1.96403d+7,1.73325d+7,1.49182d+7,1.38403d+7,1.16183d+7,1.00000d+7,
     &8.18731d+6,6.70320d+6,6.06531d+6,5.48812d+6,4.49329d+6,3.67879d+6,
     &3.01194d+6,2.46597d+6,2.23130d+6,2.01897d+6,1.65299d+6,1.35335d+6,
     &1.22456d+6,1.10803d+6,1.00259d+6,9.07180d+5,8.20850d+5,6.08101d+5,
     &5.50232d+5,4.97871d+5,4.50492d+5,4.07622d+5,3.01974d+5,2.73237d+5,
     &2.47235d+5,1.83156d+5,1.22773d+5,1.11090d+5,8.22975d+4,6.73795d+4,
     &5.51656d+4,4.08677d+4,3.69786d+4,2.92830d+4,2.73944d+4,2.47875d+4,
     &1.66156d+4,1.50344d+4,1.11378d+4,9.11882d+3,7.46586d+3,5.53084d+3,
     &5.00451d+3,3.52662d+3,3.35463d+3,2.24867d+3,2.03468d+3,1.50733d+3,
     &1.43382d+3,1.23410d+3,1.01039d+3,9.14242d+2,7.48518d+2,6.77287d+2,
     &4.53999d+2,3.71703d+2,3.04325d+2,2.03995d+2,1.48625d+2,1.36742d+2,
     &9.16609d+1,7.56736d+1,6.79041d+1,5.55951d+1,5.15780d+1,4.82516d+1,
     &4.55174d+1,4.01690d+1,3.72665d+1,3.37201d+1,3.05113d+1,2.76077d+1,
     &2.49805d+1,2.26033d+1,1.94548d+1,1.59283d+1,1.37096d+1,1.12245d+1,
     &9.90555d+0,9.18981d+0,8.31529d+0,7.52398d+0,6.16012d+0,5.34643d+0,
     &5.04348d+0,4.12925d+0,4.00000d+0,3.38075d+0,3.30000d+0,2.76792d+0,
     &2.72000d+0,2.60000d+0,2.55000d+0,2.36000d+0,2.13000d+0,2.10000d+0,
     &2.02000d+0,1.93000d+0,1.84000d+0,1.75500d+0,1.67000d+0,1.59000d+0,
     &1.50000d+0,1.47500d+0,1.44498d+0,1.37000d+0,1.33750d+0,1.30000d+0,
     &1.23500d+0,1.17000d+0,1.15000d+0,1.12535d+0,1.11000d+0,1.09700d+0,
     &1.07100d+0,1.04500d+0,1.03500d+0,1.02000d+0,9.96000d-1,9.86000d-1,
     &9.72000d-1,9.50000d-1,9.30000d-1,9.10000d-1,8.60000d-1,8.50000d-1,
     &7.90000d-1,7.80000d-1,7.05000d-1,6.25000d-1,5.40000d-1,5.00000d-1,
     &4.85000d-1,4.33000d-1,4.00000d-1,3.91000d-1,3.50000d-1,3.20000d-1,
     &3.14500d-1,3.00000d-1,2.80000d-1,2.48000d-1,2.20000d-1,1.89000d-1,
     &1.80000d-1,1.60000d-1,1.40000d-1,1.34000d-1,1.15000d-1,1.00001d-1,
     &9.50000d-2,8.00000d-2,7.70000d-2,6.70000d-2,5.80000d-2,5.00000d-2,
     &4.20000d-2,3.50000d-2,3.00000d-2,2.50000d-2,2.00000d-2,1.50000d-2,
     &1.00000d-2,6.90000d-3,5.00000d-3,3.00000d-3,1.00001d-5/
C*
      DATA LNJ,LPT,LCH,LKB,LTT / 1, 2, 3, 5, 6 /
      DATA FLNJ/'nji.src'/
     2     FLPT/'chkspc.cur'/
     3     FLCH/'chkspc.log'/
C*
      IP1=0
      ISP=0
C*
      WRITE(LTT,902)
      WRITE(LTT,902) ' CHKSPC - Check spectra in NJOY input   '
      WRITE(LTT,902) ' ------------------------------------   '
      WRITE(LTT,902)
      WRITE(LTT,902) '$Enter the source NJI filename        : '
      READ (LKB,902) FLNJ
      WRITE(LTT,902) '$Select spectrum (0=flux, 1=current)  : '
      READ (LKB,909) ISP
C*
      OPEN (UNIT=LNJ,FILE=FLNJ,STATUS='OLD')
      OPEN (UNIT=LPT,FILE=FLPT,STATUS='UNKNOWN')
      OPEN (UNIT=LCH,FILE=FLCH,STATUS='UNKNOWN')
C* Find next input
   20 READ (LNJ,901,END=80) REC
      IF(REC(1:6).EQ.'*deck ') THEN
        WRITE(LCH, * ) REC(1:12)
        GO TO 20
      END IF
      IF(REC(1:3).NE.'-- ') GO TO 20
      IF(REC(4:4).EQ.' ')   GO TO 20
      LBL=REC(4:9)
C* Find input weighting spectrum
   22 READ (LNJ,901) REC
      IF(REC(1:6).EQ.'*deck ') THEN
        WRITE(LCH, * ) REC(1:12)
        GO TO 20
      END IF
      IF(ISP.EQ.0 .AND. REC(1:6).EQ.'groupr') GO TO 40
      IF(ISP.EQ.1 .AND. REC(1:5).EQ.'wimsr' ) GO TO 50
      GO TO 22
C* Read the flux weighting spectrum in groupr
  40  READ (LNJ,901)
      READ (LNJ, * ) MAT,IGR,IGG,IWG
      IF(ABS(IWG).NE.1) GO TO 20
      READ (LNJ,901) REC
      READ (LNJ,901) REC
      READ (LNJ,901) REC
      IF(IWG.LT.0) READ (LNJ,901) REC
      READ (LNJ,904) C1,C2,L1,L2,NR,NP2
      READ (LNJ,905) NB,IT
      READ (LNJ,906) (ENR2(J),WSP2(J),J=1,NP2)
      GO TO 60
C* Read the current weighting spectrum in wimsr
  50  READ (LNJ,901)
      READ (LNJ,901)
      READ (LNJ, * ) NG,NG1,NG2
      READ (LNJ, * ) MAT,I1,AMAT,IB
      READ (LNJ, * ) NT,NS,S0,I1,R1,MT,I2,I3,I4,I5,I6,JP1
      IF(JP1.LE.1) GO TO 20
      IF(IB.GT.0) THEN
        READ (LNJ, * ) NB
        DO 51 J=1,NB
        READ (LNJ,901)
   51   CONTINUE
      END IF
      NP2=NG1+NG2
      READ (LNJ, * ,ERR=52) (WSP2(J),J=1,NG2)
   52 READ (LNJ, * ) (WSP2(J),J=1,NP2)
C* Convert total current to current per energy
      IF(NG.EQ.69) THEN
        DO 54 J=1,NP2
C*        Energy boundaries in ascending order
          JG=NG+1-J
          ENR2(J)=    0.5*(EG9(JG+1)+EG9(JG))
          WSP2(J)=WSP2(J)/(EG9(JG+1)-EG9(JG))
   54   CONTINUE
      ELSE IF(NG.EQ.172) THEN
        DO 56 J=1,NP2
C*        Energy boundaries in descending order
          JG=J
          ENR2(J)=    0.5*(EG18(JG)+EG18(JG+1))
          WSP2(J)=WSP2(J)/(EG18(JG)-EG18(JG+1))
   56   CONTINUE
      ELSE
        STOP 'CHKSPC - not yet implemented'
      END IF
C*
C* Check the spectrum against previous entries
   60 IF(IP1.LE.0)   GO TO 70
      DO 68 K=1,IP1
      IF(NP2.NE.NP1(K)) GO TO 68
      IDIF=0
      DO 64 I=1,NP2
      IF(ABS(ENR1(K,I)/ENR2(I)-1).GT.1.E-3) IDIF=1
      IF(ABS(WSP1(K,I)/WSP2(I)-1).GT.1.E-3) IDIF=1
   64 CONTINUE
C* If spectrum matched, process next set
      IF(IDIF.EQ.0) THEN
        WRITE(LCH, * ) LBL,K,' Matched'
        GO TO 20
      END IF
   68 CONTINUE
C* Write the output spectrum
   70 IP1=IP1+1
      IF(IP1.GT.MXSP) STOP 'CHKSPC ERROR - MXSP Limit exceeded'
      WRITE(LCH, * ) LBL,IP1,' New'
      WRITE(LTT, * ) LBL,IP1
      WRITE(LPT,921) LBL
      DO 72 I=1,NP2
      WRITE(LPT,922) ENR2(I),WSP2(I)*ENR2(I)
   72 CONTINUE
      WRITE(LPT,921)
      NP1(IP1)=NP2
      DO 74 I=1,NP2
      ENR1(IP1,I)=ENR2(I)
      WSP1(IP1,I)=WSP2(I)
   74 CONTINUE
      GO TO 20
C*
   80 STOP 'CHKSPC Completed'
C*
  901 FORMAT(A80)
  902 FORMAT(2A40)
  904 FORMAT(2F11.0,4I11)
  905 FORMAT(6I11)
  906 FORMAT(6F11.0)
  909 FORMAT(BN,I10)
  921 FORMAT(A6)
  922 FORMAT(1P,2E11.3)
      END
 
