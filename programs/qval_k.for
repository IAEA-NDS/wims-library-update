      PROGRAM QVAL_K
C-Title  : Program QVAL_K
C-Purpose: Tabulate distribution of deviations in Keff from QVALUE output
C-Version: August 2002
C-Author : A.Trkov, International Atomic Energy Agency, Vienna, Austria
C-M
C-M  Manual for Program QVAL_K
C-M  =========================
C-M
C-M  The program complements the QVALUE program for analysing WLUP
C-M  benchmark results. It reads summary results files, grouping
C-M  calculated and measured K-eff values as a function of the
C-M  slowing down density, sorting them in ascending order of "q",
C-M  fitting a second order polynomial and tabulating them in
C-M  PLOTTAB format. The "points" file contains point data for
C-M  each library in the summary file (without error bars), followed
C-M  by the measured values (usually 1) with error bars. The "curves"
C-M  file contains the fitted curve for each library. The set
C-M  is repeated for each ebnchmark set.
C-M    The list of benchmark files is read from QVALUE.LST, if it
C-M  exists, otherwise an internally defined default is assumed.
C-
      PARAMETER   (MXPT=200,MXLB=10,MXFL=16,MXRW=2000)
      CHARACTER*10 LABL(10)
      CHARACTER*40 BLNK,FLNM,FLCU,FLPN,FLQL,FLQV(MXFL)
      CHARACTER*120 REC
C*
      DIMENSION    AKEF(MXPT,MXLB),QQ(MXPT),LID(MXPT)
     &            ,WGT(MXPT),PAR(10),RWO(MXRW)
C*
      DATA BLNK/'                                        '/
     1     FLQL/'QVALUE.LST'/
     1     FLQV/'CRIUME1.DAT'
     1         ,'CRIUME2.DAT'
     1         ,'CRIUOX1.DAT'
     1         ,'CRIUOX2.DAT'
     1         ,'CRIUOX3.DAT'
     1         ,'WWER1.DAT'
     1         ,'WWER2.DAT'
     1         ,'WWER3.DAT'
     1         ,'CRIMOX1.DAT'
     1         ,'CRIMOX2.DAT'
     1         ,'CRITISP.DAT'
     1         ,'BNLTH2O.DAT'
     1         ,'BNLTD2O.DAT'
     1         ,'D2OUO2.DAT'
     1         ,'D2OTHU5.DAT'
     1         ,'IRPhE.DAT'/
     2     FLCU/'QVAL_K.CUR'/
     3     FLPN/'QVAL_K.PNT'/
      DATA LQV,LCU,LPN,LQL,LKB,LTT/ 1, 2, 3, 4, 5, 6 /
C*
      WRITE(LTT,940) ' QVAL_K - Tabulate QVALUE output        '
      WRITE(LTT,940) ' ===============================        '
      WRITE(LTT,940)
C*
      NFL=16
      JFL=0
C* Try reading the list of benchmark sets from QVALUE.LST
      OPEN (UNIT=LQL,FILE=FLQL,STATUS='OLD',ERR=14)
   12 READ (LQL,940,END=14) FLNM
      IF(FLNM.EQ.BLNK) GO TO 12
      JFL=JFL+1
      IF(JFL.GT.MXFL) STOP 'QVAL_K ERROR - MXFL limit exceeded'
      FLQV(JFL)=FLNM
      GO TO 12
   14 IF(JFL.GT.0) NFL=JFL
C*
C* Ouptut curves and points files
      OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
      OPEN (UNIT=LPN,FILE=FLPN,STATUS='UNKNOWN')
C*
C* Loop over all QVALUE output files
      DO KFL=1,NFL
C*
C* Open the QVALUE output file for processing
        FLNM=FLQV(KFL)
        WRITE(LTT,940) ' Begin processing the file            : ',FLNM
        OPEN (UNIT=LQV,FILE=FLNM,STATUS='OLD')
C*
C* Interpret the library names from the header
        READ (LQV,980,END=60) REC
        NL =0
        I2 =6
   20   I1=I2+1
        I2=I2+10
        IF(REC(I1:I2).NE.'          ') THEN
          NL=NL+1
          IF(NL.GT.MXLB) STOP 'K ERROR - MXLB limit exceeded'
          LABL(NL)=REC(I1:I2)
          GO TO 20
        END IF
        WRITE(LTT,942) '                  Number of libraries : ',NL-2
C* Read the data
        NP =0
   30   READ (LQV,901,END=60) QQ(NP+1),(AKEF(NP+1,J),J=1,NL)
        NP=NP+1
C* Weights are inversely proportional to the relative uncertainty
          IF(AKEF(NP,NL).GT.0) THEN
            WGT(NP)=AKEF(NP,NL-1)/AKEF(NP,NL)
          ELSE
            WGT(NP)=1
          END IF
        IF(NP.GE.MXPT) STOP 'K ERROR - MXPT limit exceeded'
        GO TO 30
C*
   60   CONTINUE
        WRITE(LTT,942) '                  Number of points    : ',NP
C* Generate index to sort the data
        CALL SRTINM(NP,LID,QQ)
C*
C* Write the calculated values to the "points" file (no error bars)
        DO I=3,NL
C...      WRITE(LCU,910) LABL(I-2)
          WRITE(LPN,910) LABL(I-2)
          DO J=1,NP
            L=LID(J)
C...        WRITE(LCU,911) QQ(L),AKEF(L,I-2)
            WRITE(LPN,921) QQ(L),AKEF(L,I-2)
          END DO
C...      WRITE(LCU,940)
          WRITE(LPN,940)
C* Fit parabola through the calculated points
          L1=LID(1)
          L2=LID(NP)
          QL=QQ(L1)
          QH=QQ(L2)
          NC=2
          IF(NP.LE.8) NC=1
          CALL LSQPLN(NP,QQ,AKEF(1,I-2),WGT,NC,PAR,RWO,MXRW,IER)
          NN=21
          WRITE(LCU,910) LABL(I-2)
          DO J=1,NN
            XX=QL+(J-1)*(QH-QL)/(NN-1)
            YY=POLYNX(XX,PAR,NC)
            WRITE(LCU,911) XX,YY
          END DO
          WRITE(LCU,940)
        END DO
C* Write the measured values to the "points" file (with error bars)
        WRITE(LPN,910) FLNM(1:10)
        DO J=1,NP
          L=LID(J)
c...      IF(L.LT.1 .OR. L.GT.NP) PRINT *,'EROR J,L',J,L
          WRITE(LPN,921) QQ(L),AKEF(L,NL-1),AKEF(L,NL),AKEF(L,NL)
        END DO
        WRITE(LPN,940)
C* Tabulation for this reactor group completed
        CLOSE(UNIT=LQV)
      END DO
C*
      STOP 'QVAL_K COMPLETED'
C*
  901 FORMAT(F6.0,10F10.0)
  910 FORMAT(A10)
  911 FORMAT(2F11.8)
  921 FORMAT(1P,E11.4,22X,3E11.4)
  940 FORMAT(2A40)
  942 FORMAT( A40,I6)
  980 FORMAT(A120)
      END
      SUBROUTINE SRTINM(N,M,X)
C-Title  : SRTINM subroutine
C-Purpose: Perform a sort (on array indices) by Insertion method
C-Description:
C-D Sort the vector of N real numbers in X in ascending order
C-D The actual entries in X remain unaffected, but on exit,
C-D M (integer array) contains the addresses of the consecutive
C-D array entries to produce a sorted sequence,
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1990)
      DIMENSION M(1),X(1)
      M(1)=1
      DO 20 J=2,N
      M(J)=J
      K = J
      T = X(J)
   10 L = M(K-1)
      IF(X(L).LE.T) GO TO 12
      M(K)=L
      K   = K-1
      IF(K.GT.1) GO TO 10
   12 M(K)=J
   20 CONTINUE
      RETURN
      END
      SUBROUTINE LSQPLN(NP, XI, YI, WI, NC, QQ, RWO, LRW, IER)
C-Title  : LSQPLN Subroutine
C-Purpose: Calculate LSQ fit polynomial coefficients for a data set
      DIMENSION XI(1),YI(1),WI(1),QQ(1),RWO(1)
      IER=0
      NQ =NC+1
      NQ2=NQ*2
      LA =1
      LF =LA+NQ*NQ
      LC =LF+NQ
      LB =LC+NQ2
      IF(LB.GT.LRW) GO TO 90
      DO 14 J=LF,LB
      RWO(J)=0.
   14 CONTINUE
C* Generate solution matrix ans RHS vector coefficients
      DO 26 J=1,NP
      CC=WI(J)
      CF=CC*YI(J)
      DO 22 I=1,NQ2
      RWO(LC-1+I)=RWO(LC-1+I)+CC
      CC=CC*XI(J)
   22 CONTINUE
      DO 24 I=1,NQ
      RWO(LF-1+I)=RWO(LF-1+I)+CF
      CF=CF*XI(J)
   24 CONTINUE
   26 CONTINUE
C* Fill solution matrix elements with precalculated coefficients
      DO 40 I=1,NQ
      DO 40 J=I,NQ
      CC =RWO(LC-2+I+J)
      RWO(LA-1+(I-1)*NQ+J)=CC
      RWO(LA-1+(J-1)*NQ+I)=CC
   40 CONTINUE
C* Solve by gaussiona elimination with partial pivoting
      CALL MTXGUP(RWO(LA),RWO(LF),QQ,NQ,LDIG,DET)
      IF(DET.NE.0) RETURN
C* Error condition - determinant zero
      IER=1
      RETURN
C* Error condition - insufficient work array length
   90 IER=2
      RETURN
      END
      FUNCTION POLYNX(X,C,NC)
C-Title  : POLINX function
C-Purpose: Polynomial Pn(x) of order NC with NC+1 coefficients C(i)
      DIMENSION C(1)
      NC1=NC+1
      F  =C(NC1)
      IF(NC.LT.1) GO TO 20
      DO 10 I=1,NC
      F  =F*X + C(NC1-I)
   10 CONTINUE
   20 POLYNX = F
      RETURN
      END
      SUBROUTINE MTXGUP(A,F,X,N,LDIG,DET)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of
C-D linear simultaneous equations  A x = F  (order n) using Gauss
C-D elimination technique with partial pivoting by rows.
C-D   Matrix elements are sequentially stored by rows (i.e. the
C-D first index refers to the column number).
C-Author : A.Trkov , Institute J.Stefan, Ljubljana, Slovenia
C-Version: 1984 Original coding
C-V 93/03 - improved zero-determinant trapping
C-V 00/11 - further refinement of zero-determinant trapping (A.Trkov)
C-
      DIMENSION A(N,N),F(N),X(N)
      DET=1.
      ER =1.
      DO 40 I=2,N
      I1=I-1
C* Find the pivot
      A1=0.
      DO 10 K=I1,N
      IF(ABS(A(K,I1)).LT.A1) GO TO 10
      A1=ABS(A(K,I1))
      K1=K
   10 CONTINUE
      IF(I1.LT.2) GO TO 12
      IF(ABS(A1/A0) .GT.1.E-5) GO TO 12
      DET=0.
      RETURN
   12 A0 =A1
      DET=DET*A1
      IF(K1.LT.I) GO TO 20
      A1=A(K1,I1)
      A(K1,I1)=A(I1,I1)
      A(I1,I1)=A1
      A1=F(K1)
      F(K1)=F(I1)
      F(I1)=A1
   20 DO 30 J=I,N
      X(J)=A(J,I1)/A(I1,I1)
      A(J,I1)=0.
   30 F(J)=F(J)-F(I1)*X(J)
      DO 40 J=I,N
      IF(K1.LT.I) GO TO 35
      A1=A(K1,J)
      A(K1,J)=A(I1,J)
      A(I1,J)=A1
   35 DO 40 K=I,N
      A1=A(K,J)
      A2=A1-A(I1,J)*X(K)
      IF(ABS(A1).GT.0.) ER=MIN(ER,ABS(A2/A1))
      A(K,J)=A2
   40 CONTINUE
C* Estimate number of digits lost due to subtraction
      LDIG=-LOG10(ER+1.E-33)+1.
C* Solve by backward substitution
   45 DO 60 I=2,N
      I1=N-I+2
      X(I1)=F(I1)/A(I1,I1)
      J1=N+1-I
      DO 60 J=1,J1
      F(J)=F(J)-X(I1)*A(J,I1)
   60 CONTINUE
      X(1)=F(1)/A(1,1)
      RETURN
      END
