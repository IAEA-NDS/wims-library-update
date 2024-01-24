      PROGRAM RPINKE
C-Title  : Program RPINKE
C-Purpose: Average contributions to Rowlands pin-cell benchmark
C-Version: Creation date 9-Oct-97
C-M
C-M  Manual for Program RPINKE
C-M  =========================
C-M  Results from various laboratories contributing to the
C-M  establisment of the reference solution are averaged in a
C-M  consistent way. The results consist of four pairs of K-inf
C-M  and K-eff values for a simple pin-cell configuration. The
C-M  four cases differ in the temperature of the fuel, can and
C-M  moderator. An entry set of K-inf and K-eff values from a
C-M  particular code may have a systematic error, but such errors
C-M  in the relative changes of K due to leakage or changes in
C-M  temperature are believed to be smaller. These changes in K
C-M  were therefore considered independent entries. A system of
C-M  equations was derived to obtain a consistent solution for
C-M  K-inf and K-eff, and therefore also for the changes in K.
C-M
C-M  Instructions:
C-M  The filename of the data file containing the K-inf and/or
C-M  K-eff values and their respective weights is requested from
C-M  input.
C-M    The results are written to "RPINKE.LOG". No additional
C-M  input is expected.
C-Author: A.Trkov, International Atomic Energy Agency, (1997)
C-
      PARAMETER   (MC=20, MP=21)
C*
      IMPLICIT     REAL*8 (A-H,O-Z)
      CHARACTER*80 REC
      CHARACTER*40 BLNK,FLNM,FLNI,FLNO
      CHARACTER*16 BL16,CODE(MC)
      CHARACTER*8  TYPE(21)
C*
      DIMENSION  AA(8,8), FF(8), QK(MP)
      DIMENSION  AK(MC,MP),WT(MC,MP), SUM(MP), WGT(MP), SIG(MP)
C*
      DATA TYPE/'     Ki1','     Ki2','     Ki3','     Ki4'
     1         ,'     Ke5','     Ke6','     Ke7','     Ke8'
     2         ,' Ki1-Ki2',' Ki2-Ki4',' Ki4-Ki3',' Ki2-Ki3'
     2         ,' Ki1-Ki3',' Ki1-Ke5',' Ki2-Ke6',' Ki3-Ke7'
     3         ,' Ki4-Ki8',' Ke5-Ke6',' Ke6-Ke8',' Ke8-Ke7'
     3         ,' Ke6-Ke7'/
      DATA SUM /MP*0.D0/
     1     WGT /MP*0.D0/
     1     SIG /MP*0.D0/
C*
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1     FLNI/'RPINU.DAT'/
     2     FLNO/'RPINKE.LOG'/
C* Define the K-filename
      WRITE(LTT,801) ' RPINKE - Rowlands Pin-cell Reference   '
      WRITE(LTT,801) ' ====================================   '
      WRITE(LTT,801)
   12 WRITE(LTT,801) ' Default file with data to be averaged: ',FLNI
      WRITE(LTT,801) '$          Enter new name to redefine : '
      READ (LKB,801) FLNM
      IF(FLNM.NE.BLNK) FLNI=FLNM
C* Open the data file containing contributing results to the
C* reference solution of the Rowlands pin-cell benchmark
      OPEN (UNIT=LIN,FILE=FLNI,STATUS='OLD',ERR=12)
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
C* Read the data
      NC= 1
   20 READ (LIN,800,END=30) REC
      IF(REC(1:8).EQ.'        ') GO TO 20
      READ (REC,802) CODE(NC),(AK(NC,J),J=1,8)
      READ (LIN,803)          (WT(NC,J),J=1,MP)
      AK(NC, 9)=AK(NC,1)-AK(NC,2)
      AK(NC,10)=AK(NC,2)-AK(NC,4)
      AK(NC,11)=AK(NC,4)-AK(NC,3)
      AK(NC,12)=AK(NC,2)-AK(NC,3)
      AK(NC,13)=AK(NC,1)-AK(NC,3)
      AK(NC,14)=AK(NC,1)-AK(NC,5)
      AK(NC,15)=AK(NC,2)-AK(NC,6)
      AK(NC,16)=AK(NC,3)-AK(NC,7)
      AK(NC,17)=AK(NC,4)-AK(NC,8)
      AK(NC,18)=AK(NC,5)-AK(NC,6)
      AK(NC,19)=AK(NC,6)-AK(NC,8)
      AK(NC,20)=AK(NC,8)-AK(NC,7)
      AK(NC,21)=AK(NC,6)-AK(NC,7)
      NC= NC+1
      IF(NC.GE.MC) STOP 'RPIKE ERROR - MC limit exceeded'
      GO TO 20
C* All data entries read
   30 CODE(NC)='                '
      NC= NC-1
      WRITE(LOU,860)
      WRITE(LOU,860) ' RPINKE - Rowlands Pin-cell Reference   '
      WRITE(LOU,860) ' ====================================   '
      WRITE(LOU,860)
      WRITE(LOU,860) ' Total number of input data sets read : ',NC
      WRITE(LOU,860)
C*
C* Begin constructing the weighted sums
      WRITE(LOU,861) '    Type','Minimum         ','Maximum         '
     1              ,'Sets','   Average'
      DO 38 IK=1,MP
C* Identify the largest and the smallest element - set weight to zero
      MX =-1
      MN =-1
      JC =0
      DO 32 IC=1,NC
      IF(WT(IC,IK).LE. 0 ) GO TO 32
      IF(MN.LT. 0) MN=IC
      IF(MX.LT. 0) MX=IC
      JC =JC+1
      IF(AK(IC,IK).LE.AK(MN,IK)) MN=IC
      IF(AK(IC,IK).GE.AK(MX,IK)) MX=IC
   32 CONTINUE
      IF(JC.GT.3) THEN
        WT(MX,IK)=0.
        WT(MN,IK)=0.
      ELSE
        MX=NC+1
        MN=NC+1
      END IF
C* Calculate the weighted sums
      DO 34 IC=1,NC
      SUM(IK)=SUM(IK)+WT(IC,IK)*AK(IC,IK)
      WGT(IK)=WGT(IK)+WT(IC,IK)
   34 CONTINUE
      AV=0.
      IF(WGT(IK).GT.0) AV=SUM(IK)/WGT(IK)
      WRITE(LOU,862) TYPE(IK),CODE(MN),CODE(MX), JC,AV
   38 CONTINUE
C*
C* Assemble the matrix of correlation coefficients
      AA(1,1)= WGT( 1)+WGT( 9)+WGT(13)+WGT(14)
      AA(1,2)=-WGT( 9)
      AA(1,3)=-WGT(13)
      AA(1,4)= 0
      AA(1,5)=-WGT(14)
      AA(1,6)= 0
      AA(1,7)= 0
      AA(1,8)= 0
      FF( 1 )= SUM( 1)+SUM( 9)+SUM(13)+SUM(14)
      AA(2,1)=-WGT(9)
      AA(2,2)= WGT( 2)+WGT( 9)+WGT(10)+WGT(12)+WGT(15)
      AA(2,3)=-WGT(12)
      AA(2,4)=-WGT(10)
      AA(2,5)= 0
      AA(2,6)=-WGT(15)
      AA(2,7)= 0
      AA(2,8)= 0
      FF( 2 )= SUM( 2)-SUM( 9)+SUM(10)+SUM(12)+SUM(15)
      AA(3,1)=-WGT(13)
      AA(3,2)=-WGT(12)
      AA(3,3)= WGT( 3)+WGT(11)+WGT(12)+WGT(13)+WGT(16)
      AA(3,4)=-WGT(11)
      AA(3,5)= 0
      AA(3,6)= 0
      AA(3,7)=-WGT(16)
      AA(3,8)= 0
      FF( 3 )= SUM( 3)-SUM(11)-SUM(12)-SUM(13)+SUM(16)
      AA(4,1)= 0
      AA(4,2)=-WGT(10)
      AA(4,3)=-WGT(11)
      AA(4,4)= WGT( 4)+WGT(10)+WGT(11)+WGT(17)
      AA(4,5)= 0
      AA(4,6)= 0
      AA(4,7)= 0
      AA(4,8)=-WGT(17)
      FF( 4 )= SUM( 4)-SUM(10)+SUM(11)+SUM(17)
      AA(5,1)=-WGT(14)
      AA(5,2)= 0
      AA(5,3)= 0
      AA(5,4)= 0
      AA(5,5)= WGT( 5)+WGT(14)+WGT(18)
      AA(5,6)=-WGT(18)
      AA(5,7)= 0
      AA(5,8)= 0
      FF( 5 )= SUM( 5)-SUM(14)+SUM(18)
      AA(6,1)= 0
      AA(6,2)=-WGT(15)
      AA(6,3)= 0
      AA(6,4)= 0
      AA(6,5)=-WGT(18)
      AA(6,6)= WGT( 6)+WGT(15)+WGT(18)+WGT(19)+WGT(21)
      AA(6,7)=-WGT(21)
      AA(6,8)=-WGT(19)
      FF( 6 )= SUM( 6)-SUM(15)-SUM(18)+SUM(19)+SUM(21)
      AA(7,1)= 0
      AA(7,2)= 0
      AA(7,3)=-WGT(16)
      AA(7,4)= 0
      AA(7,5)= 0
      AA(7,6)=-WGT(21)
      AA(7,7)= WGT( 7)+WGT(16)+WGT(20)+WGT(21)
      AA(7,8)=-WGT(20)
      FF( 7 )= SUM( 7)-SUM(16)-SUM(20)-SUM(21)
      AA(8,1)= 0
      AA(8,2)= 0
      AA(8,3)= 0
      AA(8,4)=-WGT(17)
      AA(8,5)= 0
      AA(8,6)=-WGT(19)
      AA(8,7)=-WGT(20)
      AA(8,8)= WGT( 8)+WGT(17)+WGT(19)+WGT(20)
      FF( 8 )= SUM( 8)-SUM(17)-SUM(19)+SUM(20)
C* Solve for the system of equations
      CALL MTXGUP(AA,FF,QK, 8,LD,DET)
      IF(DET.EQ.0) STOP 'RPINKE ERROR - Zero determinant'
      QK( 9)=QK(1)-QK(2)
      QK(10)=QK(2)-QK(4)
      QK(11)=QK(4)-QK(3)
      QK(12)=QK(2)-QK(3)
      QK(13)=QK(1)-QK(3)
      QK(14)=QK(1)-QK(5)
      QK(15)=QK(2)-QK(6)
      QK(16)=QK(3)-QK(7)
      QK(17)=QK(4)-QK(8)
      QK(18)=QK(5)-QK(6)
      QK(19)=QK(6)-QK(8)
      QK(20)=QK(8)-QK(7)
      QK(21)=QK(6)-QK(7)
C*
C* Calculate standard deviation
      DO 40 I=1,NC
      SIG( 1)=SIG( 1) + WT(I, 1)*(QK( 1)- AK(I, 1))**2
      SIG( 2)=SIG( 2) + WT(I, 2)*(QK( 2)- AK(I, 2))**2
      SIG( 3)=SIG( 3) + WT(I, 3)*(QK( 3)- AK(I, 3))**2
      SIG( 4)=SIG( 4) + WT(I, 4)*(QK( 4)- AK(I, 4))**2
      SIG( 5)=SIG( 5) + WT(I, 5)*(QK( 5)- AK(I, 5))**2
      SIG( 6)=SIG( 6) + WT(I, 6)*(QK( 6)- AK(I, 6))**2
      SIG( 7)=SIG( 7) + WT(I, 7)*(QK( 7)- AK(I, 7))**2
      SIG( 8)=SIG( 8) + WT(I, 8)*(QK( 8)- AK(I, 8))**2
      SIG( 9)=SIG( 9) + WT(I, 9)*(QK( 9)-(AK(I,1)-AK(I,2)))**2
      SIG(10)=SIG(10) + WT(I,10)*(QK(10)-(AK(I,2)-AK(I,4)))**2
      SIG(11)=SIG(11) + WT(I,11)*(QK(11)-(AK(I,4)-AK(I,3)))**2
      SIG(12)=SIG(12) + WT(I,12)*(QK(12)-(AK(I,2)-AK(I,3)))**2
      SIG(13)=SIG(13) + WT(I,13)*(QK(13)-(AK(I,1)-AK(I,3)))**2
      SIG(14)=SIG(14) + WT(I,14)*(QK(14)-(AK(I,1)-AK(I,5)))**2
      SIG(15)=SIG(15) + WT(I,15)*(QK(15)-(AK(I,2)-AK(I,6)))**2
      SIG(16)=SIG(16) + WT(I,16)*(QK(16)-(AK(I,3)-AK(I,7)))**2
      SIG(17)=SIG(17) + WT(I,17)*(QK(17)-(AK(I,4)-AK(I,8)))**2
      SIG(18)=SIG(18) + WT(I,18)*(QK(18)-(AK(I,5)-AK(I,6)))**2
      SIG(19)=SIG(19) + WT(I,19)*(QK(19)-(AK(I,6)-AK(I,8)))**2
      SIG(20)=SIG(20) + WT(I,20)*(QK(20)-(AK(I,8)-AK(I,7)))**2
      SIG(21)=SIG(21) + WT(I,21)*(QK(21)-(AK(I,6)-AK(I,7)))**2
   40 CONTINUE
      IF(WGT( 1).GT.0) SIG( 1)=SQRT(SIG( 1) / WGT( 1))
      IF(WGT( 2).GT.0) SIG( 2)=SQRT(SIG( 2) / WGT( 2))
      IF(WGT( 3).GT.0) SIG( 3)=SQRT(SIG( 3) / WGT( 3))
      IF(WGT( 4).GT.0) SIG( 4)=SQRT(SIG( 4) / WGT( 4))
      IF(WGT( 5).GT.0) SIG( 5)=SQRT(SIG( 5) / WGT( 5))
      IF(WGT( 6).GT.0) SIG( 6)=SQRT(SIG( 6) / WGT( 6))
      IF(WGT( 7).GT.0) SIG( 7)=SQRT(SIG( 7) / WGT( 7))
      IF(WGT( 8).GT.0) SIG( 8)=SQRT(SIG( 8) / WGT( 9))
      IF(WGT( 9).GT.0) SIG( 9)=SQRT(SIG( 9) / WGT( 9))
      IF(WGT(10).GT.0) SIG(10)=SQRT(SIG(10) / WGT(10))
      IF(WGT(11).GT.0) SIG(11)=SQRT(SIG(11) / WGT(11))
      IF(WGT(12).GT.0) SIG(12)=SQRT(SIG(12) / WGT(12))
      IF(WGT(13).GT.0) SIG(13)=SQRT(SIG(13) / WGT(13))
      IF(WGT(14).GT.0) SIG(14)=SQRT(SIG(14) / WGT(14))
      IF(WGT(15).GT.0) SIG(15)=SQRT(SIG(15) / WGT(15))
      IF(WGT(16).GT.0) SIG(16)=SQRT(SIG(16) / WGT(16))
      IF(WGT(17).GT.0) SIG(17)=SQRT(SIG(17) / WGT(17))
      IF(WGT(18).GT.0) SIG(18)=SQRT(SIG(18) / WGT(18))
      IF(WGT(19).GT.0) SIG(19)=SQRT(SIG(19) / WGT(19))
      IF(WGT(20).GT.0) SIG(20)=SQRT(SIG(20) / WGT(20))
      IF(WGT(21).GT.0) SIG(21)=SQRT(SIG(21) / WGT(21))
C* Print to log file
      WRITE(LOU,860)
      WRITE(LOU,860) ' Multiplication factors K-inf           '
      WRITE(LOU,864) '        ',(TYPE(J),J=1,4)
      WRITE(LOU,868) '  K-inf ',(  QK(J),J=1,4)
      WRITE(LOU,868) '  Sigma ',( SIG(J),J=1,4)
      WRITE(LOU,860)
      WRITE(LOU,860) ' Multiplication factors K-eff           '
      WRITE(LOU,864) '        ',(TYPE(J),J=5,8)
      WRITE(LOU,868) '  K-eff ',(  QK(J),J=5,8)
      WRITE(LOU,868) '  Sigma ',( SIG(J),J=5,8)
      WRITE(LOU,860)
      WRITE(LOU,860) ' Changes in K-inf due to temperature    '
      WRITE(LOU,864) '        ',(TYPE(J),J=9,13)
      WRITE(LOU,869) '  Dki(T)',(NINT(1.E5* QK(J)),J=9,13)
      WRITE(LOU,869) '  Sigma ',(NINT(1.E5*SIG(J)),J=9,13)
      WRITE(LOU,860)
      WRITE(LOU,860) ' Changes in K-inf due to leakage        '
      WRITE(LOU,864) '        ',(TYPE(J),J=14,17)
      WRITE(LOU,869) '  Dki(L)',(NINT(1.E5* QK(J)),J=14,17)
      WRITE(LOU,869) '  Sigma ',(NINT(1.E5*SIG(J)),J=14,17)
      WRITE(LOU,860)
      WRITE(LOU,860) ' Changes in K-eff due to temperature    '
      WRITE(LOU,864) '        ',(TYPE(J),J=18,21)
      WRITE(LOU,869) '  Dke(T)',(NINT(1.E5* QK(J)),J=18,21)
      WRITE(LOU,869) '  Sigma ',(NINT(1.E5*SIG(J)),J=18,21)
C*
      STOP 'RPINKE Completed'
C*
  800 FORMAT(A80)
  801 FORMAT(2A40)
  802 FORMAT(A16,8F8.0)
  803 FORMAT(16X,8F8.0)
  860 FORMAT(A40,I10)
  861 FORMAT(4X,A8,4X,2A16,A4,A10)
  862 FORMAT(4X,A8,4X,2A16,I4,F10.5)
  864 FORMAT(10A8)
  868 FORMAT(A8,8F8.5)
  869 FORMAT(A8,8I8)
      END
      SUBROUTINE MTXGUP(A,F,X,N,LDIG,DET)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of
C-D linear simultaneous equations  A x = F  (order n) using Gauss
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan, Ljubljana, Slovenia
C-Version: 1984 Original coding
C-V 93/03 - improved zero-determinant trapping
C-V 00/11 - further refinement of zero-determinant trapping (A.Trkov)
C-
      IMPLICIT     REAL*8 (A-H,O-Z)
      DIMENSION A(N,N),F(N),X(*)
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
