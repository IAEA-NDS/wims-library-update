      PROGRAM WEDRPU
C-Title  : Rrogram WEDRPU
C-Purpose: Process WIMS-D results for Rowlands Pu pin cell benchmark
C-M
C-M  Manual for Program WEDRPU
C-M  =========================
C-M
C-M  The WIMS-D results for the Rowlands Plutonium pin-cell
C-M  benchmarks are processed.
C-M
C-M  It is assumed that the records containing the results
C-M  of the K-inf and K-eff leakage edit of WIMS-D are extracted
C-M  on the file "RPINPU.K". Double leakage edit (transport
C-M  corrected and B1) is allowed. It is identified by the
C-M  K-inf value, which must be the same to within 1 pcm.
C-M  The K-inf, K-eff and differences in K are calculated and
C-M  printed to output. If double leakage edit is present, the
C-M  K values and differences in K due to leakage are taken
C-M  from the B1 leakage edit. Differences due to temperature
C-M  changes are taken from the transport corrected leakage
C-M  edit, since the B1 edit may be incorrect due to a single
C-M  P1 scattering matrix in the library.
C-M
C-M  The reference results are read from the "RPINKE.LOG" file
C-M  which is normally produced by the RPINKE code which
C-M  analyses and averages the results of a number of different
C-M  independent calculations.
C-M
C-M  A summary of results is written to "WEDRPU.LOG"
C-M
C-Author : A.Trkov, Institute Jozef Stefan, Slovenia (1999)
C-
      CHARACTER*120 RECI
      CHARACTER*40  FLNM,FLNI,FLRE,FLNO
C*
      DIMENSION     AKIN(4),AKEB(4),AKET(4)
     1            , AK(21) ,RK(21) ,SG(21) ,ER(21)
C*
      DATA  LIN,LRE,LOU / 1, 2 , 4 /
      DATA  FLNI/'RPINPU.K'/
     1      FLRE/'RPINKE.LOG'/
     1      FLNO/'WEDRPU.LOG'/
C*
      OPEN (UNIT=LIN,FILE=FLNI,STATUS='OLD')
      OPEN (UNIT=LRE,FILE=FLRE,STATUS='OLD')
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
C*
C* Initialize
      NK=0
C*
C* Process WIMS results - read the K-inf and K-eff from the file
   20 READ (LIN,800,END=21) RECI
      CALL UPCASE(40,RECI)
      IF(RECI(21:30).NE.'...  K-INF') GO TO 20
      READ(RECI(38:49),802) XKI
      READ(RECI(66:77),802) XKE
C* Distinguish between transport-corrected and B1 leakage edit
      IF(ABS(XKI-AKIN(NK)).LT.1.E-5) THEN
        AKEB(NK)=XKE
      ELSE
        IF(NK.LT.4) NK=NK+1
        AKIN(NK)=XKI
        AKET(NK)=XKE
        AKEB(NK)=XKE
      END IF
      GO TO 20
   21 IF(NK.NE.4) GO TO 92
C* Sort out the results
      AK (1)=AKIN(1)
      AK (2)=AKIN(2)
      AK (4)=AKIN(3)
      AK (3)=AKIN(4)
      AK (5)=1
      AK (6)=1
      AK (7)=1
      AK (8)=1
      AK( 9)=AKIN(1)-AKIN(2)
      AK(10)=0
      AK(11)=AKIN(3)-AKIN(4)
      AK(12)=0
      AK(13)=0
      AK(14)=0
      AK(15)=0
      AK(16)=0
      AK(17)=0
      AK(18)=0
      AK(19)=0
      AK(20)=0
      AK(21)=0
C*
C* Process the reference results file
   40 READ (LRE,800) RECI
      IF(RECI(1:8).NE.'  K-inf ') GO TO 40
      READ(RECI,804) (RK(J),J= 1, 4)
      READ(LRE ,804) (SG(J),J= 1, 4)
   41 READ (LRE,800) RECI
      IF(RECI(1:8).NE.'  K-eff ') GO TO 41
      READ(RECI,804) (RK(J),J= 5, 8)
      READ(LRE ,804) (SG(J),J= 5, 8)
   42 READ (LRE,800) RECI
      IF(RECI(1:8).NE.'  Dki(T)') GO TO 42
      READ(RECI,804) (RK(J),J= 9,13)
      READ(LRE ,804) (SG(J),J= 9,13)
   44 READ (LRE,800) RECI
      IF(RECI(1:8).NE.'  Dki(L)') GO TO 44
      READ(RECI,804) (RK(J),J=14,17)
      READ(LRE ,804) (SG(J),J=14,17)
   46 READ (LRE,800) RECI
      IF(RECI(1:8).NE.'  Dke(T)') GO TO 46
      READ(RECI,804) (RK(J),J=18,21)
      READ(LRE ,804) (SG(J),J=18,21)
C*
C* Calculate the error w.r.t reference
      DO 50 I=1,21
      IF( I.GT.8) AK( I)=1.E5*AK(I)
      ER( I)=(AK( I)-RK( I))
      IF( I.LE.8) ER( I)=1.E5*ER(I)
      IF( I.LE.8) SG( I)=1.E5*SG(I)
   50 CONTINUE
C*
C* Print to log file
      WRITE(LOU,840) ' Rowlands Plutonium Pin-cell Benchmark  '
      WRITE(LOU,840) ' =====================================  '
      WRITE(LOU,840)
      WRITE(LOU,840) ' Case summary                           '
      WRITE(LOU,840) ' ------------                           '
      WRITE(LOU,840) ' Case  Material Tfuel  Tcan  Tmod       '
      WRITE(LOU,840) '                  (K)   (K)   (K)       '
      WRITE(LOU,840) '   1     MOX-1    300   300   300       '
      WRITE(LOU,840) '   2     MOX-1    560   300   300       '
      WRITE(LOU,840) '   3     MOX-2    560   300   300       '
      WRITE(LOU,840) '   4     MOX-2    300   300   300       '
      WRITE(LOU,840)
      WRITE(LOU,840) ' NOTE:                                  '
      WRITE(LOU,840) ' - Reference results contain            '
      WRITE(LOU,840) '   the uncertainty in braces [pcm]      '
      WRITE(LOU,840) ' - WIMS results contain the difference  '
      WRITE(LOU,840) '   from reference in braces [pcm]       '
      WRITE(LOU,840)
      WRITE(LOU,840) ' Multiplication factors K-inf           '
      WRITE(LOU,840) ' ----------------------------           '
      WRITE(LOU,821) (J,J=1,4)
      WRITE(LOU,823) ' Ref. ',(RK(J),NINT(SG(J)),J= 1, 4)
      WRITE(LOU,823) ' WIMS ',(AK(J),NINT(ER(J)),J= 1, 4)
      WRITE(LOU,840)
      WRITE(LOU,840) ' Change in K-inf due to temperature[pcm]'
      WRITE(LOU,840) ' ---------------------------------------'
      WRITE(LOU,822) 1,'-',2, 4,'-',3
      WRITE(LOU,824) ' Ref. ',NINT(RK( 9)),NINT(SG( 9))
     1                       ,NINT(RK(11)),NINT(SG(11))
      WRITE(LOU,824) ' WIMS ',NINT(AK( 9)),NINT(ER( 9))
     1                       ,NINT(AK(11)),NINT(ER(11))
   90 STOP 'WEDRPU Completed'
C*
   92 PRINT *,NK
      STOP 'WEDRPU ERROR - EOF before all data read'
C*
  800 FORMAT(A120)
  802 FORMAT(BN,F12.0)
  804 FORMAT(8X,8F8.0)
  840 FORMAT(A40)
  821 FORMAT(' Case ',5(10X,I1,3X))
  822 FORMAT(' Case ',5(7X,I1,A1,I1,2X))
  823 FORMAT(A6,5(F8.5,'(',I4,')') )
  824 FORMAT(A6,5(I6  ,'(',I4,')') )
      END
      SUBROUTINE UPCASE(NL,STR)
C-Title  : Subroutine UPCASE
C-Purpose: Force character string string Uppercase
      CHARACTER*1 STR(NL)
      DO 20 I=1,NL
      IC =ICHAR(STR(I))
      IF(IC.GT.96) STR(I)=CHAR(IC-32)
   20 CONTINUE
      RETURN
      END
