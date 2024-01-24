      PROGRAM WEDB1A
C-Title  : WEDB1A Program
C-Purpose: Analysis of the Burnup Credit Criticality Benchmark PART-1A
C-Author : D.L.Aldama, A.Trkov (1998)
C-M
C-M  Manual for Program WEDB1A
C-M  =========================
C-M
C-M    The program analyses the results of the NEA/CRP Burnup Credit
C-M  benchmark Part-1A (criticality prediction) from the WIMS-D code.
C-M    The differences in Kinf and deltaK are calculated with
C-M  reference to the solution described in the report
C-M  JAERI-M 94-003, 1994. Reactions rates also extracted
C-M  and normalized to 1 in each energy group. Results are
C-M  saved on a separate output file.
C-M
C-M  The program was originally written for the Cuban version of the
C-M  WIMS-D/4 output. It was modified for compatibility with WIMSD5.
C-M  Also, the command line input was changed to read the filenames
C-M  from the keyboard explicitly.
C-
      PARAMETER (NFF=12, N0=28, NK=13, NG1=3)
      CHARACTER*120 LINE
      CHARACTER*40  BLNK,FLNM,FLWO,FLOU,FLRR
      CHARACTER*7  CDW(N0)
      DIMENSION IDW(N0),RRA(N0,NG1,NK),RRF(N0,NG1,NK),RRN(N0,NG1,NK)
      DIMENSION XKREF(NK),SXKREF(NK),DKREF(NK),SDKREF(NK)
      DIMENSION SUMA(NG1,NK),SUMN(NG1,NK),SUMF(NG1,NK),XNU(NFF,NG1,NK)
      DIMENSION XKINF(NK)
C* WIMS-D "1986" material identifiers
      DATA CDW/'U -234 ','U -235 ','U -236 ','U -238 ','Pu-238 ',
     6         'Pu-239 ','Pu-240 ','Pu-241 ','Pu-242 ','Am-241 ',
     6         'Am-243 ','Np-237 ',
     6         'Mo- 95 ','Tc- 99 ','Ru-101 ','Rh-103 ','Ag-109 ',
     6         'Cs-133 ','Nd-143 ','Nd-145 ','Sm-147 ','Sm-149 ',
     6         'Sm-150 ','Sm-151 ','Sm-152 ','Eu-153 ','Gd-155 ',
     6         'O - 16 '/
c    1         'U -234 ','U -235 ','U -236 ','U -238 ','Pu-238 ',
c    1         'Pu-239 ','Pu-240 ','Pu-241 ','Pu-242 ','Am-241 ',
c    1         'Am-243 ','Np-237 ',
c    1         'Mo- 95 ','Tc- 99 ','Ru-101 ','Rh-103 ','Ag-109 ',
c    1         'Cs-133 ','Nd-143 ','Nd-145 ','Sm-147 ','Sm-149 ',
c    1         'Sm-150 ','Sm-151 ','Sm-152 ','Eu-153 ','Gd-155 ',
c    1         'O - 16 '/
      DATA IDW/    234  ,   2235  ,    236  ,   8238  ,    948  ,
     6            6239  ,   1240  ,   1241  ,   1242  ,    951  ,
     6             953  ,    937  ,
     6            4095  ,   4099  ,   4101  ,   4103  ,   4109  ,
     6            4133  ,   4143  ,   4145  ,   6147  ,   4149  ,
     6            4150  ,   4151  ,   4152  ,   4153  ,   2155  ,
     6            6016  /
c    1             234  ,    235  ,    236  ,   2238  ,     -1  ,
c    1            3239  ,   1240  ,    241  ,    242  ,     -1  ,
c    1              -1  ,     -1  ,
c    1              95  ,     99  ,    101  ,    103  ,    109  ,
c    1             133  ,    143  ,    145  ,   2147  ,    149  ,
c    1             150  ,    151  ,    152  ,    153  ,   1155  ,
c    1              16  /
      DATA XKREF/1.4378,1.1402,1.0638,1.2456,1.1885,1.1123,1.0240,
     &           1.2284,1.1657,1.2635,1.2566,1.1080,1.0758/
      DATA SXKREF/0.0175,0.0169,0.0170,0.0107,0.0110,0.0164,0.0156,
     &           0.0109,0.0099,0.0108,0.0109,0.0194,0.0185/
      DATA DKREF/0.0000,-0.2976,-0.3740,-0.1922,-0.2493,-0.3255,
     &           -0.4138,-0.2094,-0.2721,-0.1743,-0.1812,-0.3290,
     &           -0.3612/
      DATA SDKREF/0.0000,0.0134,0.0154,0.0097,0.0107,0.0149,
     &            0.0147,0.0099,0.0114,0.0100,0.0097,0.0205,
     &            0.0248/
C*
      DATA LIN,LOU,LRR,LKB,LTT / 1, 2, 3, 5, 6 /
      DATA BLNK/'                                        '/
     1     FLWO/'WIMSDOUT'/
     2     FLOU/'WEDB1A.LST'/
     3     FLRR/'WEDB1A.RRT'/
C*
      WRITE(LTT,91)
      WRITE(LTT,91) ' WEDB1A - NEA Burnup Credit Benchmark   '
      WRITE(LTT,91) ' ====================================   '
      WRITE(LTT,91)
      WRITE(LTT,91) ' Part-1A  Criticality Prediction        '
      WRITE(LTT,91)
C* Define the filenames
   12 WRITE(LTT,91) ' Enter the WIMS output filename       : '
      READ (LKB,91) FLWO
      OPEN (UNIT=LIN,FILE=FLWO,STATUS='OLD',ERR=12)
C*
   14 WRITE(LTT,91) ' Default output report filename       : ',FLOU
      WRITE(LTT,91) '           Enter new name to redefine : '
      READ (LKB,91) FLNM
      IF(FLNM.NE.BLNK) FLOU=FLNM
      OPEN(UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      WRITE(LOU,91)
      WRITE(LOU,91) ' WEDB1A - NEA Burnup Credit Benchmark   '
      WRITE(LOU,91) ' ====================================   '
      WRITE(LOU,91)
      WRITE(LOU,91) ' Part-1A  Criticality Prediction        '
      WRITE(LOU,91)
      WRITE(LOU,91) ' Calculated results filename:         : ',FLWO
      WRITE(LOU,91)
      WRITE(LOU,91) ' Delta K = K(Case n) - K(Case 1)        '
      WRITE(LOU,91) ' Second row gives:                      '
      WRITE(LOU,91) '     Uncertainty (2 sigma) for reference'
      WRITE(LOU,91) '     Difference from reference for cal. '
      WRITE(LOU,91) ' Third row gives the same in %          '
      WRITE(LOU,91)
      N=2
C*
      WRITE(LTT,91) '$Enter Reaction rates report filename : '
      READ (LKB,91) FLRR
      IF(FLRR.NE.BLNK) THEN
        N=3
        OPEN (LRR,FILE=FLRR)
      END IF
C*
c  16 WRITE(LTT,91) ' Enter reference results filename     : '
c     READ (LKB,91) FLNM
c     IF(FLNM.NE.BLNK) THEN
c       FLRF=FLNM
c       OPEN(UNIT=LRF,FILE=FLRF,STATUS='OLD',ERR=16)
c       N=3
c     END IF
C*
      IK=0
      READ(LIN,'(A120)',END=400)LINE
      DO WHILE (LINE(1:15).NE.' END OF FILE ON')
        IF (LINE(27:34).EQ.'chain 14') THEN
          IK=IK+1
          CALL GETK(LIN,XKINF(IK),1)
        ELSE IF (LINE(27:34).EQ.'chain 15') THEN
          CALL GETRR(LIN,N0,NG1,IK,IDW,RRA,RRF,RRN)
        ENDIF
        READ(LIN,'(A120)',END=400)LINE
      END DO
  400 WRITE(LOU,1)'CASE','K - REF. ','K - CAL. ',
     &                    'DELTA K-REF.','DELTA K-CAL.'
      IF(N.EQ.3) WRITE(LRR,1)'CASE','K - REF. ','K - CAL. ',
     &                    'DELTA K-REF.','DELTA K-CAL.'
      DO I=1,IK
        DRHO=(XKINF(I)-XKINF(1))*100000.
        DK=DKREF(I)*100000.
        SXK=SXKREF(I)*100000.
        SDK=SDKREF(I)*100000.
        WRITE(LOU,2)I,XKREF(I),XKINF(I),INT(DK),INT(DRHO)
        IF(N.EQ.3) WRITE(LRR,20)I,XKREF(I),XKINF(I),INT(DK),INT(DRHO)
        DEVAK=(XKINF(I)-XKREF(I))*100000.
        DEVAR=(DRHO-DK)
        WRITE(LOU,3)INT(SXK),INT(DEVAK),INT(SDK),INT(DEVAR)
        IF(N.EQ.3)WRITE(LRR,30)INT(SXK),INT(DEVAK),INT(SDK),INT(DEVAR)
        IF (XKREF(I).NE.0.0) THEN
          DEVRK=(XKINF(I)/XKREF(I)-1.0)*100.0
        ELSE
          DEVRK=0.0
        ENDIF
        IF (INT(SDK).NE.0) THEN
          DEVRR=(DRHO/DK-1.0)*100.0
        ELSE
          DEVRR=0.0
        ENDIF
        WRITE(LOU,4)DEVRK,DEVRR
        WRITE(LOU,*)
        IF (N.EQ.3) THEN
          WRITE(LRR,40)DEVRK,DEVRR
          WRITE(LRR,*)
        ENDIF
      END DO
      DO I=1,IK
        DO J=1,NG1
          SUMA(J,I)=0.0
          SUMN(J,I)=0.0
          SUMF(J,I)=0.0
          DO K=1,N0
            SUMA(J,I)=SUMA(J,I)+RRA(K,J,I)
            SUMN(J,I)=SUMN(J,I)+RRN(K,J,I)
            SUMF(J,I)=SUMF(J,I)+RRF(K,J,I)
          END DO
        END DO
      END DO
C*
      DO I=1,IK
        DO J=1,NG1
          IF(SUMA(J,I).LE.0) SUMA(J,I)=1
          IF(SUMN(J,I).LE.0) SUMN(J,I)=1
          IF(SUMF(J,I).LE.0) SUMF(J,I)=1
        END DO
      END DO
C*
      DO IG=1,NG1
c       WRITE(LOU,*)' ABSORPTION RATES [%]  ENERGY GROUP: ',IG
c       WRITE(LOU,6)'  ISOT.',(J,J=1,IK)
        IF (N.EQ.3) THEN
          WRITE(LRR,*)' ABSORPTION RATES [%]  ENERGY GROUP: ',IG
          WRITE(LRR,60)'  ISOT.',(J,J=1,IK)
        ENDIF
        DO I=1,N0
c         WRITE(LOU,5)CDW(I),(RRA(I,IG,J)/SUMA(IG,J)*100.,J=1,IK)
          IF(N.EQ.3) THEN
            WRITE(LRR,50)CDW(I),(RRA(I,IG,J)/SUMA(IG,J)*100.,J=1,IK)
          ENDIF
        END DO
c       WRITE(LOU,*)
        IF(N.EQ.3) WRITE(LRR,*)
      END DO
      DO IG=1,NG1
c       WRITE(LOU,*)' NEUTRON PRODUCTION RATES [%]  ENERGY GROUP: ',IG
c       WRITE(LOU,6)'  ISOT.',(J,J=1,IK)
        IF (N.EQ.3) THEN
          WRITE(LRR,*)
     &              ' NEUTRON PRODUCTION RATES [%]  ENERGY GROUP: ',IG
          WRITE(LRR,60)'  ISOT.',(J,J=1,IK)
        ENDIF
        DO I=1,NFF
c         WRITE(LOU,5)CDW(I),(RRN(I,IG,J)/SUMN(IG,J)*100,J=1,IK)
          IF(N.EQ.3) THEN
            WRITE(LRR,50)CDW(I),(RRN(I,IG,J)/SUMN(IG,J)*100,J=1,IK)
          ENDIF
        END DO
        WRITE(LOU,*)
        IF(N.EQ.3) WRITE(LRR,*)
      END DO
      DO IG=1,NG1
c       WRITE(LOU,*)' FISSION RATES [%]  ENERGY GROUP: ',IG
c       WRITE(LOU,6)'  ISOT.',(J,J=1,IK)
        IF (N.EQ.3) THEN
          WRITE(LRR,*)' FISSION RATES [%]  ENERGY GROUP: ',IG
          WRITE(LRR,60)'  ISOT.',(J,J=1,IK)
        ENDIF
        DO I=1,NFF
c         WRITE(LOU,5)CDW(I),(RRF(I,IG,J)/SUMF(IG,J)*100,J=1,IK)
          IF(N.EQ.3) THEN
            WRITE(LRR,50)CDW(I),(RRF(I,IG,J)/SUMF(IG,J)*100,J=1,IK)
          ENDIF
        END DO
c       WRITE(LOU,*)
        IF(N.EQ.3) WRITE(LRR,*)
      END DO
      DO IG=1,NG1
c       WRITE(LOU,*)' NEUTRONS PER FISSION   ENERGY GROUP: ',IG
c       WRITE(LOU,6)'  ISOT.',(J,J=1,IK)
        IF (N.EQ.3) THEN
          WRITE(LRR,*)' NEUTRONS PER FISSION   ENERGY GROUP: ',IG
          WRITE(LRR,60)'  ISOT.',(J,J=1,IK)
        ENDIF
        DO I=1,NFF
          DO J=1,IK
            IF (RRF(I,IG,J).NE.0.0) THEN
              XNU(I,IG,J)=RRN(I,IG,J)/RRF(I,IG,J)
            ELSE
              XNU(I,IG,J)=0.0
            ENDIF
          END DO
c         WRITE(LOU,5)CDW(I),(XNU(I,IG,J),J=1,IK)
          IF (N.EQ.3) THEN
            WRITE(LRR,50)CDW(I),(XNU(I,IG,J),J=1,IK)
          ENDIF
        END DO
c       WRITE(LOU,*)
        IF(N.EQ.3) WRITE(LRR,*)
      END DO
      CLOSE(LIN)
      CLOSE(LOU)
      IF(N.EQ.3) CLOSE(LRR)
      STOP 'WEDB1A Completed'
    1 FORMAT(1X,A4,2X,A9,2X,A9,2X,A12,2X,A12)
    2 FORMAT(1X,I3,3X,F8.5,3X,F8.5,3X,I10,3X,I10)
    3 FORMAT(7X,I8,3X,I8,3X,I10,3X,I10)
    4 FORMAT(18X,F8.2,16X,F10.2)
    5 FORMAT(1X,A7,13F6.2)
    6 FORMAT(1X,A7,13I6)
   20 FORMAT(1X,I3,' ',F8.5,' ',F8.5,' ',I10,' ',I10)
   30 FORMAT(1X,3X,' ',I8,' ',I8,' ',I10,' ',I10)
   40 FORMAT(1X,3X,' ',8X,' ',F8.2,' ',10X,' ',F10.2)
   50 FORMAT(1X,A7,' ',10(F6.2,' '),F6.2)
   60 FORMAT(1X,A7,' ',10(I6,' '),I6)
   91 FORMAT(2A40)
      END
      SUBROUTINE GETK(LIN,XK,IKK)
      CHARACTER*120 LINE
      I=0
      READ(LIN,'(A120)')LINE
      DO WHILE(LINE(16:31).NE.'entry into chain')
        IF (LINE(26:35).EQ.'k-infinity') THEN
          I=I+1
          IF (I.EQ.IKK) THEN
            READ(LINE,'(36X,E13.0)')XK
          END IF
          READ(LIN,'(A120)')LINE
        END IF
        READ(LIN,'(A120)')LINE
      END DO
      BACKSPACE(LIN)
      RETURN
      END
      SUBROUTINE GETRR(LIN,N0,NG1,IK,IDW,RRA,RRF,RRN)
      DIMENSION IDW(N0),RRA(N0,NG1,*),RRF(N0,NG1,*),RRN(N0,NG1,*)
      CHARACTER*120 LINE
      READ(LIN,'(A120)')LINE
      DO WHILE (LINE(2:11).NE.'k-inf edit')
        READ(LIN,'(A120)')LINE
      END DO
      DO J=1,N0
        DO WHILE (LINE(2:8).NE.'element')
          READ(LIN,'(A120)')LINE
        END DO
        IF (LINE(16:24).EQ.'reactions') THEN
          READ(LINE,'(8X,I6)')IEL
          IPT=0
          I=1
          DO WHILE (I.LE.N0.AND.IPT.EQ.0)
            IF (IDW(I).EQ.IEL) IPT=I
            I=I+1
          END DO
          IF (IPT.EQ.0) THEN
            WRITE(*,*)' ELEMENT ',IEL,' NOT FOUND'
            STOP
          ENDIF
          READ(LIN,'(A120)')LINE
          READ(LIN,'(A120)')LINE
          DO IG=1,NG1
            READ(LIN,'(13X,E12.0)')RRA(IPT,IG,IK)
          END DO
          READ(LIN,'(A120)')LINE
          IF (LINE(2:8).EQ.'fission') THEN
            READ(LIN,'(A120)')LINE
            DO IG=1,NG1
              READ(LIN,'(13X,E12.0)')RRF(IPT,IG,IK)
            END DO
            READ(LIN,'(A120)')LINE
            READ(LIN,'(A120)')LINE
            DO IG=1,NG1
              READ(LIN,'(13X,E12.0)')RRN(IPT,IG,IK)
            END DO
          ELSE
            BACKSPACE(LIN)
          ENDIF
        ENDIF
      END DO
      BACKSPACE(LIN)
      DO WHILE(LINE(16:31).NE.'entry into chain')
        READ(LIN,'(A120)')LINE
      END DO
      BACKSPACE(LIN)
      RETURN
      END
