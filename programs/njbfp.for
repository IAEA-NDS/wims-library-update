      PROGRAM NJBFP
C-Tile: Program NJBFP
C-T
C-Purpose: Prepare NJOY fission product yields
C-P
C-Usage: NJBFP
C-U
C-U   Input filenames:
C-U    Input files
C-U        1. List file for the explicitly represented F. Ps
C-U        2. List File for all the F. Ps.
C-U        3. Evaluated nuclear data library Summary File
C-U           (GETDATA output file *.MF2)
C-U        4. *.NJB file for the explicitly represented F. Ps.
C-U           (AVRFPY output)
C-U        5. *.NJB file for all F. Ps.
C-U           (AVRFPY output)
C-U
C-U    Output Files
C-U        1. *.NJB file with the fission yields needed for NJOY input.
C-U
C-Description:
C-D   The code reads the input files and checks them for consistency.
C-D   Afterwards the pseudo fission product yield is calculated and
C-D   included on the output file (*.NJB) as required by the WIMSR
C-D   module of NJOY.
C-D     The yield of Pm-147 is split into two separate products, the
C-D   first being the precursor of Pm-188g (fraction X4147 defined in
C-D   the parameter statement) and the second the precursor of Pm-148m.
C-D
C-Author: D. L. Aldama
C-A
C-Version: 03/1999
C-V        04/2001  No. of explicit f.p. NFPE increased from 50 to 80
C-V
       PARAMETER(LTT=6,LKB=5,LLE=1,LLL=2,LIB=3,LNB=4,LNJ=7)
       PARAMETER(NFPE=80,NFP=150,NMAT=1024,N4902=4902)
C*       X4147=0.4667, ref EAF99, rounded to 0.47
       PARAMETER(N4147=4147, X4147=0.47,N5147=5147)
       CHARACTER*80 LINE
       CHARACTER*40 FLNM,FLE,FLL,BLNK,FLIB,FNAME
       CHARACTER*11 ZASYM(NMAT)
       DIMENSION AZE(NFPE),RIDE(NFPE),YE(NFPE)
       DIMENSION AZ(NFP),RID(NFP),Y(NFP)
       DIMENSION IPYP(NFP)
       DIMENSION AZL(NMAT)
       DATA BLNK/'                                        '/
       DATA FLE/'FpLstExp.dat'/
       DATA FLL/'FpLstAll.dat'/
       DATA FLIB/'ENDF.MF2'/
       WRITE(LTT,15) '     NJBFP - Prepare NJOY F.P. Yields   '
       WRITE(LTT,15) '     =================================  '
       WRITE(LTT,15)
C*
C*       Reading input files
       FLNM=BLNK
    1  WRITE(LTT,15) '     Define list file for explicit F.P: '
       WRITE(LTT,15) '     ---------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FLE
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FLE=FLNM
       OPEN (UNIT=LLE,FILE=FLE,STATUS='OLD',ERR=1)
       I=1
       READ(LLE,20) LINE
       READ(LLE,20) LINE
       DO WHILE (LINE(1:8).NE.'        ')
         READ(LINE,30) AZE(I), RIDE(I)
         IF (RIDE(I).LE.0.0) RIDE(I)=AZE(I)
         I=I+1
         READ(LLE,20,END=2) LINE
         IF(I.GT.NFPE) THEN
           STOP ' Fatal error: More than 80 explicitly represented F.Ps'
         ENDIF
       END DO
    2  CLOSE(LLE)
       KFPE=I-1
       FLNM=BLNK
    3  WRITE(LTT,15) '     Define list file for   ALL    F.P: '
       WRITE(LTT,15) '     ---------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FLL
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FLL=FLNM
       OPEN (UNIT=LLL,FILE=FLL,STATUS='OLD',ERR=3)
       I=1
       READ(LLL,20) LINE
       READ(LLL,20) LINE
       DO WHILE (LINE(1:8).NE.'        ')
         READ(LINE,30) AZ(I), RID(I)
         IF (RID(I).LE.0.0) RID(I)=AZ(I)
         I=I+1
         READ(LLL,20,END=4) LINE
         IF(I.GT.NFP) THEN
           STOP ' Fatal error: More than 150 F.Ps'
         ENDIF
       END DO
    4  CLOSE(LLL)
       KFP=I-1
       KPDO=KFP-KFPE
       IF (KPDO.LE.0) THEN
         STOP ' Fatal Error:  KFP-all  .LE.  KFP-explicit '
       ENDIF
       FLNM=BLNK
    5  WRITE(LTT,15) '      Define ENDF Library Summary file: '
       WRITE(LTT,15) '     ---------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FLIB
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FLIB=FLNM
       OPEN (UNIT=LIB,FILE=FLIB,STATUS='OLD',ERR=5)
       I=1
       READ(LIB,20) LINE
       READ(LIB,20) LINE
       DO WHILE (LINE(1:8).NE.'        ')
         READ(LINE,40) NNN,ZASYM(I),AZL(I)
         I=I+1
         READ(LIB,20,END=6) LINE
         IF(I.GT.NMAT) THEN
           STOP ' Fatal error: More than 1024 ENDF materials'
         ENDIF
       END DO
    6  KMAT=I-1
       CLOSE(LIB)
C*
C*       Process *.njb files
       DO IT=1,2
        FLNM=BLNK
        IF (IT.EQ.1) THEN
         NNN=KFPE
         WRITE(LTT,15) ' Define fiss. yields for explicit F.Ps  '
        ELSE
         NNN=KFP
         WRITE(LTT,15) '     Define fiss. yields for all F.Ps   '
        ENDIF
    8   WRITE(LTT,15) '     Define the yield (*.NJB) file for: '
        WRITE(LTT,17) '              Number of F.P. should be: ',NNN
        WRITE(LTT,15) '     ---------------------------------  '
        WRITE(LTT,15) '                     Default file name: ',FLNM
        WRITE(LTT,15) '$       Enter new name (enter=default): '
        READ (LKB,15) FLNM
        IF(FLNM.EQ.BLNK) GO TO 8
        OPEN (UNIT=LNB,FILE=FLNM,STATUS='OLD',ERR=8)
        DO I=1,NNN
          IF (IT.EQ.1) THEN
            YE(I)=0.0
          ELSE
            Y(I)=0.0
          ENDIF
        END DO
        READ(LNB,60)KKK
        KKK=KKK-4
        DO KK=1,KKK
          READ(LNB,60)IDUMMY,FPY
          KTRI=-1
          DO I=1,NNN
            IF(IT.EQ.1) THEN
              IF ((NINT(RIDE(I)).EQ.IDUMMY)) THEN
                YE(I)=FPY
                KTRI=1
              ENDIF
            ELSE IF (IT.EQ.2) THEN
              IF ((NINT(RID(I)).EQ.IDUMMY)) THEN
                 Y(I)=FPY
                 KTRI=1
              ENDIF
            ENDIF
          END DO
          IF(KTRI.LT.0) THEN
            WRITE(LTT,*)' Error: Lst & Yields files are not compatible'
     &                  , ' IT= ', IT, ' I= ', I
            STOP
          END IF
        END DO
        CLOSE(LNB)
       END DO
       DO I=1,KFP
        IF (Y(I).LT.0.0) IPYP(I)=-3
       END DO
C*
C*       Checking if NFP and NFPE yields are compatible.
C*       Selecting isotopes for pseudo fission
       YESUM=0.0
       DO I=1,KFP
         IAZ=NINT(RID(I))
         II=-1
         K=1
         DO WHILE (II.LT.0)
          KAZ=NINT(RIDE(K))
          IF (IAZ.NE.KAZ) THEN
            K=K+1
          ELSE
            II=1
            RID(I)=RIDE(K)
            IPYP(I)=-1
            YESUM=YESUM+YE(K)
            IF (YE(K) .EQ. 0.0) THEN
              IF (Y(I). EQ. 0.0) THEN
                DIF=0.0
              ELSE
                DIF=1.0
              ENDIF
            ELSE
              DIF=ABS(Y(I)/YE(K)-1.0)
            ENDIF
            IF (DIF.GT.1.0E-6) THEN
              WRITE(LTT,90)' Warning: Fission yields do not match ',
     &                       IAZ,Y(I),YE(K),' : check FP list files'
            ENDIF
          ENDIF
          IF (K.GT.KFPE) II=2
         END DO
       END DO
C*
C*       Open OUTPUT file
C*
C*        NJOY
       FLNM=BLNK
    9  WRITE(LTT,15) '     Define NJOY burnup data block file:'
       WRITE(LTT,15) '     ---------------------------------- '
       WRITE(LTT,15) '                     Default file name: ',FNAME
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FNAME=FLNM
       OPEN (UNIT=LNJ,FILE=FNAME,ERR=9)
C*
C*      Check if the fission products are on the library
C*      and calculate the yield of the pseudo fission product
       YPSUM=0.0
       DO J=1,KFP
       IF(IPYP(J).GE.0) THEN
        JAZ=NINT(10.0*AZ(J))
        I=1
        II=-1
        DO WHILE (II.LT.0)
          IF (ZASYM(I)(11:11).EQ.'M'.OR.ZASYM(I)(11:11).EQ.'m') THEN
            IAZ=NINT(10.0*AZL(I))+1
          ELSE
            IAZ=NINT(10.0*AZL(I))
          ENDIF
          IF (JAZ.EQ.IAZ) THEN
            II=1
            YPSUM=YPSUM+Y(J)
          ELSE
            I=I+1
          ENDIF
          IF (I.GT.KMAT) II=2
        END DO
        IF (II.EQ.2) THEN
          WRITE(LTT,*)' Warning: F.P with ZA= ',AZ(J),
     &                ' not found on the library file'
        ENDIF
       ENDIF
       END DO
C*
       DO I=1,KFPE
         IF (RIDE(I).EQ.N4147) THEN
           WRITE(LNJ,70)N4147,X4147*YE(I)
           WRITE(LNJ,70)N5147,(1.0-X4147)*YE(I)
         ELSE
           WRITE(LNJ,70)IFIX(RIDE(I)+0.01),YE(I)
         ENDIF
       END DO
       WRITE(LNJ,70)N4902,YPSUM
       YTOT=YESUM+YPSUM
       WRITE(LTT,* )
       WRITE(LTT,* )'***************************************'
       WRITE(LTT,75)'    Actinide file : ',FNAME
       WRITE(LTT,80)' TOTAL F.Ps YIELD : ',YTOT
       WRITE(LTT,80)'    EXPLICIT F.Ps : ',YESUM
       WRITE(LTT,80)'       PSEUDO F.P : ',YPSUM
       WRITE(LTT,* )'***************************************'
       WRITE(LTT,*)
       CLOSE(LNJ)
       STOP 'NORMAL END'
   15  FORMAT(2A40)
   16  FORMAT(A40,A11)
   17  FORMAT(A40,I5)
   20  FORMAT(A80)
   30  FORMAT(2F11.0)
   40  FORMAT(I4,2X,A11,F9.0,F12.0,F12.0,1PE13.0)
   50  FORMAT(A28,F8.1,A30)
   60  FORMAT(I6,E11.0)
   70  FORMAT(I6,1PE11.4)
   75  FORMAT(A20,A40)
   80  FORMAT(A20,1PE11.5)
   90  FORMAT(A38,I8,1x,1P2E11.4)
      END
