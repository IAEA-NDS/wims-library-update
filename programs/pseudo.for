      PROGRAM PSEUDO
C-Title  : Program PSEUDO
C-Purpose: Prepare the files needed to generate pseudo fission product.
C-Author : D. L. Aldama
C-Version:
C-V  03/1999 Original code
C-V  10/1999 Oscar Cabellos: Unix compatibility (implemented by A.Trkov)
C-V  06/2001 Uma Kannan: Extensions for 172-group library.
C-Usage: PSEUDO
C-U
C-U  Input filenames:
C-U    Input files
C-U        1.     List file for the explicitly represented F. Ps.
C-U        2.     List File for all the F. Ps.
C-U        3.     Evaluated nuclear data library Summary File
C-U               (GETDATA output file *.MF2)
C-U        4,5,6. The U-235, U-238 and Pu-239 NJB files for the
C-U               explicitly represented fission products.
C-U               (AVRFPY output)
C-U        7,8,9. The U-235, U-238 and Pu-239 NJB files for all
C-U               the fission products.
C-U               (AVRFPY output)
C-U
C-U    Output Files
C-U     1. ?.nji   Containing the NJOY input options for Pseudo F.P.
C-U                (ie. endfb6fp.nji, jef22fp.nji, cendlfp.nji, etc)
C-U     2. ?.wli   Containing the WILLIE input options for Pseudo F.P.
C-U                (ie. endfb6fp.wli, jef22fp.wli, cendlfp.wli, etc)
C-U     3. ?.bat   BATCH file to run NJOY and WILLIE for Pseudo F.P.
C-U                (ie. endfb6fp.bat, jef22fp.bat, cendlfp.bat, etc)
C-U
C-U  Running ?.bat (endfb6fp.bat, jef22fp.bat, cendlfp.bat, etc) the
C-U  file ?.xsw (endfb6fp.xsw, jef22fp.xsw, cendlfp.xsw, etc) is
C-U  generated on the %WLUPI% directory. A copy of the file named
C-U  PFP4902.xsw is also saved on your work directory.
C-U  This later file can be included in the WIMS Library running the
C-U  wimslib option of the BATCH runall for the corresponding evaluated
C-U  nuclear data library or directly using the program WILLIE.
C-U
C-Description:
C-D   The code reads the input files and checks them for consistency.
C-D   Afterwards the output files are generated according to the
C-D   evaluated nuclear data library. The average fission product is
C-D   calculated using the weights given in the corresponding data
C-D   statement for XF.
C-D
C-V        04/2001  No. of explicit f.p. NFPE increased from 50 to 80
 
       PARAMETER(LTT=6,LKB=5,LLE=1,LLL=2,LIB=3,LNJ=4,LNB=7,LWI=8,LBT=9)
       PARAMETER(NFPE=80,NFP=150,NMAT=1024,NFIS=3,SREF0=5.0E3)
       CHARACTER*80 LINE
       CHARACTER*80 FLNM,FLE,FLL,BLNK,FLIB,FNJ,FWLI,FBAT
       CHARACTER*11 ZASYM(NMAT),FZASYM(NFIS)
       CHARACTER*6  FMAT(NFP)
       DIMENSION AZE(NFPE),RIDE(NFPE),YE(NFIS,NFPE)
       DIMENSION AZ(NFP),RID(NFP),Y(NFIS,NFP)
       DIMENSION IPYP(NFP)
       DIMENSION XF(NFIS),YPSUM(NFIS),XP(NFIS,NFP)
       DIMENSION MAT(NMAT),AZL(NMAT),AWR(NMAT),SPOT(NMAT),EH(NMAT)
       DATA BLNK/'                                        '/
       DATA FLE/'FpLstExp.dat'/
       DATA FLL/'FpLstAll.dat'/
       DATA FLIB/'ENDF.MF2'/
       DATA FNJ/'PSEUDOFP.NJI'/
       DATA FWLI/'PSEUDOFP.WLI'/
       DATA FBAT/'RUNPFP.BAT'/
       DATA FZASYM/' 92-U -235 ',' 92-U -238 ',' 94-Pu-239 '/
       DATA XF/        57.0,          8.0,          35.0/
       DATA NGND/69/,NFG/14/,NRG/13/,IGREF/10/
C*
       WRITE(LTT,15) '     PSEUDO - Prepare pseudo F. P. Files'
       WRITE(LTT,15) '     ==================================='
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
           STOP 'PSEUDO ERROR - NFPE limit exceeded'
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
         READ(LINE,40) MAT(I),ZASYM(I),AZL(I),AWR(I),SPOT(I),EH(I)
         I=I+1
         READ(LIB,20,END=6) LINE
         IF(I.GT.NMAT) THEN
           STOP ' Fatal error: More than 1024 ENDF materials'
         ENDIF
       END DO
    6  KMAT=I-1
       CLOSE(LIB)
C*
C* Define the group structure
    7  WRITE(LTT,15) '$        Enter total number of groups : '
       READ (LKB,90) NGI
       IF(NGI.NE.0)   NGND =NGI
       IF(NGND.EQ.69) THEN
         NFG=14
         NRG=13
         IGREF=10
       ELSE IF(NGND.EQ.172) THEN
         NFG=45
         NRG=47
         IGREF=34
       ELSE
         WRITE(LTT,15) ' Invalid number of groups (69 or 172)   '
         GO TO 7
       END IF
C*
C*       Process *.njb files for U-235, u-238 and Pu-239
       FLNM=BLNK
       DO IT=1,2
        IF (IT.EQ.1) THEN
         NNN=KFPE
         WRITE(LTT,15) ' Define fiss. yields for explicit F.Ps  '
        ELSE
         NNN=KFP
         WRITE(LTT,15) '     Define fiss. yields for all F.Ps   '
        ENDIF
        DO KF=1,NFIS
    8    WRITE(LTT,16) '     Define the yield (*.NJB) file for: ',
     &                                                     FZASYM(KF)
         WRITE(LTT,17) '              Number of F.P. should be: ',NNN
         WRITE(LTT,15) '     ---------------------------------  '
         WRITE(LTT,15) '                     Default file name: ',FLNM
         WRITE(LTT,15) '$       Enter new name (enter=default): '
         READ (LKB,15) FLNM
         IF(FLNM.EQ.BLNK) GO TO 8
         OPEN (UNIT=LNB,FILE=FLNM,STATUS='OLD',ERR=8)
         DO I=1,NNN
          IF (IT.EQ.1) THEN
            YE(KF,I)=0.0
          ELSE
            Y(KF,I)=0.0
          ENDIF
         END DO
         READ(LNB,60)KKK
         KKK=KKK-4
         DO KK=1,KKK
           READ(LNB,60)IDUMMY,FPY
           KTRI=-1
           DO I=1,NNN
             IF((IT.EQ.1).AND.((NINT(RIDE(I)).EQ.IDUMMY))) THEN
               YE(KF,I)=FPY
               KTRI=1
             ELSE IF((IT.EQ.2).AND.((NINT(RID(I)).EQ.IDUMMY))) THEN
               Y(KF,I)=FPY
               KTRI=1
             ENDIF
           END DO
           IF(KTRI.LT.0) THEN
            WRITE(LTT,*)' Error: Lst & Yields files are not compatible'
     &                  , ' IT= ', IT, ' I= ', I, ' KF= ', KF
            STOP
           END IF
         END DO
         CLOSE(LNB)
        END DO
       END DO
C*
C*       Checking if NFP and NFPE yields are compatible.
C*       Selecting isotopes for pseudo fission product generation
       DO I=1,KFP
         IPYP(I)=0
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
            DO KF=1,NFIS
              IF (YE(KF,K) .EQ. 0.0) THEN
                IF (Y(KF,I) .EQ. 0.0 ) THEN
                  DIF=0.0
                ELSE
                  DIF=1.0
                ENDIF
              ELSE
                DIF=ABS(Y(KF,I)/YE(KF,K)-1.0)
              ENDIF
              IF (DIF.GT.1.0E-6) THEN
              WRITE(LTT,91)' Warning: Fission yields do not match ',
     &                       IAZ,Y(KF,I),YE(KF,K),KF
              ENDIF
            END DO
          ENDIF
          IF (K.GT.KFPE) II=2
         END DO
       END DO
       DO I=1,KFP
        IF (IPYP(I).GE.0) THEN
         KK=-1
         DO KF=1,NFIS
           IF(Y(KF,I).GT.0.0) KK=1
         END DO
         IF (KK.LT.0) THEN
           IPYP(I)=-3
           WRITE(LTT,92)
     &     ' Warning: Fission Product with ZA= ',RID(I),
     &     ' has null fission yield for U-235, U-238 and Pu-239'
         ENDIF
        ENDIF
       END DO
C*
C*       Open OUTPUT files
C*
C*        NJOY
       FLNM=BLNK
   10  WRITE(LTT,15) '         Define NJOY input option file: '
       WRITE(LTT,15) '         -----------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FNJ
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FNJ=FLNM
       OPEN (UNIT=LNJ,FILE=FNJ,ERR=10)
C*
C*        WILLIE
       FLNM=BLNK
   11  WRITE(LTT,15) '       Define WILLIE input option file: '
       WRITE(LTT,15) '       -------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FWLI
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FWLI=FLNM
       OPEN (UNIT=LWI,FILE=FWLI,ERR=11)
C*
C*        BATCH FILE
       FLNM=BLNK
   12  WRITE(LTT,15) '         Define command filename      : '
       WRITE(LTT,15) '         -----------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FBAT
       WRITE(LTT,15) '$       Enter new name (enter=default): '
       READ (LKB,15) FLNM
       IF(FLNM.NE.BLNK) FBAT=FLNM
       OPEN (UNIT=LBT,FILE=FBAT,ERR=12)
C* Default Unix script file (arbitrary extension --> ISYS=1)
       ISYS=1
       DO J=1,80
         IF(FBAT(J:J).EQ.'.') THEN
C* Check if DOS Batch file is required (extension ".bat" --> ISYS=0)
           IF( (FBAT(J+1:J+1).EQ.'B' .OR. FBAT(J+1:J+1).EQ.'b') .AND.
     2         (FBAT(J+2:J+2).EQ.'A' .OR. FBAT(J+2:J+2).EQ.'a') .AND.
     3         (FBAT(J+3:J+3).EQ.'T' .OR. FBAT(J+3:J+3).EQ.'t')) ISYS=0
         END IF
       END DO
C*
C*      Check if the fission products are on the library
       DO KF=1,NFIS
         YPSUM(KF)=0.0
       END DO
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
            IPYP(J)=I
            DO KF=1,NFIS
              YPSUM(KF)=YPSUM(KF)+Y(KF,J)
            END DO
          ELSE
            I=I+1
          ENDIF
          IF (I.GT.KMAT) II=2
        END DO
        IF (II.EQ.2) THEN
          IPYP(J)=-2
          WRITE(LTT,*)' Warning: Material with ZA= ',AZ(J),
     &                ' not found on the library file'
        ENDIF
       ENDIF
       END DO
       DO J=1,KFP
        I=IPYP(J)
        IF (I.GT.0) THEN
          XX=0.0
          DO KF=1,NFIS
            XP(KF,J)=Y(KF,J)/YPSUM(KF)
            XX=XX+XP(KF,J)*XF(KF)/100.0
          END DO
          IF (XX.GT.5.0E-7) THEN
            XX=SREF0/XX
          ELSE
            XX=1.0E10
          ENDIF
          CALL PNJ(LNJ,MAT(I),ZASYM(I),AWR(I),SPOT(I),
     &             EH(I),XX,RID(J),FMAT(J),NGND,NFG,NRG,IGREF)
        ENDIF
       END DO
       CALL PWL(LWI,NFIS,KFP,IPYP,FMAT,XP,YPSUM,XF,FZASYM)
       IF     (ISYS.EQ.0) THEN
C* Write DOS Batch file
         CALL PBTDOS(LBT,KFP,IPYP,FMAT,NGND)
       ELSE IF(ISYS.EQ.1) THEN
C* Write Unix script file
         CALL PBTUNX(LBT,KFP,IPYP,FMAT)
       END IF
       CLOSE(LNJ)
       CLOSE(LWI)
       CLOSE(LBT)
       STOP 'PSEUDO NORMAL END'
   15  FORMAT(2A80)
   16  FORMAT(A40,A11)
   17  FORMAT(A40,I5)
   20  FORMAT(A80)
   30  FORMAT(2F11.0)
   40  FORMAT(I4,2X,A11,F9.0,F12.0,F12.0,1PE13.0)
   50  FORMAT(A28,F8.1,A30)
   60  FORMAT(I6,E11.0)
   90  FORMAT(BN,I10)
   91  FORMAT(A38,I8,1P2E12.4,I8)
   92  FORMAT(A35,F9.1,A51)
      END
      SUBROUTINE PNJ(LNJ,MAT,ZASYM,AWR,SPOT,EH,SREF,WMAT,FNAME
     1              ,NGND,NFG,NRG,IGREF)
C*
C*    Prepare NJOY input
C*
      PARAMETER (NS0=10,NT=10,NMAX=178,N7=7,N28=28,I2=2,NP2=92)
      PARAMETER (Z0=0.0,N0=0,N1=1,N5=5,NPFLX=20000,NP1=27,NR=10)
      CHARACTER*68 LINE
      CHARACTER*11 ZASYM
      CHARACTER*9  LNAME
      CHARACTER*6  FNAME
      DIMENSION TEMP(NT),SIG0(NS0),WGHT(NMAX),P1FLX(NP1),ERW(NR)
      DIMENSION AI(N7),XLBD(N7),WSPACE(N28),P2FLX(NP2)
C*
C*    Lambda values according to F. Leszczynski using 0.98 for Be.
      DATA AI/1.007828,  9.012199,  15.990527, 26.981535,
     &       55.934505, 91.219620, 238.050980/
      DATA XLBD/ 1.000,     0.980,      0.957,     0.807,
     &           0.524,     0.372,      0.200/
C*
C*    END Library
      DATA LNAME/'ENDF/B-VI'/
C*
C*    Group boundaries for the lower 9 resonance groups in the
C*    69 energy group structure.
      DATA ERW/1425.1  , 906.898, 367.262, 148.728, 75.501,
     &           48.052,  27.7,    15.968,   9.877,  4.000/
C*
C*    General NJOY input options for fission products.
      DATA KT/1/,KS0/2/
      DATA TEMP/700.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA SIG0/1.0E10,1.0E5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA ERR/0.002/,TEMPR/0.0/,ERRMAX/0.005/
      DATA ERRTHN/0.002/
      DATA NBIN/12/,IINC/1/,ICOH/0/,IFORM/0/,NATOM/1/,MTREF/221/
      DATA TOL/0.005/,EMAX/4.0/
      DATA IGN/9/,IGG/0/,IWT/-1/,LORD/1/,MT252/252/,MF3/3/,MF6/6/
C* Neutron spectrum weighting function
      DATA WGHT/
     & 1.0000E-5, 5.250E-04, 9.0000E-3, 3.550E-01, 1.6000E-2, 5.520E-01
     &,2.4000E-2, 7.120E-01, 2.9000E-2, 7.850E-01, 3.3000E-2, 8.290E-01
     &,4.3000E-2, 8.980E-01, 5.0000E-2, 9.180E-01, 5.4000E-2, 9.210E-01
     &,5.9000E-2, 9.180E-01, 7.0000E-2, 8.920E-01, 9.0000E-2, 7.990E-01
     &,1.1200E-1, 6.860E-01, 1.4000E-1, 5.200E-01, 1.7000E-1, 3.830E-01
     &,2.1000E-1, 2.520E-01, 3.0000E-1, 1.080E-01, 4.0000E-1, 6.870E-02
     &,4.9000E-1, 5.100E-02, 5.7000E-1, 4.370E-02, 6.0000E-1, 4.130E-02
     &,1.0000E+0, 2.491E-02, 4.0000E+0, 6.786E-03, 9.1180E+3, 2.977E-06
     &,2.0000E+4, 1.413E-06, 3.0700E+4, 9.884E-07, 6.0700E+4, 5.814E-07
     &,1.2000E+5, 3.677E-07, 2.0100E+5, 2.770E-07, 2.8300E+5, 2.432E-07
     &,3.5600E+5, 2.344E-07, 3.7700E+5, 2.160E-07, 3.9900E+5, 1.738E-07
     &,4.4200E+5, 6.395E-08, 4.7400E+5, 1.381E-07, 5.0200E+5, 1.672E-07
     &,5.4000E+5, 1.936E-07, 6.5000E+5, 1.872E-07, 7.7000E+5, 1.587E-07
     &,9.0000E+5, 1.363E-07, 9.4100E+5, 1.134E-07, 1.0000E+6, 7.268E-08
     &,1.0500E+6, 9.139E-08, 1.1200E+6, 1.083E-07, 1.1900E+6, 1.228E-07
     &,1.2100E+6, 1.192E-07, 1.3100E+6, 5.451E-08, 1.4000E+6, 9.666E-08
     &,2.2200E+6, 4.684E-08, 2.3500E+6, 5.814E-08, 2.6300E+6, 3.807E-08
     &,3.0000E+6, 2.965E-08, 4.0000E+6, 1.626E-08, 5.0000E+6, 8.634E-09
     &,6.0000E+6, 4.490E-09, 8.0000E+6, 1.169E-09, 1.0000E+7, 2.947E-10
     &,1.2570E+7, 2.304E-11, 1.2600E+7, 2.236E-11, 1.2700E+7, 2.024E-11
     &,1.2800E+7, 1.832E-11, 1.2900E+7, 1.658E-11, 1.3000E+7, 1.501E-11
     &,1.3100E+7, 1.358E-11, 1.3200E+7, 1.229E-11, 1.3300E+7, 1.112E-11
     &,1.3400E+7, 1.006E-11, 1.3500E+7, 9.108E-12, 1.3600E+7, 8.242E-12
     &,1.3700E+7, 7.458E-12, 1.3800E+7, 6.748E-12, 1.3900E+7, 6.106E-12
     &,1.4070E+7, 5.151E-12, 1.4200E+7, 4.522E-12, 1.4300E+7, 4.091E-12
     &,1.4400E+7, 3.702E-12, 1.4500E+7, 3.349E-12, 1.4600E+7, 3.030E-12
     &,1.4700E+7, 2.741E-12, 1.4800E+7, 2.479E-12, 1.4900E+7, 2.243E-12
     &,1.5000E+7, 2.029E-12, 1.5100E+7, 1.835E-12, 1.5200E+7, 1.660E-12
     &,1.5300E+7, 1.501E-12, 1.5400E+7, 1.358E-12, 1.5500E+7, 1.228E-12
     &,1.5676E+7, 1.029E-12, 2.0000E+7, 1.317E-14 /
      DATA IPRW/2/,IVERW/4/,IGROUP/9/
      DATA IBURN/1/
      DATA IRES/0/,SIGP/0.0/,MTC/0/
      DATA IP1/1/,INORF/1/,IFFP/1/,ISOF/0/
      DATA NTIS/2/,EFIS/0.0/
      DATA IDCAP/0/,BR/0.0/,IDDY/0/,DY/0.0/
C*
C*     69-group British current spectrum
      DATA P1FLX/
     & 1.1800,2.763 ,4.9220,3.964 ,2.517 ,2.121 ,1.199 ,0.964 ,0.7172,
     & 0.5436,0.4331,0.3631,0.3168,0.2871,0.2677,0.231 ,0.2214,0.2143,
     & 0.2105,0.4124,0.4056,0.3008,0.2002,0.2432,0.2416,0.2094,0.3891/
C*    172-group BAPL-2 current spectrum (F.Leszczynski)
      DATA P2FLX/
     &  0.0006,0.0030,0.0054,0.0409,0.0861,0.3949,1.1086,0.7586
     & ,1.2618,3.2357,3.9678,4.9636,6.9212,4.2661,3.3180,6.0327
     & ,5.7817,2.4843,2.3472,1.8506,1.8450,1.8449,6.3888,1.7717
     & ,1.7717,0.9957,1.0067,3.1545,0.9850,0.9497,2.4542,2.5402
     & ,0.5742,1.4315,0.9019,0.8264,1.1311,0.3177,0.7200,0.2163
     & ,0.3165,1.1857,0.2828,0.8154,0.5261,0.5161,0.7468,0.2470
     & ,0.8418,0.1176,0.9349,0.2302,0.6852,0.1147,0.3433,0.4453
     & ,0.2211,0.4504,0.2174,0.8891,0.4404,0.4383,0.8372,0.6683
     & ,0.1785,0.7976,0.3989,0.2204,0.3925,0.1561,0.1384,0.1194
     & ,0.2399,0.1302,0.1512,0.1918,0.2048,0.2020,0.1953,0.2095
     & ,0.3896,0.2982,0.3914,0.2455,0.1451,0.1876,0.1864,0.2603
     & ,0.2357,0.1057,0.3682,0.0590/
C*
      IF     (NGND.EQ. 69) THEN
	      JP1= NFG+NRG
        IGN= 9
      ELSE IF(NGND.EQ.172) THEN
	      JP1= NFG+NRG
        IGN=18
      ELSE
        JP1= 0
      END IF
C*
C*     Interpolate Lambda values using cubic Spline functions.
      CALL FINSP3(AI,XLBD,N7,AWR,XLBDA,DER,1,0.0,0.0,WSPACE)
C*
C*      Select Ehigh for the flux calculator of NJOY
       II=-1
       I=1
       DO WHILE(II.LT.0)
         IF(EH.GE.ERW(I)) THEN
           EFLUX=ERW(I)
           II=1
         ELSE
           I=I+1
         END IF
         IF (I.GT.NR) THEN
          EFLUX=EH
          II=1
         ENDIF
       END DO
C*
C*      Select reference sigma0
       ISGREF=1
       KS0=1
       IF (SREF.LT.1.0E9) THEN
         SIG0(I2)=SREF
         ISGREF=I2
         KS0=I2
       ENDIF
C*
C*      Generate the file name
       IF (ZASYM(4:4).EQ.'-') THEN
         IZ=5
       ELSE IF (ZASYM(5:5).EQ.'-') THEN
         IZ=6
       ELSE
         STOP ' Fatal error: ZASYM format ERROR'
       ENDIF
       FNAME='      '
       K=1
       DO IK=IZ,11
        IF (ZASYM(IK:IK).NE.'-'.AND.ZASYM(IK:IK).NE.' ') THEN
          FNAME(K:K)=ZASYM(IK:IK)
          K=K+1
        ELSE IF (ZASYM(IK:IK).EQ.' '.AND.K.NE.1.AND.K.NE.6) THEN
          FNAME(K:K)='_'
          K=K+1
        ENDIF
       END DO
       IF(FNAME(6:6).EQ.' ') FNAME(6:6)='p'
C*
C*    Write the NJOY INPUT FILE
       WRITE(LNJ,10)'--        PSEUDO FISSION      ---          '
       WRITE(LNJ,20)'-- ',FNAME,'  PROCESS  ',ZASYM
C*   moder
       WRITE(LNJ,10)'moder  / Convert data to binary on Unit-21 '
       WRITE(LNJ,30)1,-21
       WRITE(LNJ,40)'''',LNAME,ZASYM,'''',' /'
       WRITE(LNJ,50)20,MAT
       WRITE(LNJ,60)'0 /'
C*   reconr
       WRITE(LNJ,10)'reconr / Reconstruct xs on Unit-22         '
       WRITE(LNJ,30)-21,-22
       WRITE(LNJ,65)'''',' PEND TAPE FOR ',ZASYM,' FROM ',
     &              LNAME,'''',' /'
       WRITE(LNJ,68)MAT,2,' /'
       WRITE(LNJ,80)ERR,TEMPR,ERRMAX,' /'
       WRITE(LNJ,90)'''',ZASYM,' FROM ',LNAME,'''',' /'
       WRITE(LNJ,100)'''',' PROCESSED BY NJOY-99.90+','''',' /'
       WRITE(LNJ,60)'0 /'
C*   broadr
       WRITE(LNJ,10)'broadr / Doppler broaden on Unit-23        '
       WRITE(LNJ,30)-21,-22,-23
       WRITE(LNJ,110)MAT,KT,N0,N1,TEMPR,' /'
       WRITE(LNJ,120)ERRTHN,' /'
       WRITE(LNJ,130)(TEMP(K),K=1,KT)
       WRITE(LNJ,60)'0 /'
C*   unresr
       WRITE(LNJ,10)'unresr / UR. Dopp. & Self-shield on Unit-24'
       WRITE(LNJ,30)-21,-23,-24
       WRITE(LNJ,110)MAT,KT,KS0,N1
       WRITE(LNJ,130)(TEMP(K),K=1,KT)
       WRITE(LNJ,140)(SIG0(K),K=1,KS0)
       WRITE(LNJ,60)'0 /'
C*   thermr
       WRITE(LNJ,10)'thermr / Add thermal scattering on Unit-26 '
       WRITE(LNJ,30)0,-24,-26
       WRITE(LNJ,51)N0,MAT,NBIN,KT,IINC,ICOH,IFORM,NATOM,MTREF,N1
       WRITE(LNJ,130)(TEMP(K),K=1,KT)
       WRITE(LNJ,150)TOL,EMAX
C*   groupr
       WRITE(LNJ,10)'groupr / Generate GENDF tape on Unit-25    '
       WRITE(LNJ,30)-21,-26,0,-25
       WRITE(LNJ,70)MAT,IGN,IGG,IWT,LORD,KT,KS0,N1,ISMOOTH
       WRITE(LNJ,160)'''',ZASYM,' FROM ',LNAME,'''',' /'
       WRITE(LNJ,130)(TEMP(K),K=1,KT)
       WRITE(LNJ,140)(SIG0(K),K=1,KS0)
       IF (IWT.LT.0) WRITE(LNJ,170)EFLUX,SPOT,NPFLX,' / Homogeneous'
       IF (IABS(IWT).EQ.1)THEN
         WRITE(LNJ,180)Z0,Z0,N0,N0,N1,NMAX/2
         WRITE(LNJ,190)NMAX/2,N5
         IROW=NMAX/6
         ILAST=NMAX-IROW*6
         IF (ILAST.EQ.0) THEN
           IROW=IROW-1
           ILAST=6
         ENDIF
         IROW=6*IROW
         WRITE(LNJ,200)(WGHT(K),K=1,IROW)
         WRITE(LINE,200)(WGHT(IROW+K),K=1,ILAST)
         LINE(ILAST*11+1:ILAST*11+2)=' /'
         WRITE(LNJ,205)LINE
       ENDIF
       DO K=1,KT
        WRITE(LNJ,210)MF3,' /'
        WRITE(LNJ,220)MF3,MTREF,' /'
        WRITE(LNJ,220)MF3,MT252,' /'
        WRITE(LNJ,210)MF6,' /'
        WRITE(LNJ,220)MF6,MTREF,' /'
        WRITE(LNJ,60)'0 /'
       END DO
       WRITE(LNJ,60)'0 /'
C*   wimsr
       WRITE(LNJ,10)'wimsr / Process data for WIMS              '
       WRITE(LNJ,30)-25,27
       WRITE(LNJ,30)IPRW,IVERW,IGROUP
       WRITE(LNJ,30)NGND,NFG,NRG,IGREF
       WRITE(LNJ,230)MAT,N0,WMAT,IBURN
       WRITE(LNJ,240)N0,N0,SIG0(ISGREF),IRES,SIGP,MTREF,MTC,
     &               IP1,INORF,ISOF,IFFP,JP1
       WRITE(LNJ,250)NTIS,EFIS
       WRITE(LNJ,250)IDCAP,BR
       WRITE(LNJ,250)IDDY,DY
       WRITE(LNJ,260)(XLBDA,K=1,NRG)
         IF (JP1.GT.27) THEN
           WRITE(LNJ,270)(P2FLX(K),K=1,JP1)
           GO TO 300
         ENDIF
       WRITE(LNJ,270)(P1FLX(K),K=1,JP1)
C*    stop
300    WRITE(LNJ,10)'stop                                       '
       RETURN
   10  FORMAT(A43)
   20  FORMAT(A3,A6,A11,A11)
   30  FORMAT(I3,3I4)
   40  FORMAT(A1,1X,A9,1X,A11,A1,A2)
   50  FORMAT(I3,I5,5I3,I4,I3)
   51  FORMAT(I3,I5,6I3,I4,I3)
   60  FORMAT(A3)
   65  FORMAT(A1,A15,A11,A6,A9,A1,A2)
   68  FORMAT(I4,I3,A2)
   70  FORMAT(I4,8I3)
   80  FORMAT(F5.3,F4.1,F6.3,A2)
   90  FORMAT(A1,A11,A6,A9,A1,A2)
  100  FORMAT(A1,A25,A1,A2)
  110  FORMAT(I4,3I3,F4.1,A2)
  120  FORMAT(F5.3,A2)
  130  FORMAT(F6.1,9F7.1)
  140  FORMAT(1P,E7.1,9E8.1)
  150  FORMAT(F5.3,F6.2)
  160  FORMAT(A1,A11,A6,A9,A1,A2)
  170  FORMAT(F8.2,F8.4,I6,A14)
  180  FORMAT(2F11.4,4I11)
  190  FORMAT(2I11)
  200  FORMAT(3(1PE11.4,1PE11.3))
  205  FORMAT(A68)
  210  FORMAT(I3,A2)
  220  FORMAT(I3,I4,A2)
  230  FORMAT(I4,I3,F9.1,I3)
  240  FORMAT(I2,I3,1PE8.1,I3,0PF8.4,2I4,4I3,I4)
  250  FORMAT(I6,1PE11.4)
  260  FORMAT(13F5.2)
  270  FORMAT(9F7.4)
      END
      SUBROUTINE FINSP3(XI,YI,N,XO,YO,Y1,M,F1,F2,SC)
C-Title  : FINSP3 Subroutine
C-Purpose: Interpolate a set of points using cubic spline
C-Description:
C-D  Interpolate a set of points using cubic splines. The parameters
C-D  are the following:
C-D    N   number of points on input argument mesh
C-D   XI   input argument mesh (array of N values in momotonic order)
C-D   YI   function values corresponding to XI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order)
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D   Y1   interpolated derivative values corresponding to XO(i) (Output
C-D        NOTE: if derivatives ara not required, the same array may be
C-D              specified for Y0 and Y1 (i.e.: implicit equivalence)
C-D   F1   second derivative in the first interval (usually zero)
C-D   F2   second derivative in the last  interval (usually zero)
C-D   SC   scratch array of length 3N+Max(N,M) (or more)
C-D
C-Extern.: MTXDG3
C-Literat: K.E.Atkinson, An Introduction to Numerical Analysis,
C-L        John Wiley and Sons, New York, (1978)
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1991)
      DIMENSION XI(*), YI(*), XO(*), YO(*), Y1(*), SC(*)
      IF(N.LT.3) GO TO 80
C* Define the function and the tri-diagonal matrix
      N3 = N*3
      N2 = N-2
      HO = XO(2) - XO(1)
      H2 = XI(2) - XI(1)
      Z2 = YI(2) - YI(1)
      K  = 1
      IF(HO*H2.LT.0) K =-1
      DO 30 I=1,N2
      H1 = H2
      H2 = XI(I+2) - XI(I+1)
      IF(H1*H2.LT.0) STOP 'FINSP3 non-monotonic argument'
      Z1 = Z2
      Z2 = YI(I+2) - YI(I+1)
      I3 =(I-1)*3
      SC(1+I3) = H1     / 6.
      SC(2+I3) =(H1+H2) / 3.
      SC(3+I3) =    H2  / 6.
      SC(I+N3) = Z2/H2  - Z1/H1
   30 CONTINUE
C* Solve for the second derivatives of the interpolated function
      CALL MTXDG3 (SC,SC(N3+1),SC(N3+2),N2,0)
      SC(N3+1)=F1
      SC(N3+N)=F2
C* Interpolate to the specified output grid
      L = 1
      IF(K.LT.0) L = N-1
      DO 60 I=1,M
      X = XO(I)
C* Find the appropriate input argument mesh interval
   62 X1 = XI(L+1) - X
      X0 = X       - XI(L)
      IF(X0*X1.GE.0) GO TO 64
      L = L + K
      IF(L.GE.1 .AND. L.LT.N) GO TO 62
C* Calculate the interpolated derivative and function
   64 H1 = XI(L+1) - XI(L)
      Y1(I)= (  - X1*X1*SC(N3+L) +    X0*X0*SC(N3+L+1) ) / (H1 * 2.)
     1     - (          YI(   L) -          YI(   L+1) ) /  H1
     2     + (          SC(N3+L) -          SC(N3+L+1) ) *  H1 / 6.
      YO(I)= ( X1*X1*X1*SC(N3+L) + X0*X0*X0*SC(N3+L+1) ) / (H1 * 6.)
     1     + (    X1 *  YI(   L) +    X0 *  YI(   L+1) ) /  H1
     2     - (       X1*SC(N3+L) +       X0*SC(N3+L+1) ) *  H1 / 6.
   60 CONTINUE
      RETURN
C* Special case when less than 3 input points
   80 C0 = YI(1)
      C1 = 0.
      IF(N.LT.2) GO TO 81
      C1 = (YI(2)-YI(1))/(XI(2)-XI(1))
      C0 = C0 - C1*XI(1)
   81 DO 82 I=1,M
      Y1(I) =      C1
      YO(I) = C0 + C1*XO(I)
   82 CONTINUE
      RETURN
      END
      SUBROUTINE MTXDG3(A,F,X,N,IF)
C-Title  : MTXDG3 subroutine
C-Purpose: Tridiagonal Matrix solver, Gauss elimination, no pivoting
C-Description:
C-D Solve a set of linear simultaneous equations  A x = F  (order n)
C-D assuming matrix A is tridiagonal, rows stored in first index of A.
C-D Crout-Choleski matrix decomposition, no pivoting (A = LU).
C-D Options: if=0  -decompose matrix and solve
C-D             1  -decompose matrix only
C-D             2  -solve for new F assuming matrix is decomposed
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1986
      DIMENSION A(3,N),F(N),X(N)
      N1=N-1
      IF(IF.GT.1) GO TO 45
C* Matrix decomposition - forward sweep  (A = LU)
      IF(N.LT.2) GO TO 42
      A(3,1)=-A(3,1) /  A(2,1)
      DO 40 I=2,N1
      A(2,I)= A(2,I) + A(1,I)*A(3,I-1)
   40 A(3,I)=-A(3,I) / A(2,I)
      A(2,N)= A(2,N) + A(1,N)*A(3,N1)
   42 IF(IF.GT.0) RETURN
C* Forward sweep (p = L-1 F)
   45 F(1)=  F(1)                / A(2,1)
      DO 50 I=2,N
   50 F(I)= (F(I)-A(1,I)*F(I-1)) / A(2,I)
C* Backward sweep (x = U-1 p)
      X(N)=F(N)
      DO 60 I=1,N1
      NI=N-I
   60 X(NI)= F(NI) + A(3,NI)*X(NI+1)
      RETURN
      END
      SUBROUTINE PWL(LWI,NFIS,KFP,IPYP,FMAT,XP,YPSUM,XF,FZASYM)
C*
C*      Prepare WILLIE input
C*
        CHARACTER*12 FFF(10)
        CHARACTER*11 FZASYM
        CHARACTER*6  FMAT
        DIMENSION FMAT(*),XP(NFIS,*),IPYP(*)
        DIMENSION XF(NFIS),FZASYM(NFIS),YPSUM(NFIS)
        WRITE(LWI,10)' 10. Pseudo Fission Product '
        WRITE(LWI,10)'     ---------------------- '
        WRITE(LWI,*)
        WRITE(LWI,*)
        WRITE(LWI,*)
        DO KF=1,NFIS
          FFF(KF)(1:1)=FZASYM(KF)(5:5)
          IF (FZASYM(KF)(6:6).EQ.' ') THEN
            FFF(KF)(2:2)='_'
          ELSE
            FFF(KF)(2:2)=FZASYM(KF)(6:6)
          ENDIF
          FFF(KF)(3:5)=FZASYM(KF)(8:10)
          FFF(KF)(6:12)='PFP.XSW'
          NWID=4900+KF
          WRITE(LWI,20)' Process Pseudo Fission Product for ',FZASYM(KF)
          WRITE(LWI,30)'                           XSW file ',FFF(KF)
          WRITE(LWI,35)'                 Total fission yield',YPSUM(KF)
          WRITE(LWI,*)
          WRITE(LWI,40)'MIXM'
          WRITE(LWI,50) FFF(KF)
          WRITE(LWI,55) NWID
          DO J=1,KFP
            IF (IPYP(J).GT.0) THEN
              XX=XP(KF,J)*100.00
              WRITE(LWI,60)FMAT(J),'.XSW'
              WRITE(LWI,70)XX
            END IF
          END DO
          WRITE(LWI,*)
          WRITE(LWI,*)
        END DO
        WRITE(LWI,*)
        WRITE(LWI,20)' Create Pseudo Fission Product 4902 '
        WRITE(LWI,30)'                           XSW file ',
     &               'PFP4902.XSW '
        WRITE(LWI,20)' Relative composition in percent [%]'
        DO KF=1,NFIS
          WRITE(LWI,36)FZASYM(KF),XF(KF)
        END DO
        WRITE(LWI,*)
        WRITE(LWI,40)'MIXM'
        WRITE(LWI,50)'PFP4902.XSW '
        WRITE(LWI,55) 4902
        DO KF=1,NFIS
          WRITE(LWI,50)FFF(KF)
          WRITE(LWI,70)XF(KF)
        END DO
        WRITE(LWI,*)
        WRITE(LWI,*)
        WRITE(LWI,40)'END '
        RETURN
   10   FORMAT(6X,A28)
   20   FORMAT(6X,A36,1X,A11)
   30   FORMAT(6X,A36,1X,A12)
   35   FORMAT(6X,A36,1X,1PE11.5)
   36   FORMAT(12X,A11,1X,F10.4)
   40   FORMAT(A4)
   50   FORMAT(A12)
   55   FORMAT(I6)
   60   FORMAT(A6,A4)
   70   FORMAT(1PE11.5)
      END
      SUBROUTINE PBTDOS(LBT,KFP,IPYP,FMAT,NGND)
C*
C*      Prepare DOS batch file to run NJOY and WILLIE
C*
      CHARACTER*6 FMAT(*)
      DIMENSION IPYP(*)
      WRITE(LBT,10)'@echo off'
      WRITE(LBT,10)'ECHO.    '
      WRITE(LBT,20)'ECHO     NJOY PROCESSING OF PSEUDO F. P. FOR WLUP  '
      WRITE(LBT,20)'ECHO     ----------------------------------------  '
      WRITE(LBT,10)'REM      '
      WRITE(LBT,20)'REM  Usage: BATCHNAME  %1                          '
      WRITE(LBT,20)'REM    BATCHNAME: This batch file name.            '
      WRITE(LBT,20)'REM      (Usually: endfb6fp, jef22fp, cendlfp, etc)'
      WRITE(LBT,20)'REM    %1: Evaluated Nuclear Data Library          '
      WRITE(LBT,20)'REM      (6 character string)                      '
      WRITE(LBT,20)'REM      (ie., endfb6, jef22, cendl , etc)         '
      WRITE(LBT,20)'REM  Examples:                                     '
      WRITE(LBT,20)'REM    endfb6fp   endfb6                           '
      WRITE(LBT,20)'REM    jef22fp    jef22                            '
      WRITE(LBT,10)'REM      '
      WRITE(LBT,20)'IF .%WLUPP%==.        GOTO  ERR                    '
      WRITE(LBT,20)'IF NOT EXIST  TAPE20  GOTO  ERR                    '
      WRITE(LBT,10)'ECHO.    '
      WRITE(LBT,20)'ECHO %WLUPI%%1fp.NJI    >INP.                      '
      WRITE(LBT,20)'ECHO.                >>INP.                        '
      WRITE(LBT,20)'   %WLUPP%NJISPL<INP.                              '
      DO J=1,KFP
        IF(IPYP(J).GT.0) THEN
          WRITE(LBT,30)'CALL %WLUPP%runone ',FMAT(J)
        ENDIF
      END DO
      WRITE(LBT,20)'%WLUPP%willie<%WLUPI%%1fp.WLI                      '
      IF(NGND.EQ.69) THEN
       WRITE(LBT,20)'COPY PFP4902.XSW  %WLUPI%%1fp.XSW                 '
      ELSE
       WRITE(LBT,20)'COPY PFP4902.XSW  %WLUPI%%1gx.XSW                 '
      END IF
      WRITE(LBT,20)'ECHO  *****  NORMAL ENDING  *****                  '
      WRITE(LBT,10)'GOTO EXIT'
      WRITE(LBT,10)':ERR     '
      WRITE(LBT,20)'ECHO  ***** ABNORMAL ENDING *****                  '
      WRITE(LBT,10)':EXIT    '
      WRITE(LBT,20)'DEL INP.                                           '
   10 FORMAT(A9)
   20 FORMAT(A51)
   30 FORMAT(A19,A6)
      RETURN
      END
      SUBROUTINE PBTUNX(LBT,KFP,IPYP,FMAT)
C*
C*      Prepare UNIX script file to run NJOY aand WILLIE
C*
      CHARACTER*6 FMAT(*)
      DIMENSION IPYP(*)
      WRITE(LBT,20)"#!/bin/tcsh                                        "
      WRITE(LBT,20)"echo  '  ======================================= ' "
      WRITE(LBT,20)"echo  '  NJOY PROCESS. OF PSEUDO F. P. FOR WLUP  ' "
      WRITE(LBT,20)"echo  '  ======================================= ' "
      WRITE(LBT,20)"echo \\\\n                                         "
      WRITE(LBT,20)"echo 'Usage: BATCHNAME  %1                        '"
      WRITE(LBT,20)"echo '  BATCHNAME: This batch file name.          '"
      WRITE(LBT,20)"echo '  (Usually: endfb6fp, jef22fp, cendlfp, etc)'"
      WRITE(LBT,20)"echo '  %1: Evaluated Nuclear Data Library        '"
      WRITE(LBT,20)"echo '    (6 character string)                    '"
      WRITE(LBT,20)"echo '    (ie., endfb6, jef22, cendl , etc)       '"
      WRITE(LBT,20)"echo 'Examples:                                   '"
      WRITE(LBT,20)"echo '  endfb6fp   endfb6                         '"
      WRITE(LBT,20)"echo '  jef22fp    jef22                          '"
      WRITE(LBT,20)"echo '                                            '"
      WRITE(LBT,20)"if ( $?U_D == 0 ) setenv  U_D  ../..               "
      WRITE(LBT,20)"if ( $?WLUPP == 0 )setenv WLUPP $U_D/WLUP/PROGRAMS "
      WRITE(LBT,20)"if ( $?WLUPS == 0 )setenv WLUPS $U_D/WLUP/SCRIPTS  "
      WRITE(LBT,20)"if ( $?WLUPI == 0 )setenv WLUPI $U_D/WLUP/INPUTS   "
      WRITE(LBT,20)"if ( $?WLUPN == 0 )setenv WLUPN $U_D/NJOY97        "
      WRITE(LBT,20)"set lb1 = $<                                       "
      WRITE(LBT,20)"if ( ! -e tape20 ) goto ERR                        "
      WRITE(LBT,20)"if ( -e INP. ) rm INP.                             "
      WRITE(LBT,20)"echo $WLUPI/{$lb1}.NJI  >INP.                      "
      WRITE(LBT,20)"echo ' '    >>INP.                                 "
      WRITE(LBT,20)"$WLUPP/njispl<INP.                                 "
      DO J=1,KFP
        IF(IPYP(J).GT.0) THEN
      WRITE(LBT,20)"if ( -e  nji.file ) rm nji.file                    "
      WRITE(LBT,80)"echo ",FMAT(J)," >> nji.file                       "
      WRITE(LBT,20)"$WLUPS/runone.unx < nji.file                       "
        ENDIF
      END DO
      WRITE(LBT,20)"$WLUPP/willie<$WLUPI/{$lb1}.WLI                    "
      WRITE(LBT,20)"cp   PFP4902.XSW  $WLUPI/{$lb1}.XSW                "
      WRITE(LBT,20)"echo '*****  NORMAL ENDING  *****'                 "
      WRITE(LBT,10)"GOTO EXIT                                          "
      WRITE(LBT,10)"ERR:                                               "
      WRITE(LBT,20)"echo '***** ABNORMAL ENDING *****'                 "
      WRITE(LBT,10)"EXIT:                                              "
      WRITE(LBT,20)"rm  INP.                                           "
   10 FORMAT(A9)
   20 FORMAT(A51)
   30 FORMAT(A19,A6)
   80 FORMAT(A5,A6,A38)
      RETURN
      END
