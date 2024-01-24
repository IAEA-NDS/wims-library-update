C-Title  : WEDSPE Program
C-Purpose: Extract spectra from WIMS output
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia
C-Version: 1992 Original code
C-V  02/03 A.Trkov
C-V        - Printout control from input.
C-V        - 172-group capability.
C-M
C-M  Manual for Program WEDSPE
C-M  -------------------------
C-M
C-M  The WIMS output is processed and the calculated spectra are
C-M  extracted and pronted in PLOTTAB Format
C-M
C-M  NOTES:
C-M   - Group energy boundaries are required to calculate the spectra.
C-M     At present, the 69-group WIMS-D and the 172-group X-mas scheme
C-M     energy boundaries are built into the code. WEDSPE will not work
C-M     (or not work correctly) with any other group scheme.
C-M   - Transport group schemes are defined with the FEWGROUP command
C-M     in WIMS-D input. The present version of WEDSPE has not been
C-M     coded to read this data. The only options allowed are:
C-M      * full group scheme (any library),
C-M      * 32-group transport scheme (69-group library only). WEDSPE
C-M        will not work (or not work correctly) with any other
C-M        transport group scheme.
C-
      PARAMETER   (MXGR=200)
      CHARACTER*8  HDR ,SR1A,SR1B
      CHARACTER*30 SR0A,SR0B,SRC1,SRC2,RECI
      CHARACTER*40 COMM,BLNK,FLNM,FLIN,FLOU
      DIMENSION    NGB(MXGR),MGB(MXGR),US(MXGR),SPE(MXGR),SPI(MXGR)
      DIMENSION    NG1(32)  ,US2(69) ,US3(172)
C* File units
      DATA LIN,LOU,LKB,LTT/ 1, 2, 5, 6 /
C* Default filenames
      DATA BLNK/'                                        '/
     1     FLIN/'WIMSDOUT'/
     2     FLOU/'WEDSPE.CUR'/
C* Search strings for Spectrox, Cell and Leakage spectra
      DATA SR1A,SR1B /' DUMMY  ',' DUMMY  '/
     1    ,SR0A /'1FLUX.                        '/
     1    ,SR0B /'1flux.                        '/
     1    ,SRC1 /'1FEW-GROUP REGIONAL AND CELL E'/
     2    ,SRC2 /' GROUP     DIFFUSION    DIFFUS'/
C* Assumed WIMS transport FEWGROUP structure
      DATA NG1 /
     1   2, 4, 6,11,14,21,23,25,26,27,28,29,30,32,33,35,38,40,41,43,45
     2 ,47,49,52,54,55,56,57,60,63,66,69/
C* WIMS group energy boundaries (69 and 172)
      DATA US2/
     1  10.0000E6,   6.0655E6,   3.6790E6,   2.2310E6,   1.3530E6,
     1   0.8210E6,   0.5000E6,   0.3025E6,   0.1830E6,   0.1110E6,
     2  67.3400E3,  40.8500E3,  24.7800E3,  15.0300E3,   9.1180E3,
     2   5.5300E3,   3.5191E3,   2.23945E3,  1.4251E3, 906.898   ,
     3 367.262   , 148.728   ,  75.5014   , 48.05    ,  27.7     ,
     3  15.968   ,   9.877   ,   4.0      ,  3.3     ,   2.6     ,
     4   2.1     ,   1.5     ,   1.3      ,  1.15    ,   1.123   ,
     4   1.097   ,   1.071   ,   1.045    ,  1.02    ,   0.996   ,
     5   0.972   ,   0.950   ,   0.91     ,  0.85    ,   0.78    ,
     5   0.625   ,   0.5     ,   0.4      ,  0.35    ,   0.32    ,
     6   0.3     ,   0.28    ,   0.25     ,  0.22    ,   0.18    ,
     6   0.14    ,   0.1     ,   0.08     ,  0.067   ,   0.058   ,
     7   0.05    ,   0.042   ,   0.035    ,  0.03    ,   0.025   ,
     7   0.02    ,   0.015   ,   0.01     ,  0.005 /
      DATA US3/
     & 1.96403E+7, 1.73325E+7, 1.49182E+7, 1.38403E+7, 1.16183E+7,
     & 1.00000E+7, 8.18731E+6, 6.70320E+6, 6.06531E+6, 5.48812E+6,
     & 4.49329E+6, 3.67879E+6, 3.01194E+6, 2.46597E+6, 2.23130E+6,
     & 2.01897E+6, 1.65299E+6, 1.35335E+6, 1.22456E+6, 1.10803E+6,
     & 1.00259E+6, 9.07180E+5, 8.20850E+5, 6.08101E+5, 5.50232E+5,
     & 4.97871E+5, 4.50492E+5, 4.07622E+5, 3.01974E+5, 2.73237E+5,
     & 2.47235E+5, 1.83156E+5, 1.22773E+5, 1.11090E+5, 8.22975E+4,
     & 6.73795E+4, 5.51656E+4, 4.08677E+4, 3.69786E+4, 2.92830E+4,
     & 2.73944E+4, 2.47875E+4, 1.66156E+4, 1.50344E+4, 1.11378E+4,
     & 9.11882E+3, 7.46586E+3, 5.53084E+3, 5.00451E+3, 3.52662E+3,
     & 3.35463E+3, 2.24867E+3, 2.03468E+3, 1.50733E+3, 1.43382E+3,
     & 1.23410E+3, 1.01039E+3, 9.14242E+2, 7.48518E+2, 6.77287E+2,
     & 4.53999E+2, 3.71703E+2, 3.04325E+2, 2.03995E+2, 1.48625E+2,
     & 1.36742E+2, 9.16609E+1, 7.56736E+1, 6.79041E+1, 5.55951E+1,
     & 5.15780E+1, 4.82516E+1, 4.55174E+1, 4.01690E+1, 3.72665E+1,
     & 3.37201E+1, 3.05113E+1, 2.76077E+1, 2.49805E+1, 2.26033E+1,
     & 1.94548E+1, 1.59283E+1, 1.37096E+1, 1.12245E+1, 9.90555E+0,
     & 9.18981E+0, 8.31529E+0, 7.52398E+0, 6.16012E+0, 5.34643E+0,
     & 5.04348E+0, 4.12925E+0, 4.00000E+0, 3.38075E+0, 3.30000E+0,
     & 2.76792E+0, 2.72000E+0, 2.60000E+0, 2.55000E+0, 2.36000E+0,
     & 2.13000E+0, 2.10000E+0, 2.02000E+0, 1.93000E+0, 1.84000E+0,
     & 1.75500E+0, 1.67000E+0, 1.59000E+0, 1.50000E+0, 1.47500E+0,
     & 1.44498E+0, 1.37000E+0, 1.33750E+0, 1.30000E+0, 1.23500E+0,
     & 1.17000E+0, 1.15000E+0, 1.12535E+0, 1.11000E+0, 1.09700E+0,
     & 1.07100E+0, 1.04500E+0, 1.03500E+0, 1.02000E+0, 9.96000E-1,
     & 9.86000E-1, 9.72000E-1, 9.50000E-1, 9.30000E-1, 9.10000E-1,
     & 8.60000E-1, 8.50000E-1, 7.90000E-1, 7.80000E-1, 7.05000E-1,
     & 6.25000E-1, 5.40000E-1, 5.00000E-1, 4.85000E-1, 4.33000E-1,
     & 4.00000E-1, 3.91000E-1, 3.50000E-1, 3.20000E-1, 3.14500E-1,
     & 3.00000E-1, 2.80000E-1, 2.48000E-1, 2.20000E-1, 1.89000E-1,
     & 1.80000E-1, 1.60000E-1, 1.40000E-1, 1.34000E-1, 1.15000E-1,
     & 1.00001E-1, 9.50000E-2, 8.00000E-2, 7.70000E-2, 6.70000E-2,
     & 5.80000E-2, 5.00000E-2, 4.20000E-2, 3.50000E-2, 3.00000E-2,
     & 2.50000E-2, 2.00000E-2, 1.50000E-2, 1.00000E-2, 6.90000E-3,
     & 5.00000E-3, 3.00000E-3/
C* Default number of transport and library groups
      NG =69
      NGL=69
      ILTH=0
      IHST=0
      DO 8 I=1,MXGR
      MGB(I)=I
    8 CONTINUE
C*
C* Print banner
      WRITE(LTT,96)
      WRITE(LTT,96) ' WEDSPE - Process spectra from WIMS-D   '
      WRITE(LTT,96) ' ====================================   '
      WRITE(LTT,96)
C* Define and open WIMS output file to be processed
      WRITE(LTT,96) '$Enter WIMS output filename           : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) FLIN=FLNM
      OPEN (UNIT=LIN,FILE=FLIN,STATUS='OLD')
C* Find the number of library groups
   11 READ (LIN,91,END=82) RECI
      IF(RECI(16:30).NE.'entry into main' .AND.
     &   RECI(16:30).NE.'ENTRY INTO MAIN') GO TO 11
      READ (LIN, * ) NMT,NGL
      WRITE(LTT, 97) ' Number of library materials          : ',NMT
      WRITE(LTT, 97) ' Number of library energy groups      : ',NGL
      IF     (NGL.EQ. 69) THEN
        DO 12 I=1,NGL
        US(I)=US2(I)
   12   CONTINUE
        US(NGL+1)=1.E-4
      ELSE IF(NGL.EQ.172) THEN
        DO 13 I=1,NGL
        US(I)=US3(I)
   13   CONTINUE
        US(NGL+1)=1.E-5
      ELSE
        STOP 'WEDSPE ERROR - Only 69 and 172 group library supported'
      END IF
C* Find the number of transport groups
   14 READ (LIN,91,END=84) RECI
      I1=0
   15 I1=I1+1
      IF(I1.GT.7) GO TO 14
      IF(RECI(I1:I1).EQ.' ') GO TO 15
      IF(RECI(I1:I1+3).NE.'NGRO' .AND. RECI(I1:I1+3).NE.'ngro') GO TO 14
   16 I1=I1+1
      IF(RECI(I1:I1).NE.' ') GO TO 16
      READ (RECI(I1:30), * ) NG
      WRITE(LTT, 97) ' Number of transport groups           : ',NG
      IF     (NG.EQ. 32) THEN
        IF(NGL.EQ.69) THEN
          DO 17 I=1,NG
          NGB(I)=NG1(I)
   17     CONTINUE
        ELSE
          STOP 'WEDSPE ERROR - 32g transport with 69g lib.only'
        END IF
      ELSE
        DO 18 I=1,NG
        NGB(I)=I
   18   CONTINUE
      END IF
C* Select spectra types to be processed:
      IP11=0
      IP11=0
      IP21=0
      IP22=0
      IP23=0
      IP31=0
      WRITE(LTT,96) '$    Process fuel Spectrox (No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IP11=1
      WRITE(LTT,96) '$ Process coolant Spectrox (No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IP12=1
C*
C...  WRITE(LTT,96) '$Process transport spectra (No=blank) : '
C...  READ (LKB,96) FLNM
C...  IF(FLNM.EQ.BLNK) GO TO 19
C...  IP21=1
      WRITE(LTT,96) '$   Process region spectra (No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IP22=1
      WRITE(LTT,96) '$Process cell aver.spectra (No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IP23=1
      IF(IP22.GT.0 .OR. IP23.GT.0) IP21=1
C*
   19 WRITE(LTT,96) '$  Process leakage spectra (No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IP31=1
C*
      WRITE(LTT,96)
      WRITE(LTT,96) ' Output spectra representation          '
      WRITE(LTT,96) '$  Output Lethargy spectra?(No=blank) : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) ILTH=1
      WRITE(LTT,96) '$  Histogram? (Smooth linear=blank)   : '
      READ (LKB,96) FLNM
      IF(FLNM.NE.BLNK) IHST=1
C*
C* Spectra output file in PLOTTAB format
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN'
     1     ,CARRIAGECONTROL='LIST')
C* Find the output sections containing spectra
   20 READ (LIN,91,END=90) RECI
C* Fuel and coolant spectra from spectrox calculation
      IF((IP11.EQ.1 .OR. IP12.EQ.1) .AND.
     1   (RECI     .EQ.SR0A .OR. RECI.EQ.SR0B) ) GO TO 21
C* Spectra from the transport solution
      IF( IP21.EQ.1 .AND. RECI     .EQ.SRC1) GO TO 30
C*      Region-wise spectrum printout
      IF( IP22.EQ.1 .AND. RECI(1:8).EQ.SR1A) GO TO 31
C*      Cell average spectrum
      IF( IP23.EQ.1 .AND. RECI(1:8).EQ.SR1B) GO TO 31
C* Spectrum from the leakage edit
      IF( IP31.EQ.1 .AND. RECI     .EQ.SRC2) GO TO 40
      GO TO 20
C*
C* Process the spectrum from the SPECTROX calculation
   21 WRITE(LTT,96) ' SPECTROX spectrum to be processed      '
      READ (LIN,91,END=90) RECI
      READ (LIN,91)        RECI
C* Loop for the fuel and the coolant spectrum
      DO 26 ISP=1,2
      READ (LIN,91)        RECI
      READ (LIN,91)        RECI
      READ (LIN,93,ERR=20) HDR,(SPI(I),I=1,NGL)
      COMM=BLNK
      COMM(1:8)=HDR
      IF     (HDR.EQ.'  fuel  ') THEN
        IF(IP11.NE.1) GO TO 26
      ELSE IF(HDR.EQ.'  coolnt') THEN
        IF(IP12.NE.1) GO TO 26
      ELSE
        GO TO 20
      END IF
      CALL PRNTSP(LOU,ILTH,IHST,COMM,NG,MGB,US,SPI)
   26 CONTINUE
      GO TO 20
C*
C* Process the Region and Cell spectra output
   30 WRITE(LTT,96) ' Region and Cell spectrum processed     '
      SR1A='0REGION '
      SR1B=' CELL   '
      GO TO 20
   31 RECI(1:1)=' '
      COMM=RECI
      READ (LIN,91)
      READ (LIN,91)
      DO 32 I=1,NG
      READ (LIN,92) IDM,RDM,RDM,RDM, RDM,SPE(I)
   32 CONTINUE
      CALL PRNTSP(LOU,ILTH,IHST,COMM,NG,NGB,US,SPE)
      GO TO 20
C*
C* Process the Leakage edit spectra output
   40 WRITE(LTT,96) ' LEAKAGE Spectrum to be processed       '
      DO 42 I=1,NG
      READ (LIN,94) IDM,RDM,RDM,RDM,RDM,RDM,SPE(I),SPI(I)
   42 CONTINUE
      COMM=' LEAKAGE EFF.SP.                        '
      CALL PRNTSP(LOU,ILTH,IHST,COMM,NG,NGB,US,SPE)
      COMM=' LEAKAGE INF.SP.                        '
      CALL PRNTSP(LOU,ILTH,IHST,COMM,NG,NGB,US,SPI)
      GO TO 20
C* Error traps
   82 STOP 'WEDSPE ERROR - Library groups not found'
   84 STOP 'WEDSPE ERROR - Transport groups not found'
C* File processing completed
   90 STOP 'WEDSPE Completed'
   91 FORMAT(A30)
   92 FORMAT(I5,2X,3E13.5,5X,2E13.5)
   93 FORMAT(A8,9F12.0/(8X,9F12.0))
   94 FORMAT(I4,3X,7E14.5)
   96 FORMAT(2A40)
   97 FORMAT(A40,I6)
   98 FORMAT(1P,2E11.4)
      END
      SUBROUTINE PRNTSP(LOU,ILTH,IHST,COMM,NG,NGB,EGB,SPE)
C-Title  : Subroutine PRINTSP
C-Purpose: Print the spectrum in PLOTTAB curves format
C-Description:
C-D  LOU  Logical output unit
C-D  ILTH Spectrum type flag:
C-D        0  Spectrum per unit energy
C-D        1  Spectrum per unit lethargy
C-D  IHST Spectrum interpolation flag:
C-D        0  Linear between centroid of energy boundaries
C-D        1  Histogram
C-D  COMM Header comment for curve identification
C-D  NG   Number of points
C-D  NGB  Index array to the energy boundaries.
C-D  EGB  Energy boundaries (in descending order)
C-D  SPE  Spectrum values (integrated over energy group).
C-
      CHARACTER*40 COMM
      DIMENSION    NGB(1),EGB(1),SPE(1)
C*
C* Print the header
      WRITE(LOU,93) COMM
C*
C* Spectrum normalisation constant
      SSP= 0
      DO 32 I=1,NG
      SSP= SSP+SPE(I)
   32 CONTINUE
C*
C* Print the spectrum in ascending energy order
      N2 = NGB(NG)
      E2 = EGB(N2+1)
      DO 34 J=1,NG
      I  = NG+1-J
      N2 = NGB(I)
      E1 = E2
      E2 = EGB(N2)
      IF(ILTH.EQ.1) THEN
C* Case:  Print the LETHARGY spectrum
        FF = SSP*ALOG(E2/E1)
        IF(IHST.EQ.1) THEN
C*        Histogram representation
          WRITE(LOU,98) E1,SPE(I)/FF
          WRITE(LOU,98) E2,SPE(I)/FF
        ELSE
C*        Linear representation
          IF(E1.GT.0) THEN
            EE=SQRT(E1*E2)
          ELSE
            EE=E2/2
          END IF
          WRITE(LOU,98) EE,SPE(I)/FF
        END IF
      ELSE
C* Case: Print ENERGY spectrum (scaled by 1.E12)
        FF = SSP*    (E2-E1) * 1.e-12
        IF(IHST.EQ.1) THEN
C*        Histogram representation
          WRITE(LOU,98) E1,SPE(I)/FF
          WRITE(LOU,98) E2,SPE(I)/FF
        ELSE
C*        Linear representation with intermediate points
          IF(J.EQ. 1) THEN
            WRITE(LOU,98) 1.E-5,0.
            C2=SQRT(E1*E2)
            F2=SPE(I)/FF
            WRITE(LOU,98) C2,F2
          ELSE IF(J.EQ.NG) THEN
            F1=F2
            F2=SPE(I)/FF
            C1=C2
            C2=SQRT(E1*E2)
            FB=EXP( LOG(F1)+LOG(F2/F1)*LOG(E1/C1)/LOG(C2/C1) )
            FU=EXP( LOG(F1)+LOG(F2/F1)*LOG(E2/C1)/LOG(C2/C1) )
            WRITE(LOU,98) E1,FB
            WRITE(LOU,98) C2,F2
            WRITE(LOU,98) E2,FU
            WRITE(LOU,98) 2.E+7,0.
          ELSE
            F1=F2
            F2=SPE(I)/FF
            C1=C2
            C2=SQRT(E1*E2)
            FB=EXP( LOG(F1)+LOG(F2/F1)*LOG(E1/C1)/LOG(C2/C1) )
            WRITE(LOU,98) E1,FB
            WRITE(LOU,98) C2,F2
          END IF
        END IF
      END IF
   34 CONTINUE
      WRITE(LOU,93)
      RETURN
C*
   93 FORMAT(A40)
   98 FORMAT(1P,2E11.4)
      END
