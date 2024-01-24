      PROGRAM WEDB1B
C-Title  : WEDB1B Program
C-Purpose: Analysis of the Burnup Credit Criticality Benchmark PART-1B
C-Author : D.L.Aldama, A.Trkov (1998)
C-Version:
C-V 1999/03 - Extension to process Plutonium Recycling benchmark
C-V 2006/09 - Minor additions to comments and manual.
C-V         - Add convention that burnup with -ve sign in the reference
C-V           solution file includes cooling time.
C-M
C-M  Manual for Porgram WEDB1B
C-M  =========================
C-M
C-M    The program analyses the WIMS-D results of the NEA/CRP Burnup
C-M  Credit Benchmark Part-1B and the Physics of Plutonium Recycling
C-M  Benchmark Case A.
C-M    By default, the isotopic composition and the k-inf. for each
C-M  burnup step are printed to the output list file. If the reference
C-M  results filename is specified, the LAST calculated isotopic
C-M  composition is compared to the reference one at the appropriate
C-M  burnup step. A bar-graph diagram showing differences from the
C-M  reference is printed to WEDB1B.PLT file (fixed filename).
C-M    The reference results are read from a file apecified on
C-M  input. The program assumes that at least the leading 5 records
C-M  are comments and copies them to output. The burnup is
C-M  identified from the record beginning with " BURNUP = ".
C-M  The next record is skipped. The records that follow are
C-M  expected to contain the information on the isotopic
C-M  composition as follows:
C-M    Columns     1  Blank
C-M             2- 8  Chemical symbol
C-M             9-20  Concentration [mg/gUO2]
C-M            21-26  Uncertainty (2 sigma) [%]
C-M            27-32  Ratio: Actual/Model
C-M  The concentration ratio allows the correction for the
C-M  difference between the actual fuel pellet environment and
C-M  the idealised benchmark mode. It is applicable to the
C-M  comparison with the measured values. By default it is
C-M  equal to 1.
C-M    There are more than one data sets on the reference file.
C-M  The data following the burnup are chosen where the burnup
C-M  differs by less than 1 MWd/tU from the last burnup step on
C-M  the WIMS output.
C-M
C-M  The program was originally written for the Cuban version of the
C-M  WIMS-D/4 output. It was modified for compatibility with WIMSD5.
C-M  Also, the command line input was changed to read the filenames
C-M  from the keyboard explicitly.
C-
      PARAMETER (NN=3, N0=45, NK=100, NW=120)
      CHARACTER*120 LINE
      CHARACTER*50  BLNK,FLNM,FLWO,FLOU,FLRF,FLPL
      CHARACTER*7   CDW(N0)
      DIMENSION IDW(N0,4),AW(N0), XMAS(N0),INDX(N0)
     1         ,XREF(N0),SREF(N0),WREF(N0),DN(N0)
     1         ,BURNUP(NK),XKINF(NK),DAYS(NK)
C* Default materials list
      DATA CDW/'U -234 ','U -235 ','U -236 ','U -238 ','Np-237 ',
     1         'Pu-238 ','Pu-239 ','Pu-240 ','Pu-241 ','Pu-242 ',
     2         'Am-241 ','Am-242m','Am-243 ','Cm-242 ','Cm-243 ',
     2         'Cm-244 ','Cm-245 ','Mo- 95 ','Tc- 99 ','Ru-101 ',
     3         'Rh-103 ','Pd-105 ','Pd-107 ','Pd-108 ','Ag-109 ',
     3         'Xe-131 ','Xe-135 ','Cs-133 ','Cs-135 ','Nd-143 ',
     4         'Nd-145 ','Pm-147 ','Sm-147 ','Pm-148m','Sm-149 ',
     4         'Sm-150 ','Sm-151 ','Sm-152 ','Eu-153 ','Eu-154 ',
     5         'Eu-155 ','Gd-155 ','Gd-157 ','Gd-157 ','O - 16 '/
C* Atomic weights of the selected materials
      DATA AW /234.040527,235.044144,236.045761,238.050980,237.048157,
     &         238.049469,239.052582,240.054199,241.048737,242.058411,
     &         241.056793,242.059   ,243.061035,242.058   ,243.061   ,
     &         244.063   ,245.064   , 94.905891, 99.000465,100.905838,
     &         102.905014,104.905   ,106.905   ,107.904   ,108.904549,
     &         130.906   ,134.907   ,132.905731,134.905914,142.909683,
     &         144.912872,146.915   ,146.915085,147.918   ,148.917282,
     &         149.916870,150.919479,151.920074,152.921677,153.922   ,
     &         154.923   ,154.922867,156.924072,156.924072, 15.999400/
C* WIMS ident numbers with aliases(allow for "1981" & "1986" libraries)
      DATA IDW/    234  ,   2235  ,    236  ,   8238  ,    937  ,
     &             948  ,   6239  ,   1240  ,   1241  ,   1242  ,
     &             951  ,    952  ,    953  ,    962  ,    963  ,
     &             964  ,    965  ,   4095  ,   4099  ,   4101  ,
     &            4103  ,   5105  ,   4107  ,   4108  ,   4109  ,
     &            4131  ,   4135  ,   4133  ,   5135  ,   4143  ,
     &            4145  ,   4147  ,   6147  ,   4148  ,   4149  ,
     &            4150  ,   4151  ,   4152  ,   4153  ,   4154  ,
     &            4155  ,   2155  ,   2157  ,   4157  ,   6016  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,   5147  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,    235  ,      0  ,   2238  ,      0  ,
     &               0  ,   3239  ,      0  ,    241  ,    242  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,     95  ,     99  ,    101  ,
     &             103  ,   1105  ,    107  ,    108  ,    109  ,
     &             131  ,    135  ,    133  ,   1135  ,    143  ,
     &             145  ,    147  ,   2147  ,    148  ,    149  ,
     &             150  ,    151  ,    152  ,    153  ,    154  ,
     &               0  ,      0  ,   4157  ,    157  ,     16  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,   1147  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  ,
     &               0  ,      0  ,      0  ,      0  ,      0  /
C*
      DATA BLNK/'                                        '/
     1     FLWO/'BUCR1B.OUT'/
     2     FLOU/'WEDB1B.LST'/
     3     FLRF/'BUCR1B.REF'/
     4     FLPL/'WEDB1B.PLT'/
C*
      DATA LIN,LOU,LRF,LPL,LKB,LTT/ 1, 2, 3, 4, 5, 6 /
C*
      DATA FMAS/ 0. /
C*
      WRITE(LTT,'(2A40)')' WEDB1B - Isotopic composition compariso'
      WRITE(LTT,'(2A40)')' ======================================='
      WRITE(LTT,'(2A40)')
C* Define the filenames
   12 WRITE(LTT,'(2A40)')' Enter the WIMS output filename       : '
      READ (LKB,'(2A40)')FLWO
      OPEN (UNIT=LIN,FILE=FLWO,STATUS='OLD',ERR=12)
C*
   14 WRITE(LTT,'(2A40)')' Default output report filename       : ',FLOU
      WRITE(LTT,'(2A40)')'           Enter new name to redefine : '
      READ (LKB,'(2A40)')FLNM
      IF(FLNM.NE.BLNK) FLOU=FLNM
      OPEN(UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      WRITE(LOU,'(2A40)')' WEDB1B - Isotopic composition compariso'
     &                  ,'n                                       '
      N=2
C*
   16 WRITE(LTT,'(2A40)')' Enter reference results filename     : '
      READ (LKB,'(2A40)')FLNM
      IF(FLNM.NE.BLNK) THEN
        FLRF=FLNM
        OPEN(UNIT=LRF,FILE=FLRF,STATUS='OLD',ERR=16)
        OPEN(UNIT=LPL,FILE=FLPL,STATUS='UNKNOWN')
        N=3
      END IF
C*
        IK=0
        BURNUP(1)=0
        DAYS(1)=0
C* Process the WIMS-D output until End-of-file
        READ(LIN,'(A120)')LINE
        DO WHILE (LINE(1:15).NE.' END OF FILE ON')
C*        -- Beginning of a burnup sequence identified
          IF (LINE(27:34).EQ.'chain 14') THEN
            IK=IK+1
            IF(IK.GT.NK) STOP 'WEDB1B ERROR - NK limit exceeded'
            IF (IK.EQ.1 .AND. N.EQ.2) THEN
              WRITE(LOU,'(A32)')'********************************'
              WRITE(LOU,'(A32)')' INITIAL COMPOSITION            '
            ENDIF
C*          -- First K-eff in leakage edit from previous burnup step
            CALL GETK(LIN,XKINF(IK),1)
C* Print the data for the burnup step
            IF(N.EQ.2) THEN
              WRITE(LOU,'(A32)')'********************************'
              WRITE(LOU,*)
              WRITE(LOU,'(A12,I6)')' Burnup step',IK-1
              WRITE(LOU,'('' Burnup   '',F8.2,'' GWd/tU'')') BURNUP(IK)
              WRITE(LOU,'(A10,F8.5)')' k-inf    ',XKINF(IK)
              IF(NT.GT.0) THEN
                WRITE(LOU,*)
                WRITE(LOU,'(A8,A12,A6,A16)')' ISOTOPE',' M[mg/gFuel]'
     &                                     ,'   ID ','  N[1/(barn.cm)]'
                DO I=1,NT
                  K=INDX(I)
                  WRITE(LOU,'(A8,1P,E12.3,I6,E15.7)')
     &                           CDW(K),XMAS(K),IDW(K,1),DN(K)
                END DO
                NT=0
              END IF
            END IF
          ELSE IF (LINE(15:26).EQ.'days, Irrad=') THEN
C*          -- Extract burnup of the next step
            READ(LINE( 4:13),'(F10.0)') DY
            READ(LINE(27:36),'(F10.0)') BU
            BURNUP(IK+1)=BU/1000.
            DAYS(IK+1)  =DY
          ELSE IF (LINE( 2:23).EQ.'punched material cards') THEN
            DO I=1,N0
              XMAS(I)=0.
            END DO
            CALL RWMAT(LIN,N0,IDW,AW,FMAS,BURNUP(IK),XMAS,INDX,DN,NT)
          END IF
C* Read the next record from input
          READ(LIN,'(A120)',END=60)LINE
        END DO
   60 CLOSE(LIN)
      IF(N.LT.3) GO TO 80
C* Process the reference results
      BUEOC=BURNUP(IK)
      IF(IK.GT.1) THEN
        IF(ABS(BURNUP(IK)-BURNUP(IK-1)).LT.1 .AND.
     &     ABS(DAYS(IK)-DAYS(IK-1)).GE.1) THEN
C*          Flag burnup negative if cooling time is included
          BUEOC=-BUEOC
        END IF
      END IF
      CALL RWREF(LRF,N0,CDW,BUEOC,XREF,SREF,WREF,INDX,N1)
      REWIND LRF
      DO I=1,5
        READ (LRF,'(A120)')LINE
        LINE(1:1)=' '
        WRITE(LOU,'(A120)')LINE
        WRITE(LPL,'(A120)')LINE
      END DO
      WRITE(LOU,'('' Burnup = '',F8.2,'' GWd/tU'')') BUEOC
      WRITE(LOU,'('' k-inf  = '',F8.5)')             XKINF (IK)
      WRITE(LOU,'('' ************************'')')
      WRITE(LOU,*)
      WRITE(LOU,'(2A40)')
     1 ' Calculated results                   : ',FLWO
     1,' Reference  results                   : ',FLRF
      WRITE(LOU,*)
      WRITE(LOU,'('' Isotope  Calculated Diff.   Reference Sigma'')')
      WRITE(LOU,'(''          [mg/gFuel]   [%]  [mg/gFuel]   [%]'')')
      WRITE(LOU,'('' -------------------------------------------'')')
C*
      WRITE(LPL,'('' Burnup = '',F8.2,'' GWd/tU'')') BUEOC
      WRITE(LPL,'('' ************************'')')
      WRITE(LPL,*)
      WRITE(LPL,'(2A40)')
     1 ' Calculated results                   : ',FLWO
     1,' Reference  results                   : ',FLRF
      WRITE(LPL,*)
      WRITE(LPL,'('' Legend: --- Error bar'')')
      WRITE(LPL,'(''         X   Uncertainty'')')
      WRITE(LPL,'(''         0   Zero or no data'')')
      WRITE(LPL,*)
      WRITE(LPL,'(1X,A6,1X,A61)') ' Isot.'
     1,'V_-20% _______________________V________________________+20%_V'
      NCHW=61
      EMN=-20.
      EMX= 20.
C*
      DO J=1,N1
        I=INDX(J)
        IF(XREF(I).NE.0) THEN
          XMS=XMAS(I)*WREF(I)
          ER=100.*(XMS-XREF(I))/XREF(I)
        ELSE
          XMS=XMAS(I)
          ER=0.
        END IF
        IF(XMS.GT.0) THEN
          WRITE(LOU,'(1X,A7,2(1P,E12.3,0P,F6.1))') CDW(I),XMS,ER
     1                                            ,XREF(I),SREF(I)
          CALL LNPLOT(LINE,NCHW,ER,SREF(I),EMN,EMX)
          WRITE(LPL,'(1X,A7,A61)') CDW(I),LINE(1:NCHW)
        END IF
      END DO
C* Procesing completed
   80 CLOSE(LOU)
      STOP 'WEDB1B Completed'
      END
      SUBROUTINE RWMAT(LIN,N0,IDW,AW,FMAS,BURNUP,XMAS,INDX,DN,NT)
C-Title  : Subroutine RWMAT
C-Purpose: Process Punched Materials in WIMS-D output
      CHARACTER*120 LINE
      DIMENSION AW(N0),IDW(N0,1),XMAS(N0),INDX(N0),DN(1)
     1         ,R(3),X(3)
C*
      NT=0
      FM=0.
      DO L=1,N0
        XMAS(L) =0
        DN(L)   =0
      END DO
C*
C* Read the first set of material data
      READ(LIN,'(A120)')LINE
      READ(LIN,'(A120)')LINE
      READ(LIN,'(A120)')LINE
C* Read a record from the WIMS-D output
      DO WHILE (LINE(16:31).NE.'entry into chain' .AND.
     &          LINE(1:10) .NE. ' material ')
        READ(LINE,'(3(F8.0,E15.0))',ERR=20) (R(K),X(K),K=1,3)
C* Three pairs of ID/No.Density per record are assumed
        DO K=1,3
          ID=R(K)+0.00001
          IF (ID.GT.0.AND.X(K).GT.0.0) THEN
C* Identify the nuclide and convert to g/cm3
            DO L=1,N0
            DO M=1,4
            IF(ID.EQ.IDW(L,M)) THEN
              NT      =NT + 1
              INDX(NT)=L
              GCM3    =X(K)*AW(L)
              XMAS(L) =XMAS(L)+GCM3
              DN(L)   =DN(L)  +X(K)
              FM      =FM + GCM3
            END IF
            END DO
            END DO
          ENDIF
        END DO
        READ(LIN,'(A120)',END=20)LINE
      END DO
C* Normalise the nuclide mass to mg/g-fuel
   20 IF(FMAS.EQ.0) FMAS=FM
      DO I=1,N0
        XMAS(I)=1000.0*XMAS(I)/FMAS
      END DO
      RETURN
      END
      SUBROUTINE GETK(LIN,XK,IKK)
      CHARACTER*120 LINE
      I=0
      DO WHILE(I.NE.IKK)
        READ(LIN,'(A120)')LINE
        IF (LINE(26:35).EQ.'k-infinity') THEN
          I=I+1
          IF (I.EQ.IKK) THEN
            READ(LINE,'(36X,E13.0)')XK
            RETURN
C         ELSE
C           READ(LINE,'(A120)')LINE
          END IF
        END IF
      END DO
      RETURN
      END
      SUBROUTINE RWREF(LIN,N0,CDW,BURNUP,XREF,SREF,WREF,INDX,N1)
C-Title  : RWREF Subroutine
C-Purpose: Read Reference Results file
      CHARACTER*7   CDW(N0),SYMB
      CHARACTER*120 LINE
      DIMENSION     XREF(1),SREF(1),WREF(1),INDX(1)
C* Find the appropriate burnup set
   20 READ(LIN,'(A120)')LINE
      IF (LINE(1:8).NE.' BURNUP ') GO TO 20
      READ(LINE,'(10X,F7.0)') BRREF
      IF(ABS(BURNUP-BRREF).GT.1) GO TO 20
      READ(LIN,'(A120)')LINE
C* Read the reference weight and uncertainty
      N1=0
   40 READ(LIN,'(A120)',END=60) LINE
      IF(LINE(1:10).EQ.'          ') GO TO 60
      READ(LINE,'(1X,A7,F12.0,2F6.0)') SYMB,XR,SR,WT
      IF(WT.EQ.0) WT=1.
      DO L=1,N0
        IF(SYMB.EQ.CDW(L)) THEN
          XREF(L)=XR
          SREF(L)=SR
          WREF(L)=WT
          N1=N1+1
          INDX(N1)=L
        END IF
      END DO
      GO TO 40
   60 RETURN
      END
      SUBROUTINE LNPLOT(LINE,NCHW,ER,ELM,EMN,EMX)
C-Title  : Subroutine LNPLOT
C-Purpose: Set plot line-bar character string
      CHARACTER*120 LINE
      DO 10 I=1,NCHW
      LINE(I:I)=' '
   10 CONTINUE
      LINE(  1 :  1 )='|'
      LINE(NCHW:NCHW)='|'
      F0=FLOAT(NCHW-1)*(  -EMN)/(EMX-EMN)
      L0=1+NINT(F0)
        IF(L0.LT.  1 ) L0=1
        IF(L0.GT.NCHW) L0=NCHW
      F1=FLOAT(NCHW-1)*(ER-EMN)/(EMX-EMN)
      L1=1+NINT(F1)
        IF(L1.LT.  1 ) L1=1
        IF(L1.GT.NCHW) L1=NCHW
      LA=MIN(L0,L1)
      LB=MAX(L0,L1)
      DO 20 I=LA,LB
        LINE(I:I)='-'
   20 CONTINUE
C* Mark error limits
      IF(ER.GT.0) THEN
        FL=FLOAT(NCHW-1)*( ELM-EMN)/(EMX-EMN)
      ELSE
        FL=FLOAT(NCHW-1)*(-ELM-EMN)/(EMX-EMN)
      END IF
      LL=1+NINT(FL)
      IF(LL.GT.1 .AND. LL.LT.NCHW) LINE(LL:LL)='X'
C* Mark error out of bounds
      IF(ER.GT.EMX) LINE(NCHW:NCHW)='>'
      IF(ER.LT.EMN) LINE(  1 :  1 )='<'
C* Mark centreline
      LINE(L0:L0)='+'
      IF(ABS(ER/(EMX-EMN)).LT.(0.5/NCHW)) LINE(L0:L0)='I'
C* Mark "no data"
      IF(ER.EQ. 0 ) LINE(L0:L0)='0'
      RETURN
      END
