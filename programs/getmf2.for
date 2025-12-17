      PROGRAM GETMF2
C-Title  : Program GETMF2
C-Purpose: Get relevant data from file 2 to prepare NJOY inputs
C-Author : D. L. Aldama
C-Version: 03/1999 Original code (named GETDATA)
C-V  01/2000 Add upper unresolved res. range threshold.
C-V  09/2000 Simple average of E-dep. AP (1/E caused problems
C-V          on coarse mesh starting at 1E-5 eV, case: Cl-nat
C-V          from JENDL-3.2) (A.Trkov).
C-V  12/2001 A.Trkov, IAEA
C-V          - Rename to GETMF2.
C-V          - Fix bug when no MF2 data present.
C-V          - Normalise scattering radius by sum of weights
C-V            to fix processing of some BROND evaluations, which
C-V            contain pseudo-isotopes in isotopic evaluations.
C-V  11/2004 D. L. Aldama, IAEA
C-V          - Correction to read MF=1/MT=451 in ENDF-5 format
C-V  07/2021 A. Trkov
C-V          Fix long filenames and LRF=7
C-M
C-M  Manual for Program GETMF2
C-M  =========================
C-M  The AP value and other parameters are read from the MF2/151 and
C-M  MF1/451 sections. An average potential sigma is calculated in the
C-M  WIMS resonance range (4.00eV to 9.118KeV) for each material on
C-M  the endfb tape. Parameters, which are relevant for preparing NJOY
C-M  inputs are printed on the output file:
C-M   Col.  Description
C-M   1- 4  Material MAT number.
C-M   5-16  Chemical symbol from ENDF file MF1, MT451.
C-M  17-26  Material ZA designation.
C-M  27-38  Atomic weight ratio.
C-M  39-50  Average potential cross section [barns].
C-M  51-63  Upper energy of the resolved resonance range [eV].
C-M  64-76  Upper energy of the unresolved resonance range,
C-M         zero if no unresolved resonance parameters present [eV].
C-M
C-M  The details of the processing are saved on the log file.
C-M
C-M  Instructions
C-M  The following parameters are entered from keyboard:
C-M   - Source evaluated nuclear data filename.
C-M   - Output filename.
C-M   - Log filename that contains additional processing information.
C-M
C-M  Files used:
C-M       Input file:
C-M          1. Evaluated nuclear data file
C-M       Output files:
C-M          1.  Output file (ENDF MF2 summary)
C-M          3.  Log file
C-
       PARAMETER(LIB=1,NOUT=2,NLOG=3,LKB=5,LTT=6,NI=10,NMAX=30)
       PARAMETER(PI=3.141593)
       DIMENSION NRR(NI),ZAI(NI),AWI(NI),ABNI(NI)
       DIMENSION EHIGH(NI,NMAX),AP(NI,NMAX),RRF(NI,NMAX)
       CHARACTER*66 LINE,TAPE
       CHARACTER*40 BLNK
       CHARACTER*80 FLI,FOU,FLO,FLNM
       CHARACTER*11 ZSYMAM
       DATA BLNK/'                                        '/
       DATA FLI/'ENDF.DAT'/
       DATA FOU/'ENDF.MF2'/
       DATA FLO/'GETMF2.LOG'/
C*
       WRITE(LTT,15) ' GETMF2: Retrieve data from From File 2 '
       WRITE(LTT,15) ' ======================================='
       WRITE(LTT,15)
C*
C*    Open I/O files
       FLNM=BLNK
    1  WRITE(LTT,15) '    Define Evaluated Nuclear Data Lib.: '
       WRITE(LTT,15) '    ----------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FLI
       WRITE(LTT,15) '$       Enter new name (enter=default): '
      READ (LKB,14) FLNM
      IF(FLNM(1:40).NE.BLNK) FLI=FLNM
       OPEN(UNIT=LIB, FILE=FLI, STATUS='OLD',ERR=1)
      FLNM=BLNK//BLNK
    2  WRITE(LTT,15) '    Define Output File Name (MF2 file): '
       WRITE(LTT,15) '    ----------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FOU
       WRITE(LTT,15) '$       Enter new name (enter=default): '
      READ (LKB,14) FLNM
      IF(FLNM(1:40).NE.BLNK) FOU=FLNM
       OPEN(UNIT=NOUT,FILE=FOU,ERR=2)
      FLNM=BLNK//BLNK
    3  WRITE(LTT,15) '    Define   LOG  File Name (LOG file): '
       WRITE(LTT,15) '    ----------------------------------  '
       WRITE(LTT,15) '                     Default file name: ',FLO
       WRITE(LTT,15) '$       Enter new name (enter=default): '
      READ (LKB,14) FLNM
      IF(FLNM(1:40).NE.BLNK) FLO=FLNM
       IF(FLNM.NE.BLNK) FLO=FLNM
       OPEN(UNIT=NLOG,FILE=FLO,ERR=3)
C*
C*    Process Evaluated Nuclear Data Library
       CALL RDHEAD(LIB,TAPE,MAT,MF,MT,NS)
       WRITE(NOUT,*)' MAT   Nuclide        ZA        Awt'
     1             ,'         SigPot      Eru          Euu'
       WRITE(NLOG,*)TAPE
       IMF=0
       DO WHILE (MAT.NE.-1)
         CALL RDHEAD(LIB,LINE,MAT,MF,MT,NS)
         IF(IMF.EQ.0) THEN
           IF(MF.EQ.1.AND.MT.EQ.451)THEN
             BACKSPACE(LIB)
             CALL GETZSA(LIB,ZSYMAM)
             IMF=1
           END IF
         ELSE IF (IMF.EQ.1) THEN
           IF(MF.EQ.2.AND.MT.EQ.151) THEN
             BACKSPACE(LIB)
             CALL GETAPS(LIB,ZA0,AW,NIS,NRR,ZAI,ABNI,AWI,EHIGH,AP,RRF,
     &                   IERR)
             IMF=0
             IF (IERR.EQ.0) THEN
              WRITE(NLOG,20)' MAT=',MAT,' SYM=',ZSYMAM,'  ZA=',ZA0,
     &                     '  AW=',AW,' NIS=',NIS
              APMAT=0.0
              EHMAT=2.0E7
              EUMAT=0
              ABNSU=0
              DO I=1,NIS
               ABNSU=ABNSU+ABNI(I)
               WRITE(NLOG,30)' I=',I,' ZAI=',ZAI(I),' AWI=',AWI(I),
     &                       ' ABN=',ABNI(I),' NRR=',NRR(I)
               WRITE(NLOG,40)' UPPER BOUND.',' SCAT. RADIUS',' LRU.LRF'
               NER=NRR(I)
               DO IE=1,NER
                 WRITE(NLOG,50)EHIGH(I,IE),AP(I,IE),RRF(I,IE)
               END DO
               CALL EHAPI(I,NER,EHIGH,AP,RRF,EHI,API,EUR)
               WRITE(NLOG,60)'ISO. AP:',API,' ISO. RRR/URR:',EHI
               APMAT=ABNI(I)*API+APMAT
               IF (EHI.LT.EHMAT) EHMAT=EHI
               IF (EUR.GT.EUMAT) EUMAT=EUR
              END DO
              APMAT=APMAT/ABNSU
              WRITE(NLOG,60)'MAT. AP:',APMAT,' MAT. RRR/URR:',EHMAT
              SIGPOT=4.0*PI*APMAT*APMAT
              WRITE(NOUT,70)MAT,ZSYMAM,ZA0,AW,SIGPOT,EHMAT,EUMAT
             ELSE
              WRITE(NOUT,70)MAT,ZSYMAM,ZA0,AW
             ENDIF
           ELSE IF(MF.GT.2) THEN
             IMF=0
           ENDIF
         ENDIF
       END DO
       CLOSE(LIB)
       CLOSE(NOUT)
       CLOSE(NLOG)
      STOP
14    FORMAT( A80)
15    FORMAT(2A40)
20    FORMAT(A5,I5,A5,A12,A5,F7.1,A5,F10.6,A5,I3)
30    FORMAT(A3,I2,A5,F7.1,A5,F10.6,A5,F8.6,A5,I3)
40    FORMAT(5X,A13,2X,A13,2X,A8)
50    FORMAT(5X,1PE13.6,2X,1PE13.6,2X,0PF7.2)
60    FORMAT(5X,A8,2X,1PE13.6,2X,A14,2X,1PE13.6)
70    FORMAT(I4,2X,A11,F9.1,F12.6,F12.4,1P,2E13.6)
      END
C
C   GETAPS
C
      SUBROUTINE GETAPS(LIB,ZA0,AW,NIS,NRR,ZAI,ABNI,AWI,EHIGH,AP,RRF,
     &                   IERR)
       PARAMETER(XMASS=1.008665,NI=10,NPP=10000)
       DIMENSION X(NPP),Y(NPP),NB(NPP),IT(NPP),RRF(NI,*)
       DIMENSION NRR(NI),ZAI(NI),AWI(NI),ABNI(NI),EHIGH(NI,*),AP(NI,*)
       IERR=0
       CALL RDCONT(LIB,ZA,AWR,L1,L2,NIS,N2,MAT,MF,MT,NS)
       ZA0=ZA
       AW=AWR*XMASS
       CALL RDCONT(LIB,ZAII,ABN,L1,LFW,NER,N2,MAT,MF,MT,NS)
       CALL RDCONT(LIB,C1,EH,LRU,LRF,NRO,NAPS,MAT,MF,MT,NS)
       IF (LRU.EQ.0.AND.NIS.EQ.1.AND.NER.EQ.1) THEN
C  ONLY SCATTERING RADIUS IS GIVEN
        CALL RDCONT(LIB,C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS)
        ZAI(1)=ZAII
        ABNI(1)=ABN
        NRR(1)=NER
        EHIGH(1,1)=EH
        AWI(1)=AW
        AP(1,1)=C2
        RRF(1,1)=FLOAT(LRU)+0.1*FLOAT(LRF)
       ELSE
C  RESO DATA
        BACKSPACE(LIB)
        BACKSPACE(LIB)
        DO II=1,NIS
         CALL RDCONT(LIB,ZAII,ABN,L1,LFW,NER,N2,MAT,MF,MT,NS)
         ZAI(II)=ZAII
         ABNI(II)=ABN
         NRR(II)=NER
         DO IE=1,NER
          CALL RDCONT(LIB,C1,EH,LRU,LRF,NRO,NAPS,MAT,MF,MT,NS)
          EHIGH(II,IE)=EH
          RRF(II,IE)=FLOAT(LRU)+0.1*FLOAT(LRF)
          IF (LRU.EQ.1) THEN
C            RESOLVED RESONANCES
           IF (LRF.LT.4) THEN
             IF (NRO.NE.0) THEN
               CALL RDTAB1(LIB,C1,C2,L1,L2,N1,N2,NB,IT,X,Y,MAT,MF,MT,NS)
               SUM=0.0
               SEN=0.0
               DO I=2,N2
                 SUM=SUM+0.5*(X(I)-X(I-1))*(Y(I)+Y(I-1))
                 SEN=SEN+    (X(I)-X(I-1))
               END DO
               SUM=SUM/SEN
               AP(II,IE)=SUM
               CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
             ELSE
               CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
               IF (LRF.EQ.3.AND.C2.EQ.0.) THEN
                 READ(LIB,'(11X,F11.0)')C2
                 BACKSPACE LIB
               ENDIF
               AP(II,IE)=C2
             ENDIF
             DO K=1,NLS
              CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
              AWI(II)=C1*XMASS
             END DO
           ELSE IF(LRF.EQ.4) THEN
             IF (NRO.NE.0) THEN
               CALL RDTAB1(LIB,C1,C2,L1,L2,N1,N2,NB,IT,X,Y,MAT,MF,MT,NS)
               SUM=0.0
               DO I=2,N2
                 SUM=0.5*(X(I)-X(I-1))*(Y(I)/X(I)+Y(I-1)/X(I-1))+SUM
               END DO
               AP(II,IE)=SUM/LOG(X(N2)/X(1))
               CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
             ELSE
               CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
               AP(II,IE)=C2
             ENDIF
             CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
             AWI(II)=C1*XMASS
             DO JJ=1,NLS
              CALL RDCONT(LIB,C1,C2,L1,L2,NJS,N2,MAT,MF,MT,NS)
              DO K=1,NJS
               CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
              END DO
             END DO
           ELSE
C             CASE OF Cl-35 for ENDFB-VII.1 (PATCH)
             IF (ZA0.EQ.17035.0.AND.LRF.EQ.7)THEN
               AP(II,IE)=0.36680
               AWI(II)=AW
             ELSE
               WRITE(*,*)' THIS PROGRAM CAN NOT WORK WITH'
               WRITE(*,*)' LRF=5, LRF=6 or LRF=7'
               WRITE(*,*)' ZA0=',ZA0, ' AW=',AW, ' LRF=',LRF
               IERR=1
             ENDIF
           ENDIF
          ELSE IF (LRU.EQ.2) THEN
C            UNRESOLVED RESONANCES
           IF(LRF.EQ.1) THEN
             IF(LFW.EQ.0) THEN
               CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
               AP(II,IE)=C2
               DO K=1,NLS
                CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
                AWI(II)=C1*XMASS
               END DO
             ELSE
               CALL RDLIST(LIB,C1,C2,L1,L2,N1,NLS,X,MAT,MF,MT,NS)
               AP(II,IE)=C2
               DO K=1,NLS
                 CALL RDCONT(LIB,C1,C2,L1,L2,NJS,N2,MAT,MF,MT,NS)
                 AWI(II)=C1*XMASS
                 DO JJ=1,NJS
                   CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
                 END DO
               END DO
             ENDIF
           ELSE
             CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
             AP(II,IE)=C2
             DO K=1,NLS
               CALL RDCONT(LIB,C1,C2,L1,L2,NJS,N2,MAT,MF,MT,NS)
               AWI(II)=C1*XMASS
               DO JJ=1,NJS
                 CALL RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
               END DO
             END DO
           ENDIF
          ELSE IF (LRU.EQ.0.AND.NIS.GT.1)THEN
           CALL RDCONT(LIB,C1,C2,L1,L2,NLS,N2,MAT,MF,MT,NS)
           AP(II,IE)=C2
          ENDIF
         END DO
        END DO
       ENDIF
       RETURN
      END
C
C    GETZSA
C
      SUBROUTINE GETZSA(LIB,ZSYMAM)
       CHARACTER*66 TEXT
       CHARACTER*12 ZSYMAM
       DO I=1,2
        CALL RDHEAD(LIB,TEXT,MAT,MF,MT,NS)
       END DO
       IF (TEXT(56:66).EQ.'          6') THEN
         NV=3
       ELSE
         NV=2
       ENDIF
       DO I=1,NV
        CALL RDHEAD(LIB,TEXT,MAT,MF,MT,NS)
       END DO
       READ(TEXT,10)ZSYMAM
       RETURN
10     FORMAT(A11)
      END
C
C     RDHEAD, RDCONT, RDLIST, RDTAB1
C
      SUBROUTINE RDHEAD(LIB,TEXT,MAT,MF,MT,NS)
       CHARACTER*66 TEXT
       READ(LIB,10)TEXT,MAT,MF,MT,NS
       RETURN
10     FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE RDCONT(LIB,C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS)
       READ(LIB,10)C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS
       RETURN
10     FORMAT(2E11.0,4I11,I4,I2,I3,I5)
      END
      SUBROUTINE RDLIST(LIB,C1,C2,L1,L2,N1,N2,X,MAT,MF,MT,NS)
       DIMENSION X(*)
       READ(LIB,10)C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS
       READ(LIB,20)(X(I),I=1,N1)
       RETURN
10     FORMAT(2E11.0,4I11,I4,I2,I3,I5)
20     FORMAT(6E11.0)
      END
      SUBROUTINE RDTAB1(LIB,C1,C2,L1,L2,N1,N2,NB,ITL,X,Y,MAT,MF,MT,NS)
       DIMENSION NB(*),ITL(*),X(*),Y(*)
       READ(LIB,10)C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS
       READ(LIB,20)(NB(I),ITL(I),I=1,N1)
       READ(LIB,30)(X(I),Y(I),I=1,N2)
       RETURN
10     FORMAT(2E11.0,4I11,I4,I2,I3,I5)
20     FORMAT(6I11)
30     FORMAT(6E11.0)
      END
C
C      EHAPI
C
      SUBROUTINE EHAPI(I,NER,EHIGH,AP,RRF,EHI,API,EUR)
       PARAMETER(ELW=4.0,EHW=9118.0,NI=10)
       DIMENSION EHIGH(NI,*),AP(NI,*),RRF(NI,*)
       EUR=0
       IF (NER.EQ.1) THEN
         API=AP(I,1)
       ELSE IF (EHIGH(I,NER).LT.ELW) THEN
         API=AP(I,NER)
       ELSE IF (EHIGH(I,1).GT.EHW) THEN
         API=AP(I,1)
       ELSE
         IF (EHIGH(I,1).GT.ELW) THEN
           IMIN=1
         ELSE
           IMIN=1
           DO WHILE(EHIGH(I,IMIN).LE.ELW)
             IMIN=IMIN+1
           END DO
         ENDIF
         IF (EHIGH(I,NER).LT.EHW) THEN
           IMAX=NER
           EHIGH(I,NER)=EHW
         ELSE
           IMAX=IMIN
           DO WHILE(EHIGH(I,IMAX).LT.EHW)
             IMAX=IMAX+1
           END DO
           EHIGH(I,IMAX)=EHW
         ENDIF
         SUM=0.0
         EL=ELW
         DO IE=IMIN,IMAX
           SUM=AP(I,IE)*LOG(EHIGH(I,IE)/EL)+SUM
           EL=EHIGH(I,IE)
         END DO
         API=SUM/LOG(EHW/ELW)
       END IF
C       II=0
C       IE=1
C       DO WHILE (II.EQ.0)
        DO IE=1,NER
        LRU=INT(RRF(I,IE))
        IF (LRU.LT.2) THEN
          EHI=EHIGH(I,IE)
C          IE=IE+1
        ELSE
          EUR=EHIGH(I,IE)
C          II=1
        ENDIF
C        IF (IE.GT.NER) II=1
       END DO
       RETURN
      END
