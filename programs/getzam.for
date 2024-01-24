       PROGRAM GETZAM
C-Title  : GETZAM Program
C-Purpose: Extract one or more materials from an ENDF file
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana
C-Version: April 2000 - original code
C-M
C-M  Manual for Program GETZAM
C-M  =========================
C-M  The program retrieves material files from an evaluated nuclear
C-M  data library. The retrieval is done either by the ZA or the
C-M  MAT number. The definition ZA=1000*Z+A applies, where Z is the
C-M  atomic number and A is the mass number.
C-M
C-M  Instructions
C-M  The parameters listed below are read from the default input.
C-M  They may be entered interactively from the keyboard in response
C-M  to the prompts printed on the default output (usually the
C-M  terminal screen):
C-M  - Output ENDF filename, which is to contain the extracted data.
C-M  - Source ENDF filename, which is to be processed.
C-M  - Requested material range. Two entries IM1, IM2 are read
C-M    assuming 10 columns width for each (blanks are ignored).
C-M    The remaining part of the record (from column 21 onwards)
C-M    may be used for comments.
C-M      The requested range implies all materials with identifiers
C-M    in the range between IM1 and IM2, inclusive. The identifiers
C-M    are the material ZA of the MAT designation. Selection by the
C-M    MAT designation is flagged by negative ident numbers.
C-M      If Abs(IM2) is less than Abs(IM1) then IM2 is assumed equal
C-M    to IM1, implying a single material request on this record.
C-M    NOTE: When specifying the range of materials, both entries
C-M          defining the range must imply either the ZA designation
C-M          (positive entries) or MAT designation (negative entries).
C-M  - Any number of additional requests as above may be specified.
C-M    The maximum limit on the number of retrieved materials is
C-M    MXMT, defined in the PARAMETER statement in the code. The list
C-M    of requests is terminated by a blank record or zero.
C-M
C-
      PARAMETER   (MXMT=100)
      CHARACTER*66 C66,H66
      CHARACTER*40 BLNK,FLNM,FLIN,FLOU
      DIMENSION    AMT1(MXMT),AMT2(MXMT)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'ENDF.dat'/
     2    ,FLOU/'ENDF.OUT'/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,901) ' GETZAM - Extract materials from ENDF   '
      WRITE(LTT,901) ' ====================================   '
      WRITE(LTT,901)
C* Define the output file
   14 WRITE(LTT,901) ' Default output ENDF filename         : ',FLOU
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,901) FLNM
      IF(FLNM.NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C* Define the source file
   16 WRITE(LTT,901) ' Default source ENDF filename         : ',FLIN
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,901) FLNM
      IF(FLNM.NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=16)
C* Define materials to be extracted
      NMT=0
      GO TO 22
C* Warn of illegal requests
C* Read the requests
   22 WRITE(LTT,901) '$Request material (MAT<0,ZA>0,End=0)  : '
      READ (LKB,922,END=40,ERR=22) IM1,IM2
      IF(IM1.EQ.0) GO TO 40
      IF(ABS(IM2).LT.ABS(IM1)) IM2=IM1
C* Check entry validity (watch for integer overflow)
      I12=(IM1/ABS(IM1))*(IM2/ABS(IM2))
      IF(I12.LT.0) THEN
        WRITE(LTT,901) ' Illegal request - please redo          '
        GO TO 22
      END IF
      NMT=NMT+1
      IF(NMT.GT.MXMT) STOP 'GETZAM ERROR - MXMT limit exceeded'
      AMT1(NMT)=IM1
      AMT2(NMT)=IM2
      GO TO 22
C*
C* Print processing information
   40 WRITE(LTT,901)
      WRITE(LTT,901) '                     Output ENDF file : ',FLOU
      WRITE(LTT,901) '                     Source ENDF file : ',FLIN
      WRITE(LTT,902) '                  Number of materials : ',NMT
      WRITE(LTT,901)
C*
C* Begin processing the data
      READ (LIN,940) C66,MAT,MF,MT
      WRITE(LOU,940) C66,MAT,MF,MT
C* Next material
   60 READ (LIN,940) C66,MAT,MF,MT
      IF(MAT.LT.0) GO TO 80
      READ (LIN,940) H66,MAT,MF,MT
      READ (C66,924) ZA
      READ (H66,924) DM,DM,ID,LIS0
      ZA=ZA+0.1*LIS0
C* Check if material is to be copied
      CALL MATCHK(MAT,ZA,NMT,AMT1,AMT2,ISEL)
      IF     (ISEL.LT.0) THEN
        GO TO 80
      ELSE IF(ISEL.GT.0) THEN
C* Copy current material to output
        WRITE(LTT,961) MAT,ZA
        WRITE(LOU,940) C66,MAT,MF,MT
        WRITE(LOU,940) H66,MAT,MF,MT
   62   READ (LIN,940) C66,MAT,MF,MT
        WRITE(LOU,940) C66,MAT,MF,MT
        IF(MAT.GT.0) GO TO 62
      ELSE
C* Skip to next material
   64   READ (LIN,940) C66,MAT,MF,MT
        IF(MAT.GT.0) GO TO 64
        END IF
      GO TO 60
C* End of file processing
   80 C66=BLNK//BLNK(1:26)
      MAT=-1
      MF = 0
      MT = 0
      WRITE(LOU,940) C66,MAT,MF,MT
      STOP 'GETZAM Completed'
C*
  901 FORMAT(2A40)
  902 FORMAT(A40,I10)
  922 FORMAT(BN,2I10)
  924 FORMAT(2F11.0,4I11)
  940 FORMAT(A66,I4,I2,I3)
  961 FORMAT(' Copy MAT',I6,'  ZA',F9.1)
      END
      SUBROUTINE MATCHK(MAT,ZA,NMT,AMT1,AMT2,ISEL)
C-Title  : Matchk Subroutine
C-Purpose: Check if MAT/MT is in specified range
      DIMENSION AMT1(NMT),AMT2(NMT)
      ISEL=0
      IALL=0
      DO 20 M=1,NMT
      IF(AMT1(M).EQ.0) THEN
        GO TO 20
      ELSE IF(AMT1(M).LT.0) THEN
C* Check by MAT number
        IALL=1
        MAT1=NINT(-AMT1(M))
        MAT2=NINT(-AMT2(M))
        IF(MAT.LT.MAT1 .OR. MAT.GT.MAT2) GO TO 20
      ELSE
C* Check by ZA number
        IALL=1
        MAT1=NINT(10*AMT1(M))
        MAT2=NINT(10*AMT2(M))
        IZA =NINT(10*ZA)
        IF(IZA.LT.MAT1 .OR. IZA.GT.MAT2) GO TO 20
      END IF
C* Case: Material matching successful
      IF(AMT1(M).EQ.AMT2(M)) THEN
        AMT1(M)=0
        AMT2(M)=0
      END IF
      ISEL=1
      RETURN
   20 CONTINUE
C* Case: Material not matching
      IF(IALL.EQ.0) ISEL=-1
      RETURN
      END
