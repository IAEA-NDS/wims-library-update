      PROGRAM NJISPL
C-Title  : NJISPL Program
C-Purpose: Split Merged NJOY input into separate files
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1998).
C-M
C-M  Manual for Program NJISPL
C-M  =========================
C-M
C-M  For the convenience of maintaining a set of NJOY97 input files,
C-M  all files are copied onto a single file named 'wlupnji.inp'.
C-M  Separate Njoy input files are delimited with a record beginning
C-M  with the string:
C-M    "-- name  "
C-M  where 'name' is the separate NJOY input generic filename to
C-M  which the extension '.nji' is appended. This is consistent
C-M  with the NJOY-97.45+ input convention which interprets such
C-M  records as comments.
C-M
C-M  Instructions:
C-M  The following input is expected from keyboard:
C-M  - Master NJOY input filename (default: wlupnji.inp).
C-M  - Specific NJOY input file fo be extracted (default: blank to
C-M    process all files).
C-M
C-M  Note: The specific NJOY input generic filenames must be
C-M        typed exactly. The list can be extracted from the
C-M        master input with a system search utility,
C-M        displaying all records beginning with "-- ".
C-M        For example:
C-M          find   "-- " wlupnji.inp >wlupnji.lst      (Dos)
C-M          grep   "-- " wlupnji.inp >wlupnji.lst      (Unix)
C-M          search wlupnji.inp "-- " /out=wlupnji.lst  (Vax/VMS)
C-M
C-
      CHARACTER*6    FM
      CHARACTER*80   REC
      CHARACTER*40   BLNK,FLNM,FLAL,FLNJ
 
      DATA LIN,LNJ,LKB,LTT
     1    /  1,  2,  5,  6 /
      DATA BLNK/'                                        '/
     1     FLAL/'wlupnji.inp'/
C*
C*
      WRITE(LTT,92) ' NJISPL - Split Merged NJOY Input File  '
      WRITE(LTT,92) ' =====================================  '
      WRITE(LTT,92)
      WRITE(LTT,92) ' Default Master NJOY input file       : ',FLAL
      WRITE(LTT,92) '$         Enter a new name to redefine: '
      READ (LKB,92) FLNM
      IF(FLNM.NE.BLNK) FLAL=FLNM
      WRITE(LTT,92) ' Enter NJOY input filename to extract   '
      WRITE(LTT,92) '$              (Blank to extract all) : '
      READ (LKB,92) FLNM
C*
      OPEN (UNIT=LIN,FILE=FLAL,STATUS='OLD',ERR=82)
      WRITE(LTT,92)
      WRITE(LTT,92) ' Opened merged NJOY input file        : ',FLAL
      NFL=0
C*
C* Read a record from the main input file
 20   READ (LIN,98,END=60) REC
      IF(REC(1:3).EQ.'-- ') THEN
C* Skip comment lines beginning with blanks
        IF(REC(4:9).EQ.'      ') GO TO 20
C* Begin a new Njoy input file
        LN=4
 22     LN=LN+1
        IF(REC(LN:LN).NE.' ') GO TO 22
        FLNJ=REC(4:LN-1)//'.nji'
        IF(LNJ.GT.0) CLOSE(UNIT=LNJ)
        IF(FLNM.NE.BLNK .AND. REC(4:LN).NE.FLNM(1:LN-3)) THEN
          LNJ=-ABS(LNJ)
          GO TO 20
        ELSE
          LNJ =ABS(LNJ)
        END IF
        OPEN (UNIT=LNJ,FILE=FLNJ,STATUS='UNKNOWN')
        WRITE(LTT,92) '                         Writing file : ',FLNJ
        NFL=NFL+1
        WRITE(LNJ,98) REC
      ELSE
C* Check if an output file is open
        IF(LNJ.LT.0) GO TO 20
C* Determine the record length
        LN=80
 24     IF(REC(LN:LN).NE.' ') GO TO 26
        LN=LN-1
        IF(LN.GT.1) GO TO 24
C* Write a record to the NJOY input file
 26     WRITE(FM ,99) LN
        WRITE(LNJ,FM) REC(1:LN)
      END IF
C* Process the next record from the main input file
      GO TO 20
C*
C* All files written
 60   IF(LNJ.GT.0) THEN
        CLOSE(UNIT=LNJ)
C*     WRITE(LTT,92) '                         Written file : ',FLNJ
      END IF
      WRITE(LTT,92)
      WRITE(LTT,93) '        Total number of files written : ',NFL
      STOP 'NJISPL Completed'
C*
C* Error trap
 82   WRITE(LTT,92) ' NJISPL ERROR - No Merged NJOY input  : ',FLAL
C*
 92   FORMAT(2A40)
 93   FORMAT(A40,I4)
 98   FORMAT(A80)
 99   FORMAT('(A',I3.3,')')
      END
