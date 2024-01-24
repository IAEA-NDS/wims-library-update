C     Program Dopbench for calculation of Doppler coefficient
C     Authors : L.ERRADI, A.HTET, E.CHAKIR
      PROGRAM DOPBENCH
       CAll ENTETEDOP
       CALL NBDC
       CALL DOPPLER
      END
C *********************************************************
C *********************************************************
      SUBROUTINE ENTETEDOP
C *********************************************************
      OPEN(UNIT=5,file='ENTETEDOP.RES',STATUS='UNKNOWN')
      WRITE(5,1000)
1000  FORMAT (20x,44(1H*)/
     1 20x,'*       WIMS LIBRARY UPDATE PROJECT        *'/
     2 20x,'*          LIBRARIES VALIDATION            *'/
     2 20x,'*       FOR THE DOPPLER COEFFICIENT        *'/
     3 20x,44(1H*)///
     4 15x,54(1H*)/
     5 15x,'*           L.ERRADI, A.HTET AND E.CHAKIR',12x,1(1H*)/
     6 15x,'*      GROUPE DE PHYSIQUE DES REACTEURS NUCLEAIRES',3x,1H*/
     7 15x,'*           FACULTE DES SCIENCES -  RABAT',12x,1(1H*)/
     8 15x,'*                      MOROCCO',23x,1(1H*)/
     9 15x,54(1H*)////)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE NBDC
C *********************************************************
      OPEN(UNIT=10,file='NBDC.RES',STATUS='UNKNOWN')
      WRITE(10,1400)
1400  FORMAT('NUMERICAL BENCHMARK ON DOPPLER COEFFICIENT'/
     1 42(1H*)//)
      RETURN
      END
C *********************************************************
      SUBROUTINE DOPPLER
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(10),XKE(10),DT(10),C(10),E(10), REF(10),delta(10)
      OPEN (UNIT=3,FILE='DOPPLER.OUT',STATUS='old')
      OPEN (UNIT=5,file='DOPPLER.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='DOPPLER.T',STATUS='UNKNOWN')
      OPEN (UNIT=7,FILE='DOPPLER.E',STATUS='UNKNOWN')
      OPEN (UNIT=8,FILE='DOPPLER.REF',STATUS='UNKNOWN')
C
      DO I=1,10
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C*
C      do i=1,10
C       WRITE(*,*)XKI(i),XKE(i)
C      enddo
      do i=1,5
       read(8,1300)ref(i)
      enddo
      read(7,*)(e(j),j=1,5)
      read(6,*)(dt(j),j=1,10)
      j=0
      do i=2,10,2
       j=j+1
       c(j)=(XKE(i)-XKE(i-1))/(XKE(i)*XKE(i-1)*(DT(i)-DT(i-1))*1.E-5)
       delta(j)=((c(j)-ref(j))/ref(j))*100
      enddo
      write(5,950)
      do 60 i=1,10
       ik = mod(i,2)
       if(ik.eq.0) then
        write(5,901)dt(i),XKE(i)
       else
        write(5,900)dt(i),XKE(i)
       endif
       jl = int(i/2)
       if(i.ne.1) then
        goto 601
       else
        write(5,1100)e(i),c(i),ref(i),delta(i)
        goto 60
       endif
  601  if(ik.eq.0) go to 60
       write(5,1100)e(jl+1),c(jl+1),ref(jl+1),delta(jl+1)
   60 continue
      write(5,1250)
      close(3)
      close(5)
      close(6)
      close(7)
      close(8)
      close(10)
C*
  800 FORMAT(A120)
  802 FORMAT(E14.7)
  950 FORMAT(1X,24X,33(1H*)/
     1 '                         *     DOPPLER COEFFICIENT       *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND CALCULATED DOPPLER COEFFICIENT OF REACTIVITY:'//
     412x,'TEMP. (°K)',5x,'K-EFF'/
     51x,'e (wt%)',33x,'DOPPLER COEF.(pcm/°K)',4x,'REF. (pcm/°K)'
     6,4x,'delta (%)'//)
  900 FORMAT(12x,F7.2,2x,E14.7)
  901 FORMAT(12x,F7.2,2x,E14.7//)
 1200 FORMAT(1x,F2.0,1x,F2.0,1x,F2.0,1x,F2.0)
 1100 FORMAT(F6.3,43x,F6.3,15x,F5.2,10x,F5.2)
 1250 FORMAT(55(1H-)/)
 1300 FORMAT(5(1x,F5.2))
      RETURN
      END
