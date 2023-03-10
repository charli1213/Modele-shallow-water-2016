SUBROUTINE VENT(TAU)
! Sous-routine qui nous donne le profil du vent.
! IN : Rien
! OUT : TAU


  USE Parametres
  IMPLICIT NONE  
  REAL :: TAU(Ny+1)
  INTEGER :: i
  
  DO i =1,Ny+1
     TAU(i) = 0 + (SIN(i*pi/Ny))**2
  END DO

END SUBROUTINE VENT
