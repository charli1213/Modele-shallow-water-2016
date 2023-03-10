

  SUBROUTINE calc_ener(u,v,eta,ener)

    ! Calcul de l'énergie cinétique (ener(1)) et de l'énergie potentielle
    ! (ener(2)).

    ! IN  : u,v,eta
    ! OUT : ener

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Ny+1,Nx+1,Nz) :: u,v
    REAL, dimension(Ny+1,Nx+1)    :: eta
    REAL                          :: ener(2)

    INTEGER                       :: i,j

    ener(1) = 0
    ener(2) = 0

    DO i = 1,Nx+1
       DO j = 1,Ny+1

          ener(1) = ener(1) + 0.5*(u(i,j,1)**2 + v(i,j,1)**2 + u(i,j,2)**2 & 
               + v(i,j,2)**2)

          ener(2) = ener(2) + g*eta(i,j)**2

       END DO
    END DO


  END SUBROUTINE calc_ener
