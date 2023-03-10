

  SUBROUTINE calc_ener(u,v,ubc,vbc,ubt,vbt,eta,ener)

    ! Calcul de l'énergie cinétique (ener(1)) globale, de l'énergie potentielle
    ! (ener(2)), énergie cinétique barocline (ener(3)) et barotrope (ener(4)).

    ! IN  : u,v,ubc,vbc,ubt,vbt,eta
    ! OUT : ener

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Ny+1,Nx+1,Nz) :: u,v
    REAL, dimension(Ny+1,Nx+1)    :: eta,ubc,vbc,ubt,vbt
    REAL                          :: ener(4)

    INTEGER                       :: i,j

    ener(1) = 0
    ener(2) = 0
    ener(3) = 0
    ener(4) = 0

    DO i = 1,Nx+1
       DO j = 1,Ny+1

          ener(1) = ener(1) + 0.5*(u(i,j,1)**2 + v(i,j,1)**2 + u(i,j,2)**2 & 
               + v(i,j,2)**2)

          ener(2) = ener(2) + g*eta(i,j)**2

          ener(3) = ener(3) + 0.5*(ubc(i,j)**2 + vbc(i,j)**2)

          ener(4) = ener(4) + 0.5*(ubt(i,j)**2 + vbt(i,j)**2)

       END DO
    END DO


  END SUBROUTINE calc_ener
