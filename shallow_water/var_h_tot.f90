


SUBROUTINE var_h_tot(eta,var_h)

USE Parametres

IMPLICIT NONE

REAL :: eta(n,Ny+1,Nx+1), var_h

INTEGER :: i,j

var_h = 0


DO i = 1,Ny
   DO j = 1,Nx
      
      var_h  = var_h + eta(1,i,j) + eta(2,i,j)

   END DO
END DO


END SUBROUTINE var_h_tot
      
