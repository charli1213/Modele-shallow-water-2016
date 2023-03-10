

SUBROUTINE qnt_mvt(Ux,Vy,mvt)

USE Parametres

IMPLICIT NONE

REAL :: Ux(n,Ny+1,Nx+1), Vy(n,Ny+1,Nx+1), mvt(3)

INTEGER :: i,j

mvt(1) = 0
mvt(2) = 0

DO j = 1,Ny
   DO i = 1,Nx
      
      mvt(1) = mvt(1) + rho1 * (Ux(1,i,j))

      mvt(2) = mvt(2) + rho2 * (Ux(2,i,j))
   END DO
END DO

mvt(3) = mvt(1) + mvt(2)

END SUBROUTINE qnt_mvt

