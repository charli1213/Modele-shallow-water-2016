

SUBROUTINE calc_ener(eta,h,u,v,ener,ener_cin)

USE PARAMETRES

IMPLICIT NONE

REAL :: h(n,Ny+1,Nx+1), u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1)
REAL :: ener(3), ener_cin(n,Ny+1,Nx+1)

INTEGER :: i,j

ener(1) = 0
ener(2) = 0

DO j = 1,Nx
   DO i = 1,Ny

      ener(1) = ener(1) + g*(h(1,i,j)+h(2,i,j))**2 + 0.5*h(1,i,j) * & 
           (u(1,i,j+1)**2 + u(1,i,j)**2 + v(1,i+1,j)**2 + v(1,i,j)**2)

      ener(2) = ener(2) + g*h(2,i,j)**2 + 0.5*h(2,i,j) * & 
           (u(2,i,j+1)**2 + u(2,i,j)**2 + v(2,i+1,j)**2 + v(2,i,j)**2)

   END DO
END DO

ener(3) = ener(1) + ener(2)

DO j = 1,Nx+1
   DO i = 1,Ny+1
      
      ener_cin(1,i,j) = u(1,i,j)**2 + v(1,i,j)**2
      ener_cin(2,i,j) = u(2,i,j)**2 + v(2,i,j)**2

   END DO
END DO



END SUBROUTINE calc_ener



