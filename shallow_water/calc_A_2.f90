


SUBROUTINE calc_A(u,v,eta,b,A)

USE Parametres

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1)
REAL :: A(n,Ny+1,Nx+1), b(Ny+1,Nx+1)

INTEGER :: i,j

DO j = 1,Nx
     DO i =1,Ny
        
        A(1,i,j) = 0.25*(u(1,i,j)**2 + u(1,i,j+1)**2 + v(1,i+1,j)**2 + v(1,i,j)**2)  &
             + g*eta(1,i,j)
        
        A(2,i,j) = 0.25*(u(2,i,j)**2 + u(2,i,j+1)**2 + v(2,i+1,j)**2 + v(2,i,j)**2) & 
             + g2 * eta(2,i,j) +  g * eta(1,i,j)
     END DO
  END DO

A(1,:,Nx+1) = A(1,:,1)
A(1,Ny+1,:) = A(1,1,:)

A(2,:,Nx+1) = A(2,:,1)
A(2,Ny+1,:) = A(2,1,:)


END SUBROUTINE calc_A
