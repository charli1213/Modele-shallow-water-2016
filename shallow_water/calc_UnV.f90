

SUBROUTINE calc_UnV(u,v,h,Ux,Vy)

USE Parametres

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), h(n,Ny+1,Nx+1)
REAL :: Ux(n,Ny+1,Nx+1), Vy(n,Ny+1,Nx+1)

INTEGER :: i,j


DO j = 2,Nx+1
     DO i = 1,Ny+1

        Ux(1,i,j) = 0.5*(H1)*u(1,i,j) !0.5*(h(1,i,j-1) + h(1,i,j))*u(1,i,j)
        Ux(2,i,j) = 0.5*(H2)*u(2,i,j) !0.5*(h(2,i,j-1) + h(2,i,j))*u(2,i,j)

     END DO
  END DO

Ux(1,:,1) = Ux(1,:,Nx+1)
Ux(2,:,1) = Ux(2,:,Nx+1)


DO i = 2,Ny+1
     DO j = 1,Nx+1

        Vy(1,i,j) = 0.5*(H1)*v(1,i,j) !0.5*(h(1,i-1,j) + h(1,i,j))*v(1,i,j)
        Vy(2,i,j) = 0.5*(H2)*v(2,i,j) !0.5*(h(2,i-1,j) + h(2,i,j))*v(2,i,j)

     END DO
  END DO

  
  Vy(1,1,:) = Vy(1,Ny+1,:)
  Vy(2,1,:) = Vy(2,Ny+1,:)

END SUBROUTINE calc_UnV
