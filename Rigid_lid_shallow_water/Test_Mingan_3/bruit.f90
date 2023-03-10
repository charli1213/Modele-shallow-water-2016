

SUBROUTINE bruit(eta,x,y)

USE Parametres

IMPLICIT NONE

REAL :: eta(Ny+1,Nx+1), fourier_rand(Ny,Nx), x(Nx), y(Ny)


INTEGER :: i,j 

REAL :: r1, r2, r3, r4, r5, t1, t2, t3, t4, t5



r1 = Ly/25*(1 + rand(0))
r2 = Ly/25*(1 + rand(0))
r3 = Ly/25*(1 + rand(0))
r4 = Ly/25*(1 + rand(0))
r5 = Ly/25*(1 + rand(0))

t1 = 10*(1 + rand(0))
t2 = 15*(1 + rand(0))
t3 = 20*(1 + rand(0))
t4 = 25*(1 + rand(0))
t5 = 30*(1 + rand(0))

DO j = 1,Ny
   DO i = 1,Nx

      fourier_rand(i,j) = (sin(2*pi*(x(i)-t1)/r1)& 
           + sin(2*pi*(y(j)-t2)/r2) & 
           + sin(2*pi*(x(i)-t3)/r3) + sin(2*pi*(y(j) - t4)/r4) & 
           + sin(2*pi*(x(i)-t5)/r5))
   END DO
END DO

DO j = 1,Ny
   DO i = 1,Nx
      !eta(2,i,j) = eta(2,i,j) + rand(0)
      !eta(2,i,j) = 0.8*eta(2,i,j) + 0.2*eta(2,i,j)*rand(0)
      eta(i,j) = eta(i,j) + 0.01*fourier_rand(i,j)
   END DO
END DO
eta(Nx+1,:) = eta(1,:)
eta(:,Ny+1) = eta(:,1)



END SUBROUTINE bruit
