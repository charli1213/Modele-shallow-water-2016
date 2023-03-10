


SUBROUTINE BERNOUILLI(u,v,eta,B)
  ! Fonction qui calcule la «Bernouilli function» de chaque couche
  ! IN : u,v,eta
  ! OUT : B

  USE Parametres
  IMPLICIT NONE
  
  REAL :: u(Nx+1,Ny+1,Nz), v(Nx+1,Ny+1,Nz), eta(Nx+1,Ny+1)
  REAL :: B(Nx+1,Ny+1,Nz)
  
  INTEGER :: i,j,k

  DO k = 1,Nz
     DO i = 1,Nx
        DO j = 1,Ny

           B(i,j,k) = (k-1)*g*eta(i,j) + 0.25 * (u(i,j,k)**2 + u(i+1,j,k)**2 & 
                + v(i,j,k)**2 + v(i,j+1,k)**2) 
        END DO
     END DO
  END DO

  
  ! Périodicité :
  B(Nx+1,:,:) = B(1,:,:)
  B(:,Ny+1,:) = B(:,1,:)

END SUBROUTINE BERNOUILLI
