

SUBROUTINE Q_SPEED(u,v,Q,Qu,Qv)

  ! Fonction qui calcule la moyenne des vitesse multiplié par Q.
  ! IN : u, v, Q
  ! OUT : Qu,Qv
  
  USE parametres
  IMPLICIT NONE
  
  REAL :: u(Nx+1,Ny+1,Nz), v(Nx+1,Ny+1,Nz)
  REAL :: Q(Nx+1,Ny+1,Nz)
  REAL :: Qu(Nx+1,Ny+1,Nz), Qv(Nx+1,Ny+1,Nz)

  INTEGER :: i,j,k
  
  DO k = 1,Nz
     DO i = 2,Nx+1
        DO j = 1,Ny+1

           Qv(i,j,k) = 0.5*(v(i,j,k)+v(i-1,j,k))*Q(i,j,k)

        END DO
     END DO
  END DO
  
  Qv(1,:,:) = Qv(Nx+1,:,:)
  
  DO k = 1,Nz
     DO i = 1,Nx+1
        DO j = 2,Ny+1

           Qu(i,j,k) = 0.5*(u(i,j,k)+u(i,j-1,k))*Q(i,j,k)

        END DO
     END DO
  END DO
  
  Qu(:,1,:) = Qu(:,Ny+1,:)

  

END SUBROUTINE Q_SPEED
