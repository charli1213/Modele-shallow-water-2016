

SUBROUTINE CALC_RHS_SPEED(u,v,B,Qu,Qv,rhs_u,rhs_v,lap_u,lap_v)

  ! Fonction qui calcule les variations de u et de v
  ! IN : B, Qu, Qv, lap_u, lap_v, u, v
  ! OUT : rhs_u, rhs_v

  use parametres
  IMPLICIT NONE

  REAL, dimension(Nx+1,Ny+1,Nz) :: B, Qu, Qv, rhs_u, rhs_v, lap_u, lap_v
  REAL, dimension(Nx+1,Ny+1,Nz) :: u, v

  INTEGER :: i,j,k

  DO k = 1,Nz


     DO i = 2,Nx+1
        DO j = 1,Ny

           rhs_u(i,j,k) = 0.5*(Qv(i,j,k)+Qv(i,j+1,k)) & 
                - (B(i,j,k)-B(i-1,j,k))/dx - mu*lap_u(i,j,k) &
                + (2-k)*a_tau*cos(pi*(j-Ny/2)*dy/Ly) & ! Vent en u
                - (k-1)*res*u(i,j,k) ! Frottement

        END DO
     END DO
     
     ! Périodique

     rhs_u(1,:,k)    = rhs_u(Nx+1,:,k)
     rhs_u(:,Ny+1,k) = rhs_u(:,1,k)


  END DO

  ! Périodique

  !rhs_u(1,:,:) = rhs_u(Nx+1,:,:)
  !rhs_u(:,Ny+1,:) = rhs_u(:,1,:)

  DO k = 1,Nz


     DO i = 1,Nx
        DO j = 2,Ny+1

           rhs_v(i,j,k) = -0.5*(Qu(i,j,k)+Qu(i+1,j,k)) & 
                - (B(i,j,k)-B(i,j-1,k))/dy - mu*lap_v(i,j,k)&
                - (k-1)*res*v(i,j,k)
           
        END DO
     END DO

     ! Périodique

     rhs_v(Nx+1,:,k) = rhs_v(1,:,k)
     rhs_v(:,1,k)    = rhs_v(:,Ny+1,k)


  END DO


  ! Périodique

  !rhs_v(Nx+1,:,:) = rhs_v(1,:,:)
  !rhs_v(:,1,:) = rhs_v(:,Ny+1,:)

  !DO j = 1, Ny+1
     !print *, j, 'rhs_v(100,j,1)', rhs_v(100,j,1)
  !END DO

  !STOP

 
END SUBROUTINE CALC_RHS_SPEED

