

SUBROUTINE calc_flux(u,v,h,Ux, Vy, QU, QV, A, flux_u, flux_v, &
     flux_h, lap_u, lap_v, masqueX, masqueY,TAU)

  USE Parametres

  IMPLICIT NONE

  
  REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), h(n,Ny+1,Nx+1)
  REAL :: Ux(n,Ny+1,Nx+1), Vy(n,Ny+1,Nx+1)
  REAL :: QU(n,Ny+1,Nx+1), QV(n,Ny+1,Nx+1)
  REAL :: A(n,Ny+1,Nx+1), lap_u(n,Ny+1,Nx+1), lap_v(n,Ny+1,Nx+1)
  REAL :: TAU(Ny+1)
  
  REAL :: flux_u(n,Ny+1,Nx+1), flux_v(n,Ny+1,Nx+1), flux_h(n,Ny+1,Nx+1)
  
  REAL ::  masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)

  INTEGER :: i,j



  ! =================Calcul des flux pour la deuxième couche===============


  ! Flux pour u2


  DO j = 2,Nx+1
     DO i = 1,Ny
        
        flux_u(2,i,j) = -(A(2,i,j) - A(2,i,j-1)) / dx + QV(2,i,j) &
             + mu*lap_u(2,i,j) - res*u(2,i,j)
        
     END DO
  END DO

  flux_u(2,Ny+1,:) = flux_u(2,1,:)
  flux_u(2,:,1)    = flux_u(2,:,Nx+1)



  ! Flux pour v2


  DO j = 1,Nx
     DO i = 2,Ny+1
        
        flux_v(2,i,j) = -(A(2,i,j) - A(2,i-1,j)) / dy - QU(2,i,j) &
             + mu*lap_v(2,i,j) - res*v(2,i,j)
        
     END DO
  END DO


  flux_v(2,1,:)    = flux_v(2,Ny+1,:)
  flux_v(2,:,Nx+1) = flux_v(2,:,1)
  


  ! Flux pour h2


  DO j = 1,Nx
     DO i = 1,Ny
        
        flux_h(2,i,j) = -(Ux(2,i,j+1) - Ux(2,i,j)) / dx &
             - (Vy(2,i+1,j) - Vy(2,i,j)) / dy
     END DO
  END DO

  flux_h(2,Ny+1,:) = flux_h(2,1,:)
  flux_h(2,:,Nx+1) = flux_h(2,:,1)
  


    ! =========== Calcul des flux pour la première couche ==============
  

  ! Flux pour u


  DO j = 2,Nx+1
     DO i = 1,Ny
        
        flux_u(1,i,j) = -(A(1,i,j) - A(1,i,j-1)) / dx + QV(1,i,j) &
             + mu*lap_u(1,i,j) + a_tau*tau(i)/(rho1*h(1,i,j))
        
     END DO
  END DO


  flux_u(1,:,1)    = flux_u(1,:,Nx+1)
  flux_u(1,Ny+1,:) = flux_u(1,1,:)



  

  ! Flux pour v


  DO j = 1,Nx
     DO i = 2,Ny+1
        
        flux_v(1,i,j) = -(A(1,i,j) - A(1,i-1,j)) / dy - QU(1,i,j) &
             + mu*lap_v(1,i,j)

     END DO
  END DO

  flux_v(1,1,:)    = flux_v(1,Ny+1,:)
  flux_v(1,:,Nx+1) = flux_v(1,:,1)


  
  


  ! Flux pour eta

  DO j = 1,Nx
     DO i = 1,Ny
        
        flux_h(1,i,j) = -(Ux(1,i,j+1) - Ux(1,i,j)) / dx &
             - (Vy(1,i+1,j) - Vy(1,i,j)) / dy + flux_h(2,i,j)
     END DO
  END DO
  
  flux_h(1,:,Nx+1) = flux_h(1,:,1)
  flux_h(1,Ny+1,:) = flux_h(1,1,:)

  

  
END SUBROUTINE calc_flux
