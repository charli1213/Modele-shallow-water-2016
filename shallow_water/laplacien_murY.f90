SUBROUTINE laplacien(u,lap_u,v,lap_v)

USE Parametres

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1)
REAL :: lap_u(n,Ny+1,Nx+1), lap_v(n,Ny+1,Nx+1)

INTEGER :: i,j

DO j = 2,Nx
     DO i = 2,Ny
        
        lap_u(1,i,j) = (u(1,i,j+1) - 2*u(1,i,j) + u(1,i,j-1)) / dx**2 &
             + (u(1,i+1,j) - 2*u(1,i,j) + u(1,i-1,j)) / dy**2

        lap_u(2,i,j) = (u(2,i,j+1) - 2*u(2,i,j) + u(2,i,j-1)) / dx**2 &
             + (u(2,i+1,j) - 2*u(2,i,j) + u(2,i-1,j)) / dy**2

        lap_v(1,i,j) = (v(1,i,j+1) - 2*v(1,i,j) + v(1,i,j-1)) / dx**2 &
             + (v(1,i+1,j) - 2*v(1,i,j) + v(1,i-1,j)) / dy**2 

        lap_v(2,i,j) = (v(2,i,j+1) - 2*v(2,i,j) + v(2,i,j-1)) / dx**2 &
             + (v(2,i+1,j) - 2*v(2,i,j) + v(2,i-1,j)) / dy**2 

     END DO
  END DO



! Ligne du haut
lap_u(1,1,2:Nx) = (u(1,1,3:Nx+1) - 2*u(1,1,2:Nx) + u(1,1,1:Nx-1)) / dx**2 &
     + (u(1,2,2:Nx) - 2*u(1,1,2:Nx) + 0) / dy**2

lap_u(2,1,2:Nx) = (u(2,1,3:Nx+1) - 2*u(2,1,2:Nx) + u(2,1,1:Nx-1)) / dx**2 &
     + (u(2,2,2:Nx) - 2*u(2,1,2:Nx) + 0) / dy**2

lap_v(1,1,2:Nx) = (v(1,1,3:Nx+1) - 2*v(1,1,2:Nx) + v(1,1,1:Nx-1)) / dx**2 &
     + (v(1,2,2:Nx) - 2*v(1,1,2:Nx) + 0) / dy**2 

lap_v(2,1,2:Nx) = (v(2,1,3:Nx+1) - 2*v(2,1,2:Nx) + v(2,1,1:Nx-1)) / dx**2 &
     + (v(2,2,2:Nx) - 2*v(2,1,2:Nx) + 0) / dy**2 



! Côté gauche
lap_u(1,2:Ny,1) = (u(1,2:Ny,2) - 2*u(1,2:Ny,1) + u(1,2:Ny,Nx)) / dx**2 &
     + (u(1,3:Ny+1,1) - 2*u(1,2:Ny,1) + u(1,1:Ny-1,1)) / dy**2

lap_u(2,2:Ny,1) = (u(2,2:Ny,2) - 2*u(2,2:Ny,1) + u(2,2:Ny,Nx)) / dx**2 &
     + (u(2,3:Ny+1,1) - 2*u(2,2:Ny,1) + u(2,1:Ny-1,1)) / dy**2

lap_v(1,2:Ny,1) = (v(1,2:Ny,2) - 2*v(1,2:Ny,1) + v(1,2:Ny,Nx)) / dx**2 &
     + (v(1,3:Ny+1,1) - 2*v(1,2:Ny,1) + v(1,1:Ny-1,1)) / dy**2 

lap_v(2,2:Ny,1) = (v(2,2:Ny,2) - 2*v(2,2:Ny,1) + v(2,2:Ny,Nx)) / dx**2 &
     + (v(2,3:Ny+1,1) - 2*v(2,2:Ny,1) + v(2,1:Ny-1,1)) / dy**2 

! Coin en haut à gauche
lap_u(1,1,1) = (u(1,1,2) - 2*u(1,1,1) + u(1,1,Nx)) / dx**2 &
     + (u(1,2,1) - 2*u(1,1,1) + 0) / dy**2

lap_u(2,1,1) = (u(2,1,2) - 2*u(2,1,1) + u(2,1,Nx)) / dx**2 &
     + (u(2,2,1) - 2*u(2,1,1) + 0) / dy**2

lap_v(1,1,1) = (v(1,1,2) - 2*v(1,1,1) + v(1,1,Nx)) / dx**2 &
     + (v(1,2,1) - 2*v(1,1,1) + 0) / dy**2 

lap_v(2,1,1) = (v(2,1,2) - 2*v(2,1,1) + v(2,1,Nx)) / dx**2 &
     + (v(2,2,1) - 2*v(2,1,1) + 0) / dy**2 

! Périodocité

lap_u(1,Ny+1,:) = lap_u(1,1,:) 
lap_u(2,Ny+1,:) = lap_u(2,1,:)
lap_v(1,Ny+1,:) = lap_v(1,1,:)
lap_v(2,Ny+1,:) = lap_v(2,1,:)

lap_u(1,:,Nx+1) = lap_u(1,:,1) 
lap_u(2,:,Nx+1) = lap_u(2,:,1)
lap_v(1,:,Nx+1) = lap_v(1,:,1)
lap_v(2,:,Nx+1) = lap_v(2,:,1)



END SUBROUTINE laplacien
