

  SUBROUTINE laplacien(u,v,lap_u,lap_v)

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx+1,Ny+1,Nz) :: u,v,lap_u,lap_v

    INTEGER                       :: i,j,k

    DO k = 1,Nz
       DO i = 2,Nx
          DO j =2,Ny

             lap_u(i,j,k) = (u(i,j+1,k) - 2*u(i,j,k) + u(i,j-1,k)) / dx**2 &
                  + (u(i+1,j,k) - 2*u(i,j,k) + u(i-1,j,k)) / dy**2

             lap_v(i,j,k) = (v(i,j+1,k) - 2*v(i,j,k) + v(i,j-1,k)) / dx**2 &
                  + (v(i+1,j,k) - 2*v(i,j,k) + v(i-1,j,k)) / dy**2

          END DO
       END DO
    END DO

    DO k =1,Nz
       

       ! Côté gauche
       
       lap_u(1,2:Ny,k) = (u(1,3:Ny+1,k) - 2*u(1,2:Ny,k) + u(1,1:Ny-1,k))/dx**2 &
            + (u(2,2:Ny,k) - 2*u(1,2:Ny,k) + u(Nx,2:Ny,k)) / dy**2

       lap_v(1,2:Ny,k) = (v(1,3:Ny+1,k) - 2*v(1,2:Ny,k) + v(1,1:Ny-1,k))/dx**2 &
            + (v(2,2:Ny,k) - 2*v(1,2:Ny,k) + v(Nx,2:Ny,k)) / dy**2


       ! Ligne du haut

       lap_u(2:Nx,1,k) = (u(2:Nx,2,k) - 2*u(2:Nx,1,k) + u(2:Nx,Ny,k)) / dx**2 &
            + (u(3:Nx+1,1,k) - 2*u(2:Nx,1,k) + u(1:Nx-1,1,k)) / dy**2

       lap_v(2:Nx,1,k) = (v(2:Nx,2,k) - 2*v(2:Nx,1,k) + v(2:Nx,Ny,k)) / dx**2 &
            + (v(3:Nx+1,1,k) - 2*v(2:Nx,1,k) + v(1:Nx-1,1,k)) / dy**2


       ! Coin en haut à gauche
       lap_u(1,1,k) = (u(2,1,k) - 2*u(1,1,k) + u(Nx,1,k)) / dx**2 &
            + (u(1,2,k) - 2*u(1,1,k) + u(1,Ny,k)) / dy**2

       lap_v(1,1,k) = (v(2,1,k) - 2*v(1,1,k) + v(Nx,1,k)) / dx**2 &
            + (v(1,2,k) - 2*v(1,1,k) + v(1,Ny,k)) / dy**2
 

       ! Périodocité
       
       lap_u(Ny+1,:,k) = lap_u(1,:,k) 
       lap_v(Ny+1,:,k) = lap_v(1,:,k)

       lap_u(:,Nx+1,k) = lap_u(:,1,k) 
       lap_v(:,Nx+1,k) = lap_v(:,1,k)

    END DO


  END SUBROUTINE laplacien

             
