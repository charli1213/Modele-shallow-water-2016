

SUBROUTINE calc_nb_onde(kx, ky, k2, kxo, kyo, ko2)

  ! Sous-routine calculant les nombres d'onde pour fftw
  
  ! IN  : kx, ky, k2, kxo, kyo, ko2
  ! OUT : same
  
  Use Parametres
  IMPLICIT NONE
  
  
  REAL, dimension(Nx/2+1,Ny)             :: kx, ky, k2, k22
  REAL, dimension(Nx,Ny-1)               :: kxo, kyo, ko2
  
  INTEGER :: i,j
  
  DO j = 1,Ny
     DO i = 1,Nx/2+1
        
        IF (i < (Nx/2+2)) THEN
           
           kx(i,j) = tpi*(i-1)/Lx
           
        ELSE
           
           kx(i,j) = tpi*(-Nx+i-1)/Lx
           
        END IF
        
        IF (j < (ny/2+2)) THEN
           
           ky(i,j) = tpi*(j-1)/Ly 
           
        ELSE
           
           ky(i,j) = tpi*(-Ny+j-1)/Ly
           
        END IF
        
     END DO
  END DO
  
  k2 = kx**2 + ky**2
  
  DO j = 1,Ny-1
     DO i = 1,Nx
        
        IF (i < (Nx/2+2)) THEN
           
           kxo(i,j) = tpi*(i-1)/Lx
           
        ELSE
           
           kxo(i,j) = tpi*(-Nx+i-1)/Lx
           
        END IF
        
        kyo(i,j) = tpi*(j-1)/Ly/2
        
     END DO
  END DO
  
  ko2  = kxo**2 + kyo**2

  
  
  
END SUBROUTINE calc_nb_onde
   
