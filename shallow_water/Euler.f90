


SUBROUTINE Euler(u,v,eta,flux_u,flux_v,flux_h,masqueX,masqueY)


  USE Parametres

  
  IMPLICIT none 

  REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1)
  REAL :: flux_u(n,Ny+1,Nx+1), flux_v(n,Ny+1,Nx+1), flux_h(n,Ny+1,Nx+1)

  REAL :: masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)

  INTEGER :: i,j

  DO j = 1,Nx+1
     DO i = 1,Ny+1
        
        u(1,i,j)   = u(1,i,j) + (dt)*flux_u(1,i,j)
        v(1,i,j)   = v(1,i,j) + (dt)*flux_v(1,i,j)
        eta(1,i,j) = eta(1,i,j) + (dt)*flux_h(1,i,j)

        u(2,i,j)   = u(2,i,j) + (dt)*flux_u(2,i,j)
        v(2,i,j)   = v(2,i,j) + (dt)*flux_v(2,i,j)
        eta(2,i,j) = eta(2,i,j) + (dt)*flux_h(2,i,j)
        
     END DO
  END DO

  u(1,:,:) = masqueX*u(1,:,:)
  u(2,:,:) = masqueX*u(2,:,:)

  v(1,:,:) = masqueY*v(1,:,:)
  v(2,:,:) = masqueY*v(2,:,:)

END SUBROUTINE Euler
