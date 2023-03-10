


SUBROUTINE VORT(u,v,f,Q)

  ! Fonction qui calcule la valeur de la vorticité totale dans chaaque couche.
  ! IN : u,v,f
  ! OUT : Q

  USE Parametres
  IMPLICIT NONE

  REAL :: u(Nx+1,Ny+1,Nz), v(Nx+1,Ny+1,Nz)
  REAL :: f(Ny+1)
  REAL :: Q(Nx+1,Ny+1,Nz)

  INTEGER :: i,j,k
  
  DO k = 1,Nz
     DO i = 2,Nx+1
        DO j = 2,Ny+1

           Q(i,j,k) = f(j) + (v(i,j,k)-v(i-1,j,k))/dx - (u(i,j,k)-u(i,j-1,k))/dy 

        END DO
     END DO
  END DO

  
  Q(1,:,:) = Q(Nx+1,:,:)
  Q(:,1,:) = Q(:,Ny+1,:)

  

END SUBROUTINE VORT
