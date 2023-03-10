

SUBROUTINE calc_rhs_eta(Ux,Vy,rhs_eta)
  ! Sous-routine qui calcules la variation de eta à chaque itérations.
  ! IN : Ux, Vy
  ! OUT : rhs_eta(:,:,X)
  
  ! COMMENTAIRES :  
  ! - On n'a pas besoin d'updater la valeur de rhs_eta pour la première couche
  ! car on considère un rigid lid. On prend donc seulement la variation de la
  ! seconde couche.

  USE Parametres
  IMPLICIT NONE

  REAL, dimension(Nx+1,Ny+1,Nz) :: Ux, Vy
  REAL, dimension(Nx+1,Ny+1)    :: rhs_eta

  INTEGER :: i,j,k
  
  k = 2

  DO i = 1,Nx
     DO j = 1,Ny

        rhs_eta(i,j) = -(Ux(i+1,j,k)-Ux(i,j,k))/dx - (Vy(i,j+1,k)-Vy(i,j,k))/dy

     END DO
  end do
  
  rhs_eta(Nx+1,:) = rhs_eta(1,:)
  rhs_eta(:,Ny+1) = rhs_eta(:,1)

END SUBROUTINE CALC_RHS_ETA
