

SUBROUTINE FLUXES(u,v,hleft,hdown,Ux,Vy)

  ! Fonction qui calcule les flux
  ! IN : u,v,hleft,hdown
  ! OUT : Ux,Vy

  USE parametres
  IMPLICIT NONE

  REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown, Ux, Vy

  INTEGER :: i,j,k
  
  DO k = 1,Nz
     DO i = 1,Nx+1
        Do j = 1,Ny+1

           Ux(i,j,k) = u(i,j,k)*hleft(i,j,k)
           Vy(i,j,k) = v(i,j,k)*hdown(i,j,k)

        END DO
     END DO
  END DO



END SUBROUTINE FLUXES
