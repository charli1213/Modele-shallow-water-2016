

  SUBROUTINE calc_speed_bt(psi,uxi,vxi,ubt,vbt)

    ! Sous-routine calculant les vitesses barotropiques à partir de 
    ! la fonction courant psi

    ! IN  : psi,uxi,vxi
    ! OUT : ubt, vbt 


    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx+1,Ny+1) :: psi, ubt, vbt
    REAL                       :: uxi, vxi
    
    INTEGER :: i, j


    DO i = 1,Nx
       DO j = 1,Ny

          ubt(i,j) = -(psi(i,j+1) - psi(i,j))/dy + uxi
          vbt(i,j) =  (psi(i+1,j) - psi(i,j))/dx + vxi

       END DO
    END DO

    

    ! Périodique

    ubt(:,Ny+1) = ubt(:,1)
    ubt(Nx+1,:) = ubt(1,:)

    vbt(:,Ny+1) = vbt(:,1)
    vbt(Nx+1,:) = vbt(1,:)


  END SUBROUTINE calc_speed_bt

    
