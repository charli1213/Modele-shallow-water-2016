

  SUBROUTINE calc_rhs_speed_bc(rhs_u,rhs_v,rhs_ubc,rhs_vbc)
    ! Sous-routine calculant rhs des vitesse baroclines ubc et vbc. 
    
    ! IN  : rhs_u, rhs_v
    ! OUT : rhs_ubc,rhs_vbc

    USE Parametres
    IMPLICIT NONE


    REAL, dimension(Nx+1,Ny+1,Nz) :: rhs_u, rhs_v
    REAL, dimension(Nx+1,Ny+1)    :: rhs_ubc, rhs_vbc

    INTEGER :: i,j

    rhs_ubc(:,:) = rhs_u(:,:,2) - rhs_u(:,:,1)
    rhs_vbc(:,:) = rhs_v(:,:,2) - rhs_v(:,:,1)

    


  END SUBROUTINE calc_rhs_speed_bc
