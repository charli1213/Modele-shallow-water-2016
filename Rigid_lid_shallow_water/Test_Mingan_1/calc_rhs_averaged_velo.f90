

  SUBROUTINE calc_rhs_averaged_velo(rhs_ubt,rhs_vbt,rhs_uxi,rhs_vxi)

    ! Calcul des rhs des moyennes des vitesses barotropes 

    ! IN  : rhs_ubt, rhs_vbt
    ! OUT : rhs_uxi, rhs_vxi

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx+1,Ny+1) :: rhs_ubt, rhs_vbt
    REAL                       :: rhs_uxi, rhs_vxi

    rhs_uxi= sum(rhs_ubt(:,:))/Nx/Ny
    rhs_vxi= sum(rhs_vbt(:,:))/Nx/Ny

  END SUBROUTINE calc_rhs_averaged_velo
