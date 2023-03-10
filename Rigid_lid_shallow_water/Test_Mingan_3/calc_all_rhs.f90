


  SUBROUTINE calc_all_rhs(ubc,vbc,eta,psi,uxi,vxi,u,v,hleft,hdown, & 
       pr2c,pc2r,k2,datc,f,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi,rhs_uxi,rhs_vxi)


    ! Sous_routine calculant tous les rhs utiles pour le time step

    ! IN  : psi,eta,ubc,vbc,uxi,vxi,u,v,hleft,hdown,pr2c,pc2r,k2,f,datc
    ! OUT : rhs_psi, rhs_eta, rhs_ubc, rhs_vbc, rhs_uxi, rhs_vxi


    Use Parametres
    IMPLICIT NONE

    ! Variables

    REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, eta, psi
    REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown
    REAL                          :: uxi, vxi

    ! RHS

    REAL, dimension(Nx+1,Ny+1)    :: rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
    REAL, dimension(Nx+1,Ny+1)    :: rhs_ubt, rhs_vbt, rhs_zheta
    REAL, dimension(Nx+1,Ny+1,Nz) :: rhs_u, rhs_v
    REAL                          :: rhs_uxi, rhs_vxi

    ! FFT

    INTEGER*8                             :: pr2c, pc2r
    REAL, dimension(Nx/2+1,Ny)            :: k2
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc


    REAL, dimension(Ny+1)                 :: f

    INTEGER                               :: j


    CALL rhs_speed_eta(u,v,hleft,hdown,eta,f,rhs_u,rhs_v,rhs_eta)

    CALL calc_rhs_speed_bt(ubc,vbc,rhs_u,rhs_v,rhs_eta,hleft,hdown, & 
       rhs_ubt,rhs_vbt)
    
    CALL calc_rhs_averaged_velo(rhs_ubt, rhs_vbt, rhs_uxi, rhs_vxi)

    CALL calc_rhs_speed_bc(rhs_u,rhs_v,rhs_ubc,rhs_vbc)

    CALL calc_rhs_zheta(rhs_ubt, rhs_vbt, rhs_zheta)

    CALL calc_rhs_psi(pr2c,pc2r,k2,rhs_zheta(1:Nx,1:Ny),datc,rhs_psi(1:Nx,1:Ny))

    ! rhs_psi périodique

    rhs_psi(Nx+1,:) = rhs_psi(1,:)
    rhs_psi(:,Ny+1) = rhs_psi(:,1)


    
  END SUBROUTINE calc_all_rhs
