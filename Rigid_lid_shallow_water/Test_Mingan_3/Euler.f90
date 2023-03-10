

  SUBROUTINE Euler(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi, & 
       rhs_uxi, rhs_vxi, f, pr2c, pc2r, k2, datc)

    ! Premier pas de temps fait avec la méthode d'Euler

    ! IN  : ubc, vbc, eta, psi, uxi, vxi, f, pr2c, pc2r, k2, datc
    ! OUT : ubc,vbc,eta,psi,uxi,vxi,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi,
    !       rhs_uxi,rhs_vxi



    USE Parametres
    IMPLICIT NONE



     REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, eta, psi, ubt, vbt, eta_b
     REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown
     REAL                          :: uxi, vxi, ener(2)

     REAL, dimension(Nx+1,Ny+1,3)  :: rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
     REAL, dimension(3)            :: rhs_uxi, rhs_vxi
     REAL, dimension(Nx+1,Ny+1)    :: rhs_u, rhs_v, rhs_ubt, rhs_vbt

     INTEGER*8                             :: pr2c, pc2r
     REAL, dimension(Nx/2+1,Ny)            :: k2
     DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc

     REAL, dimension(Ny+1)         :: f




    ! Calcul des variables dépendantes de psi,eta,ubc,vbc

     CALL calc_variables_dependantes(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,u,v,ubt,vbt,& 
       hleft,hdown)

    ! Calcul des rhs pour le time step
    
     CALL calc_all_rhs(ubc,vbc,eta,psi,uxi,vxi,u,v,hleft,hdown,pr2c,pc2r,k2, & 
          datc,f,rhs_ubc(:,:,1),rhs_vbc(:,:,1),rhs_eta(:,:,1),rhs_psi(:,:,1), & 
          rhs_uxi(1), rhs_vxi(1))



    ! Time step à l'aide de la méthode d'Euler

    ubc(:,:) = ubc(:,:) + (dt/N_init) * rhs_ubc(:,:,1)
    vbc(:,:) = vbc(:,:) + (dt/N_init) * rhs_vbc(:,:,1)
    eta(:,:) = eta(:,:) + (dt/N_init) * rhs_eta(:,:,1)
    psi(:,:) = psi(:,:) + (dt/N_init) * rhs_psi(:,:,1)

    uxi      =   uxi    + (dt/N_init) * rhs_uxi(1)
    vxi      =   vxi    + (dt/N_init) * rhs_vxi(1)

    !ubc(:,:) = ubc(:,:) + (dt) * rhs_ubc(:,:,1)
    !vbc(:,:) = vbc(:,:) + (dt) * rhs_vbc(:,:,1)
    !eta(:,:) = eta(:,:) + (dt) * rhs_eta(:,:,1)
    !psi(:,:) = psi(:,:) + (dt) * rhs_psi(:,:,1)

    !uxi      =   uxi    + (dt) * rhs_uxi(1)
    !vxi      =   vxi    + (dt) * rhs_vxi(1)

    


  END SUBROUTINE Euler
    
