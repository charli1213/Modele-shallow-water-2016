

  SUBROUTINE AB3_i(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi, & 
       rhs_uxi, rhs_vxi, f, pr2c, pc2r, k2, datc)

    ! Pas suivants faits avec la méthode AB3

    ! IN  : ubc, vbc, eta, psi, rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
    ! OUT : ubc, vbc, eta, psi, rhs_ubc, rhs_vbc, rhs_eta, rhs_psi

    Use Parametres
    IMPLICIT NONE

     REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, eta, psi, ubt, vbt, eta_b
     REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown
     REAL                          :: uxi, vxi, ener(2)

     REAL, dimension(Nx+1,Ny+1,3)  :: rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
     REAL, dimension(Nx+1,Ny+1)    :: rhs_u, rhs_v, rhs_ubt, rhs_vbt
     REAL, dimension(3)            :: rhs_uxi, rhs_vxi

     INTEGER*8                             :: pr2c, pc2r
     REAL, dimension(Nx/2+1,Ny)            :: k2
     DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc

     REAL, dimension(Ny+1)         :: f

    ! Calcul des variables dépendantes de psi,eta,ubc,vbc

     CALL calc_variables_dependantes(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,u,v,ubt,vbt,& 
       hleft,hdown)

    ! Calcul des rhs pour le time step
    
    CALL calc_all_rhs(ubc,vbc,eta,psi,uxi,vxi,u,v,hleft,hdown,pr2c,pc2r,k2,  & 
         datc,f,rhs_ubc(:,:,3),rhs_vbc(:,:,3),rhs_eta(:,:,3),rhs_psi(:,:,3), & 
         rhs_uxi(3), rhs_vxi(3))
    

    ! Time step à l'aide de la méthode d'Euler

    ubc(:,:) = ubc(:,:) + ((dt/12.0)/N_init) & 
         * (23.0*rhs_ubc(:,:,3) - 16.0*rhs_ubc(:,:,2) + 5.0*rhs_ubc(:,:,1))

    vbc(:,:) = vbc(:,:) + ((dt/12.0)/N_init) & 
         * (23.0*rhs_vbc(:,:,3) - 16.0*rhs_vbc(:,:,2) + 5.0*rhs_vbc(:,:,1))

    eta(:,:) = eta(:,:) + ((dt/12.0)/N_init) & 
         * (23.0*rhs_eta(:,:,3) - 16.0*rhs_eta(:,:,2) + 5.0*rhs_eta(:,:,1))

    psi(:,:) = psi(:,:) + ((dt/12.0)/N_init) & 
         * (23.0*rhs_psi(:,:,3) - 16.0*rhs_psi(:,:,2) + 5.0*rhs_psi(:,:,1))

    uxi = uxi + (dt/12.0)*(23.0*rhs_uxi(3) - 16.0*rhs_uxi(2) + 5.0*rhs_uxi(1))
    
    vxi = vxi + (dt/12.0)*(23.0*rhs_vxi(3) - 16.0*rhs_vxi(2) + 5.0*rhs_vxi(1))



  END SUBROUTINE AB3_i
