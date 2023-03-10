

  SUBROUTINE initialisation(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc,rhs_vbc,rhs_eta, & 
       rhs_psi, rhs_uxi, rhs_vxi, f, pr2c, pc2r, k2, datc)

    ! Initialisation, on sépare les deux premiers pas de temps en N_init fois.
    ! Premier pas avec Euler, deuxième avec AB2, suivants avec AB3.
    ! On sauvegarde les "vrai" rhs quand on tombe sur dt et 2*dt

    ! IN  : ubc, vbc, eta, psi, f, pr2c, pc2r, k2, datc, eta_b
    ! OUT : rhs_ubc, rhs_vbc, rhs_eta, rhs_psi

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx+1,Ny+1)   :: ubc, vbc, eta, psi, eta_b
    REAL                         :: uxi, vxi, ener(2)
    REAL, dimension(Nx+1,Ny+1,3) :: rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
    REAL, dimension(Nx+1,Ny+1,3) :: rhs_ubc_i, rhs_vbc_i, rhs_eta_i, rhs_psi_i
    REAL, dimension(3)           :: rhs_uxi, rhs_vxi, rhs_uxi_i, rhs_vxi_i
    REAL, dimension(Ny+1)        :: f
    
    INTEGER*8                             :: pr2c, pc2r, po2r
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc
    REAL, dimension(Nx,Ny)                :: datr
    REAL, dimension(Nx/2+1,Ny)            :: kx, ky, k2

    INTEGER :: t
    
    CALL Euler(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc_i,rhs_vbc_i,rhs_eta_i, & 
         rhs_psi_i,rhs_uxi_i, rhs_vxi_i,f,pr2c,pc2r,k2,datc)


    CALL AB2(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc_i,rhs_vbc_i,rhs_eta_i, & 
         rhs_psi_i,rhs_uxi_i, rhs_vxi_i,f,pr2c,pc2r,k2,datc)


    DO t = 3, N_init
       
       
       CALL AB3_i(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc_i,rhs_vbc_i,rhs_eta_i, & 
         rhs_psi_i,rhs_uxi_i, rhs_vxi_i,f,pr2c,pc2r,k2,datc)

       ! Reset rhs_i
        
        rhs_ubc_i(:,:,1) = rhs_ubc_i(:,:,2) 
        rhs_ubc_i(:,:,2) = rhs_ubc_i(:,:,3)
        
        rhs_vbc_i(:,:,1) = rhs_vbc_i(:,:,2)
        rhs_vbc_i(:,:,2) = rhs_vbc_i(:,:,3)

        rhs_eta_i(:,:,1) = rhs_eta_i(:,:,2)
        rhs_eta_i(:,:,2) = rhs_eta_i(:,:,3)

        rhs_psi_i(:,:,1) = rhs_psi_i(:,:,2)
        rhs_psi_i(:,:,2) = rhs_psi_i(:,:,3)

        rhs_uxi_i(1)     = rhs_uxi_i(2)
        rhs_uxi_i(2)     = rhs_uxi_i(3) 

        rhs_vxi_i(1)     = rhs_vxi_i(2)
        rhs_vxi_i(2)     = rhs_vxi_i(3)

     END DO

    ! Sauvegarde des rhs pour dt = 1

    rhs_ubc(:,:,1) = rhs_ubc_i(:,:,3)
    rhs_vbc(:,:,1) = rhs_vbc_i(:,:,3)
    rhs_psi(:,:,1) = rhs_psi_i(:,:,3)
    rhs_eta(:,:,1) = rhs_eta_i(:,:,3)
    
    rhs_uxi(1)     = rhs_uxi_i(3)
    rhs_vxi(1)     = rhs_vxi_i(3)


    DO t = 1,N_init

       CALL AB3_i(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc_i,rhs_vbc_i,rhs_eta_i, & 
         rhs_psi_i,rhs_uxi_i, rhs_vxi_i,f,pr2c,pc2r,k2,datc)

       ! Reset rhs_i
        
        rhs_ubc_i(:,:,1) = rhs_ubc_i(:,:,2) 
        rhs_ubc_i(:,:,2) = rhs_ubc_i(:,:,3)
        
        rhs_vbc_i(:,:,1) = rhs_vbc_i(:,:,2)
        rhs_vbc_i(:,:,2) = rhs_vbc_i(:,:,3)

        rhs_eta_i(:,:,1) = rhs_eta_i(:,:,2)
        rhs_eta_i(:,:,2) = rhs_eta_i(:,:,3)

        rhs_psi_i(:,:,1) = rhs_psi_i(:,:,2)
        rhs_psi_i(:,:,2) = rhs_psi_i(:,:,3)

        rhs_uxi_i(1)     = rhs_uxi_i(2)
        rhs_uxi_i(2)     = rhs_uxi_i(3) 

        rhs_vxi_i(1)     = rhs_vxi_i(2)
        rhs_vxi_i(2)     = rhs_vxi_i(3)

     END DO
       
     ! Sauvegarde des rhs pour dt = 2

    rhs_ubc(:,:,2) = rhs_ubc_i(:,:,3)
    rhs_vbc(:,:,2) = rhs_vbc_i(:,:,3)
    rhs_psi(:,:,2) = rhs_psi_i(:,:,3)
    rhs_eta(:,:,2) = rhs_eta_i(:,:,3)

    rhs_uxi(2)     = rhs_uxi_i(3)
    rhs_vxi(2)     = rhs_vxi_i(3)

  END SUBROUTINE initialisation
    

