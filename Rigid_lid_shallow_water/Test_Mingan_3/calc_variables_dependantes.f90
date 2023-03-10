

  SUBROUTINE calc_variables_dependantes(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,u,v,ubt,vbt, & 
       hleft,hdown)

    ! Sous-routine calculant toutes les variables dépendantes des variables 
    ! principales

    ! IN  : ubc, vbc, eta, psi, uxi, vxi, eta_b
    ! OUT : u,v,ubt,vbt,hleft,hdown

    Use Parametres
    IMPLICIT NONE


    REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, eta, psi, eta_b 
    REAL, dimension(Nx+1,Ny+1)    :: ubt, vbt
    REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown
    REAL                          :: uxi, vxi, ener(2)

    
    CALL calc_speed_bt(psi,uxi,vxi,ubt,vbt)
    CALL calc_h(eta(1:Nx+1,1:Ny+1) ,eta_b(1:Nx+1,1:Ny+1),hleft(1:Nx+1,1:Ny+1,:),hdown(1:Nx+1,1:Ny+1,:))
    CALL calc_speed(ubt,vbt,ubc,vbc,eta,hleft,hdown,u,v)
    CALL calc_ener(u,v,eta,ener)


  END SUBROUTINE calc_variables_dependantes
