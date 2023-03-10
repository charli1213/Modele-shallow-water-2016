

SUBROUTINE rhs_speed_eta(u,v,hleft,hdown,eta,f,rhs_u,rhs_v,rhs_eta,Q)

  ! Sous-routine qui sort tous les RHS du modèle de Sadourny.
  ! IN : u,v,h,eta,f
  ! OUT : rhs_u,rhs_v,rhs_eta,Q
  

  USE parametres 
  IMPLICIT NONE
  
  REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, rhs_u, rhs_v, hleft, hdown
  REAL, dimension(Nx+1,Ny+1,Nz) :: Ux, Vy, B, Q, Qu, Qv,lap_u,lap_v, nabla4_u, nabla4_v
  REAL, dimension(Nx+1,Ny+1)    :: eta, rhs_eta
  REAL, dimension(Ny+1)         :: f

  CALL fluxes(u,v,hleft,hdown,Ux,Vy)
  CALL laplacien(u,v,lap_u,lap_v)
  CALL laplacien(lap_u,lap_v,nabla4_u,nabla4_v) ! Création de nabla_4
  CALL bernouilli(u,v,eta,B)
  CALL vort(u,v,f,Q)
  CALL q_speed(u,v,Q,Qu,Qv)
  CALL calc_rhs_speed(u,v,B,Qu,Qv,rhs_u,rhs_v,nabla4_u,nabla4_v)
  CALL calc_rhs_eta(Ux,Vy,rhs_eta)

end SUBROUTINE rhs_speed_eta

  

