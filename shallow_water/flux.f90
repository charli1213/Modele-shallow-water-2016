
SUBROUTINE flux(u,v,eta,b,Q,f,flux_u,flux_v,flux_h,masqueX,masqueY,ener,ener_cin, &
     var_h, mvt,tau)

USE PARAMETRES

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1), h(n,Ny+1,Nx+1)
REAL :: b(Ny+1,Nx+1)
REAL :: Ux(n,Ny+1,Nx+1), Vy(n,Ny+1,Nx+1)
REAL :: Q(n,Ny+1,Nx+1), QU(n,Ny+1,Nx+1), QV(n,Ny+1,Nx+1)
REAL :: A(n,Ny+1,Nx+1), lap_u(n,Ny+1,Nx+1), lap_v(n,Ny+1,Nx+1)
REAL :: TAU(Ny+1)

REAL :: flux_u(n,Ny+1,Nx+1), flux_v(n,Ny+1,Nx+1), flux_h(n,Ny+1,Nx+1)

REAL :: f(Ny+1,Nx+1), ener(n+1), ener_cin(n,Ny+1,Nx+1), var_h, mvt(n+1)

REAL :: masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)


call calc_h(eta,h,b)
call calc_UnV(u,v,h,Ux,Vy)
call calc_A(u,v,eta,b,A)
call calc_Q(u,v,h,Q,f,masqueX,masqueY)
call calc_QU_n_QV(u,v,Ux,Vy,Q,QU,QV)
call laplacien(u,lap_u,v,lap_v)
call calc_ener(eta,h,u,v,ener,ener_cin)
call var_h_tot(eta,var_h)
call qnt_mvt(Ux,Vy,mvt)


call calc_flux(u,v,h,Ux,Vy,QU,QV,A,flux_u,flux_v,flux_h,lap_u,lap_v,masqueX,masqueY,tau)

END SUBROUTINE flux
