

SUBROUTINE Pas_suivants(u,v,eta,b,flux_u1,flux_u2,flux_u3,flux_v1,flux_v2, &

     flux_v3,flux_h1,flux_h2,flux_h3,Q,f,masqueX,masqueY,ener,ener_cin,var_h,mvt,tau)

  USE Parametres

  IMPLICIT NONE

  REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1) 
  REAL :: b(Ny+1,Nx+1), Q(n,Ny+1,Nx+1)
  REAL :: TAU(Ny+1)

  REAL :: flux_u1(n,Ny+1,Nx+1), flux_u2(n,Ny+1,Nx+1), flux_u3(n,Ny+1,Nx+1)
  REAL :: flux_v1(n,Ny+1,Nx+1), flux_v2(n,Ny+1,Nx+1), flux_v3(n,Ny+1,Nx+1)
  REAL :: flux_h1(n,Ny+1,Nx+1), flux_h2(n,Ny+1,Nx+1), flux_h3(n,Ny+1,Nx+1)

  REAL :: f(Ny+1,Nx+1), ener(n+1), ener_cin(n,Ny+1,Nx+1), var_h, mvt(n+1)

  REAL :: masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)


  call flux(u,v,eta,b,Q,f,flux_u3,flux_v3,flux_h3,masqueX,masqueY,ener, & 
       ener_cin,var_h, mvt,tau)

  call AB3(u,v,eta,flux_u1,flux_u2,flux_u3,flux_v1,flux_v2,flux_v3,flux_h1,&
     flux_h2, flux_h3, masqueX, masqueY)

END SUBROUTINE Pas_suivants
