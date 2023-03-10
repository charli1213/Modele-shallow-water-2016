


Module Parametres

    implicit none
    

    ! Parametres spatiales

    integer sc, Nx, Ny
    double precision Lx, dx, Ly, dy
    
    parameter (Nx  = 100)
    parameter (Ny  = 100)
    parameter (Lx  = 500000)
    parameter (Ly  = 500000)
    parameter (dy  = Ly/Ny)
    parameter (dx  = Lx/Nx)
    

    ! Parametres temporels

    integer Nt
    double precision dt, t0, tf

    parameter (dt = 5)
    parameter (Nt = 1000)

    ! Parametres physiques

    integer n
    real H1, H2, R_int, R_ext, c, r
    real g, g2, g3, pi, hmax, phi, omega, f0, rho1, rho2, rho3, mu, Res, Beta, a_tau

    parameter (H1    = 1000)
    parameter (H2    = 1000)
    parameter (g     = 9.81)
    parameter (R     = 6371*1E3)
    parameter (pi    = 3.141592)
    parameter (hmax  = 500.0)
    parameter (phi   = pi/4)
    parameter (omega = 7.2*1E-5)
    parameter (f0    = 2*omega*sin(phi))
    parameter (Beta  = 0 )!+ 2*omega*cos(phi) / R)
    parameter (rho1  = 950)
    parameter (rho2  = 960)
    parameter (rho3  = 125)
    parameter (g2    = g * ((rho2 - rho1)/ rho1))
    parameter (g3    = g * ((rho3 - rho2)/ rho1))
    parameter (mu    = 10 )
    parameter (R_int = 0  + sqrt(g2*H2) / f0)
    parameter (R_ext = 0  + sqrt(g*(H1 + H2)) / f0)
    parameter (c     = 0.25*Beta*R_int**2)
    parameter (res   = 0 )!+ 1E-7)
    parameter (n     = 2)
    parameter (a_tau = 0 )! + 0.01)
    
    

  End Module Parametres
