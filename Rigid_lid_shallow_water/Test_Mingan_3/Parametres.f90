


Module Parametres

    IMPLICIT NONE
    

    ! Parametres spatiales

    INTEGER ::  Nx, Ny, Nz
    REAL    ::  Lx, dx, Ly, dy
    
    parameter (Nx  = 512)
    parameter (Ny  = 512)
    parameter (Nz  = 2)
    parameter (Lx  = 500000)
    parameter (Ly  = 500000)
    parameter (dy  = Ly/Ny)
    parameter (dx  = Lx/Nx)
    

    ! Parametres temporels

    INTEGER ::  Nt, dt, N_init

    parameter (dt     = 400)
    parameter (Nt     = 25*3600*24/dt)
    parameter (N_init = 3)

    ! Parametres physiques

    REAL    :: H1, H2, Ht, He, R_int, R_ext, c, res, g, g2, pi, tpi, Ld
    REAL    :: hmax, phi, omega, f0, rho1, rho2, mu, R, Beta, a_tau
    INTEGER :: bot1, bot2

    parameter (H1    = 1000)
    parameter (H2    = 1000)
    parameter (Ht    = H1 + H2)
    parameter (He    = (H1 * H2) / Ht)
    parameter (bot1  = -1)
    parameter (bot2  = +1)
    parameter (Ld    = 5e3)
    parameter (f0    = 0 + 1E-4 )! +2*omega*sin(phi))
    parameter (g     = (f0*Ld)**2 / He)
    parameter (R     = 6371*1E3)
    parameter (pi    = 2.*asin(1.))
    parameter (tpi   = 4.*asin(1.))
    parameter (hmax  = 10.0)
    parameter (phi   = pi/4)
    parameter (omega = 7.2*1E-5)
    parameter (Beta  = 0 ) !+ 2*omega*cos(phi) / R)
    parameter (rho1  = 950)
    parameter (rho2  = 960)
    parameter (g2    = 0.1 ) !+ 9.8 * ((rho2 - rho1)/ rho1))
    parameter (mu    = 2.E6 )!+ 1E-3*f0*dx**4)
    parameter (R_int = 0 )!  + sqrt(g2*H2) / f0)
    parameter (R_ext = 0 )!  + sqrt(g*(H1 + H2)) / f0)
    parameter (c     = 0.25*Beta*R_int**2)
    parameter (res   = 1E-7)
    parameter (a_tau = 0.1/(rho1*H1))
    
    

  End Module Parametres

