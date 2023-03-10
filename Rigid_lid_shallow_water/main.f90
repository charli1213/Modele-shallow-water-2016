

!                         Main program - Shallow water 



  INCLUDE 'Parametres.f90'


  PROGRAM main

    
    USE Parametres
    IMPLICIT NONE

    INCLUDE '/share/apps/fftw/3.3.4/include/fftw3.f'

   

    ! Variables

    REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, eta, psi, eta_b, Qbt
    REAL, dimension(Nx+1,Ny+1,Nz) :: Q
    REAL, dimension(Nx/2+1,Ny)    :: modes_Qbt
    REAL                          :: uxi, vxi, ener(4)

    ! RHS

    REAL, dimension(Nx+1,Ny+1,3)  :: rhs_ubc, rhs_vbc, rhs_eta, rhs_psi
    REAL, dimension(3)            :: rhs_uxi, rhs_vxi 
 

    ! FFT

    INTEGER*8                             :: pr2c, pc2r, pr2o, po2r
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc
    DOUBLE PRECISION, dimension(Nx,Ny)    :: datr
    REAL, dimension(Nx/2+1,Ny)            :: kx, ky, k2
    REAL, dimension(Nx,Ny,6)              :: orn
    REAL, dimension(Nx,Ny-1)              :: outr, kxo, kyo, ko2

   
    ! Paramètres physiques

    REAL :: f(Ny+1), ubt0, ubc0, psi0
    
    ! Grille

    REAL, dimension(Nx) :: x, xs
    REAL, dimension(Ny) :: y, ys

    ! Bruit

    REAL                                 :: kmin, kmax
    INTEGER                              :: par
    DOUBLE PRECISION, dimension(Nx,Ny)   :: intermediaire_double
    REAL, dimension(Nx,Ny)               :: bruit
    REAL, dimension(0:Nx+1,0:Ny+1)       :: grid

    ! Stockage

    CHARACTER(18) :: string
    CHARACTER(18) :: string_ubc, string_vbc, string_eta, string_psi
    CHARACTER(18) :: string_Q1, string_Q2, string_Qbt, string_modesQbt
    REAL          :: saved_ener(Nt-3,4)


    ! Indices

    INTEGER :: i,j,k,t,count






    ! Calcul de la grille

    DO i = 1,Nx
       
       x(i)  = -Lx/2 + (i-1)*dx
       xs(i) = -Lx/2 + (i-0.5)*dx

    END DO

    DO j = 1,Ny

       y(j)  = -Ly/2 + (j-1)*dy
       ys(j) = -Ly/2 + (j-0.5)*dy

    END DO

 

    ! Calcul de l'approximation Beta


    CALL calc_f(f,ys)

    
    ! Initialisation de la FFTW

     datr = 0; datc=0; outr=0; 
     !pr2c = 0
     !pc2r = 0

     

     CALL dfftw_plan_dft_r2c_2d(pr2c,Nx,Ny,datr,datc,fftw_estimate)
     CALL dfftw_plan_dft_c2r_2d(pc2r,Nx,Ny,datc,datr,fftw_estimate)

     CALL dfftw_plan_r2r_2d(pr2o,Nx,Ny-1,datr(:,2:ny),outr, &
                fftw_r2hc,fftw_rodft00,fftw_estimate)
     CALL dfftw_plan_r2r_2d(po2r,Nx,Ny-1,outr,datr(:,2:ny), &
                fftw_hc2r,fftw_rodft00,fftw_estimate)


     

     !pr2c = 505228784
     !pc2r = 505293296




    ! Initialisation des variables ubc, vbc, eta, psi, uxi, vxi

    ubt0 = 0.01
    ubc0 = 0.05
    psi0 = 5*ubt0*Ly/tpi
    
    
    ! psi

    DO i = 1,Nx
       DO j = 1,Ny
          
          psi(i,j) = 0 !+ psi0*sin(tpi*y(j)/Ly)+0.001*psi0*sin(20*tpi*x(i)/Lx)

       END DO
    END DO


    ! Périodique

    psi(:,Ny+1) = psi(:,1)
    psi(Nx+1,:) = psi(1,:)

    ! Bruit sur psi

    kmin = 10
    kmax = 13
    par  = -1

    CALL bruit4(kmin,kmax,par,grid)

    psi(:,:) = grid(1:Nx+1,1:Ny+1)*ubt0/(kmax*tpi/Ly)

    eta_b(:,:) = 0


    ! ubc, vbc et eta

    DO i = 1,Nx
       DO j = 1,Ny
          
          ubc(i,j) = 0 !+ (psi(i,j+1)-psi(i,j))/dy
          vbc(i,j) = 0 !-(psi(i+1,j)-psi(i,j))/dx
          !eta(i,j) =  -(f0 / g)*psi(i,j)
          !eta(i,j) = hmax * exp(-(x(j)**2 + y(i)**2) / (Lx/12.0)**2)
          !eta(i,j) = hmax*sin(tpi*y(j)/Ly) !+0.001*hmax*sin(20*tpi*x(i)/Lx)
          eta(i,j) = f0/g*(Ly/tpi)*ubc0*cos(tpi*y(j)/Ly)
          eta_b(i,j) = 0


       END DO
    END DO

    !eta_b(50:150,50:150) = 550

    ! Périodique

    eta(:,Ny+1) = eta(:,1) 
    eta(Nx+1,:) = eta(1,:)
    
    eta_b(:,Ny+1) = eta_b(:,1) 
    eta_b(Nx+1,:) = eta_b(1,:)
    
    ! Bruit sur eta

    CALL bruit4(kmin,kmax,par,grid)

    eta(:,:) = eta(:,:) + 5.E-3*grid(1:Nx+1,1:Ny+1)/(kmax*tpi/Ly)

    
    

    ! Variation de ubc et vbc

    DO j = 1,Ny
       ubc(:,j) = -(g/f0)*(eta(:,j+1)-eta(:,j))/dy
    END DO
    

    !DO i = 1,Nx
       !vbc(i,:) = -(g/f0)*(eta(i+1,:) - eta(i,:))/dx
    !END DO


    ! Périodique
    
    ubc(:,Ny+1) = ubc(:,1)
    !ubc(Nx+1,:) = ubc(1,:)

    !vbc(:,Ny+1) = vbc(:,1)
    vbc(Nx+1,:) = vbc(1,:)
       

    ! Bruit

    !CALL bruit_2(eta,0.4)
    !CALL bruit_2(psi,0.1)
    !CALL bruit_2(ubc,0.08)

    
    
       


    ! uxi, vxi 

    uxi = 0.
    vxi = 0.




    ! Stockage données initiales

    count = 1

    INCLUDE 'write_fields.f90'
     

     
  

     ! Calcul des nombres d'onde

     CALL calc_nb_onde(kx, ky, k2, kxo, kyo, ko2)


     
     


     ! Premiers pas de temps

     CALL initialisation(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc,rhs_vbc, & 
          rhs_eta, rhs_psi, rhs_uxi, rhs_vxi, Q, Qbt, f, pr2c, pc2r, k2, datc)





     !CALL Euler(ubc,vbc,eta,psi,uxi,vxi,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi, & 
       !rhs_uxi, rhs_vxi, f, pr2c, pc2r, k2, datc)


     

     !CALL AB2(ubc,vbc,eta,psi,uxi,vxi,rhs_ubc,rhs_vbc,rhs_eta,rhs_psi, & 
       !rhs_uxi, rhs_vxi, f, pr2c, pc2r, k2, datc)

     

     ! Boucle principale

     

     DO t = 3,Nt

        CALL AB3(ubc,vbc,eta,psi,eta_b,uxi,vxi,ener,rhs_ubc,rhs_vbc,rhs_eta, & 
             rhs_psi,rhs_uxi, rhs_vxi,Q,Qbt,modes_Qbt,f,pr2c, pc2r, k2, datc)


        ! Stockage énergie 

        saved_ener(t,1) = ener(1)
        saved_ener(t,2) = ener(2)
        saved_ener(t,3) = ener(3)
        saved_ener(t,4) = ener(4)

        ! Stockage des variables
        
        IF (mod(t,100) == 0) THEN

           INCLUDE 'write_fields.f90'
           print *, t, eta(50,50)

        END IF


        ! Reset rhs
        
        rhs_ubc(:,:,1) = rhs_ubc(:,:,2) 
        rhs_ubc(:,:,2) = rhs_ubc(:,:,3)
        
        rhs_vbc(:,:,1) = rhs_vbc(:,:,2)
        rhs_vbc(:,:,2) = rhs_vbc(:,:,3)

        rhs_eta(:,:,1) = rhs_eta(:,:,2)
        rhs_eta(:,:,2) = rhs_eta(:,:,3)

        rhs_psi(:,:,1) = rhs_psi(:,:,2)
        rhs_psi(:,:,2) = rhs_psi(:,:,3)

        rhs_uxi(1)     = rhs_uxi(2)
        rhs_uxi(2)     = rhs_uxi(3) 

        rhs_vxi(1)     = rhs_vxi(2) 
        rhs_vxi(2)     = rhs_vxi(3)


     END DO

     CALL dfftw_destroy_plan(pr2c)
     CALL dfftw_destroy_plan(pc2r)


     ! Stockage des paramètres

   Open(unit=20,file='Parametres.txt')
     write(20,*) Nx
     write(20,*) Lx
     write(20,*) dx
     write(20,*) Ny
     write(20,*) Ly
     write(20,*) dy
     write(20,*) Nt
     write(20,*) dt
     write(20,*) H1
     write(20,*) H2
     write(20,*) f0
     write(20,*) Beta
     write(20,*) mu
     write(20,*) Ld
     write(20,*) g
     write(20,*) count
  Close(unit=20)

  ! Stockage de l'énergie

  Open(unit=30,file='Energie.txt')
  
  DO t = 1,Nt-3

        write(30,*) saved_ener(t,:)

  END DO

  Close(unit=30)

        
     


   END PROGRAM main





   
        

   ! Sous-routines

   INCLUDE 'calc_f.f90'
   INCLUDE 'calc_nb_onde.f90'
   INCLUDE 'bruit4.f90'
   
   INCLUDE 'calc_h.f90'
   INCLUDE 'calc_speed.f90'
   INCLUDE 'calc_speed_bt.f90'
   INCLUDE 'calc_variables_dependantes.f90'
   INCLUDE 'calc_ener.f90'
   INCLUDE 'calc_spectre.f90'

   INCLUDE 'laplacien.f90'
   INCLUDE 'bernouilli.f90'
   INCLUDE 'fluxes.f90'
   INCLUDE 'q_speed.f90'
   INCLUDE 'vorticite.f90'
   INCLUDE 'calc_rhs_speed.f90'
   INCLUDE 'calc_rhs_eta.f90'
   INCLUDE 'rhs_speed_eta.f90'

   INCLUDE 'calc_rhs_speed_bt.f90'
   INCLUDE 'calc_rhs_averaged_velo.f90'
   INCLUDE 'calc_rhs_speed_bc.f90'
   INCLUDE 'calc_rhs_zheta.f90'
   INCLUDE 'calc_rhs_psi.f90'
   INCLUDE 'calc_all_rhs.f90'

   INCLUDE 'initialisation.f90'
   INCLUDE 'Euler.f90'
   INCLUDE 'AB2.f90'
   INCLUDE 'AB3_i.f90'
   INCLUDE 'AB3.f90'
   INCLUDE 'bruit_2.f90'
   
 

   
    


    
    
