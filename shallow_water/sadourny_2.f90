

!Allo


  
!                          |===================================|
!                          |Programe principale - Shallow-water|
!                          |===================================|



  ! Les paramètres sont stockés dans un module

  INCLUDE 'Parametres.f90'


  PROGRAM sadourny



  USE Parametres  
  IMPLICIT NONE

  

! ============================ Déclaration des variables =======================



  !   | - u et v      : les vitesses horizontales                    |
  !   | - eta         : déplacement des couches                      |
  !   | - b           : la topographie au fond                       |
  !   | - Q           : la vorticité potentielle                     |
  !   | - x et xs     : la double grille en x                        |
  !   | - y et ys     : la double grille en y                        |
  !   | - f           : la Beta-approximation du facteur de Coriolis |
  !   | - flux_u1,2,3 : flux de la variable u                        |
  !   | - flux_v1,2,3 : flux de la variable v                        |
  !   | - flux_h1,2,3 : flux de la variable eta                      |
  !   | - masque      : masque pour placer des murs, îles, etc       |


  REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1)
  REAL :: h(n,Ny+1,Nx+1), eta(n,Ny+1,Nx+1)
  REAL :: b(Ny+1,Nx+1)
  REAL :: Q(n,Ny+1,Nx+1)
  REAL :: x(Nx), xs(Nx), y(Ny), ys(Ny)
  REAL :: f(Ny+1,Nx+1), Delta_u, pente
  REAL :: ener(n+1), ener_cin(n,Ny+1,Nx+1), var_h, mvt(n+1)
  REAL :: TAU(Ny+1)

  REAL :: flux_u1(n,Ny+1,Nx+1), flux_u2(n,Ny+1,Nx+1), flux_u3(n,Ny+1,Nx+1)
  REAL :: flux_v1(n,Ny+1,Nx+1), flux_v2(n,Ny+1,Nx+1), flux_v3(n,Ny+1,Nx+1)
  REAL :: flux_h1(n,Ny+1,Nx+1), flux_h2(n,Ny+1,Nx+1), flux_h3(n,Ny+1,Nx+1)

  REAL :: masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)

 

  
! ============================== Pour le stockage ==============================



  REAL :: htot(Ny,Nx,4), utot(Ny+1,Nx+1,4), vtot(Ny+1,Nx+1,4)
  REAL :: h2tot(Ny,Nx,4), u2tot(Ny+1,Nx+1,4), v2tot(Ny+1,Nx+1,4)
  

  REAL :: vort_tot(Ny+1,Nx+1,4)
  REAL :: vort2_tot(Ny+1,Nx+1,4)

  REAL :: energie(3,Nt-2), variation(Nt-2), mouvement(3,Nt-2)
  REAL :: energie_cin(Ny+1,Nx+1,3), energie2_cin(Ny+1,Nx+1,3)


  INTEGER :: i, j, k



! ============================= Discrétisation de l'axe ========================

  DO j = 1, Nx
     x(j)  = -Lx/2 + (j-1)*dx
     xs(j) = -Lx/2 + (j-0.5)*dx
  END DO

  DO i = 1, Ny
     y(i)  = -Ly/2 + (i-1)*dy
     ys(i) = -Ly/2 + (i-0.5)*dy
  END DO


! ==================== Approximation Beta pour facteur de Coriolis =============

  CALL calc_f(f,ys)
   

! ========================== Création du masque ================================

  masqueX(:,:) = 1
  masqueY(:,:) = 1

 
  masqueY(1,:)   = 0
  masqueY(Ny:Ny+1,:) = 0
  
  masqueX(:,1)    = 1
  masqueX(:,Nx:Nx+1) = 1
  
 
! Ilot
  !masqueX(15:25,15:25) = 0
  !masqueY(15:25,15:25) = 0



! =========================Conditions initiales=================================


  ! Velocité initiale

  DO j = 1,Nx
     DO i = 1,Ny
    
        u(1,i,j)   = 0.2
        v(1,i,j)   = 0
        u(2,i,j)   = -0.2
        v(2,i,j)   = 0
     
     END DO
  END DO
   

  ! Pente de l'interface correspondant au cisaillement vertical

  Delta_u = u(1,1,1) - u(2,1,1)
  CALL calc_pente(Delta_u,pente)



  ! Hauteur initiale
  
  DO j = 1,Nx
     DO i = 1,Ny

        ! b(1,i)      = (2/Lx)*x(i) + 1
        ! h(2,1,i)    = 4 + 0.6*sin(2*i*pi/Nx) + 0.3*sin(6*i*pi/Nx)
        ! h(1,1,i)    = 6 + hmax * exp(-x(i)**2 / (Lx/6.0)**2)
        
        b(i,j) =  0 ! + 200 * exp(-(x(j)**2 + y(i)**2) / (Lx/6.0)**2)
        !b(i,j)     = y(i) + 150
        !eta(2,i,j) = 0
        !eta(2,i,j) = 100 * exp(-(x(j)**2 + y(i)**2) / (Lx/6.0)**2)
        eta(2,i,j) = pente*(i)*dy 
        !eta(2,i,j) =  0  + 100 * exp(-(x(j)**2 + y(i)**2) / ((Lx + Ly)/12.0)**2)
        !eta(1,i,j) = hmax * 0.5 * (sin(8*x(j)/Lx) + sin(8*y(i)/Ly))
        !eta(1,i,j) = hmax * exp(-(x(j)**2 + y(i)**2) / (Lx/6.0)**2)
        eta(1,i,j) = 0 ! + rand(0)

     
     END DO
  END DO

  ! Ajout de bruit sur eta2



  !CALL bruit(eta,x,y)
  CALL VENT(tau)



  ! (Condition périodique sur les bords)


  u(1,:,Nx+1)    = u(1,:,1)
  u(1,Ny+1,:)    = u(1,1,:)

  v(1,:,Nx+1)    = v(1,:,1)
  v(1,Ny+1,:)    = v(1,1,:)
  
  eta(1,:,Nx+1)  = eta(1,:,1)
  eta(1,Ny+1,:)  = eta(1,1,:)

  u(2,:,Nx+1)    = u(2,:,1)
  u(2,Ny+1,:)    = u(2,1,:)

  v(2,:,Nx+1)    = v(2,:,1)
  v(2,Ny+1,:)    = v(2,1,:)

  eta(2,:,Nx+1)  = eta(2,:,1)
  eta(2,Ny+1,:)  = eta(2,1,:)

  b(:,Nx+1)      = b(:,1)
  b(Ny+1,:)      = b(1,:)

  ! Applique les masques pour les bords non périodiques 

  u(1,:,:) = masqueX*u(1,:,:)
  u(2,:,:) = masqueX*u(2,:,:)

  v(1,:,:) = masqueY*v(1,:,:)
  v(2,:,:) = masqueY*v(2,:,:)

  !==================== Stockage des données initiales ========================

  
  !utot(:,:,1)  = u(1,1:Ny,1:Nx)
  !vtot(:,:,1)  = v(1,1:Ny,1:Nx)
  utot(:,:,1)  = u(1,:,:)
  vtot(:,:,1)  = v(1,:,:)
  htot(:,:,1)  = eta(1,1:Ny,1:Nx)
  !htot(:,:,1)  = eta(1,:,:)

  
  !u2tot(:,:,1) = u(2,1:Ny,1:Nx)
  !v2tot(:,:,1) = v(2,1:Ny,1:Nx)
  u2tot(:,:,1) = u(2,:,:)
  v2tot(:,:,1) = v(2,:,:)
  h2tot(:,:,1) = eta(2,1:Ny,1:Nx)
  !h2tot(:,:,1) = eta(2,:,:)

  

  !=======Initialisation pour les 2 premiers pas de temps (Euler et AB2)=======


  CALL initialisation(u,v,eta,b,flux_u1,flux_v1,flux_h1,flux_u2,flux_v2, &
       flux_h2, Q, f, masqueX,masqueY,ener,ener_cin,var_h,mvt,tau)





  !===============================Boucle principale============================


  DO i = 3,Nt

     CALL Pas_suivants(u,v,eta,b,flux_u1,flux_u2,flux_u3,flux_v1,flux_v2, &
          flux_v3,flux_h1,flux_h2,flux_h3,Q,f,masqueX,masqueY,ener,ener_cin,var_h,mvt,tau)



     ! Stockage de l'energie, variation de h et qnt de mouvement
     ! (à partir de la 3e itérations)

     energie(1,i-2) = ener(1)
     energie(2,i-2) = ener(2)
     energie(3,i-2) = ener(3)

     variation(i-2) = var_h

     mouvement(1,i-2) = mvt(1)
     mouvement(2,i-2) = mvt(2)
     mouvement(3,i-2) = mvt(3)
     
     
     ! (Stockage de données pour deux itérations)


     IF (i == Nt/3) THEN

        !utot(:,:,2)  = u(1,1:Ny,1:Nx)
        !vtot(:,:,2)  = v(1,1:Ny,1:Nx)
        utot(:,:,2)  = u(1,:,:)
        vtot(:,:,2)  = v(1,:,:)
        htot(:,:,2)  = eta(1,1:Ny,1:Nx)

        !htot(:,:,2)  = eta(1,:,:)

        !u2tot(:,:,2) = u(2,1:Ny,1:Nx)
        !v2tot(:,:,2) = v(2,1:Ny,1:Nx)
        u2tot(:,:,2)  = u(2,:,:)
        v2tot(:,:,2)  = v(2,:,:)
        h2tot(:,:,2) = eta(2,1:Ny,1:Nx)

        !h2tot(:,:,2)  = eta(2,:,:)


        !vort_tot(:,:,2)  = Q(1,1:Ny,1:Nx)
        !vort2_tot(:,:,2) = Q(2,1:Ny,1:Nx)

        vort_tot(:,:,2)  = Q(1,:,:)
        vort2_tot(:,:,2) = Q(2,:,:)

        energie_cin(:,:,1)  = ener_cin(1,:,:)
        energie2_cin(:,:,1) = ener_cin(2,:,:)

        

     END IF

     IF (i == 2*Nt/3) THEN

        !utot(:,:,3)  = u(1,1:Ny,1:Nx)
        !vtot(:,:,3)  = v(1,1:Ny,1:Nx)
        utot(:,:,3)  = u(1,:,:)
        vtot(:,:,3)  = v(1,:,:)
        htot(:,:,3)  = eta(1,1:Ny,1:Nx)

        !htot(:,:,3)  = eta(1,:,:)


        !u2tot(:,:,3) = u(2,1:Ny,1:Nx)
        !v2tot(:,:,3) = v(2,1:Ny,1:Nx)
        u2tot(:,:,3)  = u(2,:,:)
        v2tot(:,:,3)  = v(2,:,:)
        h2tot(:,:,3) = eta(2,1:Ny,1:Nx)

        !h2tot(:,:,3)  = eta(2,:,:)

        !vort_tot(:,:,3)  = Q(1,1:Ny,1:Nx)
        !vort2_tot(:,:,3) = Q(2,1:Ny,1:Nx)

        vort_tot(:,:,3)  = Q(1,:,:)
        vort2_tot(:,:,3) = Q(2,:,:)

        energie_cin(:,:,2)  = ener_cin(1,:,:)
        energie2_cin(:,:,2) = ener_cin(2,:,:)

     END IF

  

     ! (Réinitialisation des flux)
    

           flux_u1(1,:,:) = flux_u2(1,:,:)
           flux_u2(1,:,:) = flux_u3(1,:,:)

           flux_v1(1,:,:) = flux_v2(1,:,:)
           flux_v2(1,:,:) = flux_v3(1,:,:)

           flux_h1(1,:,:) = flux_h2(1,:,:)
           flux_h2(1,:,:) = flux_h3(1,:,:)

           flux_u1(2,:,:) = flux_u2(2,:,:)
           flux_u2(2,:,:) = flux_u3(2,:,:)

           flux_v1(2,:,:) = flux_v2(2,:,:)
           flux_v2(2,:,:) = flux_v3(2,:,:)

           flux_h1(2,:,:) = flux_h2(2,:,:)
           flux_h2(2,:,:) = flux_h3(2,:,:)

       

        END DO

  ! Stockage des données finales

        !utot(:,:,4)  = u(1,1:Ny,1:Nx)
        !vtot(:,:,4)  = v(1,1:Ny,1:Nx)
        utot(:,:,4)  = u(1,:,:)
        vtot(:,:,4)  = v(1,:,:)
        htot(:,:,4)  = eta(1,1:Ny,1:Nx)

        !htot(:,:,3)  = eta(1,:,:)

        !u2tot(:,:,4) = u(2,1:Ny,1:Nx)
        !v2tot(:,:,4) = v(2,1:Ny,1:Nx)
        u2tot(:,:,4)  = u(2,:,:)
        v2tot(:,:,4)  = v(2,:,:)
        h2tot(:,:,4) = eta(2,1:Ny,1:Nx)

        !h2tot(:,:,4)  = eta(2,:,:)

        !vort_tot(:,:,4)  = Q(1,1:Ny,1:Nx)
        !vort2_tot(:,:,4) = Q(2,1:Ny,1:Nx)

        vort_tot(:,:,4)  = Q(1,:,:)
        vort2_tot(:,:,4) = Q(2,:,:)

        energie_cin(:,:,3)  = ener_cin(1,:,:)
        energie2_cin(:,:,3) = ener_cin(2,:,:)


 

  ! ====================Pour l'affichage graphique=============================




! Stockage h1
  

  Open(unit=10,file='H_t1.txt')
        
  do i=1,Ny
     write(10,*) htot(i,:,1)
  end do
  
  close(unit=10)


  Open(unit=11,file='H_t2.txt')
        
  do i = 1,Ny
     write(11,*) htot(i,:,2)
  end do
  
  close(unit=11)


  Open(unit=12,file='H_t3.txt')
        
  do i = 1,Ny
     write(12,*) htot(i,:,3)
  end do
  
  close(unit=12)


  Open(unit=13,file='H_t4.txt')
        
  do i = 1,Ny 
     write(13,*) htot(i,:,4)
  end do
  
  close(unit=13)


! Stockage h2

  Open(unit=14,file='H2_t1.txt')
        
  do i=1,Ny
     write(14,*) h2tot(i,:,1)
  end do
  
  close(unit=14)


  Open(unit=15,file='H2_t2.txt')
       
  do i = 1,Ny
     write(15,*) h2tot(i,:,2)
  end do
  
  close(unit=15)


  Open(unit=16,file='H2_t3.txt')
        
  do i = 1,Ny 
     write(16,*) h2tot(i,:,3)
  end do
  
  close(unit=16)


  Open(unit=17,file='H2_t4.txt')
        
  do i = 1,Ny  
     write(17,*) h2tot(i,:,4)
  end do
  
  close(unit=17)


! Stockage vort1

  
  Open(unit=18,file='vort_t1.txt')
        
  do i = 1,Ny+1 
     write(18,*) vort_tot(i,:,1)
  end do
  
  close(unit=18)


  
  Open(unit=19,file='vort_t2.txt')
        
  do i = 1,Ny+1  
     write(19,*) vort_tot(i,:,2)
  end do
  
  close(unit=19)

  
  Open(unit=21,file='vort_t3.txt')
        
  do i = 1,Ny+1
     write(21,*) vort_tot(i,:,3)
  end do
  
  close(unit=21)


  Open(unit=22,file='vort_t4.txt')
        
  do i = 1,Ny+1 
     write(22,*) vort_tot(i,:,4)
  end do
  
  close(unit=22)

! Stockage vort2


  Open(unit=37,file='vort2_t2.txt')
        
  do i = 1,Ny+1 
     write(37,*) vort2_tot(i,:,2)
  end do
  
  close(unit=37)

  Open(unit=38,file='vort2_t3.txt')
        
  do i = 1,Ny+1  
     write(38,*) vort2_tot(i,:,3)
  end do
  
  close(unit=38)

  Open(unit=39,file='vort2_t4.txt')
        
  do i = 1,Ny+1
     write(39,*) vort2_tot(i,:,4)
  end do
  
  close(unit=39)

  


! Stockage u1

  Open(unit=23,file='u1_t1.txt')
        
  do i = 1,Ny+1  
     write(23,*) utot(i,:,1)
  end do
  
  close(unit=23)


  
  Open(unit=24,file='u1_t2.txt')
        
  do i = 1,Ny+1  
     write(24,*) utot(i,:,2)
  end do
  
  close(unit=24)

  
  Open(unit=25,file='u1_t3.txt')
        
  do i = 1,Ny+1  
     write(25,*) utot(i,:,3)
  end do
  
  close(unit=25)

  Open(unit=26,file='u1_t4.txt')
        
  do i = 1,Ny+1  
     write(26,*) utot(i,:,4)
  end do
  
  close(unit=26)


! Stockage de v1


  Open(unit=29,file='v1_t1.txt')
        
  do i = 1,Ny+1  
     write(29,*) vtot(i,:,1)
  end do
  
  close(unit=29)


  
  Open(unit=30,file='v1_t2.txt')
        
  do i = 1,Ny+1  
     write(30,*) vtot(i,:,2)
  end do
  
  close(unit=30)

  
  Open(unit=31,file='v1_t3.txt')
        
  do i = 1,Ny+1  
     write(31,*) vtot(i,:,3)
  end do
  
  close(unit=31)

  Open(unit=32,file='v1_t4.txt')
        
  do i = 1,Ny+1  
     write(32,*) vtot(i,:,4)
  end do
  
  close(unit=32)
  

 ! Topographie 

  Open(unit=27, file='Bottom.txt')

  do i = 1,Ny  
     write(27,*) b(i,1:Nx)
  end do
  
  close(unit=27)


! Énergie

  Open(unit = 28, file = 'energie.txt')

  write(28,*) energie(3,:)

  close(unit = 28)


! Variation hauteur

  Open(unit = 33, file = 'var_h.txt')

  write(33,*) variation

  close(unit = 33)

! Quantité de mouvements

  Open(unit = 34, file = 'qnt_mvt.txt')

  write(34, *) mouvement(1,:)
  write(34, *) mouvement(2,:)
  write(34, *) mouvement(3,:)

  Close(unit = 34)

! Stockage u2

  Open(unit=35,file='u2_t4.txt')
        
  do i = 1,Ny+1  
     write(35,*) u2tot(i,:,4)
  end do
  
  close(unit=35)

! Stockage v2

  Open(unit=36,file='v2_t4.txt')
        
  do i = 1,Ny+1  
     write(36,*) v2tot(i,:,4)
  end do
  
  close(unit=36)

! Stockage énergie cinétique

  Open(unit=40,file='ener1_cin_t1.txt')
        
  do i = 1,Ny+1  
     write(40,*) energie_cin(i,:,1)
  end do
  
  close(unit=40)

  Open(unit=41,file='ener1_cin_t2.txt')
        
  do i = 1,Ny+1  
     write(41,*) energie_cin(i,:,2)
  end do
  
  close(unit=41)

  Open(unit=42,file='ener1_cin_t3.txt')
        
  do i = 1,Ny+1  
     write(42,*) energie_cin(i,:,3)
  end do
  
  close(unit=42)


  Open(unit=43,file='ener2_cin_t1.txt')
        
  do i = 1,Ny+1  
     write(43,*) energie2_cin(i,:,1)
  end do
  
  close(unit=43)

  Open(unit=44,file='ener2_cin_t2.txt')
        
  do i = 1,Ny+1  
     write(44,*) energie2_cin(i,:,2)
  end do
  
  close(unit=44)

  Open(unit=45,file='ener2_cin_t3.txt')
        
  do i = 1,Ny+1  
     write(45,*) energie2_cin(i,:,3)
  end do
  
  close(unit=45)





  




  
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
     write(20,*) R_ext
     write(20,*) R_int
     write(20,*) c
     write(20,*) pente
  Close(unit=20)

 

  


  

 
     

END PROGRAM sadourny


! =============================Sous-routines===================================

include 'initialisation.f90'
include 'Pas_suivants_2.f90'

include 'calc_f.f90'
include 'calc_h.f90'
include 'calc_UnV.f90'
include 'calc_Q_2.f90'
include 'calc_QU_n_QV.f90'
include 'calc_ener.f90'
include 'var_h_tot.f90'
include 'laplacien_2.f90'
include 'calc_A_2.f90'
include 'calc_pente.f90'
include 'bruit.f90'
include 'qnt_mvt.f90'

include 'flux.f90'
include 'calc_flux_2.f90'

include 'Euler.f90'
include 'AB2.f90'
include 'AB3.f90'
include 'Vent.f90'

