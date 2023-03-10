SUBROUTINE bruit_2(psi, deg)
  !Fonction qui crée du bruit!
  !IN : psi,deg
  !OUT : psi

  use Parametres
  IMPLICIT NONE

  REAL :: psi(Nx+1,Ny+1), bruit(Nx+1,Ny+1)
  REAL :: deg ! pourcentage maximal de bruit par rapport à la valeur «psi».
  REAL :: phi_phase, cte_x, cte_y ! La phase et k_n/n
  INTEGER :: nb_cos ! Nombre de cos à superposer
  REAL :: kx,ky
  INTEGER :: i,j,l


  nb_cos = 10

  cte_x = tpi/Lx
  cte_y = tpi/Ly

  bruit(:,:) = 0

  

  DO l = 1,nb_cos

     ! On prépare kx,ky et la constante de phase phi pour les cos
     ! On génère aléatoirement nos nombres d'ondes et une constante de phase.
     phi_phase = rand()*tpi
     kx = rand()*cte_x*Nx
     ky = rand()*cte_y*Ny

     ! On ne veut que de petit k pour avoir de grandes longueurs d'onde.
     IF (kx > cte_x*Nx/4) THEN
        kx = kx/10
     END IF
     IF (ky > cte_y*Ny/4) THEN
        ky = ky/10
     END IF
     
     ! On coupe nos nombres d'onde à l'aide de «modulo» pour que nos ondes
     ! soient continues au rebords. On doit donc avoir des k_n = 2*pi*n/Lx ou 
     ! Ly pour se faire.
     kx = kx - mod(kx,cte_x)
     ky = ky - mod(ky,cte_y)
     
     DO j = 1,Ny+1
        DO i = 1,Nx+1
           bruit(i,j) = bruit(i,j) + cos(kx*(i*dx)+ky*(j*dy)+phi_phase)/nb_cos
        END DO
     END DO
  END DO
  
  psi = psi + deg*bruit

END SUBROUTINE bruit_2
           
     

  
  



  
