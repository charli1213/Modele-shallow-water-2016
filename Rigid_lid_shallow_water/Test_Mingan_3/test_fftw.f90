

   INCLUDE 'Parametres.f90'


  PROGRAM main

    
    USE Parametres
    IMPLICIT NONE

    INCLUDE '/share/apps/fftw/3.3.4/include/fftw3.f'

   

    ! Variables

    DOUBLE PRECISION  :: IN(Nx,Ny)
    COMPLEX*16 :: OUT(Nx/2+1,Ny)
    integer*8 :: plan, plan2
    
    INTEGER i,j,kx,ky
    
    ! On crée une bouce qui nous crée une série de sinus avec des k différents.
    
    DO kx = 1,5
       DO  ky = 1,5
          DO i = 1,Nx
             DO j = 1,Ny
                IN(i,j) = IN(i,j) + SIN(kx*dx*i) + SIN(ky*dy*j)
             END DO
          END DO
       END DO
    END DO

    !OPEN(UNIT=0,FILE='sinus.txt')
    !DO j=1,Ny
       !WRITE(0,*) IN(:,j)
    !END DO
    !CLOSE(UNIT=0)
    
    ! FFTW
    CALL dfftw_plan_dft_r2c_2d(plan,Nx,Ny,IN,OUT,FFTW_ESTIMATE)

    print *, 'pr2c', plan

    CALL dfftw_execute_dft_r2c(plan,IN,OUT)
    CALL dfftw_destroy_plan(plan)


    !OPEN(UNIT=0,FILE='sinus2.txt')
    !DO j=1,Ny
       !WRITE(0,*) OUT(:,j)
    !END DO
    !CLOSE(UNIT=0)

    ! Maintenant, on va revenir en arrière et faire la TF inverse de notre TF pour ravoir la
    ! solution initiale.

    ! IFFTW
    CALL dfftw_plan_dft_c2r_2d(plan2,Nx,Ny,OUT,IN,FFTW_ESTIMATE)

    print *, 'pc2r', plan2

    CALL dfftw_execute_dft_c2r(plan2,OUT,IN)
    CALL dfftw_destroy_plan(plan2)

    ! La solution n'a pas le bon «scale», on divise donc par le facteur Nx*Ny pour le
    ! mettre dans le bon ordre de grandeur.
    IN(:,:) = IN(:,:)/(Nx*Ny)
    
    !OPEN(UNIT=0,FILE='sinus3.txt')
    !DO j=1,Ny
       !WRITE(0,*) IN(:,j)
    !END DO
    !CLOSE(UNIT=0)

    

  END PROGRAM MAIN


    
    

    
