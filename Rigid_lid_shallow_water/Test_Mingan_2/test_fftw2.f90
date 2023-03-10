
INCLUDE 'Parametres.f90'


  PROGRAM test

    USE Parametres
    IMPLICIT NONE


    INCLUDE '/share/apps/fftw/3.3.4/include/fftw3.f'

    DOUBLE PRECISION, dimension(Nx,Ny)    :: fonction
    REAL, dimension(Nx/2+1,Ny)            :: modes_fonction2
    REAL, dimension(Nx)                   :: x,y

    INTEGER*8                             :: pr2c, pc2r
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny)  :: datc, modes_fonction
    DOUBLE PRECISION, dimension(Nx,Ny)    :: datr


    INTEGER                :: i,j




    datr = 0; datc=0; 

    CALL dfftw_plan_dft_r2c_2d(pr2c,Nx,Ny,datr,datc,fftw_estimate)
    CALL dfftw_plan_dft_c2r_2d(pc2r,Nx,Ny,datc,datr,fftw_estimate)


     DO i = 1,Nx
       
       x(i)  = -Lx/2 + (i-1)*dx

    END DO

    DO j = 1,Ny

       y(j)  = -Ly/2 + (j-1)*dy

    END DO


    DO i = 1,Nx

       fonction(i,:) = sin(2*pi*x(i)/(Lx/(255))) !* sin(2*pi*x(i)/(Lx/2)) !& 
            !* sin(2*pi*x(i)/(Lx/4)) 

    END DO

    DO j = 1,Ny
       
       fonction(:,j) = fonction(:,j)*sin(2*pi*y(j)/(Ly/(255)))

    END DO

    !DO i = 1,Nx
       
       !fonction(:,i) = sin(2*pi*y(i)/(Ly))

    !END DO

    CALL dfftw_execute_dft_r2c(pr2c,fonction,modes_fonction)

    
    
    DO i = 1,Nx/2+1
       DO j = 1,Ny

          modes_fonction2(i,j) = abs(modes_fonction(i,j))

       END DO
    END DO

    
    
    OPEN(unit=10,file='sinus_tf.txt')
    DO i =1,Nx/2+1

       WRITE(10,*) modes_fonction2(i,1:Ny)

    END DO
    CLOSE(unit=10)

    OPEN(unit=11, file='sinus.txt')
    DO i = 1,Nx
       
       WRITE(11,*) fonction(i,1:Ny)
    END DO
    CLOSE(unit=11)

  END PROGRAM test
