
  SUBROUTINE bruit3(pc2r,datc,bruit)

    ! Une des multiples fonctons dont l'utilité est de créer du bruit sur un champs.
    
    USE Parametres
    IMPLICIT NONE

    INTEGER*8                            :: pc2r
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny):: datc
    DOUBLE PRECISION, dimension(Nx,Ny)   :: intermediaire_double
    INTEGER :: i,j
    
    REAL :: bruit(Nx,Ny)
    

    datc = 0
    bruit(:,:) = 0

    DO i = 1,10
       DO j =1,10
          
          datc(i,j) = rand(0)

       END DO
    END DO


    CALL dfftw_execute_dft_c2r(pc2r,datc,intermediaire_double)

    bruit(:,:) = intermediaire_double(:,:)
    
    

  END SUBROUTINE bruit3

    
