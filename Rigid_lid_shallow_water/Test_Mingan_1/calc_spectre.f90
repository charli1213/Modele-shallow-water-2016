

  SUBROUTINE calc_spectre(pr2c,Qbt,modes_Qbt)

    
    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx,Ny)               :: Qbt
    REAL, dimension(Nx/2+1,Ny)           :: modes_Qbt
    DOUBLE PRECISION, dimension(Nx,Ny)   :: intermediaire_double
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny) :: modes_complexes
    INTEGER*8                            :: pr2c

    INTEGER                              :: i,j


    intermediaire_double(:,:) = Qbt(:,:)

    CALL dfftw_execute_dft_r2c(pr2c,intermediaire_double,modes_complexes)

    DO i = 1,Nx/2+1
       DO j = 1,Ny

          modes_Qbt(i,j) = abs(modes_complexes(i,j))

       END DO
    END DO

  END SUBROUTINE calc_spectre

    
