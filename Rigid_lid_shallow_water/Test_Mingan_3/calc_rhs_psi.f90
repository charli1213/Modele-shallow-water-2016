
  SUBROUTINE calc_rhs_psi(pr2c,pc2r,k2,datr,datc,rhs_psi)

    ! Calcul l'inverse du laplacien de psi

    ! IN  : pr2c, pc2r, k2, datr, datc
    ! OUT : rhs_psi

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx,Ny)                 :: datr, rhs_psi
    REAL, dimension(Nx/2+1,Ny)             :: k2
    DOUBLE COMPLEX, dimension(Nx/2+1,Ny)   :: datc
    DOUBLE PRECISION, dimension(Nx,Ny)     :: intermediaire_double
    INTEGER*8                              :: pr2c, pc2r

    INTEGER                                :: i,j
    
    

    intermediaire_double(:,:) = datr(:,:)
    
    CALL dfftw_execute_dft_r2c(pr2c,intermediaire_double,datc)

    
    
    

    datc = datc/Nx/Ny
    
    

    DO j = 1,Ny
       DO i = 1,Nx/2+1

          IF ( k2(i,j) /= 0 )  THEN

             datc(i,j) = - datc(i,j)/k2(i,j)

          ELSE

             datc(i,j) = 0  
     
          END IF
         
       END DO
    END DO



    CALL dfftw_execute_dft_c2r(pc2r,datc,intermediaire_double)

    datr(:,:) = intermediaire_double(:,:) 
    rhs_psi(:,:) = datr(:,:)

    

  END SUBROUTINE calc_rhs_psi
