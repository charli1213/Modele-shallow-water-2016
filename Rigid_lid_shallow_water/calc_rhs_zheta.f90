

  SUBROUTINE calc_rhs_zheta(rhs_ubt, rhs_vbt, rhs_zheta)

    ! Sous-routine calculant rhs de la vorticité zheta 

    ! IN  : rhs_ubt, rhs_vbt
    ! OUT : rhs_zheta

    USE Parametres
    IMPLICIT NONE


    REAL, dimension(Nx+1,Ny+1) :: rhs_ubt, rhs_vbt, rhs_zheta

    INTEGER :: i,j

    
    DO i = 2,Nx+1
       DO j = 2,Ny+1

          rhs_zheta(i,j) = (rhs_vbt(i,j) - rhs_vbt(i-1,j))/dx & 
               -(rhs_ubt(i,j) - rhs_ubt(i,j-1))/dy

       END DO
    END DO

    !print *, 'rhs_vbt(100,100)', rhs_vbt(100,100), & 
         !'rhs_vbt(99,100)',rhs_vbt(99,100)
    !print *, 'rhs_ubt(100,100)', rhs_ubt(100,100), &
         !'rhs_ubt(100,99)', rhs_ubt(100,99)

    !print *, 'rhs_zheta(100,100)', rhs_zheta(100,100)

    ! Périodique

    rhs_zheta(1,:) = rhs_zheta(Nx+1,:)
    rhs_zheta(:,1) = rhs_zheta(:,Ny+1)



  END SUBROUTINE calc_rhs_zheta
