

  SUBROUTINE calc_speed(ubt,vbt,ubc,vbc,eta,hleft,hdown,u,v)

    ! Sous-routine calculant les vitesses dans chaque couche à partir 
    ! des vitesses barotropes et baroclines et de eta

    ! IN  : ubt, vbt, ubc, vbc, eta
    ! OUT : u, v

    USE Parametres
    IMPLICIT NONE


    REAL, dimension(Nx+1,Ny+1)    :: ubt, vbt, ubc, vbc, eta
    REAL, dimension(Nx+1,Ny+1,Nz) :: u, v, hleft, hdown

    INTEGER :: bot(2)
    INTEGER :: j,k

    bot(1) = bot1
    bot(2) = bot2


    DO k = 1,Nz

       u(:,:,k) = ubt(:,:) + bot(k) * (Ht - hleft(:,:,k)) * ubc(:,:) / Ht
       v(:,:,k) = vbt(:,:) + bot(k) * (Ht - hdown(:,:,k)) * vbc(:,:) / Ht

    END DO




  END SUBROUTINE calc_speed
