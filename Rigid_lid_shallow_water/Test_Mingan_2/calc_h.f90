


  SUBROUTINE calc_h(eta,eta_b,hleft,hdown)


    ! Sous-routine calculant la hauteur de chaque couche centré sur
    ! les vitesses u (left) et v (down)

    ! IN  : eta, eta_b
    ! OUT : hleft, hdown

    USE Parametres
    IMPLICIT NONE

    REAL, dimension(Nx+1,Ny+1)    :: eta, eta_b
    REAL, dimension(Nx+1,Ny+1,Nz) :: hleft, hdown

    INTEGER :: H(2), bot(2)
    INTEGER :: i,j,k

    H(1)   = H1
    H(2)   = H2
    bot(1) = bot1
    bot(2) = bot2


    DO k = 1,Nz


       DO i = 2,Nx+1
          DO j = 2,Ny+1

             hleft(i,j,k) = H(k) + bot(k)*(eta(i,j) + eta(i-1,j)) / 2 - (k-1)*(eta_b(i,j) + eta_b(i-1,j))/2
             hdown(i,j,k) = H(k) + bot(k)*(eta(i,j) + eta(i,j-1)) / 2 - (k-1)*(eta_b(i,j) + eta_b(i,j-1))/2

          END DO
       END DO

       ! Périodique
       
       hleft(1,:,k) = hleft(Nx+1,:,k)
       hleft(:,1,k) = hleft(:,Ny+1,k)

       hdown(1,:,k) = hdown(Nx+1,:,k)
       hdown(:,1,k) = hdown(:,Ny+1,k)

    END DO


  END SUBROUTINE calc_h
