

  SUBROUTINE calc_rhs_speed_bt(ubc,vbc,rhs_u,rhs_v,rhs_eta,hleft,hdown, & 
       rhs_ubt,rhs_vbt)


    ! Sous-routine calculant les rhs des vitesses barotropes. Il faut faire
    ! un calcul de moyennes pour centrer rhs_eta

    ! IN  : ubc, vbc, rhs_u, rhs_v, hleft, hdown
    ! OUT : rhs_ubt, rhs_vbt


    USE Parametres
    IMPLICIT NONE


    REAL, dimension(Nx+1,Ny+1)    :: ubc, vbc, rhs_ubt, rhs_vbt, rhs_eta
    REAL, dimension(Nx+1,Ny+1)    :: rhs_etaleft, rhs_etadown
    REAL, dimension(Nx+1,Ny+1,Nz) :: rhs_u, rhs_v, hleft, hdown

    INTEGER :: i,j


    ! Centrer rhs_eta en u (left) et v (down)

    
    DO i = 2,Nx+1
       DO j = 2,Ny+1

          rhs_etaleft(i,j) = (rhs_eta(i-1,j) + rhs_eta(i,j))/2
          rhs_etadown(i,j) = (rhs_eta(i,j-1) + rhs_eta(i,j))/2

       END DO
    END DO

    ! Périodique

    rhs_etaleft(1,:) = rhs_etaleft(Nx+1,:)
    rhs_etaleft(:,1) = rhs_etaleft(:,Ny+1)

    rhs_etadown(1,:) = rhs_etadown(Nx+1,:)
    rhs_etadown(:,1) = rhs_etadown(:,Ny+1)


    ! Calcul de rhs_ubc et rhs_vbc


    rhs_ubt(:,:) = hleft(:,:,1)*rhs_u(:,:,1) + hleft(:,:,2)*rhs_u(:,:,2) & 
         + ubc(:,:)*rhs_etaleft(:,:)

    rhs_vbt(:,:) = hdown(:,:,1)*rhs_v(:,:,1) + hdown(:,:,2)*rhs_v(:,:,2) &
         + vbc(:,:)*rhs_etadown(:,:)

    rhs_ubt(:,:) = rhs_ubt(:,:) / Ht
    rhs_vbt(:,:) = rhs_vbt(:,:) / Ht

  END SUBROUTINE calc_rhs_speed_bt





