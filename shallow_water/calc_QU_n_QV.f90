

SUBROUTINE calc_QU_n_QV(u,v,Ux,Vy,Q,QU,QV)

USE Parametres

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), Ux(n,Ny+1,Nx+1), Vy(n,Ny+1,Nx+1)
REAL :: Q(n,Ny+1,Nx+1), QU(n,Ny+1,Nx+1), QV(n,Ny+1,Nx+1)

INTEGER :: i,j


DO j = 1,Nx
     DO i = 2,Ny+1
        
        QU(1,i,j) = 0.25*((Ux(1,i-1,j) + Ux(1,i,j))*Q(1,i,j) + &
             (Ux(1,i,j+1) + Ux(1,i-1,j+1))*Q(1,i,j+1))

        QU(2,i,j) = 0.25*((Ux(2,i-1,j) + Ux(2,i,j))*Q(2,i,j) + &
             (Ux(2,i,j+1) + Ux(2,i-1,j+1))*Q(2,i,j+1))

     END DO
  END DO

QU(1,1,:)    = QU(1,Ny+1,:)
QU(1,:,Nx+1) = QU(1,:,1)

QU(2,1,:)    = QU(2,Ny+1,:)
QU(2,:,Nx+1) = QU(2,:,1)


DO j = 2,Nx+1
     DO i = 1,Ny
        
        QV(1,i,j) = 0.25*((Vy(1,i,j) + Vy(1,i,j-1))*Q(1,i,j) + &
             (Vy(1,i+1,j) + Vy(1,i+1,j-1))*Q(1,i+1,j))

        QV(2,i,j) = 0.25*((Vy(2,i,j) + Vy(2,i,j-1))*Q(2,i,j) + &
             (Vy(2,i+1,j) + Vy(2,i+1,j-1))*Q(2,i+1,j))

     END DO
  END DO

  QV(1,Ny+1,:) = QV(1,1,:)
  QV(1,:,1)    = QV(1,:,Nx+1)

  QV(2,Ny+1,:) = QV(2,1,:)
  QV(2,:,1)    = QV(2,:,Nx+1)



END SUBROUTINE calc_QU_n_QV




  
