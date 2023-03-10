
  


SUBROUTINE calc_Q(u,v,h,Q,f,masqueX,masqueY)

USE Parametres

IMPLICIT NONE

REAL :: u(n,Ny+1,Nx+1), v(n,Ny+1,Nx+1), h(n,Ny+1,Nx+1)
REAl :: Q(n,Ny+1,Nx+1), f(Ny+1,Nx+1)
REAL :: masque(Ny+1,Nx+1), masqueX(Ny+1,Nx+1), masqueY(Ny+1,Nx+1)

INTEGER :: i,j

masque = masqueX*masqueY

 DO j = 2,Nx+1
     DO i = 2,Ny+1
        

           Q(1,i,j) = ( ( ( v(1,i,j)-v(1,i,j-1) )  / dx  &
                - ( u(1,i,j)-u(1,i-1,j) )   / dy )*masque(i,j) &
                + f(i,j) )&
                / (0.25*(h(1,i,j) + h(1,i-1,j) + h(1,i,j-1) + h(1,i-1,j-1)))
           
           Q(2,i,j) = ( ( (v(2,i,j)-v(2,i,j-1))  / dx  &
                - (u(2,i,j)-u(2,i-1,j) )   / dy )*masque(i,j) &
                + f(i,j) ) &
                / (0.25*(h(2,i,j) + h(2,i-1,j) + h(2,i,j-1) + h(2,i-1,j-1)))
           
        

        END DO
     END DO

  Q(1,:,1) = Q(1,:,Nx+1)
  Q(1,1,:) = Q(1,Ny+1,:)

  Q(2,:,1) = Q(2,:,Nx+1)
  Q(2,1,:) = Q(2,Ny+1,:)


END SUBROUTINE calc_Q
