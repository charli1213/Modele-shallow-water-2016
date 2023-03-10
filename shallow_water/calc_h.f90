

subroutine calc_h(eta, h, b)

  use Parametres

  implicit none

  real eta(n,Ny+1,Nx+1), h(n,Ny+1,Nx+1), b(Ny+1,Nx+1)

  integer  i,j



  do j = 1,Nx+1
     do i = 1,Ny+1

        h(1,i,j) = H1 + eta(1,i,j) - eta(2,i,j)
        h(2,i,j) = H2 + eta(2,i,j) - b(i,j)
        
     end do
  end do


end subroutine calc_h
