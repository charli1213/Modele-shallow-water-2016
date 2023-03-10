

subroutine calc_f(f, ys)

  use Parametres

  implicit none

  real f(Ny+1,Nx+1), ys(Ny)
  integer i

  do i = 1,Ny

     f(i,:) = f0 + Beta*ys(i)

  end do

  f(Ny+1,:) = f(1,:)


end subroutine calc_f
