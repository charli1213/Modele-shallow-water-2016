

subroutine calc_f(f, ys)

  use Parametres

  implicit none

  real f(Ny+1), ys(Ny)
  integer j

  do j = 1,Ny

     f(j) = f0 + Beta*ys(j)

  end do

  f(Ny+1) = f(1)


end subroutine calc_f
