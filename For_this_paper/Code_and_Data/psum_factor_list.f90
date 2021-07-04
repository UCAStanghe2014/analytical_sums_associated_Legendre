subroutine psum_factor_list(id,n,ft,nbegin,mset)
  implicit none
  integer,intent(in) :: id,n
  real(kind=8),intent(out) :: ft
  integer,intent(out) :: nbegin,mset
  if (id ==  1) ft =   1d0
  if (id ==  2) ft = (n*1d0)
  if (id ==  3) ft = (n*1d0)*(n*1d0)
  if (id ==  4) ft = (n*1d0)*(n*1d0)*(n*1d0)
  if (id ==  5) ft = 1d0/(n*1d0)
  if (id ==  6) ft = 1d0/(n-1d0)
  if (id ==  7) ft = 1d0/(n+1d0)
  if (id ==  8) ft = (n*1d0)*(n*1d0)
  if (id ==  9) ft = (n*1d0)
  if (id == 10) ft = 1d0
  if (id == 11) ft = 1d0/(n*1d0)
  if (id == 12) ft = 1d0/(n+1d0)
  if (id == 13) ft = 1d0/(n*1d0)/(n*1d0)
  if (id == 14) ft = (n*1d0)
  if (id == 15) ft = 1d0
  if (id == 16) ft = 1d0/(n*1d0)
  if (id == 17) ft = 1d0/(n-1d0)
  if (id == 18) ft = 1d0/(n+1d0)
  if (id == 19) ft = 1d0/(n*1d0)/(n*1d0)
  nbegin = 1
  if (id==6 .or. (id>=14 .and. id<=19)) nbegin = 2
  if (id<=7) mset = 0
  if (id>=8 .and. id<=13)  mset = 1
  if (id>=14 .and. id<=19) mset = 2
end subroutine