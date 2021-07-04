Module module_nolp
  implicit none
  contains
  integer function delta(i,j)
    implicit none
    integer :: i,j
    delta = 0
    if (i==j) delta = 1
  end function
  real(kind=8) function nolfactor(n,m)
    implicit none
    integer :: n,m,ii
    real(kind=8) :: ff
    ff = 1d0
    do ii = 1,2*m
      ff = ff/(n-m+ii*1d0)
    end do
    nolfactor = sqrt((2d0*n+1d0)*(2d0-delta(m,0))*ff)
  end function
  integer function len_nm2s(n,m)
    ! length of the list {p00,p10,p11,p20,p21,p22,...pnm}
    implicit none
    integer :: n,m
    integer :: s
    s = (n + 1)*n/2 + m +1
    len_nm2s = s
  end function
  subroutine len_s2nm(s,n,m)
    implicit none
    integer :: s
    integer :: n,m
    integer :: n0,n1
    n0 = floor((sqrt(8.0d0*s-7.0d0)-1.0d0)/2.0d0) ! possible n by eq. s==n(n+1)/2+m+1
    n1 = n0 + 1
    n = n1
    if (len_nm2s(n1,0)>s) n = n0
    m = s-len_nm2s(n-1,n-1)-1
  end subroutine
  subroutine nol_pnm(nmax,theta,P)
  ! P(s) is normalized by Sqrt[(2-KroneckerDelta[m,0])(2n+1)*(n-m)!/(n+m)!]
  implicit none
  integer ::nmax
  real(kind=8) :: theta ! [0,Pi]
  real(kind=8) :: P(:)  ![p00,P10,P11,P20,P21,P22,...]
  real(kind=8) :: t1,t2,Pknown(6)
  real(kind=8)::Wnm,Wn_1m
  real(kind=8)::Cnm,Dnm,Hnm
  integer n,m,s
  t1= cos(theta)
  t2= sin(theta)
  Pknown(1)=  1.0d0
  Pknown(2)=  sqrt(3.0d0)*t1
  Pknown(3) = sqrt(3.0d0)*t2
  Pknown(4) = 3.0d0*sqrt(5.0d0) / 2.0d0*t1*t1 - sqrt(5.0d0) / 2.0d0
  Pknown(5) = sqrt(15.0d0)*t2*t1
  Pknown(6) = sqrt(15.0d0) / 2.0d0*t2*t2
  s=len_nm2s(nmax,nmax)
  if (s<6) then
    P(1:s)=Pknown(1:s)
  else
    P(1:6)=Pknown(1:6)
  end if
  do n=3,nmax,1
    do m=0,n,1
      s=len_nm2s(n,m)
      if(m==0)then
        Wnm = sqrt((2.0d0*n + 1.0d0)*(2.0d0*n - 1.0d0) / ((1.0d0*n + 1.0d0*m)*(1.0d0*n - 1.0d0*m)))
        Wn_1m = sqrt((2.0d0*n - 1.0d0)*(2.0d0*n - 3.0d0) / ((1.0d0*n + 1.0d0*m - 1.0d0)*(1.0d0*n - 1.0d0*m - 1.0d0)))
        P(s) = Wnm*(t1*P(len_nm2s(n-1,m)) - 1.0d0 / Wn_1m*P(len_nm2s(n-2,m)))
      end if
      if(m==1)then
        Wnm = sqrt((2.0d0*n + 1.0d0)*(2.0d0*n - 1.0d0) / ((1.0d0*n + 1.0d0*m)*(1.0d0*n - 1.0d0*m)))
        Wn_1m = sqrt((2.0d0*n - 1.0d0)*(2.0d0*n - 3.0d0) / ((1.0d0*n + 1.0d0*m - 1.0d0)*(1.0d0*n - 1.0d0*m - 1.0d0)))
        P(s) = Wnm*(t1*P(len_nm2s(n-1,m)) - 1.0d0 / Wn_1m*P(len_nm2s(n-2,m)))
      end if
      if(m==2)then
        Cnm = sqrt((2.0d0*n + 1.0d0)*(1.0d0*n + 1.0d0*m - 2.0d0)*(1.0d0*n + 1.0d0*m - 3.0d0) /&
          ((2.0d0*n - 3.0d0)*(1.0d0*n + 1.0d0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        Dnm = sqrt((1.0d0*n - 1.0d0*m + 1.0d0)*(1.0d0*n - 1.0d0*m + 2.0d0) /&
          ((1.0d0*n + 1.0d0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        Hnm = sqrt((2.0d0*n + 1.0d0)*(1.0d0*n - 1.0d0*m)*(1.0d0*n - 1.0d0*m - 1.0d0) /&
          ((2.0d0*n - 3.0d0)*(1.0d0*n + 1.0d0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        if(n>=m+2)then
          P(s)=sqrt(2.0d0)*Cnm*(P(len_nm2s(n-2,m-2)))-sqrt(2.0d0)*Dnm*(P(len_nm2s(n,m-2)))+Hnm*(P(len_nm2s(n-2,m)))
        else
          P(s)=sqrt(2.0d0)*Cnm*(P(len_nm2s(n-2,m-2)))-sqrt(2.0d0)*Dnm*(P(len_nm2s(n,m-2)))
        end if
      end if
      if(m>=3)then
        Cnm = sqrt((2.0d0*n + 1.0d0)*(1.0d0*n + 1.0d0*m - 2.0d0)*(1.0d0*n + 1.0d0*m - 3.0d0) / &
          ((2.0d0*n - 3.0d0)*(1.0d0*n + 1.0d0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        Dnm = sqrt((1.0d0*n - 1.0d0*m + 1.0d0)*(1.0d0*n - 1.0d0*m + 2.0d0) / &
          ((1.0d0*n + 1.0d0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        Hnm = sqrt((2.0d0*n + 1.0d0)*(1.0d0*n - 1.0d0*m)*(1.0d0*n - 1.0d0*m - 1.0d0) / &
          ((2.0d0*n - 3.0d0)*(1.0d0*n + 1.0*m)*(1.0d0*n + 1.0d0*m - 1.0d0)))
        if(n>=m+2)then
          P(s)=Cnm*(P(len_nm2s(n-2,m-2)))-Dnm*(P(len_nm2s(n,m-2)))+&
            Hnm*(P(len_nm2s(n-2,m)))
        else
          P(s)=Cnm*(P(len_nm2s(n-2,m-2)))-Dnm*(P(len_nm2s(n,m-2)))
        end if
      end if
    end do
  end do
  end subroutine
  subroutine nol_dpnm(nmax,theta,P,DP)
  ! dPnm/dtheta
  implicit none
  integer :: nmax
  real(kind=8) :: theta
  real(kind=8) :: P(:)
  real(kind=8) :: DP(:)
  integer n,m,s
  call nol_pnm(nmax,theta,P)
  do n=0,nmax,1
    do m=0,n,1
      s=len_nm2s(n,m)
      if(m==0)then
        DP(s)=-sqrt(n*(n + 1.0d0) / 2.0d0)*P(len_nm2s(n,1))
      end if
      if(m==1)then
        DP(s)=-1.0d0 / 2.0d0*sqrt((n - 1.0d0)*(n + 2.0d0))*P(len_nm2s(n,2)) + &
          sqrt(n*(n + 1.0d0)*2.0d0) / 2.0d0*P(len_nm2s(n,0))
      end if
      if(m>=2 .and. m<n)then
        DP(s)=-1.0d0 / 2.0d0*sqrt((n - m)*(n + m + 1.0d0))*P(len_nm2s(n,m+1))+ &
          1.0d0 / 2.0d0*sqrt((n + m)*(n - m + 1.0d0))*P(len_nm2s(n,m-1))
      end if
      if(m==n .and. n>=2)then
        DP(s)=sqrt(n/2.0d0)*P(len_nm2s(n,n-1))
      end if
    end do
  end do
  end subroutine
  subroutine nol_pnm_over_sin(nmax,theta,P,PSN)
  ! Pnm/sin(theta)
  implicit none
  integer :: nmax
  real(kind=8) :: theta
  real(kind=8) :: P(:)
  real(kind=8) :: PSN(:)
  integer :: n,m,s
  real(kind=8) :: ctheta
  real(kind=8) :: stheta
  real(kind=8) :: anm,bnm,f
  ctheta=cos(theta)
  stheta=sin(theta)
  call nol_pnm(nmax,theta,P)
  do n=0,nmax,1
    do m=0,n,1
      s=len_nm2s(n,m)
      if(m/=0)then
        anm = sqrt((n - m)*(n + m + 1.0d0))
        bnm = sqrt((n + m)*(n - m + 1.0d0))
        f = sqrt((2.0d0)/(2.0d0-delta(0,m-1)))
        PSN(s) = stheta*P(s) + ctheta/(2.0d0*m)*( anm*P(len_nm2s(n,m+1)) + f*bnm*P(len_nm2s(n,m-1)) )
      else
        PSN(s) = P(s)/stheta
      end if
    end do
  end do
  end subroutine
  subroutine nol_ddpnm(nmax,theta,P,DP,DDP)
  !d^2(Pnm)/d(theta)^2
  implicit none
  integer :: nmax
  real(kind=8) :: theta
  real(kind=8) :: P(:)
  real(kind=8) :: DP(:)
  real(kind=8) :: DDP(:)
  integer n,m,s
  call nol_dpnm(nmax,theta,P,DP)
  do n=0,nmax,1
    do m=0,n,1
      s=len_nm2s(n,m)
      if(m==0)then
        DDP(s)=-sqrt(n*(n+1.0)/2.0d0)*DP(len_nm2s(n,1))
      end if
      if(m==1)then
        DDP(s)=-sqrt((n-1)*(n+2.0d0))/2.0d0*DP(len_nm2s(n,2)) + &
                sqrt(2.0d0*n*(n+1.0d0)) / 2.0d0*DP(len_nm2s(n,0))
      end if
      if(m>=2 .and. m<n)then
        DDP(s)=-sqrt((n - m)*(n + m + 1.0d0)) / 2.0d0*DP(len_nm2s(n,m+1)) + &
                sqrt((n + m)*(n - m + 1.0d0)) / 2.0d0*DP(len_nm2s(n,m-1))
      end if
      if(m==n .and. n>=2)then
        DDP(s)=sqrt(n/2.0d0)*DP(len_nm2s(n,n-1))
      end if
    end do
  end do
  end subroutine
end module module_nolp