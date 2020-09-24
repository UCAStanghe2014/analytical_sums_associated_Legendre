  Subroutine pnm_sum(ii, t, c, p)
  Implicit None
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=16), Intent (In) :: t
  Real (Kind=16), Intent (In) :: c
  Real (Kind=16), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=16) :: pii
  Real (Kind=16) :: s
  Real (Kind=16) :: w

  w = sqrt(1D0+t**2-2D0*t*c)
  s = sqrt(1D0-c**2)
  pii = 0D0

  If (ii==1) Then
    pii = (c+((-4D0)+5D0*c**2)*t+c*((-9D0)+c**2)*t**2+2D0*(5D0+(-1D0)*c**2)*t**3+(-1D0)*t**4*(c+t))*w**(-7)
  End If
  If (ii==2) Then
    pii = (c+((-2D0)+c**2)*t+t**2*((-1D0)*c+t))*w**(-5)
  End If
  If (ii==3) Then
    pii = (c+(-1D0)*t)*w**(-3)
  End If
  If (ii==4) Then
    pii = t**(-1D0)*(1D0+(-1D0)*w)*w**(-1)
  End If
  If (ii==5) Then
    pii = t**(-1)*log(2D0*(1D0+(-1D0)*c*t+w)**(-1))
  End If
  If (ii==6) Then
    pii = t**(-1)*(1D0+(-1)*w) + c*((-1D0)+log(2D0)) + (-1D0)*c*log(1D0+(-1D0)*c*t+w)
  End If
  If (ii==7) Then
    pii = (-1D0)*t**(-1) + t**(-2)*log((1D0+(-1D0)*c)**(-1)*((-1D0)*c+t+sqrt(1D0+(-2D0)*c*t+t**2)))
  End If
  If (ii==8) Then
    pii = (-5D0)*s*t*((-1D0)*c+t)*(1D0+c*t+(-2D0)*t**2)*w**(-7) + (-1D0)*s*w**(-5)*((-2D0)+5D0*t**2+w**2)
  End If
  If (ii==9) Then
    pii = s*(1D0+c*t+(-2D0)*t**2)*w**(-5)
  End If
  If (ii==10) Then
    pii = s*w**(-3)
  End If
  If (ii==11) Then
    pii = s*w**(-1D0)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-1)
  End If
  If (ii==12) Then
    pii = s*w**(-1D0)*(1D0+(-1D0)*c*t+w)**(-1)
  End If
  If (ii==13) Then
    pii = s**(-1)*t**(-1)*(log((1D0+c)*(c+(-1D0)*t+w)**(-1))+c*log((1D0/2D0)*(1D0+(-1D0)*c*t+w)))
  End If
  If (ii==14) Then
    pii = 15D0*s**2*(c+(-1D0)*t)*t**2*w**(-7D0) + 6D0*s**2*t*w**(-5)
  End If
  If (ii==15) Then
    pii = 3D0*s**2*t*w**(-5)
  End If
  If (ii==16) Then
    pii = 2D0*c*w**(-1)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-1) + t**(-1)*w**(-3)*((-1D0)+c*t+w**3)
  End If
  If (ii==17) Then
    pii = s**2*t*w**(-3)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-2)*((-1D0)*c*t*(1D0+(-1D0)*w+w**2)+(1D0+w)*(1D0+2D0*w**2))
  End If
  If (ii==18) Then
    pii = w**(-3)*(1D0+(-1D0)*c*t+w)**(-1)*(c*((-1D0)+w)*w+t*(s**2+w))
  End If
  If (ii==19) Then
    pii = t**(-1) + (-1D0)*t**(-1)*w**(-1) + t**(-1)*log(2D0) + (-2D0)*s**(-2)*t**(-1)*log(2D0) + &
      &2D0*c*s**(-2)*t**(-1)*log(1D0+c) + (-2D0)*c*s**(-2)*t**(-1)*log(c+(-1D0)*t+w) + &
      &(-1D0)*t**(-1)*log(1D0+(-1D0)*c*t+w) + 2D0*s**(-2)*t**(-1)*log(1D0+(-1D0)*c*t+w)
  End If

  p = pii

  End Subroutine pnm_sum
