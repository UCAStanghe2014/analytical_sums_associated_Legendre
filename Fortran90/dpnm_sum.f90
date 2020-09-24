  Subroutine dpnm_sum(ii, t, c, p)
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
    pii = s*((-1D0)+(-15D0)*c*t+(-18D0)*((-2D0)+c**2)*t**2+(-1D0)*c*((-39D0)+c**2)*t**3+&
      &3D0*((-20D0)+c**2)*t**4+9D0*c*t**5+8D0*t**6)*w**(-9)
  End If

  If (ii==2) Then
    pii = s*w**(-7)*((-5D0)*t*(c+((-2D0)+c**2)*t+(-1D0)*c*t**2+t**3)+((-1D0)+(-2D0)*c*t+t**2)*w**2)
  End If

  If (ii==3) Then
    pii = (-1D0)*s*(1D0+c*t+(-2D0)*t**2)*w**(-5)
  End If
  If (ii==4) Then
    pii = (-1D0)*s*w**(-3)
  End If
  If (ii==5) Then
    pii = (-1D0)*s*w**(-1)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-1)
  End If
  If (ii==6) Then
    pii = (-1D0)*s*((1D0+2D0*c*t*w+(-1D0)*w**2)*(w+(-1D0)*c*t*w+w**2)**(-1)+log(2D0)+(-1D0)*log(1D0+(-1D0)*c*t+w))
  End If
  If (ii==7) Then
    pii = s**(-1)*t**(-2)*(1D0+(-1D0)*c*t+(-1D0)*w)*w**(-1)
  End If
  If (ii==8) Then
    pii = 5D0*t**2*(10D0*c**3+(-6D0)*c**4*t+t*(8D0+7D0*s**4+(-6D0)*t**2)+c**2*t*((-1D0)+3D0*t**2)+&
      &c*((-11D0)+2D0*t**2+t**4))*w**(-9) + t*((-12D0)+17D0*c**2+(-6D0)*c*s**2*t+3D0*s**2*t**2)*w**(-7) +&
      &c*(1D0+2D0*c*t+(-1)*t**2)*w**(-5)
  End If
  If (ii==9) Then
    pii = w**(-7)*(t*(w**2+s**2*((-5D0)+10D0*t**2+(-2D0)*w**2))+c*(w**2+(-1D0)*t**2*(5D0*s**2+2D0*w**2)))
  End If
  If (ii==10) Then
    pii = w**(-5)*((-3D0)*s**2*t+c*w**2)
  End If
  If (ii==11) Then
    pii = (c+(-1D0)*t)*w**(-3) + (-1D0)*t*w**(-1)*(1D0+(-1D0)*c*t+w)**(-1)
  End If
  If (ii==12) Then
    pii = w**(-3)*(1D0+(-1D0)*c*t+w)**(-2)*((-1D0)*c**3*t**2+c*(t**2+w**2+w**3)+t*((-1D0)*w**2+(-1D0)*s**2*(1D0+2D0*w)))
  End If
  If (ii==13) Then
    pii = (-1D0)*c*((-1D0)+c*t+(-1D0)*w)**(-1)*w**(-1)*(1D0+w) + ((-1D0)+c)*s**(-2)*t**(-1)*((-1D0)+w)*(c+w)**(-1) +&
      &(-1D0)*c*w**(-1)*(c+w)**(-1)*(c+(-1D0)*t+w)**(-1) + &
      &(-1D0)*s**(-2)*t**(-1)*(c*log((1D0+c)*(c+(-1D0)*t+w)**(-1))+log((1D0/2D0)*(1D0+(-1D0)*c*t+w)))
  End If
  If (ii==14) Then
    pii = (-15D0)*s*((-2D0)+5D0*s**2)*t**2*w**(-7) + 12D0*c*s*t*w**(-5) + &
      & (-15D0)*s*t**3*w**(-9)*(7D0*c*s**2+(-7D0)*s**2*t+2D0*c*w**2)
  End If
  If (ii==15) Then
    pii = 3D0*s*t*w**(-7)*((-5D0)*s**2*t+2D0*c*w**2)
  End If
  If (ii==16) Then
    pii = s*(4D0+(-5D0)*c*t+t**2)*w**(-5) + &
      &(-2D0)*s*w**(-3)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-2)*(t**2*w+(1D0+w)*(1D0+(-1D0)*c*t+w))
  End If
  If (ii==17) Then
    pii = (-2D0)*s + (-2D0)*s*w**(-3) + (-2D0)*c**(-2)*s**3*w**(-3)*(1D0+w)**6*(1D0+(-1D0)*c*t+w)**(-3) +&
      & c**(-2)*s**3*w**(-5)*(3D0+3D0*c*t+(-1D0)*w**2*(4D0+w**3)) + &
      & c**(-2)*s*((-1D0)+c*t+(-1D0)*w)**(-1)*w**(-5)*(1D0+w)**2*(2D0*c**2*w**2*((-1D0)+2D0*w)+s**2*(3D0+(-6D0)*w+6D0*w**3)) +&
      & c**(-2)*s*w**(-4)*(1D0+w)**4*(1D0+(-1D0)*c*t+w)**(-2)*(2D0*c**2*w**2+3D0*s**2*((-1D0)+w*(2D0+w)))
  End If
  If (ii==18) Then
    pii = (-3D0)*s*((-1D0)+c*t)*w**(-5) + (-2D0)*s*w**(-3)*(1D0+(-1D0)*c*t+w)**(-1)*(1D0+w*(1D0+w)**2*(1D0+(-1D0)*c*t+w)**(-1))
  End If
  If (ii==19) Then
    pii = s*w**(-3) + 2D0*c*s**(-3)*t**(-1)*(1D0+(-1D0)*c*t)*w**(-1) + &
      &(1D0+c**2)*s**(-1)*w**(-1)*(1D0+w)*(1D0+(-1D0)*c*t+w)**(-1) + &
      & (-2D0)*s**(-3)*t**(-1)*((1D0+c**2)*log((1D0+c)*(c+(-1D0)*t+w)**(-1))+c*(1D0+2D0*log((1D0/2D0)*(1D0+(-1D0)*c*t+w))))
  End If
  
  p  = pii
  
  End Subroutine dpnm_sum
