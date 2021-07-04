Module module_psum
  implicit none
  contains
  Subroutine pnm_sum(ii, t, c, p)
  Implicit None
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=8), Intent (In) :: t
  Real (Kind=8), Intent (In) :: c
  Real (Kind=8), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=8) :: pii
  Real (Kind=8) :: s
  Real (Kind=8) :: w
  w = sqrt(1D0+t**2-2D0*t*c)
  s = sqrt(1D0-c**2)
  pii = 0D0
  If (ii==4) Then
    pii = (c+((-4D0)+5D0*c**2)*t+c*((-9D0)+c**2)*t**2+2D0*(5D0+(-1D0)*c**2)*t**3+(-1D0)*t**4*(c+t))*w**(-7)
  End If
  If (ii==3) Then
    pii = (c+((-2D0)+c**2)*t+t**2*((-1D0)*c+t))*w**(-5)
  End If
  If (ii==2) Then
    pii = (c+(-1D0)*t)*w**(-3)
  End If
  If (ii==1) Then
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
  Subroutine dpnm_sum(ii, t, c, p)
  Implicit None
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=8), Intent (In) :: t
  Real (Kind=8), Intent (In) :: c
  Real (Kind=8), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=8) :: pii
  Real (Kind=8) :: s
  Real (Kind=8) :: w
  w = sqrt(1D0+t**2-2D0*t*c)
  s = sqrt(1D0-c**2)
  pii = 0D0
  If (ii==4) Then
    pii = s*((-1D0)+(-15D0)*c*t+(-18D0)*((-2D0)+c**2)*t**2+(-1D0)*c*((-39D0)+c**2)*t**3+&
      &3D0*((-20D0)+c**2)*t**4+9D0*c*t**5+8D0*t**6)*w**(-9)
  End If
  If (ii==3) Then
    pii = s*w**(-7)*((-5D0)*t*(c+((-2D0)+c**2)*t+(-1D0)*c*t**2+t**3)+((-1D0)+(-2D0)*c*t+t**2)*w**2)
  End If
  If (ii==2) Then
    pii = (-1D0)*s*(1D0+c*t+(-2D0)*t**2)*w**(-5)
  End If
  If (ii==1) Then
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
  Subroutine ddpnm_sum(ii, t, c, p)
  Implicit None
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=8), Intent (In) :: t
  Real (Kind=8), Intent (In) :: c
  Real (Kind=8), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=8) :: pii
  Real (Kind=8) :: s
  Real (Kind=8) :: w
  w = sqrt(1D0+t**2-2D0*t*c)
  s = sqrt(1D0-c**2)
  pii = 0D0
  If (ii==4) Then
    pii = 9D0*s**2*t*(1D0+15D0*c*t+18D0*((-2D0)+c**2)*t**2+c*((-39D0)+c**2)*t**3+(-3D0)*((-20D0)+c**2)*t**4+&
      &(-9D0)*c*t**5+(-8D0)*t**6)*w**(-11) + ((-4D0)*c**4*t**3+9D0*c**3*t**2*((-6D0)+t**2)+&
      &(-3D0)*t*((-5D0)+13D0*t**2+3D0*t**4)+3D0*c**2*t*((-10D0)+27D0*t**2+6D0*t**4)+&
      &c*((-1D0)+72D0*t**2+(-66D0)*t**4+8D0*t**6))*w**(-9)
  End If
  If (ii==3) Then
    pii = 35D0*s**2*t**5*w**(-9) + (-1D0)*c*w**(-5) + (-5D0)*c*t**4*w**(-9)*(7D0*s**2+w**2) +&
      &(-5D0)*t**3*w**(-9)*((-7D0)+7D0*c**4+21D0*s**2+(-1D0)*w**2+3D0*s**2*w**2) + &
      &t*w**(-7)*((-5D0)+15D0*s**2+(-2D0)*w**2+4D0*s**2*w**2) + c*t**2*w**(-9)*(35D0*s**2+5D0*w**2+25D0*s**2*w**2+w**4)
  End If
  If (ii==2) Then
    pii = w**(-7)*((-1D0)*c*w**2+c*t**2*(5D0*s**2+2D0*w**2)+(-1D0)*t*(w**2+s**2*((-5D0)+10D0*t**2+(-2D0)*w**2)))
  End If
  If (ii==1) Then
    pii = w**(-5)*(3D0*s**2*t+(-1D0)*c*w**2)
  End If
  If (ii==5) Then
    pii = (-1D0/4D0)*(8D0*c+4D0*((-7D0)+4D0*s**2)*t+c*(35D0+c**2+(-3D0)*s**2)*t**2+&
      &4D0*((-5D0)+3D0*s**2)*t**3+4D0*c*t**4)*w**(-3)*(1D0+(-1D0)*c*t+w)**(-2) +&
      &(t*(5D0+(-2D0)*s**2+t**2)+(-2D0)*c*(1D0+2D0*t**2))*w**(-2)*(1D0+(-1D0)*c*t+w)**(-2)
  End If
  If (ii==6) Then
    pii = 2D0*s**2*t*w**(-2)*(1D0+(-1D0)*c*t+w)**(-2) + ((-1D0)*c+t+2D0*s**2*t)*w**(-1)*(1D0+(-1D0)*c*t+w)**(-2) +&
      &(1D0+(-1D0)*c*t+w)**(-2)*((-6D0)*c**2*t+c*w+t*(4D0+((-3D0)+5D0*s**2)*w)) + &
      t*w**(-3)*(1D0+(-1D0)*c*t+w)**(-2)*(s**2+c*t*((-1D0)+c**2*(1D0+(-1D0)*w**3*((-3D0)+log(2D0))))) +&
      &c*(1D0+(-1D0)*c*t+w)**(-2)*((-1D0)*t**2*log(2D0)+(-1D0)*(1D0+w)*log(4D0)+c*t*((-2D0)+w*log(4D0)+log(16D0))) +&
      &c*(1D0+(-1D0)*c*t+w)**(-2)*((-1D0)*((-2D0)+s**2)*t**2+2D0*(1D0+w)+(-2D0)*c*t*(2D0+w))*log(1D0+(-1D0)*c*t+w)
  End If
  If (ii==7) Then
    pii = t**(-2)*((-1D0)*t+c*t**2)*w**(-3) + s**(-2)*t**(-2)*w**(-1)*((-1D0)*c+t+c*w)
  End If
  If (ii==8) Then
    pii = (-5D0)*s*t**3*((-98D0)*c**4*t+16D0*c**3*(7D0+2D0*t**2)+6D0*c**2*t*(11D0+3D0*t**2)+&
      &t*(79D0+63D0*s**4+(-45D0)*t**2+2D0*t**4)+c*((-139D0)+(-34D0)*t**2+7D0*t**4))*w**(-11) +&
      s*w**(-5)*((-2D0)+w**2) + c*s*t*w**(-7)*((-50D0)+9D0*w**2) + 5D0*s*t**2*w**(-9)*(28D0+w**4+(-1D0)*c**2*(49D0+6D0*w**2))
  End If
  If (ii==9) Then
    pii = s*w**(-9)*(35D0*s**2*t**2*(1D0+(c+(-2D0)*t)*t)+5D0*t*(2D0*t+c*((-3D0)+(-5D0)*c*t+6D0*t**2))*w**2+&
      &((-1D0)+2D0*t*((-2D0)*c+t))*w**4)
  End If
  If (ii==10) Then
    pii = 15D0*s**3*t**2*w**(-7) + (-1D0)*s*w**(-5)*(9D0*c*t+w**2)
  End If
  If (ii==11) Then
    pii = (-3D0)*s*(c+(-1D0)*t)*t*w**(-5) + (-1D0)*s*w**(-3) + s*t**2*w**(-3)*(1D0+(-1D0)*c*t+w)**(-2)*((-1D0)*c*t+(1D0+w)**2)
  End If
  If (ii==12) Then
    pii = s*((-3D0)*(1D0+(-1D0)*c*t)*w**(-5)+w**(-3)+w**(-2)*(1D0+(-1D0)*c*t+w)**(-1)*(w**(-1)+(1D0+w)**2*(1D0+(-1D0)*c*t+w)**(-1)))
  End If
  If (ii==13) Then
    pii = (-1D0)*s*w**(-3) + (-1D0)*s**(-1)*w**(-1) + &
      &s**(-3)*t**(-1)*((1D0+c**2)*log((1D0+c)*(c+(-1D0)*t+w)**(-1))+2D0*c*log((1D0/2D0)*(1D0+(-1D0)*c*t+w)))
  End If
  If (ii==14) Then
    pii = 15D0*c*(2D0+(-19D0)*s**2)*t**2*w**(-7) + &
      &12D0*(1D0+(-2D0)*s**2)*t*w**(-5) + 105D0*s**2*t**4*w**(-11D0)*(9D0*c*s**2+(-9D0)*s**2*t+5D0*c*w**2) +&
      &15D0*t**3*w**(-9)*((-35D0)*s**2+63D0*s**4+2D0*(1D0+(-2D0)*c**2)*w**2)
  End If
  If (ii==15) Then
    pii = 3D0*t*w**(-9)*(35D0*s**4*t**2+(-25D0)*c*s**2*t*w**2+(2D0+(-4D0)*s**2)*w**4)
  End If
  If (ii==16) Then
    pii = 3D0*s**2*t*((-3D0)+t*(c+2D0*t))*w**(-7) + 3D0*c*w**(-5)*((-1D0)+c*t+w**2) + &
      & 2D0*c*s**(-2)*w**(-3)*(1D0+c*t+2D0*w**2) + 2D0*s**(-2)*t**(-1)*w**(-3)*((-2D0)+2D0*w**3) + &
      & 6D0*c*s**(-4)*t**(-1)*w**(-1)*(t+c*((-2D0)+c*t+2D0*w))
  End If
  If (ii==17) Then
    pii = (9D0*c+4D0*c**3+(-1D0)*c**5)*s**(-4) + 5D0*c*t**2*((-11D0)+c**2+10D0*c*t+(-30D0)*t**2)*w**(-7) + &
      & (-5D0)*s**(-2)*t*(3D0+c**4+20D0*c**2*t**2*(2D0+t**2)+(-4D0)*c*t*(5D0+10D0*t**2+t**4))*w**(-7) + &
      & (c+(-1D0)*t)*(1D0+2D0*t*(c+6D0*t))*w**(-5) + &
      & (-2D0)*s**(-2)*(c+(-1D0)*t)*((-1D0)+t**2*((-25D0)+6D0*c*t+(-4D0)*t**2))*w**(-5) + &
      & 12D0*s**(-4)*(c+(-1D0)*t)*((-1D0)+(-1D0)*t**2*(6D0+t**2)+4D0*c*(t+t**3))*w**(-5)
  ENDIF
  If (ii==18) Then
    pii = (-7D0)*c*s**(-4)*(31D0+5D0*c**4+(-26D0)*s**2)*w**(-7) + &
      &14D0*s**(-4)*(5D0+c**4+(-5D0)*s**2)*t**(-1)*w**(-7) + s**(-4)*(181D0+200*c**4+39D0*c**6+(-183D0)*s**2)*t*w**(-7) +&
      & (-1D0)*c*s**(-4)*(3D0*(99D0+40*c**4+c**6)+(-241D0)*s**2)*t**2*w**(-7) + &
      &s**(-4)*(103D0+155D0*c**4+(-6D0)*c**6+(-86D0)*s**2)*t**3*w**(-7) + &
      &4D0*s**(-4)*((-3D0)+2D0*s**2)*(7D0*c+(-1D0)*t)*t**4*w**(-7) + 2D0*c*(5D0+c**2)*s**(-4)*t**(-2)*w**(-7)*((-1D0)+w**7)
  End If
  If (ii==19) Then
    pii = (-3D0)*s**2*t*w**(-5) + s**(-2)*t**(-1)*w**(-1)*((-2D0)+(-1D0)*c**2+(-3D0)*c*t+2D0*w+c**2*w) + &
      &(-1D0)*t**(-1)*w**(-3)*(t**2+w**2+(-1D0)*w**3) + &
      &(-12D0)*c**2*s**(-4)*t**(-1)*log(2D0) + &
      &(-1D0)*s**(-2)*t**(-1)*log(16D0) + 2D0*c*(5D0+c**2)*s**(-4)*t**(-1)*log((-1D0)*((-1D0)+c)**(-1)*((-1D0)*c+t+w)) + &
      &4D0*(1D0+2D0*c**2)*s**(-4)*t**(-1)*log(1D0+(-1D0)*c*t+w)
  End If
  p = pii
  End Subroutine ddpnm_sum
  Subroutine xp_pnm_sum(ii, t, c, p)
  Implicit None
  INTEGER, PARAMETER :: QUAD = SELECTED_REAL_KIND(32)
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=QUAD), Intent (In) :: t
  Real (Kind=QUAD), Intent (In) :: c
  Real (Kind=QUAD), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=QUAD) :: pii
  Real (Kind=QUAD) :: s
  Real (Kind=QUAD) :: w
  w = sqrt(1.0_QUAD+t**2-2.0_QUAD*t*c)
  s = sqrt(1.0_QUAD-c**2)
  pii = 0.0_QUAD
  If (ii==4) Then
    pii = (c+((-4.0_QUAD)+5.0_QUAD*c**2)*t+c*((-9.0_QUAD)+c**2)*t**2+&
      2.0_QUAD*(5.0_QUAD+(-1.0_QUAD)*c**2)*t**3+(-1.0_QUAD)*t**4*(c+t))*w**(-7)
  End If
  If (ii==3) Then
    pii = (c+((-2.0_QUAD)+c**2)*t+t**2*((-1.0_QUAD)*c+t))*w**(-5)
  End If
  If (ii==2) Then
    pii = (c+(-1.0_QUAD)*t)*w**(-3)
  End If
  If (ii==1) Then
    pii = t**(-1.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*w)*w**(-1)
  End If
  If (ii==5) Then
    pii = t**(-1)*log(2.0_QUAD*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1))
  End If
  If (ii==6) Then
    pii = t**(-1)*(1.0_QUAD+(-1)*w)+c*((-1.0_QUAD)+log(2.0_QUAD))+(-1.0_QUAD)*c*log(1.0_QUAD+(-1.0_QUAD)*c*t+w)
  End If
  If (ii==7) Then
    pii = (-1.0_QUAD)*t**(-1)+t**(-2)*log((1.0_QUAD+(-1.0_QUAD)*c)**(-1)*((-1.0_QUAD)*c+t+sqrt(1.0_QUAD+(-2.0_QUAD)*c*t+t**2)))
  End If
  If (ii==8) Then
    pii = (-5.0_QUAD)*s*t*((-1.0_QUAD)*c+t)*(1.0_QUAD+c*t+(-2.0_QUAD)*t**2)*w**(-7)+&
      (-1.0_QUAD)*s*w**(-5)*((-2.0_QUAD)+5.0_QUAD*t**2+w**2)
  End If
  If (ii==9) Then
    pii = s*(1.0_QUAD+c*t+(-2.0_QUAD)*t**2)*w**(-5)
  End If
  If (ii==10) Then
    pii = s*w**(-3)
  End If
  If (ii==11) Then
    pii = s*w**(-1.0_QUAD)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)
  End If
  If (ii==12) Then
    pii = s*w**(-1.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)
  End If
  If (ii==13) Then
    pii = s**(-1)*t**(-1)*(log((1.0_QUAD+c)*(c+(-1.0_QUAD)*t+w)**(-1))+c*log((1.0_QUAD/2.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)))
  End If
  If (ii==14) Then
    pii = 15.0_QUAD*s**2*(c+(-1.0_QUAD)*t)*t**2*w**(-7.0_QUAD)+6.0_QUAD*s**2*t*w**(-5)
  End If
  If (ii==15) Then
    pii = 3.0_QUAD*s**2*t*w**(-5)
  End If
  If (ii==16) Then
    pii = 2.0_QUAD*c*w**(-1)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)+t**(-1)*w**(-3)*((-1.0_QUAD)+c*t+w**3)
  End If
  If (ii==17) Then
    pii = s**2*t*w**(-3)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-1.0_QUAD)*c*t*(1.0_QUAD+(-1.0_QUAD)*w+w**2)+&
      (1.0_QUAD+w)*(1.0_QUAD+2.0_QUAD*w**2))
  End If
  If (ii==18) Then
    pii = w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)*(c*((-1.0_QUAD)+w)*w+t*(s**2+w))
  End If
  If (ii==19) Then
    pii = t**(-1)+(-1.0_QUAD)*t**(-1)*w**(-1)+t**(-1)*log(2.0_QUAD)+(-2.0_QUAD)*s**(-2)*t**(-1)*log(2.0_QUAD)+&
      &2.0_QUAD*c*s**(-2)*t**(-1)*log(1.0_QUAD+c)+(-2.0_QUAD)*c*s**(-2)*t**(-1)*log(c+(-1.0_QUAD)*t+w)+&
      &(-1.0_QUAD)*t**(-1)*log(1.0_QUAD+(-1.0_QUAD)*c*t+w)+2.0_QUAD*s**(-2)*t**(-1)*log(1.0_QUAD+(-1.0_QUAD)*c*t+w)
  End If
  p = pii
  End Subroutine xp_pnm_sum
  Subroutine xp_dpnm_sum(ii, t, c, p)
  Implicit None
  INTEGER, PARAMETER :: QUAD = SELECTED_REAL_KIND(32)
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=QUAD), Intent (In) :: t
  Real (Kind=QUAD), Intent (In) :: c
  Real (Kind=QUAD), Intent (Out) :: p
  ! Variable declarations
  Real (Kind=QUAD) :: pii
  Real (Kind=QUAD) :: s
  Real (Kind=QUAD) :: w
  w = sqrt(1.0_QUAD+t**2-2.0_QUAD*t*c)
  s = sqrt(1.0_QUAD-c**2)
  pii = 0.0_QUAD
  If (ii==4) Then
    pii = s*((-1.0_QUAD)+(-15.0_QUAD)*c*t+(-18.0_QUAD)*((-2.0_QUAD)+c**2)*t**2+&
      (-1.0_QUAD)*c*((-39.0_QUAD)+c**2)*t**3+&
      3.0_QUAD*((-20.0_QUAD)+c**2)*t**4+9.0_QUAD*c*t**5+8.0_QUAD*t**6)*w**(-9)
  End If
  If (ii==3) Then
    pii = s*w**(-7)*((-5.0_QUAD)*t*(c+((-2.0_QUAD)+c**2)*t+&
      (-1.0_QUAD)*c*t**2+t**3)+((-1.0_QUAD)+(-2.0_QUAD)*c*t+t**2)*w**2)
  End If
  If (ii==2) Then
    pii = (-1.0_QUAD)*s*(1.0_QUAD+c*t+(-2.0_QUAD)*t**2)*w**(-5)
  End If
  If (ii==1) Then
    pii = (-1.0_QUAD)*s*w**(-3)
  End If
  If (ii==5) Then
    pii = (-1.0_QUAD)*s*w**(-1)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)
  End If
  If (ii==6) Then
    pii = (-1.0_QUAD)*s*((1.0_QUAD+2.0_QUAD*c*t*w+(-1.0_QUAD)*w**2)*(w+(-1.0_QUAD)*c*t*w+w**2)**(-1)+&
      log(2.0_QUAD)+(-1.0_QUAD)*log(1.0_QUAD+(-1.0_QUAD)*c*t+w))
  End If
  If (ii==7) Then
    pii = s**(-1)*t**(-2)*(1.0_QUAD+(-1.0_QUAD)*c*t+(-1.0_QUAD)*w)*w**(-1)
  End If
  If (ii==8) Then
    pii = 5.0_QUAD*t**2*(10.0_QUAD*c**3+(-6.0_QUAD)*c**4*t+t*(8.0_QUAD+7.0_QUAD*s**4+(-6.0_QUAD)*t**2)+&
      c**2*t*((-1.0_QUAD)+3.0_QUAD*t**2)+&
      c*((-11.0_QUAD)+2.0_QUAD*t**2+t**4))*w**(-9)+t*((-12.0_QUAD)+17.0_QUAD*c**2+&
      (-6.0_QUAD)*c*s**2*t+3.0_QUAD*s**2*t**2)*w**(-7)+&
      c*(1.0_QUAD+2.0_QUAD*c*t+(-1)*t**2)*w**(-5)
  End If
  If (ii==9) Then
    pii = w**(-7)*(t*(w**2+s**2*((-5.0_QUAD)+10.0_QUAD*t**2+(-2.0_QUAD)*w**2))+&
      c*(w**2+(-1.0_QUAD)*t**2*(5.0_QUAD*s**2+2.0_QUAD*w**2)))
  End If
  If (ii==10) Then
    pii = w**(-5)*((-3.0_QUAD)*s**2*t+c*w**2)
  End If
  If (ii==11) Then
    pii = (c+(-1.0_QUAD)*t)*w**(-3)+(-1.0_QUAD)*t*w**(-1)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)
  End If
  If (ii==12) Then
    pii = w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-1.0_QUAD)*c**3*t**2+&
      c*(t**2+w**2+w**3)+t*((-1.0_QUAD)*w**2+(-1.0_QUAD)*s**2*(1.0_QUAD+2.0_QUAD*w)))
  End If
  If (ii==13) Then
    pii = (-1.0_QUAD)*c*((-1.0_QUAD)+c*t+(-1.0_QUAD)*w)**(-1)*w**(-1)*(1.0_QUAD+w)+&
      ((-1.0_QUAD)+c)*s**(-2)*t**(-1)*((-1.0_QUAD)+w)*(c+w)**(-1) +&
      (-1.0_QUAD)*c*w**(-1)*(c+w)**(-1)*(c+(-1.0_QUAD)*t+w)**(-1)+&
      (-1.0_QUAD)*s**(-2)*t**(-1)*(c*log((1.0_QUAD+c)*(c+(-1.0_QUAD)*t+w)**(-1))+&
      log((1.0_QUAD/2.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)))
  End If
  If (ii==14) Then
    pii = (-15.0_QUAD)*s*((-2.0_QUAD)+5.0_QUAD*s**2)*t**2*w**(-7)+12.0_QUAD*c*s*t*w**(-5)+&
      (-15.0_QUAD)*s*t**3*w**(-9)*(7.0_QUAD*c*s**2+(-7.0_QUAD)*s**2*t+2.0_QUAD*c*w**2)
  End If
  If (ii==15) Then
    pii = 3.0_QUAD*s*t*w**(-7)*((-5.0_QUAD)*s**2*t+2.0_QUAD*c*w**2)
  End If
  If (ii==16) Then
    pii = s*(4.0_QUAD+(-5.0_QUAD)*c*t+t**2)*w**(-5)+&
      (-2.0_QUAD)*s*w**(-3)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*(t**2*w+(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w))
  End If
  If (ii==17) Then
    pii = (-2.0_QUAD)*s+(-2.0_QUAD)*s*w**(-3)+(-2.0_QUAD)*c**(-2)*s**3*w**(-3)*(1.0_QUAD+w)**6*&
      (1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-3) +&
      c**(-2)*s**3*w**(-5)*(3.0_QUAD+3.0_QUAD*c*t+(-1.0_QUAD)*w**2*(4.0_QUAD+w**3))+&
      c**(-2)*s*((-1.0_QUAD)+c*t+(-1.0_QUAD)*w)**(-1)*w**(-5)*(1.0_QUAD+w)**2*&
      (2.0_QUAD*c**2*w**2*((-1.0_QUAD)+2.0_QUAD*w)+s**2*(3.0_QUAD+(-6.0_QUAD)*w+6.0_QUAD*w**3)) +&
      c**(-2)*s*w**(-4)*(1.0_QUAD+w)**4*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*(2.0_QUAD*c**2*w**2+&
      3.0_QUAD*s**2*((-1.0_QUAD)+w*(2.0_QUAD+w)))
  End If
  If (ii==18) Then
    pii = (-3.0_QUAD)*s*((-1.0_QUAD)+c*t)*w**(-5)+(-2.0_QUAD)*s*w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)*&
      (1.0_QUAD+w*(1.0_QUAD+w)**2*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1))
  End If
  If (ii==19) Then
    pii = s*w**(-3)+2.0_QUAD*c*s**(-3)*t**(-1)*(1.0_QUAD+(-1.0_QUAD)*c*t)*w**(-1)+&
      (1.0_QUAD+c**2)*s**(-1)*w**(-1)*(1.0_QUAD+w)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)+&
      (-2.0_QUAD)*s**(-3)*t**(-1)*((1.0_QUAD+c**2)*log((1.0_QUAD+c)*(c+(-1.0_QUAD)*t+w)**(-1))+&
      c*(1.0_QUAD+2.0_QUAD*log((1.0_QUAD/2.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t+w))))
  End If
  p  = pii
  End Subroutine xp_dpnm_sum
  Subroutine xp_ddpnm_sum(ii, t, c, p)
  Implicit None
  INTEGER, PARAMETER :: QUAD = SELECTED_REAL_KIND(32)
  ! Arguments declarations
  Integer, Intent(In) :: ii
  Real(KIND=QUAD), Intent(In) :: t
  Real(KIND=QUAD), Intent(In) :: c
  Real(KIND=QUAD), Intent(Out) :: p
  ! Variable declarations
  Real(KIND=QUAD) :: pii
  Real(KIND=QUAD) :: s
  Real(KIND=QUAD) :: w
  w = sqrt(1.0_QUAD+t**2 - 2.0_QUAD*t*c)
  s = sqrt(1.0_QUAD - c**2)
  pii = 0.0_QUAD
  If (ii == 4) Then
    pii = 9.0_QUAD*s**2*t*(1.0_QUAD+15.0_QUAD*c*t+18.0_QUAD*((-2.0_QUAD)+c**2)*t**2+c*((-39.0_QUAD)+c**2)*t**3+&
      (-3.0_QUAD)*((-20.0_QUAD)+c**2)*t**4+&
      (-9.0_QUAD)*c*t**5+(-8.0_QUAD)*t**6)*w**(-11)+((-4.0_QUAD)*c**4*t**3+9.0_QUAD*c**3*t**2*((-6.0_QUAD)+t**2)+&
      (-3.0_QUAD)*t*((-5.0_QUAD)+13.0_QUAD*t**2+3.0_QUAD*t**4)+3.0_QUAD*c**2*t*((-10.0_QUAD)+27.0_QUAD*t**2+6.0_QUAD*t**4)+&
      c*((-1.0_QUAD)+72.0_QUAD*t**2+(-66.0_QUAD)*t**4+8.0_QUAD*t**6))*w**(-9)
  End If
  If (ii == 3) Then
    pii = 35.0_QUAD*s**2*t**5*w**(-9)+(-1.0_QUAD)*c*w**(-5)+(-5.0_QUAD)*c*t**4*w**(-9)*(7.0_QUAD*s**2+w**2)+&
      (-5.0_QUAD)*t**3*w**(-9)*((-7.0_QUAD)+7.0_QUAD*c**4+21.0_QUAD*s**2+(-1.0_QUAD)*w**2+3.0_QUAD*s**2*w**2)+&
      t*w**(-7)*((-5.0_QUAD)+15.0_QUAD*s**2+(-2.0_QUAD)*w**2+4.0_QUAD*s**2*w**2)+c*t**2*w**(-9)*(35.0_QUAD*s**2+&
      5.0_QUAD*w**2+25.0_QUAD*s**2*w**2+w**4)
  End If
  If (ii == 2) Then
    pii = w**(-7)*((-1.0_QUAD)*c*w**2+c*t**2*(5.0_QUAD*s**2+2.0_QUAD*w**2)+&
      (-1.0_QUAD)*t*(w**2+s**2*((-5.0_QUAD)+10.0_QUAD*t**2+(-2.0_QUAD)*w**2)))
  End If
  If (ii == 1) Then
    pii = w**(-5)*(3.0_QUAD*s**2*t+(-1.0_QUAD)*c*w**2)
  End If
  If (ii == 5) Then
    pii = (-1.0_QUAD/4.0_QUAD)*(8.0_QUAD*c+4.0_QUAD*((-7.0_QUAD)+4.0_QUAD*s**2)*t+c*(35.0_QUAD+c**2+(-3.0_QUAD)*s**2)*t**2+&
      4.0_QUAD*((-5.0_QUAD)+3.0_QUAD*s**2)*t**3+4.0_QUAD*c*t**4)*w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)+&
      (t*(5.0_QUAD+(-2.0_QUAD)*s**2+t**2)+(-2.0_QUAD)*c*(1.0_QUAD+2.0_QUAD*t**2))*w**(-2)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)
  End If
  If (ii == 6) Then
    pii = 2.0_QUAD*s**2*t*w**(-2)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)+((-1.0_QUAD)*c+t+2.0_QUAD*s**2*t)*w**(-1)* &
      (1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)+&
      (1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-6.0_QUAD)*c**2*t+c*w+t*(4.0_QUAD+((-3.0_QUAD)+5.0_QUAD*s**2)*w))+&
      t*w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*(s**2+c*t*((-1.0_QUAD)+c**2*(1.0_QUAD+&
      (-1.0_QUAD)*w**3*((-3.0_QUAD)+log(2.0_QUAD)))))+&
      c*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-1.0_QUAD)*t**2*log(2.0_QUAD)+(-1.0_QUAD)*(1.0_QUAD+w)*log(4.0_QUAD)+&
      c*t*((-2.0_QUAD)+w*log(4.0_QUAD)+log(16.0_QUAD)))+&
      c*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-1.0_QUAD)*((-2.0_QUAD)+s**2)*t**2+&
      2.0_QUAD*(1.0_QUAD+w)+(-2.0_QUAD)*c*t*(2.0_QUAD+w))* &
      log(1.0_QUAD+(-1.0_QUAD)*c*t+w)
  End If
  If (ii == 7) Then
    pii = t**(-2)*((-1.0_QUAD)*t+c*t**2)*w**(-3)+s**(-2)*t**(-2)*w**(-1)*((-1.0_QUAD)*c+t+c*w)
  End If
  If (ii == 8) Then
    pii = (-5.0_QUAD)*s*t**3*((-98.0_QUAD)*c**4*t+16.0_QUAD*c**3*(7.0_QUAD+2.0_QUAD*t**2)+&
      6.0_QUAD*c**2*t*(11.0_QUAD+3.0_QUAD*t**2)+&
      t*(79.0_QUAD+63.0_QUAD*s**4+(-45.0_QUAD)*t**2+2.0_QUAD*t**4)+c*((-139.0_QUAD)+(-34.0_QUAD)*t**2+7.0_QUAD*t**4))*w**(-11) +&
      s*w**(-5)*((-2.0_QUAD)+w**2)+c*s*t*w**(-7)*((-50.0_QUAD)+9.0_QUAD*w**2)+5.0_QUAD*s*t**2*w**(-9)*(28.0_QUAD+w**4+&
      (-1.0_QUAD)*c**2*(49.0_QUAD+6.0_QUAD*w**2))
  End If
  If (ii == 9) Then
    pii = s*w**(-9)*(35.0_QUAD*s**2*t**2*(1.0_QUAD+(c+(-2.0_QUAD)*t)*t)+&
      5.0_QUAD*t*(2.0_QUAD*t+c*((-3.0_QUAD)+(-5.0_QUAD)*c*t+6.0_QUAD*t**2))*w**2+&
      ((-1.0_QUAD)+2.0_QUAD*t*((-2.0_QUAD)*c+t))*w**4)
  End If
  If (ii == 10) Then
    pii = 15.0_QUAD*s**3*t**2*w**(-7)+(-1.0_QUAD)*s*w**(-5)*(9.0_QUAD*c*t+w**2)
  End If
  If (ii == 11) Then
    pii = (-3.0_QUAD)*s*(c+(-1.0_QUAD)*t)*t*w**(-5)+(-1.0_QUAD)*s*w**(-3)+&
      s*t**2*w**(-3)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-2)*((-1.0_QUAD)*c*t+(1.0_QUAD+w)**2)
  End If
  If (ii == 12) Then
    pii = s*((-3.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t)*w**(-5)+w**(-3)+w**(-2)* &
      (1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)*(w**(-1)+(1.0_QUAD+w)**2*(1.0_QUAD+(-1.0_QUAD)*c*t+w)**(-1)))
  End If
  If (ii == 13) Then
    pii = (-1.0_QUAD)*s*w**(-3)+(-1.0_QUAD)*s**(-1)*w**(-1)+&
      s**(-3)*t**(-1)*((1.0_QUAD+c**2)*log((1.0_QUAD+c)* &
      (c+(-1.0_QUAD)*t+w)**(-1))+2.0_QUAD*c*log((1.0_QUAD/2.0_QUAD)*(1.0_QUAD+(-1.0_QUAD)*c*t+w)))
  End If
  If (ii == 14) Then
    pii = 15.0_QUAD*c*(2.0_QUAD+(-19.0_QUAD)*s**2)*t**2*w**(-7)+&
      12.0_QUAD*(1.0_QUAD+(-2.0_QUAD)*s**2)*t*w**(-5)+105.0_QUAD*s**2*t**4*w**(-11.0_QUAD)* &
      (9.0_QUAD*c*s**2+(-9.0_QUAD)*s**2*t+5.0_QUAD*c*w**2)+&
      15.0_QUAD*t**3*w**(-9)*((-35.0_QUAD)*s**2+63.0_QUAD*s**4+2.0_QUAD*(1.0_QUAD+(-2.0_QUAD)*c**2)*w**2)
  End If
  If (ii == 15) Then
    pii = 3.0_QUAD*t*w**(-9)*(35.0_QUAD*s**4*t**2+(-25.0_QUAD)*c*s**2*t*w**2+(2.0_QUAD+(-4.0_QUAD)*s**2)*w**4)
  End If
  If (ii == 16) Then
    pii = 3.0_QUAD*s**2*t*((-3.0_QUAD)+t*(c+2.0_QUAD*t))*w**(-7)+3.0_QUAD*c*w**(-5)*((-1.0_QUAD)+c*t+w**2)+&
      2.0_QUAD*c*s**(-2)*w**(-3)*(1.0_QUAD+c*t+2.0_QUAD*w**2)+&
      2.0_QUAD*s**(-2)*t**(-1)*w**(-3)*((-2.0_QUAD)+2.0_QUAD*w**3)+&
      6.0_QUAD*c*s**(-4)*t**(-1)*w**(-1)*(t+c*((-2.0_QUAD)+c*t+2.0_QUAD*w))
  End If
  If (ii == 17) Then
    pii = (9.0_QUAD*c+4.0_QUAD*c**3+(-1.0_QUAD)*c**5)*s**(-4)+5.0_QUAD*c*t**2*((-11.0_QUAD)+&
      c**2+10.0_QUAD*c*t+(-30.0_QUAD)*t**2)*w**(-7)+&
      (-5.0_QUAD)*s**(-2)*t*(3.0_QUAD+c**4+20.0_QUAD*c**2*t**2*(2.0_QUAD+t**2)+&
      (-4.0_QUAD)*c*t*(5.0_QUAD+10.0_QUAD*t**2+t**4))*w**(-7)+&
      (c+(-1.0_QUAD)*t)*(1.0_QUAD+2.0_QUAD*t*(c+6.0_QUAD*t))*w**(-5)+&
      (-2.0_QUAD)*s**(-2)*(c+(-1.0_QUAD)*t)*((-1.0_QUAD)+&
      t**2*((-25.0_QUAD)+6.0_QUAD*c*t+(-4.0_QUAD)*t**2))*w**(-5)+&
      12.0_QUAD*s**(-4)*(c+(-1.0_QUAD)*t)*((-1.0_QUAD)+&
      (-1.0_QUAD)*t**2*(6.0_QUAD+t**2)+4.0_QUAD*c*(t+t**3))*w**(-5)
  END IF
  If (ii == 18) Then
    pii = (-7.0_QUAD)*c*s**(-4)*(31.0_QUAD+5.0_QUAD*c**4+(-26.0_QUAD)*s**2)*w**(-7)+&
      14.0_QUAD*s**(-4)*(5.0_QUAD+c**4+(-5.0_QUAD)*s**2)*t**(-1)*w**(-7)+&
      s**(-4)*(181.0_QUAD+200*c**4+39.0_QUAD*c**6+(-183.0_QUAD)*s**2)*t*w**(-7)+&
      (-1.0_QUAD)*c*s**(-4)*(3.0_QUAD*(99.0_QUAD+40*c**4+c**6)+(-241.0_QUAD)*s**2)*t**2*w**(-7)+&
      s**(-4)*(103.0_QUAD+155.0_QUAD*c**4+(-6.0_QUAD)*c**6+(-86.0_QUAD)*s**2)*t**3*w**(-7)+&
      4.0_QUAD*s**(-4)*((-3.0_QUAD)+2.0_QUAD*s**2)*(7.0_QUAD*c+(-1.0_QUAD)*t)*t**4*w**(-7)+&
      2.0_QUAD*c*(5.0_QUAD+c**2)*s**(-4)*t**(-2)*w**(-7)*((-1.0_QUAD)+w**7)
  End If
  If (ii == 19) Then
    pii = (-3.0_QUAD)*s**2*t*w**(-5)+s**(-2)*t**(-1)*w**(-1)*((-2.0_QUAD)+&
      (-1.0_QUAD)*c**2+(-3.0_QUAD)*c*t+2.0_QUAD*w+c**2*w)+&
      (-1.0_QUAD)*t**(-1)*w**(-3)*(t**2+w**2+(-1.0_QUAD)*w**3)+&
      (-12.0_QUAD)*c**2*s**(-4)*t**(-1)*log(2.0_QUAD)+&
      (-1.0_QUAD)*s**(-2)*t**(-1)*log(16.0_QUAD)+&
      2.0_QUAD*c*(5.0_QUAD+c**2)*s**(-4)*t**(-1)*log((-1.0_QUAD)* &
      ((-1.0_QUAD)+c)**(-1)*((-1.0_QUAD)*c+t+w))+&
      4.0_QUAD*(1.0_QUAD+2.0_QUAD*c**2)*s**(-4)*t**(-1)*log(1.0_QUAD+(-1.0_QUAD)*c*t+w)
  End If
  p = pii
  End Subroutine xp_ddpnm_sum
  Subroutine pnm_sum_theta0(ii, c, p)
  Implicit None
  ! Arguments declarations
  Integer, Intent (In) :: ii
  Real (Kind=8), Intent (In) :: c
  Real (Kind=8), Intent (Out) :: p(3)
  ! Variable declarations
  Real (Kind=8) :: pii(3)
  Real (Kind=8) :: s
  s = sqrt(1.0d0-c**2)
  pii = 0D0
  If (ii==4) Then
    pii(1) =  3d0/(2*s**3)
    pii(2) = -9d0/(2*s**4)
    pii(3) =  18d0/(s**5)
  End If
  If (ii==3) Then
    pii(1) =  -1d0/(s**3)
    pii(2) =   3d0/(s**4)
    pii(3) = -12d0/(s**5)
  End If
  If (ii==2) Then
    pii(1) = -1d0/(2d0*s)
    pii(2) =  1d0/(2d0*s**2)
    pii(3) = -1d0/(s**3)
  End If
  If (ii==1) Then
    pii(1) =  1d0/s
    pii(2) = -1d0/(s**2)
    pii(3) =  2d0/(s**3)
  End If
  If (ii==5) Then
    pii(1) =  log(2d0/s)
    pii(2) = -1d0/s
    pii(3) =  1d0/(s**2)
  End If
  If (ii==6) Then
    pii(1) =  log(2d0/s)
    pii(2) = -1d0/s
    pii(3) =  1d0/(s**2)
  End If
  If (ii==7) Then
    pii(1) = -1d0+log(2d0/s)
    pii(2) = -1d0/s
    pii(3) =  1d0/(s**2)
  End If
  If (ii==8) Then
    pii(1) =  -3d0/(s**4)
    pii(2) =  12d0/(s**5)
    pii(3) = -60d0/(s**6)
  End If
  If (ii==9) Then
    pii(1) = -1d0/(2d0*s**2)
    pii(2) =  1d0/(s**3)
    pii(3) = -3d0/(s**4)
  End If
  If (ii==10) Then
    pii(1) =  1d0/(s**2)
    pii(2) = -2d0/(s**3)
    pii(3) =  6d0/(s**4)
  End If
  If (ii==11) Then
    pii(1) =  1d0/s
    pii(2) = -1d0/(s**2)
    pii(3) =  2d0/(s**3)
  End If
  If (ii==12) Then
    pii(1) =  1d0/s
    pii(2) = -1d0/(s**2)
    pii(3) = 2d0/(s**3)
  End If
  If (ii==13) Then
    pii(1) =  1d0
    pii(2) = -3d0/4d0+log(4d0)/4d0-log(s)/2d0
    pii(3) = -1d0/(2d0*s)
  End If
  If (ii==14) Then
    pii(1) =  -3d0/(2d0*s**3)
    pii(2) =   9d0/(2d0*s**4)
    pii(3) = -18d0/(s**5)
  End If
  If (ii==15) Then
    pii(1) =  3d0/(s**3)
    pii(2) = -9d0/(s**4)
    pii(3) = 36d0/(s**5)
  End If
  If (ii==16 .or. ii==17 .or. ii==18) Then
    pii(1) =  2d0/(s**2)
    pii(2) = -4d0/(s**3)
    pii(3) = 12d0/(s**4)
  End If
  If (ii==19) Then
    pii(1) =  1d0/s
    pii(2) = -1d0/(s**2)
    pii(3) =  2d0/(s**3)
  End If
  p = pii
  End Subroutine pnm_sum_theta0
End Module module_psum