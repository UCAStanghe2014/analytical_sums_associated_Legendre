Program mian
  Use module_nolp, Only: nol_ddpnm, len_nm2s, len_s2nm, nolfactor
  Use module_psum, Only: pnm_sum, dpnm_sum, ddpnm_sum, xp_pnm_sum, xp_dpnm_sum, xp_ddpnm_sum, pnm_sum_theta0
  Implicit None
  Integer (Kind=4) :: nmax
  Integer (Kind=4) :: nm, n, m, ii, jj, kk
  Real (Kind=8), Parameter :: deg = 0.01745329251994330D0
  Integer, Parameter :: quad = selected_real_kind(32)
  Real (Kind=8) :: t, c, s, pzero_tay(3), pzero_fun(3)
  Real (Kind=quad) :: xp_pzero_fun(3),xp_fsump,xp_dfsump,xp_ddfsump
  Real (Kind=8) :: angle, step
  Real (Kind=8) :: theta, pn
  Real (Kind=8) :: sump, fsump
  Real (Kind=8) :: dsump, dfsump
  Real (Kind=8) :: ddsump, ddfsump
  Real (Kind=8) :: epsilon
  Real (Kind=8), Allocatable :: p(:), dp(:), ddp(:)
  Real (Kind=8) :: re1, re2, re3
  Integer :: id, nbegin, mset
  Real (Kind=8) :: temp, ft
  Character (256) :: strname
  Character (2) :: sid

!// part one
  epsilon = 1D0/2D0
  nmax = 600
  nm = len_nm2s(nmax, nmax)
  Allocate (p(nm))
  Allocate (dp(nm))
  Allocate (ddp(nm))

  Write (*, *) 'Calculate all sums...'
  Do kk = 1, 19
    id = kk
    Write (sid, '(I0.2)') id
    strname = 'p' // sid // 'sum.txt'
    Open (Unit=11, File='2021-epsilon1over2-'//trim(strname), Action='write')
    Open (Unit=12, File='2021-epsilon1over2-'//'d'//trim(strname), Action='write')
    Open (Unit=13, File='2021-epsilon1over2-'//'dd'//trim(strname), Action='write')
    angle = 1D-3
    Do While (.True.)
      If (angle>180D0) Then
        Exit
      End If
      If (angle<0.5D0) step = 2D-3
      If (angle>0.5D0 .And. angle<1D1) step = 2D-1
      If (angle>1.0D1 .And. angle<1.780D2) step = 2D0
      If (angle>1.780D2) step = 2D-3
      theta = angle*deg

      !// low precesion
      !Call   pnm_sum(id, epsilon, cos(theta),   fsump)
      !Call  dpnm_sum(id, epsilon, cos(theta),  dfsump)
      !Call ddpnm_sum(id, epsilon, cos(theta), ddfsump)

      !// high precesion
      c=cos(theta)
      Call   xp_pnm_sum(id, real(epsilon,kind=quad), real(c,kind=quad), xp_fsump)
      Call  xp_dpnm_sum(id, real(epsilon,kind=quad), real(c,kind=quad), xp_dfsump)
      Call xp_ddpnm_sum(id, real(epsilon,kind=quad), real(c,kind=quad), xp_ddfsump)
      fsump=real(xp_fsump,kind=8)
      dfsump=real(xp_dfsump,kind=8)
      ddfsump=real(xp_ddfsump,kind=8)

      Call nol_ddpnm(nmax, theta, p, dp, ddp)
      sump = 0D0
      dsump = 0D0
      ddsump = 0D0
      Do ii = 1, nm
        Call len_s2nm(ii, n, m)
        Call psum_factor_list(id, 2, temp, nbegin, mset)
        If (m==mset .And. n>=nbegin) Then !// find pnm
          Call psum_factor_list(id, n, ft, nbegin, mset) !// set ft
          pn = p(ii)/nolfactor(n, m) !// denormalize
          sump = sump + epsilon**(n-1)*pn*ft
          pn = dp(ii)/nolfactor(n, m) !// denormalize
          dsump = dsump + epsilon**(n-1)*pn*ft
          pn = ddp(ii)/nolfactor(n, m) !// denormalize
          ddsump = ddsump + epsilon**(n-1)*pn*ft
        End If
      End Do
      If (angle>1.0D-3 .And. angle<180.0D0) Then
        re1 = log10(abs(100D0*(sump-fsump)/fsump))
        re2 = log10(abs(100D0*(dsump-dfsump)/dfsump))
        re3 = log10(abs(100D0*(ddsump-ddfsump)/ddfsump))
        If (re1<-30) re1 = -14.0
        If (re2<-30) re2 = -14.0
        If (re3<-30) re3 = -14.0
        Write (11, '( 4(ES23.16,2x),F8.3 )') angle, cos(theta), sump, fsump, re1
        Write (12, '( 4(ES23.16,2x),F8.3 )') angle, cos(theta), dsump, dfsump, re2
        Write (13, '( 4(ES23.16,2x),F8.3 )') angle, cos(theta), ddsump, ddfsump, re3
      End If
      angle = angle + step
    End Do
    Close (11)
  End Do
  Deallocate (p)
  Deallocate (dp)
  Deallocate (ddp)
 
!// part two
  t = 1D0
  Do ii = 1, 19
    Write (sid, '(I0.2)') ii
    strname = 'p' // sid // 'sum_at_theta0_twomethods.txt'
    Open (Unit=11, File=trim(strname), Action='write')
    theta = 0.0001D0
    Do While (theta<10D0)
      c = cos(theta*deg)
      s = sin(theta*deg)
      Call pnm_sum_theta0(ii, c, pzero_tay)
      Call xp_pnm_sum(ii, real(t,kind=quad), real(c,kind=quad), xp_pzero_fun(1))
      Call xp_dpnm_sum(ii, real(t,kind=quad), real(c,kind=quad), xp_pzero_fun(2))
      Call xp_ddpnm_sum(ii, real(t,kind=quad), real(c,kind=quad), xp_pzero_fun(3))
      Write (11, '(4(2X,ES16.4))') s, pzero_tay(1)/xp_pzero_fun(1), pzero_tay(2)/xp_pzero_fun(2), pzero_tay(3)/xp_pzero_fun(3)
      theta = theta + 1D-3
    End Do
    Close (11)
  End Do

End Program mian
