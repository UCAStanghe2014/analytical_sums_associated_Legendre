  Program mian
  Implicit None
  Integer :: ii
  Real (Kind=16) :: t
  Real (Kind=16) :: c
  Real (Kind=16) :: p, dp, ddp
  t = 1D0/8D0
  c = 1D0/Sqrt(2D0)
  Open(Unit = 11, File = 'table_of_sums_fortran.txt', Action = 'Write')
  Write (11, '(A)') '# c = cos(theta) = 1/sqrt(2), epsilon = 1/8'
  Write (11, '(A)') '# ID     Pnm              dPnm             ddPnm'
  Do ii = 1, 19, 1
    Call   pnm_sum(ii, t, c,   p)
    Call  dpnm_sum(ii, t, c,  dp)
    Call ddpnm_sum(ii, t, c, ddp)
    Write (11, '(I0.2,3(1X,ES16.4))') ii, p, dp, ddp
  ENDDO
  Close(Unit = 11)
  End Program mian
