!>@file  set_components_flags.f90
!!       module set_components_flags
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Set component labels
!!
!!@verbatim
!!      subroutine s_set_components_flags(comp_name, phys_name,         &
!!     &          icomp, ncomp, ncomp_org, rst_name)
!!@endverbatim
!
      module set_components_flags
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara), parameter :: hd_scl = 'scalar'
      character(len=kchara), parameter :: hd_vec = 'vector'
      character(len=kchara), parameter :: hd_tsr = 'sym_tensor'
      character(len=kchara), parameter :: hd_ats = 'asym_tensor'
!
      character(len=kchara), parameter                                  &
     &                      :: hd_vec_sph = 'spherical_vector'
      character(len=kchara), parameter                                  &
     &                      :: hd_vec_cyl = 'cylindrical_vector'
      character(len=kchara), parameter                                  &
     &                      :: hd_tsr_sph = 'spherical_sym_tensor'
      character(len=kchara), parameter                                  &
     &                      :: hd_tsr_cyl = 'cylindrical_sym_tensor'
!
      character(len=kchara), parameter :: hd_amplitude = 'amplitude'
      character(len=kchara), parameter :: hd_magnitude = 'magnitude'
      character(len=kchara), parameter :: hd_norm =      'norm'
!
      character(len=kchara), parameter :: hd_vec_x = 'x'
      character(len=kchara), parameter :: hd_vec_y = 'y'
      character(len=kchara), parameter :: hd_vec_z = 'z'
      character(len=kchara), parameter :: hd_vec_r = 'r'
      character(len=kchara), parameter :: hd_vec_t = 'theta'
      character(len=kchara), parameter :: hd_vec_p = 'phi'
      character(len=kchara), parameter :: hd_vec_s = 's'
      character(len=kchara), parameter :: hd_vec_rd = 'radial'
      character(len=kchara), parameter :: hd_vec_ev = 'elevation'
      character(len=kchara), parameter :: hd_vec_az = 'azimuth'
      character(len=kchara), parameter :: hd_vec_cl = 'cylinder_r'
!
      character(len=kchara), parameter                                  &
     &                      :: hd_tsr_mag =  'norm_sym_tensor'
      character(len=kchara), parameter                                  &
     &                      :: hd_tsr_norm = 'mag_sym_tensor'
      character(len=kchara), parameter                                  &
     &                      :: hd_ats_mag =  'norm_asym_tensor'
      character(len=kchara), parameter                                  &
     &                      :: hd_ats_norm = 'mag_asym_tensor'
!
      character(len=kchara), parameter :: hd_tsr_xx = 'xx'
      character(len=kchara), parameter :: hd_tsr_xy = 'xy'
      character(len=kchara), parameter :: hd_tsr_xz = 'xz'
      character(len=kchara), parameter :: hd_tsr_yy = 'yy'
      character(len=kchara), parameter :: hd_tsr_yz = 'yz'
      character(len=kchara), parameter :: hd_tsr_zz = 'zz'
!
      character(len=kchara), parameter :: hd_tsr_rr = 'rr'
      character(len=kchara), parameter :: hd_tsr_rt = 'rt'
      character(len=kchara), parameter :: hd_tsr_rp = 'rp'
      character(len=kchara), parameter :: hd_tsr_tt = 'tt'
      character(len=kchara), parameter :: hd_tsr_tp = 'tp'
      character(len=kchara), parameter :: hd_tsr_pp = 'pp'
!
      character(len=kchara), parameter :: hd_tsr_ss =     'ss'
      character(len=kchara), parameter :: hd_tsr_sp =     'sp'
      character(len=kchara), parameter :: hd_tsr_sz =     'sz'
      character(len=kchara), parameter :: hd_tsr_pp_cyl = 'pp_cyl'
      character(len=kchara), parameter :: hd_tsr_pz =     'pz'
      character(len=kchara), parameter :: hd_tsr_zz_cyl = 'zz_cyl'
!
!
!
      integer(kind = kint), parameter :: ncomp_SCALAR =      1
      integer(kind = kint), parameter :: ncomp_VECTOR =      3
      integer(kind = kint), parameter :: ncomp_SYM_TENSOR =  6
      integer(kind = kint), parameter :: ncomp_ASYM_TENSOR = 3
!
!
      integer(kind = kint), parameter :: icomp_SCALAR =        1
      integer(kind = kint), parameter :: icomp_VECTOR =      101
      integer(kind = kint), parameter :: icomp_SYM_TENSOR =  201
      integer(kind = kint), parameter :: icomp_ASYM_TENSOR = 301
!
      integer(kind = kint), parameter :: icomp_SPH_VECTOR =      102
      integer(kind = kint), parameter :: icomp_CYL_VECTOR =      103
      integer(kind = kint), parameter :: icomp_SPHL_SYM_TENSOR = 202
      integer(kind = kint), parameter :: icomp_CYL_SYM_TENSOR =  203
!
      integer(kind = kint), parameter :: icomp_NORM =               0
      integer(kind = kint), parameter :: icomp_NORM_SYM_TENSOR =  200
      integer(kind = kint), parameter :: icomp_NORM_ASYM_TENSOR = 300
!
      integer(kind = kint), parameter :: icomp_X =           1
      integer(kind = kint), parameter :: icomp_Y =           2
      integer(kind = kint), parameter :: icomp_Z =           3
      integer(kind = kint), parameter :: icomp_RADIAL =     11
      integer(kind = kint), parameter :: icomp_THETA =      12
      integer(kind = kint), parameter :: icomp_PHI =        13
      integer(kind = kint), parameter :: icomp_CYLINDER_R = 14
!
      integer(kind = kint), parameter :: icomp_XX =       1
      integer(kind = kint), parameter :: icomp_XY =       2
      integer(kind = kint), parameter :: icomp_XZ =       3
      integer(kind = kint), parameter :: icomp_YY =       4
      integer(kind = kint), parameter :: icomp_YZ =       5
      integer(kind = kint), parameter :: icomp_ZZ =       6
      integer(kind = kint), parameter :: icomp_RR =     221
      integer(kind = kint), parameter :: icomp_RT =     222
      integer(kind = kint), parameter :: icomp_RP =     223
      integer(kind = kint), parameter :: icomp_TT =     224
      integer(kind = kint), parameter :: icomp_TP =     225
      integer(kind = kint), parameter :: icomp_PP =     226
!
      integer(kind = kint), parameter :: icomp_SS =     231
      integer(kind = kint), parameter :: icomp_SP =     232
      integer(kind = kint), parameter :: icomp_SZ =     233
      integer(kind = kint), parameter :: icomp_PP_cyl = 234
      integer(kind = kint), parameter :: icomp_PZ =     235
      integer(kind = kint), parameter :: icomp_ZZ_cyl = 236
!
      private :: hd_scl, hd_vec, hd_tsr, hd_ats
      private :: hd_vec_sph, hd_vec_cyl, hd_tsr_sph, hd_tsr_cyl
      private :: hd_amplitude, hd_magnitude, hd_norm
      private :: hd_tsr_mag, hd_tsr_norm, hd_ats_mag, hd_ats_norm
      private :: hd_vec_x, hd_vec_y, hd_vec_z
      private :: hd_vec_r, hd_vec_t, hd_vec_p, hd_vec_s
      private :: hd_vec_rd, hd_vec_ev, hd_vec_az, hd_vec_cl
      private :: hd_tsr_xx, hd_tsr_xy, hd_tsr_xz, hd_tsr_yy
      private :: hd_tsr_yz, hd_tsr_zz, hd_tsr_rr, hd_tsr_rt
      private :: hd_tsr_rp, hd_tsr_tt, hd_tsr_tp, hd_tsr_pp
      private :: hd_tsr_ss, hd_tsr_sp, hd_tsr_sz, hd_tsr_pz
      private :: hd_tsr_pp_cyl, hd_tsr_zz_cyl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_components_flags(comp_name, phys_name,           &
     &          icomp, ncomp, ncomp_org, rst_name)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: comp_name, phys_name
!
      integer(kind = kint), intent(inout) :: icomp, ncomp, ncomp_org
      character(len=kchara), intent(inout) :: rst_name
!
!
!
      if     (cmp_no_case(comp_name, hd_scl))then
        icomp =     icomp_SCALAR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SCALAR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec)) then
        icomp =     icomp_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr)) then
        icomp =     icomp_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_ats)) then
        icomp =     icomp_ASYM_TENSOR
        ncomp =     ncomp_ASYM_TENSOR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_sph)) then
        icomp =     icomp_SPH_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1100) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_cyl)) then
        icomp =     icomp_CYL_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1200) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_sph)) then
        icomp =     icomp_SPHL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1100) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_cyl)) then
        icomp =     icomp_CYL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1200) trim(phys_name)
!
      else if(cmp_no_case(comp_name, hd_amplitude)                      &
     &   .or. cmp_no_case(comp_name, hd_magnitude)                      &
     &   .or. cmp_no_case(comp_name, hd_norm)) then
        icomp =     icomp_NORM
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1300) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_x)) then
        icomp =     icomp_X
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1001) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_y)) then
        icomp =     icomp_Y
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1002) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_z)) then
        icomp =     icomp_Z
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1003) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_rd)                         &
     &   .or. cmp_no_case(comp_name, hd_vec_r)) then
        icomp =     icomp_RADIAL
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1011) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_ev)                         &
     &   .or. cmp_no_case(comp_name, hd_vec_t)) then
        icomp =     icomp_THETA
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1012) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_az)                         &
     &   .or. cmp_no_case(comp_name, hd_vec_p)) then
        icomp =     icomp_PHI
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1013) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_vec_cl)                         &
     &   .or. cmp_no_case(comp_name, hd_vec_s)) then
        icomp =     icomp_CYLINDER_R
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1014) trim(phys_name)
!
      else if(cmp_no_case(comp_name, hd_tsr_mag)                        &
     &   .or. cmp_no_case(comp_name, hd_tsr_norm)) then
        icomp =     icomp_NORM_SYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_ats_mag)                        &
     &   .or. cmp_no_case(comp_name, hd_ats_norm)) then
        icomp =     icomp_NORM_ASYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_xx)) then
        icomp =     icomp_XX
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1061) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_xy)) then
        icomp =     icomp_XY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1062) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_xz)) then
        icomp =     icomp_XZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1063) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_yy)) then
        icomp =     icomp_YY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1064) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_yz)) then
        icomp =     icomp_YZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1065) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_zz)) then
        icomp =     icomp_ZZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1066) trim(phys_name)
!
      else if(cmp_no_case(comp_name, hd_tsr_rr)) then
        icomp =     icomp_RR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1221) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_rt)) then
        icomp =     icomp_RT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1222) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_rp)) then
        icomp =     icomp_RP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1223) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_tt)) then
        icomp =     icomp_TT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1224) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_tp)) then
        icomp =     icomp_TP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1225) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_pp)) then
        icomp =     icomp_PP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1226) trim(phys_name)
!
      else if(cmp_no_case(comp_name, hd_tsr_ss)) then
        icomp =     icomp_SS
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1231) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_sp)) then
        icomp =     icomp_SP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1232) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_sz)) then
        icomp =     icomp_SZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1233) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_pp_cyl)) then
        icomp =     icomp_PP_cyl
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1234) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_pz)) then
        icomp =     icomp_PZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1235) trim(phys_name)
      else if(cmp_no_case(comp_name, hd_tsr_zz_cyl)) then
        icomp =     icomp_ZZ_cyl
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1236) trim(phys_name)
!
      else
        icomp =     1
        ncomp =     0
        ncomp_org = 1
        write(rst_name,1000) trim(phys_name)
      end if
!
 1000 format (a)
 1100 format (a, "_sph")
 1200 format (a, "_cyl")
 1300 format (a, "_mag")
 1001 format (a, "_x")
 1002 format (a, "_y")
 1003 format (a, "_z")
 1011 format (a, "_r")
 1012 format (a, "_theta")
 1013 format (a, "_phi")
 1014 format (a, "_s")
 1061 format (a, "_xx")
 1062 format (a, "_xy")
 1063 format (a, "_xz")
 1064 format (a, "_yy")
 1065 format (a, "_yz")
 1066 format (a, "_zz")
!
 1221 format (a, "_rr")
 1222 format (a, "_rt")
 1223 format (a, "_rp")
 1224 format (a, "_tt")
 1225 format (a, "_tp")
 1226 format (a, "_pp")
 1231 format (a, "_ss")
 1232 format (a, "_sp")
 1233 format (a, "_sz")
 1234 format (a, "_pp")
 1235 format (a, "_pz")
 1236 format (a, "_zz")
!
      end subroutine s_set_components_flags
!
!  ---------------------------------------------------------------------
!
      end module set_components_flags
