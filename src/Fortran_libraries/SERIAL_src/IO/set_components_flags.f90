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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_components_flags(comp_name, phys_name,           &
     &          icomp, ncomp, ncomp_org, rst_name)
!
      use t_field_labels
      use t_multi_flag_labels
      use m_component_flags
      use m_more_component_flags
      use skip_comment_f
!
      character(len=kchara), intent(in) :: comp_name, phys_name
!
      integer(kind = kint), intent(inout) :: icomp, ncomp, ncomp_org
      character(len=kchara), intent(inout) :: rst_name
!
!
!
      call init_more_componnet_flags()
      if     (cmp_no_case(comp_name, scalar%name))then
        icomp =     icomp_SCALAR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SCALAR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, vector%name)) then
        icomp =     icomp_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, sym_tensor%name)) then
        icomp =     icomp_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, asym_tensor%name)) then
        icomp =     icomp_ASYM_TENSOR
        ncomp =     ncomp_ASYM_TENSOR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if(cmp_no_case(comp_name, spherical_vector%name)) then
        icomp =     icomp_SPH_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1100) trim(phys_name)
      else if(cmp_no_case(comp_name, cylindrical_vector%name)) then
        icomp =     icomp_CYL_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1200) trim(phys_name)
      else if(cmp_no_case(comp_name, spherical_sym_tensor%name)) then
        icomp =     icomp_SPHL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1100) trim(phys_name)
      else if(cmp_no_case(comp_name, cylindrical_sym_tensor%name)) then
        icomp =     icomp_CYL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1200) trim(phys_name)
!
      else if(check_mul_flags(comp_name, magnitude_flags)) then
        icomp =     icomp_NORM
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1300) trim(phys_name)
      else if(cmp_no_case(comp_name, V_x%name)) then
        icomp =     icomp_X
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1001) trim(phys_name)
      else if(cmp_no_case(comp_name, V_y%name)) then
        icomp =     icomp_Y
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1002) trim(phys_name)
      else if(cmp_no_case(comp_name, V_z%name)) then
        icomp =     icomp_Z
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1003) trim(phys_name)
      else if(check_mul_flags(comp_name, radial_flags)) then
        icomp =     icomp_RADIAL
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1011) trim(phys_name)
      else if(check_mul_flags(comp_name, theta_flags)) then
        icomp =     icomp_THETA
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1012) trim(phys_name)
      else if(check_mul_flags(comp_name, phi_flags)) then
        icomp =     icomp_PHI
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1013) trim(phys_name)
      else if(check_mul_flags(comp_name, cyrindrical_r_flags)) then
        icomp =     icomp_CYLINDER_R
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1014) trim(phys_name)
!
      else if(check_mul_flags(comp_name, sym_tensor_magnitude_flags)    &
     &        ) then
        icomp =     icomp_NORM_SYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if(check_mul_flags(comp_name, asym_tensor_magnitude_flags)   &
     &        ) then
        icomp =     icomp_NORM_ASYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if(cmp_no_case(comp_name, T_xx%name)) then
        icomp =     icomp_XX
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1061) trim(phys_name)
      else if(cmp_no_case(comp_name, T_xy%name)) then
        icomp =     icomp_XY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1062) trim(phys_name)
      else if(cmp_no_case(comp_name, T_xz%name)) then
        icomp =     icomp_XZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1063) trim(phys_name)
      else if(cmp_no_case(comp_name, T_yy%name)) then
        icomp =     icomp_YY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1064) trim(phys_name)
      else if(cmp_no_case(comp_name, T_yz%name)) then
        icomp =     icomp_YZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1065) trim(phys_name)
      else if(cmp_no_case(comp_name, T_zz%name)) then
        icomp =     icomp_ZZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1066) trim(phys_name)
!
      else if(cmp_no_case(comp_name, T_rr%name)) then
        icomp =     icomp_RR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1221) trim(phys_name)
      else if(cmp_no_case(comp_name, T_rt%name)) then
        icomp =     icomp_RT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1222) trim(phys_name)
      else if(cmp_no_case(comp_name, T_rp%name)) then
        icomp =     icomp_RP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1223) trim(phys_name)
      else if(cmp_no_case(comp_name, T_tt%name)) then
        icomp =     icomp_TT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1224) trim(phys_name)
      else if(cmp_no_case(comp_name, T_tp%name)) then
        icomp =     icomp_TP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1225) trim(phys_name)
      else if(cmp_no_case(comp_name, T_pp%name)) then
        icomp =     icomp_PP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1226) trim(phys_name)
!
      else if(cmp_no_case(comp_name, T_ss%name)) then
        icomp =     icomp_SS
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1231) trim(phys_name)
      else if(cmp_no_case(comp_name, T_sp%name)) then
        icomp =     icomp_SP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1232) trim(phys_name)
      else if(cmp_no_case(comp_name, T_sz%name)) then
        icomp =     icomp_SZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1233) trim(phys_name)
      else if(cmp_no_case(comp_name, T_pp%name)) then
        icomp =     icomp_PP_cyl
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1234) trim(phys_name)
      else if(cmp_no_case(comp_name, T_pz%name)) then
        icomp =     icomp_PZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1235) trim(phys_name)
      else if(cmp_no_case(comp_name, T_zz%name)) then
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
      call dealloc_more_componnet_flags
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
