!
!      module set_components_flags
!
!        programmed by H.Matsui on March, 2009
!
!      subroutine s_set_components_flags(comp_name, phys_name,          &
!     &          icomp, ncomp, ncomp_org, rst_name)
!
      module set_components_flags
!
      use m_precision
!
      implicit  none
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
      character(len=kchara), intent(in) :: comp_name, phys_name
!
      integer(kind = kint), intent(inout) :: icomp, ncomp, ncomp_org
      character(len=kchara), intent(inout) :: rst_name
!
!
!
      if (     comp_name .eq. 'scalar'                                  &
     &    .or. comp_name .eq. 'Scalar'                                  &
     &    .or. comp_name .eq. 'SCALAR') then
        icomp =     icomp_SCALAR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SCALAR
        write(rst_name,1000) trim(phys_name)
      else if (comp_name .eq. 'vector'                                  &
     &    .or. comp_name .eq. 'Vector'                                  &
     &    .or. comp_name .eq. 'VECTOR') then
        icomp =     icomp_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1000) trim(phys_name)
      else if (comp_name .eq. 'sym_tensor'                              &
     &    .or. comp_name .eq. 'Sym_tensor'                              &
     &    .or. comp_name .eq. 'SYM_TENSOR') then
        icomp =     icomp_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if (comp_name .eq. 'asym_tensor'                             &
     &    .or. comp_name .eq. 'Asym_tensor'                             &
     &    .or. comp_name .eq. 'ASYM_TENSOR') then
        icomp =     icomp_ASYM_TENSOR
        ncomp =     ncomp_ASYM_TENSOR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1000) trim(phys_name)
      else if (comp_name .eq. 'spherical_vector'                        &
     &    .or. comp_name .eq. 'Spherical_vector'                        &
     &    .or. comp_name .eq. 'SPHERICAL_VECTOR') then
        icomp =     icomp_SPH_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1100) trim(phys_name)
      else if (comp_name .eq. 'cylindrical_vector'                      &
     &    .or. comp_name .eq. 'Cylindrical_vector'                      &
     &    .or. comp_name .eq. 'CYLINDRICAL_VECTOR') then
        icomp =     icomp_CYL_VECTOR
        ncomp =     ncomp_VECTOR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1200) trim(phys_name)
      else if (comp_name .eq. 'spherical_sym_tensor'                    &
     &    .or. comp_name .eq. 'Spherical_sym_tensor'                    &
     &    .or. comp_name .eq. 'SPHERICAL_SYM_TENSOR') then
        icomp =     icomp_SPHL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1100) trim(phys_name)
      else if (comp_name .eq. 'cylindrical_sym_tensor'                  &
     &    .or. comp_name .eq. 'Cylindrical_sym_tensor'                  &
     &    .or. comp_name .eq. 'CYLINDRICAL_SYM_TENSOR') then
        icomp =     icomp_CYL_SYM_TENSOR
        ncomp =     ncomp_SYM_TENSOR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1200) trim(phys_name)
!
      else if (comp_name .eq. 'norm'                                    &
     &    .or. comp_name .eq. 'Norm'                                    &
     &    .or. comp_name .eq. 'NORM') then
        icomp =     icomp_NORM
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1300) trim(phys_name)
      else if (comp_name .eq. 'x'                                       &
     &    .or. comp_name .eq. 'X') then
        icomp =     icomp_X
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1001) trim(phys_name)
      else if (comp_name .eq. 'y'                                       &
     &    .or. comp_name .eq. 'Y') then
        icomp =     icomp_Y
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1002) trim(phys_name)
      else if (comp_name .eq. 'z'                                       &
     &    .or. comp_name .eq. 'Z') then
        icomp =     icomp_Z
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1003) trim(phys_name)
      else if (comp_name .eq. 'radial'                                  &
     &    .or. comp_name .eq. 'Radial'                                  &
     &    .or. comp_name .eq. 'RADIAL'                                  &
     &    .or. comp_name .eq. 'r'                                       &
     &    .or. comp_name .eq. 'R'     ) then
        icomp =     icomp_RADIAL
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1011) trim(phys_name)
      else if (comp_name .eq. 'elevation'                               &
     &    .or. comp_name .eq. 'Elevation'                               &
     &    .or. comp_name .eq. 'ELEVATION'                               &
     &    .or. comp_name .eq. 'theta'                                   &
     &    .or. comp_name .eq. 'Theta'                                   &
     &    .or. comp_name .eq. 'THETA') then
        icomp =     icomp_THETA
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1012) trim(phys_name)
      else if (comp_name .eq. 'azimuth'                                 &
     &    .or. comp_name .eq. 'Azimuth'                                 &
     &    .or. comp_name .eq. 'AZIMUTH'                                 &
     &    .or. comp_name .eq. 'phi'                                     &
     &    .or. comp_name .eq. 'Phi'                                     &
     &    .or. comp_name .eq. 'PHI') then
        icomp =     icomp_PHI
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1013) trim(phys_name)
      else if (comp_name .eq. 'cylinder_r'                              &
     &    .or. comp_name .eq. 'Cylinder_r'                              &
     &    .or. comp_name .eq. 'CYLINDER_R'                              &
     &    .or. comp_name .eq. 's'                                       &
     &    .or. comp_name .eq. 'S') then
        icomp =     icomp_CYLINDER_R
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_VECTOR
        write(rst_name,1014) trim(phys_name)
!
      else if (comp_name .eq. 'norm_sym_tensor'                         &
     &    .or. comp_name .eq. 'Norm_sym_tensor'                         &
     &    .or. comp_name .eq. 'NORM_SYM_TENSOR') then
        icomp =     icomp_NORM_SYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if (comp_name .eq. 'norm_asym_tensor'                        &
     &    .or. comp_name .eq. 'Norm_asym_tensor'                        &
     &    .or. comp_name .eq. 'NORM_ASYM_TENSOR') then
        icomp =     icomp_NORM_ASYM_TENSOR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_ASYM_TENSOR
        write(rst_name,1300) trim(phys_name)
      else if (comp_name .eq. 'xx'                                      &
     &    .or. comp_name .eq. 'XX') then
        icomp =     icomp_XX
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1061) trim(phys_name)
      else if (comp_name .eq. 'xy'                                      &
     &    .or. comp_name .eq. 'XY') then
        icomp =     icomp_XY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1062) trim(phys_name)
      else if (comp_name .eq. 'xz'                                      &
     &    .or. comp_name .eq. 'XZ') then
        icomp =     icomp_XZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1063) trim(phys_name)
      else if (comp_name .eq. 'yy'                                      &
     &    .or. comp_name .eq. 'YY') then
        icomp =     icomp_YY
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1064) trim(phys_name)
      else if (comp_name .eq. 'yz'                                      &
     &    .or. comp_name .eq. 'YZ') then
        icomp =     icomp_YZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1065) trim(phys_name)
      else if (comp_name .eq. 'zz'                                      &
     &    .or. comp_name .eq. 'ZZ') then
        icomp =     icomp_ZZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1066) trim(phys_name)
!
      else if (comp_name .eq. 'rr'                                      &
     &    .or. comp_name .eq. 'RR') then
        icomp =     icomp_RR
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1221) trim(phys_name)
      else if (comp_name .eq. 'rt'                                      &
     &    .or. comp_name .eq. 'RT') then
        icomp =     icomp_RT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1222) trim(phys_name)
      else if (comp_name .eq. 'rp'                                      &
     &    .or. comp_name .eq. 'RP') then
        icomp =     icomp_RP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1223) trim(phys_name)
      else if (comp_name .eq. 'tt'                                      &
     &    .or. comp_name .eq. 'TT') then
        icomp =     icomp_TT
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1224) trim(phys_name)
      else if (comp_name .eq. 'tp'                                      &
     &    .or. comp_name .eq. 'TP') then
        icomp =     icomp_TP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1225) trim(phys_name)
      else if (comp_name .eq. 'pp'                                      &
     &    .or. comp_name .eq. 'PP') then
        icomp =     icomp_PP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1226) trim(phys_name)
!
      else if (comp_name .eq. 'ss'                                      &
     &    .or. comp_name .eq. 'SS') then
        icomp =     icomp_SS
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1231) trim(phys_name)
      else if (comp_name .eq. 'sp'                                      &
     &    .or. comp_name .eq. 'SP') then
        icomp =     icomp_SP
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1232) trim(phys_name)
      else if (comp_name .eq. 'sz'                                      &
     &    .or. comp_name .eq. 'SZ') then
        icomp =     icomp_SZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1233) trim(phys_name)
      else if (comp_name .eq. 'pp_cyl'                                  &
     &    .or. comp_name .eq. 'PP_cyl') then
        icomp =     icomp_PP_cyl
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1234) trim(phys_name)
      else if (comp_name .eq. 'pz'                                      &
     &    .or. comp_name .eq. 'PZ') then
        icomp =     icomp_PZ
        ncomp =     ncomp_SCALAR
        ncomp_org = ncomp_SYM_TENSOR
        write(rst_name,1235) trim(phys_name)
      else if (comp_name .eq. 'zz_cyl'                                  &
     &    .or. comp_name .eq. 'ZZ_cyl') then
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
 1300 format (a, "_norm")
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
