!>@file   m_component_flags.f90
!!        module m_component_flags
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief flags of components in control
!!
!!@verbatim
!!      integer(kind = kint) function num_flag_scalar_comp()
!!      integer(kind = kint) function num_flag_vector_comp()
!!      integer(kind = kint) function num_flag_sym_tensor_comp()
!!      integer(kind = kint) function num_flag_asym_tensor_comp()
!!
!!      subroutine set_flag_scalar_comp(n_comps, names, maths)
!!      subroutine set_flag_vector_comp(n_comps, names, maths)
!!      subroutine set_flag_sym_tensor_comp(n_comps, names, maths)
!!      subroutine set_flag_asym_tensor_comp(n_comps, names, maths)
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    componant label for scalar
!!        scalar:             label for scalar  @f$ S @f$
!!
!!    componant label for vector
!!        vector:             label for vector
!!         @f$ (V_{x}, V_{y}, V_{z}) @f$
!!        spherical_vector:   label for vector on spherical coordinate
!!         @f$ (V_{r}, V_{\\theta}, V_{\\phi}) @f$
!!        cylindrical_vector: label for vector on cylindrical coordinate
!!         @f$ (V_{s}, V_{\\phi}, V_{z}) @f$
!!
!!        magnitude: label for magnitude of vector  @f$ |V| @f$
!!
!!        x: label for x-component @f$ V_{x} @f$
!!        y: label for y-component @f$ V_{y} @f$
!!        z: label for z-component @f$ V_{z} @f$
!!
!!        r: label for radial component                  @f$ V_{r} @f$
!!        theta: label for elevation component        @f$ V_{\theta}  @f$
!!        phi:   label for azimuthal component           @f$ V_{\phi} @f$
!!        s:     label for cylindrical radius component  @f$ V_{s} @f$
!!
!!
!!    componant label for symmetric tensor
!!        sym_tensor:  label for symmetric tensor  
!!         @f$ (T_{xx}, T_{xy}, T_{xz}, T_{yy}, T_{yz}, T_{zz}) @f$
!!        spherical_sym_tensor: label for symmetric tensor
!!                              on spherical coordinate  
!!         @f$ (T_{rr}, T_{r \theta}, T_{r \phi}, 
!!              T_{\theta \theta}, T_{\theta \phi}, T_{\phi \phi}) @f$
!!        cylindrical_sym_tensor: label for symmetric tensor
!!                               on cylindrical coordinate  
!!         @f$ (T_{ss}, T_{s \phi}, T_{sz}, 
!!              T_{\phi \phi}, T_{\phi z}, T_{zz}) @f$
!!
!!        magnitude:  label for magnitude of vector @f$ || T || @f$
!!
!!        xx: label for xx-component  @f$ T_{xx} @f$
!!        xy: label for xy-component  @f$ T_{xy} @f$
!!        xz: label for xz-component  @f$ T_{xz} @f$
!!        yy: label for yy-component  @f$ T_{yy} @f$
!!        yz: label for yz-component  @f$ T_{yz} @f$
!!        zz: label for zz-component  @f$ T_{zz} @f$
!!
!!        rr: label for rr component        @f$ T_{rr} @f$
!!        rt: label for r-theta component   @f$ T_{r \theta}  @f$
!!        rp: label for r-phi component     @f$ T_{r \phi} @f$
!!        tt: label for theta-theta component 
!!                 @f$ T_{\theta \theta}  @f$
!!        tp: label for theta-phi component  @f$ T_{\theta \phi} @f$
!!        pp: label for phi-phi component    @f$ T_{\phi \phi} @f$
!!
!!        ss: label for ss-component      @f$ T_{ss} @f$
!!        sp: label for s-phi-component   @f$ T_{s \phi} @f$
!!        sz: label for sz-component      @f$ T_{sz} @f$
!!        pp: label for phi-phi-component @f$ T_{\phi \phi} @f$
!!        pz: label for phi-z-component   @f$ T_{\phi z} @f$
!!        zz: label for zz-component      @f$ T_{zz} @f$
!!
!!
!!    componant label for asymmetric tensor
!!        asym_tensor:  label for asymmetric tensor  
!!         @f$ (T_{xy}, T_{xz}, T_{yz}) @f$
!!        spherical_asym_tensor: label for asymmetric tensor
!!                              on spherical coordinate  
!!         @f$ (T_{r \theta}, T_{r \phi}, T_{\theta \phi}) @f$
!!        cylindrical_asym_tensor: label for asymmetric tensor
!!                               on cylindrical coordinate  
!!         @f$ (T_{s \phi}, T_{sz}, T_{\phi z}) @f$
!!
!!        magnitude:  label for magnitude of vector @f$ || T || @f$
!!
!!        xy: label for xy-component  @f$ T_{xy} @f$
!!        xz: label for xz-component  @f$ T_{xz} @f$
!!        yz: label for yz-component  @f$ T_{yz} @f$
!!
!!        rt: label for r-theta component   @f$ T_{r \theta}  @f$
!!        rp: label for r-phi component     @f$ T_{r \phi} @f$
!!        tp: label for theta-phi component  @f$ T_{\theta \phi} @f$
!!
!!        sp: label for s-phi-component   @f$ T_{s \phi} @f$
!!        sz: label for sz-component      @f$ T_{sz} @f$
!!        pz: label for phi-z-component   @f$ T_{\phi z} @f$
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
      module m_component_flags
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!!
      integer(kind = kint), parameter, private :: ntype_scalar =  1
      integer(kind = kint), parameter, private :: ntype_vector = 11
      integer(kind = kint), parameter, private :: ntype_tensor = 22
      integer(kind = kint), parameter, private :: ntype_as_tsr = 13
!
!>        Field label for scalar
!!         @f$ S @f$
      type(field_def), parameter :: scalar                              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'scalar',                                  &
     &                math = '$ S $')
!
!>        Field label for vector
!!         @f$ (V_{x}, V_{y}, V_{z}) @f$
      type(field_def), parameter :: vector                              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vector',                                  &
     &                math = '$ (V_{x}, V_{y}, V_{z}) $')
!>        Field label for vector on spherical coordinate
!!         @f$ (V_{r}, V_{\\theta}, V_{\\phi}) @f$
      type(field_def), parameter :: spherical_vector                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'spherical_vector',                        &
     &                math = '$ (V_{r}, V_{\\theta}, V_{\\phi}) $')
!>        Field label for vector on cylindrical coordinate
!!         @f$ (V_{s}, V_{\\phi}, V_{z}) @f$
      type(field_def), parameter :: cylindrical_vector                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'cylindrical_vector',                      &
     &                math = '$ (V_{s}, V_{\\phi}, V_{z}) $')
!
!>        Field label for magnitude of vector
!!         @f$ |V| @f$
      type(field_def), parameter :: magnitude                           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnitude',                               &
     &                math = '$ |V| $')
!
!>        Field label for x-component
!!         @f$ V_{x} @f$
      type(field_def), parameter :: V_x                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'x',                                       &
     &                math = '$ V_{x} $')
!>        Field label for y-component
!!         @f$ V_{y} @f$
      type(field_def), parameter :: V_y                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'y',                                       &
     &                math = '$ V_{y} $')
!>        Field label for z-component
!!         @f$ V_{z}  @f$
      type(field_def), parameter :: V_z                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'z',                                       &
     &                math = '$ V_{z} $')
!
!>        Field label for radial component
!!         @f$ V_{r} @f$
      type(field_def), parameter :: V_r                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'r',                                       &
     &                math = '$ V_{r} $')
!>        Field label for elevation component
!!         @f$ V_{\theta}  @f$
      type(field_def), parameter :: V_theta                             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'theta',                                   &
     &                math = '$ V_{\theta} $')
!>        Field label for azimuthal component
!!         @f$ V_{\phi} @f$
      type(field_def), parameter :: V_phi                               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'phi',                                     &
     &                math = '$ V_{\phi} $')
!>        Field label for cylindrical radius component
!!         @f$ V_{s} @f$
      type(field_def), parameter :: V_s                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 's',                                       &
     &                math = '$ V_{s} $')
!
!
!>        Field label for symmetric tensor
!!         @f$ (T_{xx}, T_{xy}, T_{xz}, T_{yy}, T_{yz}, T_{zz}) @f$
      type(field_def), parameter :: sym_tensor                          &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'sym_tensor',                              &
     &                math = '$ (T_{xx}, T_{xy}, T_{xz},'               &
     &                    // ' T_{yy}, T_{yz}, T_{zz}) $')
!>        Field label for symmetric tensor on spherical coordinate
!!         @f$ (T_{rr}, T_{r \theta}, T_{r \phi}, 
!!              T_{\theta \theta}, T_{\theta \phi}, T_{\phi \phi}) @f$
      type(field_def), parameter :: spherical_sym_tensor                &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'spherical_sym_tensor',                    &
     &                math = '$ (T_{rr}, T_{r \theta}, T_{r \phi},'     &
     &                    // ' T_{\theta \theta}, T_{\theta \phi},'     &
     &                    // ' T_{\phi \phi}) $')
!>        Field label for symmetric tensor on cylindrical coordinate
!!         @f$ (T_{ss}, T_{s \phi}, T_{sz}, 
!!              T_{\phi \phi}, T_{\phi z}, T_{zz}) @f$
      type(field_def), parameter :: cylindrical_sym_tensor              &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'cylindrical_sym_tensor',                  &
     &                math = '$ (T_{ss}, T_{s \phi}, T_{sz},'           &
     &                    // ' T_{\phi \phi}, T_{\phi z}, T_{zz}) $')
!
!>        Field label for xx-component
!!         @f$ T_{xx} @f$
      type(field_def), parameter :: T_xx                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'xx',                                      &
     &                math = '$ T_{xx} $')
!>        Field label for xy-component
!!         @f$ T_{xy} @f$
      type(field_def), parameter :: T_xy                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'xy',                                      &
     &                math = '$ T_{xy} $')
!>        Field label for xz-component
!!         @f$ T_{xz}  @f$
      type(field_def), parameter :: T_xz                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'xz',                                      &
     &                math = '$ T_{xz} $')
!>        Field label for yy-component
!!         @f$ T_{yy} @f$
      type(field_def), parameter :: T_yy                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'yy',                                      &
     &                math = '$ T_{yy} $')
!>        Field label for yz-component
!!         @f$ T_{yz} @f$
      type(field_def), parameter :: T_yz                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'yz',                                      &
     &                math = '$ T_{yz} $')
!>        Field label for zz-component
!!         @f$ T_{zz}  @f$
      type(field_def), parameter :: T_zz                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'zz',                                      &
     &                math = '$ T_{zz} $')
!
!>        Field label for rr component
!!         @f$ T_{rr} @f$
      type(field_def), parameter :: T_rr                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'rr',                                      &
     &                math = '$ T_{rr} $')
!>        Field label for r-theta component
!!         @f$ T_{r \theta}  @f$
      type(field_def), parameter :: T_rt                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'rt',                                      &
     &                math = '$ T_{r \theta} $')
!>        Field label for r-phi component
!!         @f$ T_{r \phi} @f$
      type(field_def), parameter :: T_rp                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'rp',                                      &
     &                math = '$ T_{r \phi} $')
!>        Field label for theta-theta component
!!         @f$ T_{\theta \theta}  @f$
      type(field_def), parameter :: T_tt                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'tt',                                      &
     &                math = '$ T_{\theta \theta} $')
!>        Field label for theta-phi component
!!         @f$ T_{\theta \phi} @f$
      type(field_def), parameter :: T_tp                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'tp',                                      &
     &                math = '$ T_{\theta \phi} $')
!>        Field label for phi-phi component
!!         @f$ T_{\phi \phi} @f$
      type(field_def), parameter :: T_pp                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pp',                                      &
     &                math = '$ T_{\phi \phi} $')
!
!>        Field label for ss-component
!!         @f$ T_{ss} @f$
      type(field_def), parameter :: T_ss                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'ss',                                      &
     &                math = '$ T_{ss} $')
!>        Field label for s-phi-component
!!         @f$ T_{s \phi} @f$
      type(field_def), parameter :: T_sp                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'sp',                                      &
     &                math = '$ T_{s \phi} $')
!>        Field label for sz-component
!!         @f$ T_{sz}  @f$
      type(field_def), parameter :: T_sz                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'sz',                                      &
     &                math = '$ T_{sz} $')
!>        Field label for phi-z-component
!!         @f$ T_{\phi z} @f$
      type(field_def), parameter :: T_pz                                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pz',                                      &
     &                math = '$ T_{\phi z} $')
!
!
!>        Field label for asymmetric tensor
!!         @f$ (T_{xy}, T_{xz}, T_{yz}) @f$
      type(field_def), parameter :: asym_tensor                         &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'asym_tensor',                             &
     &                math = '$ (T_{xy}, T_{xz}, T_{yz}) $')
!>        Field label for symmetric tensor on spherical coordinate
!!         @f$ (T_{r \theta}, T_{r \phi}, T_{\theta \phi}) @f$
      type(field_def), parameter :: spherical_asym_tensor               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'spherical_asym_tensor',                   &
     &                math = '$ (T_{r \theta}, T_{r \phi},'             &
     &                    // ' T_{\phi \phi}) $')
!>        Field label for symmetric tensor on cylindrical coordinate
!!         @f$ (T_{s \phi}, T_{sz}, T_{\phi z}) @f$
      type(field_def), parameter :: cylindrical_asym_tensor             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'cylindrical_asym_tensor',                 &
     &                math = '$ (T_{s \phi}, T_{sz}, T_{\phi z}) $')
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_scalar_comp()
      num_flag_scalar_comp = ntype_scalar
      return
      end function num_flag_scalar_comp
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_vector_comp()
      num_flag_vector_comp = ntype_vector
      return
      end function num_flag_vector_comp
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_sym_tensor_comp()
      num_flag_sym_tensor_comp = ntype_tensor
      return
      end function num_flag_sym_tensor_comp
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_asym_tensor_comp()
      num_flag_asym_tensor_comp = ntype_as_tsr
      return
      end function num_flag_asym_tensor_comp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_flag_scalar_comp(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ntype_scalar)
      character(len = kchara), intent(inout) :: names(ntype_scalar)
      character(len = kchara), intent(inout) :: maths(ntype_scalar)
!
!
      call set_field_labels(scalar,                                     &
     &    n_comps( 1), names( 1), maths( 1))
!
      end subroutine set_flag_scalar_comp
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_vector_comp(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ntype_vector)
      character(len = kchara), intent(inout) :: names(ntype_vector)
      character(len = kchara), intent(inout) :: maths(ntype_vector)
!
!
      call set_field_labels(vector,                                     &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(spherical_vector,                           &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(cylindrical_vector,                         &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(magnitude,                                  &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(V_x,                                        &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(V_y,                                        &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(V_z,                                        &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(V_r,                                        &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(V_theta,                                    &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(V_phi,                                      &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(V_s,                                        &
     &    n_comps(11), names(11), maths(11))
!
      end subroutine set_flag_vector_comp
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_sym_tensor_comp(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ntype_tensor)
      character(len = kchara), intent(inout) :: names(ntype_tensor)
      character(len = kchara), intent(inout) :: maths(ntype_tensor)
!
!
      call set_field_labels(sym_tensor,                                 &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(spherical_sym_tensor,                       &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(cylindrical_sym_tensor,                     &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(magnitude,                                  &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(T_xx,                                       &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(T_xy,                                       &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(T_xz,                                       &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(T_yy,                                       &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(T_yz,                                       &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(T_zz,                                       &
     &    n_comps(10), names(10), maths(10))
!
      call set_field_labels(T_rr,                                       &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(T_rt,                                       &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(T_rp,                                       &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(T_tt,                                       &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(T_tp,                                       &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(T_pp,                                       &
     &    n_comps(16), names(16), maths(16))
!
      call set_field_labels(T_ss,                                       &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(T_sp,                                       &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(T_sz,                                       &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(T_pp,                                       &
     &    n_comps(20), names(20), maths(20))
      call set_field_labels(T_pz,                                       &
     &    n_comps(21), names(21), maths(21))
      call set_field_labels(T_zz,                                       &
     &    n_comps(22), names(22), maths(22))
!
      end subroutine set_flag_sym_tensor_comp
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_asym_tensor_comp(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ntype_as_tsr)
      character(len = kchara), intent(inout) :: names(ntype_as_tsr)
      character(len = kchara), intent(inout) :: maths(ntype_as_tsr)
!
!
      call set_field_labels(asym_tensor,                                &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(spherical_asym_tensor,                      &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(cylindrical_asym_tensor,                    &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(magnitude,                                  &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(T_xy,                                       &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(T_xz,                                       &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(T_yz,                                       &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(T_rt,                                       &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(T_rp,                                       &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(T_tp,                                       &
     &    n_comps(10), names(10), maths(10))
!
      call set_field_labels(T_sp,                                       &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(T_sz,                                       &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(T_pz,                                       &
     &    n_comps(13), names(13), maths(13))
!
      end subroutine set_flag_asym_tensor_comp
!
! ----------------------------------------------------------------------
!
      end module m_component_flags
