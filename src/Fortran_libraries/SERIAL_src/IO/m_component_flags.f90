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
!!      subroutine set_xyz_direction_array(array_c2i)
!!      subroutine set_xyzw_direction_array(array_c2i)
!!      subroutine set_scalar_direction_array(array_c2i)
!!      subroutine set_vector_direction_array(array_c2i)
!!      subroutine set_sym_tensor_direction_array(array_c2i)
!!      subroutine set_asym_tensor_direction_array(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
!>        Field label for w-component
!!         @f$ V_{w}  @f$
      type(field_def), parameter :: V_w                                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'w',                                       &
     &                math = '$ V_{w} $')
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
      subroutine set_xyz_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(V_x, array_c2i)
      call set_field_label_to_ctl(V_y, array_c2i)
      call set_field_label_to_ctl(V_z, array_c2i)
!
      end subroutine set_xyz_direction_array
!
! ----------------------------------------------------------------------
!
      subroutine set_xyzw_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(V_x, array_c2i)
      call set_field_label_to_ctl(V_y, array_c2i)
      call set_field_label_to_ctl(V_z, array_c2i)
      call set_field_label_to_ctl(V_w, array_c2i)
!
      end subroutine set_xyzw_direction_array
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_scalar_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(scalar, array_c2i)
!
      end subroutine set_scalar_direction_array
!
! ----------------------------------------------------------------------
!
      subroutine set_vector_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(vector, array_c2i)
      call set_field_label_to_ctl(spherical_vector, array_c2i)
      call set_field_label_to_ctl(cylindrical_vector, array_c2i)
!
      call set_field_label_to_ctl(magnitude, array_c2i)
!
      call set_field_label_to_ctl(V_x, array_c2i)
      call set_field_label_to_ctl(V_y, array_c2i)
      call set_field_label_to_ctl(V_z, array_c2i)
!
      call set_field_label_to_ctl(V_r, array_c2i)
      call set_field_label_to_ctl(V_theta, array_c2i)
      call set_field_label_to_ctl(V_phi, array_c2i)
      call set_field_label_to_ctl(V_s, array_c2i)
!
      end subroutine set_vector_direction_array
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_tensor_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(sym_tensor, array_c2i)
      call set_field_label_to_ctl(spherical_sym_tensor, array_c2i)
      call set_field_label_to_ctl(cylindrical_sym_tensor, array_c2i)
!
      call set_field_label_to_ctl(magnitude, array_c2i)
!
      call set_field_label_to_ctl(T_xx, array_c2i)
      call set_field_label_to_ctl(T_xy, array_c2i)
      call set_field_label_to_ctl(T_xz, array_c2i)
      call set_field_label_to_ctl(T_yy, array_c2i)
      call set_field_label_to_ctl(T_yz, array_c2i)
      call set_field_label_to_ctl(T_zz, array_c2i)
!
      call set_field_label_to_ctl(T_rr, array_c2i)
      call set_field_label_to_ctl(T_rt, array_c2i)
      call set_field_label_to_ctl(T_rp, array_c2i)
      call set_field_label_to_ctl(T_tt, array_c2i)
      call set_field_label_to_ctl(T_tp, array_c2i)
      call set_field_label_to_ctl(T_pp, array_c2i)
!
      call set_field_label_to_ctl(T_ss, array_c2i)
      call set_field_label_to_ctl(T_sp, array_c2i)
      call set_field_label_to_ctl(T_sz, array_c2i)
      call set_field_label_to_ctl(T_pp, array_c2i)
      call set_field_label_to_ctl(T_pz, array_c2i)
      call set_field_label_to_ctl(T_zz, array_c2i)
!
      end subroutine set_sym_tensor_direction_array
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_tensor_direction_array(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(asym_tensor, array_c2i)
      call set_field_label_to_ctl(spherical_asym_tensor, array_c2i)
      call set_field_label_to_ctl(cylindrical_asym_tensor, array_c2i)
      call set_field_label_to_ctl(magnitude, array_c2i)
!
      call set_field_label_to_ctl(T_xy, array_c2i)
      call set_field_label_to_ctl(T_xz, array_c2i)
      call set_field_label_to_ctl(T_yz, array_c2i)
!
      call set_field_label_to_ctl(T_rt, array_c2i)
      call set_field_label_to_ctl(T_rp, array_c2i)
      call set_field_label_to_ctl(T_tp, array_c2i)
!
      call set_field_label_to_ctl(T_sp, array_c2i)
      call set_field_label_to_ctl(T_sz, array_c2i)
      call set_field_label_to_ctl(T_pz, array_c2i)
!
      end subroutine set_asym_tensor_direction_array
!
! ----------------------------------------------------------------------
!
      end module m_component_flags
