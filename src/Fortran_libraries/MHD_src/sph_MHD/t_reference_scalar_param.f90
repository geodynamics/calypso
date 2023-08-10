!>@file   t_reference_scalar_param.f90
!!@brief  module t_reference_scalar_param
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!!@n    Mmodified by H. Matsui in Jan, 2017
!
!> @brief set reference fields for MHD simulation from control data
!!
!!@verbatim
!!      subroutine set_reference_scalar_ctl                             &
!!     &        (charaflag, ref_ctl, ref_param, takepiro)
!!      subroutine set_linear_ref_scalar_ctl                            &
!!     &         (ref_temp_ctl, low_temp_ctl, high_temp_ctl, ref_param)
!!      subroutine set_takepiro_scalar_ctl                              &
!!     &         (stratified_ctl, takepiro_ctl, takepiro)
!!        type(read_character_item), intent(in) :: ref_temp_ctl
!!        type(read_character_item), intent(in) :: stratified_ctl
!!        type(reference_point_control), intent(in) :: low_temp_ctl
!!        type(reference_point_control), intent(in) :: high_temp_ctl
!!        type(reference_point_control), intent(in) :: takepiro_ctl
!!        type(reference_scalar_param), intent(inout) :: ref_param
!!        type(takepiro_model_param), intent(inout) :: takepiro
!!@endverbatim
!
      module t_reference_scalar_param
!
      use m_precision
      use m_error_IDs
!
      use m_machine_parameter
      use t_file_IO_parameter
!
      implicit  none
!
!>      flag for solving full temperature
      integer (kind=kint), parameter :: id_no_ref_temp = 0
!>      flag to use referece temperature as a function of @f$ x @f$
      integer (kind=kint), parameter :: id_x_ref_temp = 0
!>      flag to use referece temperature as a function of @f$ y @f$
      integer (kind=kint), parameter :: id_y_ref_temp = 0
!>      flag to use referece temperature as a function of @f$ z @f$
      integer (kind=kint), parameter :: id_z_ref_temp = 0
!>      flag to use referece temperature as a function of @f$ r @f$
      integer (kind=kint), parameter :: id_sphere_ref_temp = 100
!>      flag to obtain diffusive profile numerically
      integer (kind=kint), parameter :: id_read_file = 10
!>      flag to obtain diffusive profile numerically
      integer (kind=kint), parameter :: id_numerical_solution = 999
!>      flag to use linearly decrease referece temperature
!!      as a function of @f$ r @f$
      integer (kind=kint), parameter :: id_linear_r_ref_temp = 200
!>      takepiro model flag
      integer (kind=kint), parameter :: id_takepiro_temp = 1000
!
!
      character(len = kchara), parameter :: label_none = 'none'
      character(len = kchara), parameter                                &
     &               :: label_sph_shell = 'spherical_shell'
      character(len = kchara), parameter                                &
     &               :: label_sph_const_heat = 'sph_constant_heat'
      character(len = kchara), parameter :: label_takepiro = 'takepiro'
!
      character(len = kchara), parameter :: label_linear_x = 'linear_x'
      character(len = kchara), parameter :: label_linear_y = 'linear_y'
      character(len = kchara), parameter :: label_linear_z = 'linear_z'
!
      character(len = kchara), parameter                                &
     &               :: label_get_numerical = 'numerical_solution'
      character(len = kchara), parameter :: label_load_file = 'file'
!
      type reference_scalar_param
!>        switch to use perturbation of scalar
        logical :: flag_ref_field = .FALSE.
!>        temperature setting
        integer (kind=kint) :: iflag_reference = id_no_ref_temp
!>        Structure of file name
        type(field_IO_params) :: ref_file_IO
!
!>        reference lowest temperature (at upper boundary)
        real (kind = kreal) :: low_value
!>        reference highest temperature (at lower boundary)
        real (kind = kreal) :: high_value
!>        position at lowest temperature (upper boundary)
        real (kind = kreal) :: depth_top
!>        position at highest temperature (lower boundary)
        real (kind = kreal) :: depth_bottom
      end type reference_scalar_param
!
!
      type takepiro_model_param
!>       Parameter for stratified layer (amplitude)
        real  (kind=kreal) :: stratified_sigma
!>       Parameter for stratified layer (thckness)
        real  (kind=kreal) :: stratified_width
!>       Parameter for stratified layer (radius)
        real  (kind=kreal) :: stratified_outer_r
      end type takepiro_model_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_reftemp_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call set_sph_reftemp_list_array(array_c)
!
      call append_c_to_ctl_array(label_linear_x, array_c)
      call append_c_to_ctl_array(label_linear_y, array_c)
      call append_c_to_ctl_array(label_linear_z, array_c)
!
      end subroutine set_reftemp_list_array
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_reftemp_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(label_none, array_c)
      call append_c_to_ctl_array(label_sph_shell, array_c)
      call append_c_to_ctl_array(label_get_numerical, array_c)
      call append_c_to_ctl_array(label_load_file, array_c)
      call append_c_to_ctl_array(label_sph_const_heat, array_c)
!
      end subroutine set_sph_reftemp_list_array
!
! -----------------------------------------------------------------------
!
      end module t_reference_scalar_param
