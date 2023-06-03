!>@file   t_ctl_param_psf_compares.f90
!!@brief  module t_ctl_param_psf_compares
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief parameters to compare multiple sectiong data
!!
!!@verbatim
!!      subroutine set_control_for_psf_compare(psf_cmp_ctls, psf_cmp)
!!        type(psf_compare_control), intent(in) :: psf_cmp_ctls
!!        type(psf_compare_param), intent(inout):: psf_cmp
!!      integer(kind = kint) function compare_psf_data(psf_cmp)
!!        type(psf_compare_param), intent(in):: psf_cmp
!!@endverbatim
!
      module t_ctl_param_psf_compares
!
      use m_precision
      use m_machine_parameter
      use t_file_IO_parameter
!
      type psf_compare_param
        type(field_IO_params) :: psf1_file_param
        type(field_IO_params) :: psf2_file_param
        integer(kind = kint) :: istep_psf
      end type psf_compare_param
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine set_control_for_psf_compare(psf_cmp_ctls, psf_cmp)
!
      use t_ctl_data_psf_compare
      use set_sections_file_ctl
!
      type(psf_compare_control), intent(in) :: psf_cmp_ctls
      type(psf_compare_param), intent(inout):: psf_cmp
!
      character(len=kchara), parameter :: default_psf_prefix = 'psf'
!
      call set_read_psf_file_ctl(default_psf_prefix,                    &
     &    psf_cmp_ctls%first_psf%file_prefix_ctl,                       &
     &    psf_cmp_ctls%first_psf%file_format_ctl,                       &
     &    psf_cmp%psf1_file_param)
      call set_read_psf_file_ctl(default_psf_prefix,                    &
     &    psf_cmp_ctls%second_psf%file_prefix_ctl,                      &
     &    psf_cmp_ctls%second_psf%file_format_ctl,                      &
     &    psf_cmp%psf2_file_param)
!
      psf_cmp%istep_psf = psf_cmp_ctls%i_step_surface_ctl%intvalue
!
      end subroutine set_control_for_psf_compare
!
!  --------------------------------------------------------------------
!
      subroutine compare_psf_data(psf_cmp, icount_error)
!
      use m_precision
      use m_machine_parameter
!
      use t_ucd_data
      use t_psf_results
      use append_phys_data
      use compare_mesh_structures
!
      implicit none
!
      type(psf_compare_param), intent(in):: psf_cmp
      integer(kind = kint), intent(inout) :: icount_error
!
      type(psf_results) :: psf_1, psf_2
      type(time_data) :: t_IO_u
      type(ucd_data):: psf_ucd
!
      integer :: np_ucd
      integer(kind = kint) :: icou_error
!
!
      call load_psf_data_to_link_IO                                     &
     &   (psf_cmp%istep_psf, psf_cmp%psf1_file_param,                   &
     &    np_ucd, t_IO_u, psf_1, psf_ucd)
      call load_psf_data_to_link_IO                                     &
     &   (psf_cmp%istep_psf, psf_cmp%psf2_file_param,                   &
     &    np_ucd, t_IO_u, psf_2, psf_ucd)
!
      call compare_node_position(0, psf_1%psf_nod, psf_2%psf_nod,       &
     &                           icount_error)
      call compare_ele_connect(0, psf_1%psf_ele, psf_2%psf_ele,         &
     &                         icou_error)
      icount_error = icount_error + icou_error
      call compare_field_data(psf_1%psf_phys, psf_2%psf_phys,           &
     &                        icou_error)
      icount_error = icount_error + icou_error
!
      if(icount_error .eq. 0) then
        write(*,*) trim(psf_cmp%psf1_file_param%file_prefix), ' and ',  &
     &             trim(psf_cmp%psf2_file_param%file_prefix),           &
     &            ' have same data.'
      else
        write(*,*) trim(psf_cmp%psf1_file_param%file_prefix), ' and ',  &
     &             trim(psf_cmp%psf2_file_param%file_prefix),           &
     &            ' is different.'
      end if
!
      call dealloc_psf_results(psf_1)
      call dealloc_psf_results(psf_2)
!
      end subroutine compare_psf_data
!
!-----------------------------------------------------------------------
!
      end module t_ctl_param_psf_compares
