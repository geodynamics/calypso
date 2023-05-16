!>@file   t_sph_grid_maker_in_sim.f90
!!@brief  module t_sph_grid_maker_in_sim
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine set_ctl_4_sph_grid_maker(nprocs, psph_ctl,           &
!!     &          sph_file_prefix, sph_file_param, sph_maker, ierr)
!!        type(parallel_sph_shell_control), intent(in) :: psph_ctl
!!        type(read_character_item), intent(in) :: sph_file_prefix
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!@endverbatim
!
      module t_sph_grid_maker_in_sim
!
      use m_precision
!
      use t_spheric_parameter
      use t_const_spherical_grid
!
      implicit none
!
!>      Structure to check and construct spherical shell mesh
      type sph_grid_maker_in_sim
!>        Switch to construct spherical shell grid
        logical :: make_SPH_flag =    .FALSE.
!>        Switch to output constructed spherical shell grid
        logical :: mesh_output_flag = .FALSE.
!
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!>        Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_4_sph_grid_maker(nprocs, psph_ctl,             &
     &          sph_file_prefix, sph_file_param, sph_maker, ierr)
!
      use t_ctl_data_gen_sph_shell
      use t_control_array_character
      use t_file_IO_parameter
      use m_file_format_switch
      use set_ctl_4_shell_grids
!
      integer, intent(in) :: nprocs
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(read_character_item), intent(in) :: sph_file_prefix
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      integer(kind = kint), intent(inout) :: ierr
!
!   set spherical shell parameters
!
      ierr = 0
      if(psph_ctl%iflag_sph_shell .gt. 0) then
        sph_maker%make_SPH_flag = .TRUE.
        sph_maker%mesh_output_flag = .TRUE.
!
        if(sph_file_param%iflag_format .eq. id_no_file                  &
     &    .or. sph_file_prefix%iflag .eq. 0) then
          sph_maker%mesh_output_flag = .FALSE.
        end if
!
        if (iflag_debug.gt.0) write(*,*) 'set_control_4_shell_grids'
        call set_control_4_shell_grids                                  &
     &     (nprocs, psph_ctl%spctl, psph_ctl%sdctl,                     &
     &      sph_maker%sph_tmp, sph_maker%gen_sph, ierr)
      end if
!
      end subroutine set_ctl_4_sph_grid_maker
!
! ----------------------------------------------------------------------
!
      end module t_sph_grid_maker_in_sim
