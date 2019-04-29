!>@file   t_sph_spectr_data.f90
!!@brief  module t_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!      subroutine alloc_phys_rj_name_t(sph_phys)
!!      subroutine alloc_phys_rj_data_t(nnod_rj, sph_phys)
!!
!!      subroutine dealloc_phys_rj_name_t(sph_phys)
!!
!!      subroutine check_rj_spectr_name_t(sph_phys)
!!      subroutine check_rj_spectr_data_t(id_rank, sph_phys)
!!@endverbatim
!!
!!@n @param id_rank process ID
!
      module t_sph_spectr_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!>!!       Structure for spectr data
!!      Number of fields for spectrum data  @f$ f(r,j) @f$
!!          num_phys_rj = rj_fld%num_phys
!!      Total number of components for spectrum data
!!      @f$ f(r,j) @f$
!!          ntot_phys_rj = rj_fld%ntot_phys
!!      Number of components for each field @f$ f(r,j) @f$
!!          num_phys_comp_rj = rj_fld%num_component
!!      End address of d_rj for each field @f$ f(r,j) @f$
!!          istack_phys_comp_rj = rj_fld%istack_component
!!      Field name for @f$ f(r,j) @f$
!!          phys_name_rj = rj_fld%phys_name
!!      Spectr data @f$ f(r,j) @f$
!!          d_rj = rj_fld%d_fld
!!      Integer flag for monitoring output
!!       for spectr data @f$ f(r,j) @f$
!!          iflag_monitor_rj = rj_fld%iflag_monitor
!!      Number of fields for visualization output
!!       @f$ f(r,\theta,\phi) @f$
!!          num_phys_rj_vis = rj_fld%num_phys_viz
!!      Total number of components  for visualization output
!!       @f$ f(r,\theta,\phi) @f$
!!          ntot_comp_rj_vis = rj_fld%ntot_phys_viz
      type sph_phys_data
!>        Structure for spectr data @f$ f(r,j) @f$
        type(phys_data) :: rj_fld
      end type sph_phys_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_phys_rj_name_t(sph_phys)
!
      type(sph_phys_data), intent(inout) :: sph_phys
!
!
      call alloc_phys_name_type(sph_phys%rj_fld)
!
      end subroutine alloc_phys_rj_name_t
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine alloc_phys_rj_data_t(nnod_rj, sph_phys)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(sph_phys_data), intent(inout) :: sph_phys
!
!
      call alloc_phys_data_type(nnod_rj, sph_phys%rj_fld)
!
      end subroutine alloc_phys_rj_data_t
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_phys_rj_name_t(sph_phys)
!
      type(sph_phys_data), intent(inout) :: sph_phys
!
!
      call dealloc_phys_data_type(sph_phys%rj_fld)
      call dealloc_phys_name_type(sph_phys%rj_fld)
!
      end subroutine dealloc_phys_rj_name_t
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_name_t(sph_phys)
!
      type(sph_phys_data), intent(in) :: sph_phys
!
      integer(kind = kint), parameter :: id_six = 6
!
      write(*,*) 'check_nodal_field_name_type for sph_phys%rj_fld'
      call check_nodal_field_name_type(id_six, sph_phys%rj_fld)
!
      end subroutine check_rj_spectr_name_t
!
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_data_t(id_rank, sph_phys)
!
      integer, intent(in) :: id_rank
      type(sph_phys_data), intent(in) :: sph_phys
!
      integer(kind = kint) :: i_fld, id
!
!
      write(*,*) 'check_nodal_field_name_type for sph_phys%rj_fld'
      id = 50+id_rank
      call check_nodal_field_name_type(id,sph_phys%rj_fld)
!
      write(id,*) 'sph_phys%rj_fld'
      do i_fld = 1, sph_phys%rj_fld%num_phys
        call check_nodal_data(id, sph_phys%rj_fld,                      &
     &      sph_phys%rj_fld%ntot_phys,                                  &
     &      sph_phys%rj_fld%istack_component(i_fld-1)+1)
      end do
!
      end subroutine check_rj_spectr_data_t
!
!  --------------------------------------------------------------------
!
      end module t_sph_spectr_data
