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
!!      subroutine alloc_reft_rj_data_t(nri, sph_phys)
!!
!!      subroutine dealloc_phys_rj_name_t(sph_phys)
!!      subroutine dealloc_reft_rj_data_t(sph_phys)
!!
!!      subroutine check_rj_spectr_name_t(sph_phys)
!!      subroutine check_rj_spectr_data_t(my_rank, nnod_rj, sph_phys)
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module t_sph_spectr_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
      type phys_w_start
!>        Structure for field data
        type(phys_data) :: fld
!>        Start field address of scalar fields
        integer (kind=kint) :: istart_scalar
!>        Start field address of vector fields
        integer (kind=kint) :: istart_vector
!>        Start field address of tensor fields
        integer (kind=kint) :: istart_tensor
!
!>        Number of fields of scalar fields
        integer (kind=kint) :: num_scalar
!>        Number of fields of vector fields
        integer (kind=kint) :: num_vector
!>        Number of fields of tensor fields
        integer (kind=kint) :: num_tensor
      end type phys_w_start
!
!>!!       Structure for spectr data
!!      Number of fields for spectrum data  @f$ f(r,j) @f$
!!          num_phys_rj = phys_rj%fld%num_phys
!!      Total number of components for spectrum data
!!      @f$ f(r,j) @f$
!!          ntot_phys_rj = phys_rj%fld%ntot_phys
!!      Number of components for each field @f$ f(r,j) @f$
!!          num_phys_comp_rj = phys_rj%fld%num_component
!!      End address of d_rj for each field @f$ f(r,j) @f$
!!          istack_phys_comp_rj = phys_rj%fld%istack_component
!!      Field name for @f$ f(r,j) @f$
!!          phys_name_rj = phys_rj%fld%phys_name
!!      Spectr data @f$ f(r,j) @f$
!!          d_rj = phys_rj%fld%d_fld
!!      Integer flag for monitoring output
!!       for spectr data @f$ f(r,j) @f$
!!          iflag_monitor_rj = phys_rj%fld%iflag_monitor
!!      Number of fields for visualization output
!!       @f$ f(r,\theta,\phi) @f$
!!          num_phys_rj_vis = phys_rj%fld%num_phys_viz
!!      Total number of components  for visualization output
!!       @f$ f(r,\theta,\phi) @f$
!!          ntot_comp_rj_vis = phys_rj%fld%ntot_phys_viz
      type sph_phys_data
!>        Structure for spectr data @f$ f(r,j) @f$
        type(phys_w_start) :: phys_rj
!>    reference temerature spectr @f$ f(r,j) @f$
!!@verbatim
!!        reftemp_rj(kr,0) ... T_0
!!        reftemp_rj(kr,1) ... d T_0 / dr
!!@endverbatim
        real (kind=kreal), pointer :: reftemp_rj(:,:)
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
      call alloc_phys_name_type(sph_phys%phys_rj%fld)
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
      call alloc_phys_data_type(nnod_rj, sph_phys%phys_rj%fld)
!
      end subroutine alloc_phys_rj_data_t
!
!  --------------------------------------------------------------------
!
      subroutine alloc_reft_rj_data_t(nri, sph_phys)
!
      integer(kind = kint), intent(in) :: nri
      type(sph_phys_data), intent(inout) :: sph_phys
!
      allocate( sph_phys%reftemp_rj(nri,0:1)   )
      sph_phys%reftemp_rj =  0.0d0
!
      end subroutine alloc_reft_rj_data_t
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine dealloc_phys_rj_name_t(sph_phys)
!
      type(sph_phys_data), intent(inout) :: sph_phys
!
!
      call dealloc_phys_data_type(sph_phys%phys_rj%fld)
      call dealloc_phys_name_type(sph_phys%phys_rj%fld)
!
      end subroutine dealloc_phys_rj_name_t
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_reft_rj_data_t(sph_phys)
!
      type(sph_phys_data), intent(inout) :: sph_phys
!
      deallocate( sph_phys%reftemp_rj )
!
      end subroutine dealloc_reft_rj_data_t
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_name_t(sph_phys)
!
      type(sph_phys_data), intent(in) :: sph_phys
!
!
      write(*,*) 'sph_phys%phys_rj%fld'
      call check_nodal_field_name_type(sph_phys%phys_rj%fld)
!
      end subroutine check_rj_spectr_name_t
!
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_data_t(my_rank, nnod_rj, sph_phys)
!
      integer(kind = kint), intent(in) :: my_rank, nnod_rj
      type(sph_phys_data), intent(in) :: sph_phys
!
      integer(kind = kint) :: i_fld
!
!
      write(*,*) 'sph_phys%phys_rj%fld'
      call check_nodal_field_name_type(sph_phys%phys_rj%fld)
!
      write(50+my_rank,*) 'sph_phys%phys_rj%fld'
      do i_fld = 1, sph_phys%phys_rj%fld%num_phys
        call check_nodal_data_type(my_rank, sph_phys%phys_rj%fld,       &
     &      nnod_rj, sph_phys%phys_rj%fld%ntot_phys,                    &
     &      sph_phys%phys_rj%fld%istack_component(i_fld-1)+1)
      end do
!
      end subroutine check_rj_spectr_data_t
!
!  --------------------------------------------------------------------
!
      end module t_sph_spectr_data
