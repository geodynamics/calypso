!>@file   t_spectr_data_4_assemble.f90
!!@brief  module t_spectr_data_4_assemble
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine alloc_spectr_data_4_assemble(sph_asbl)
!!      subroutine dealloc_spectr_data_4_assemble(sph_asbl)
!!        type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!!@endverbatim
!
      module t_spectr_data_4_assemble
!
      use m_precision
      use t_SPH_mesh_field_array
      use t_SPH_mesh_field_data
      use t_phys_data
      use t_field_data_IO
      use t_time_data
      use t_sph_radial_interpolate
      use parallel_assemble_sph
!
      implicit none
!
!
      type spectr_data_4_assemble
        integer :: np_sph_org
        type(sph_mesh_array) :: org_sph_array
!
        integer :: np_sph_new
        type(SPH_mesh_field_data) :: new_sph_data
!
        type(rj_assemble_tbl), allocatable :: j_table(:)
!
        type(field_IO) :: new_fst_IO
        type(time_data) :: fst_time_IO
!
        type(sph_radial_interpolate) :: r_itp
      end type spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_spectr_data_4_assemble(sph_asbl)
!
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
!
      call alloc_sph_mesh_array(sph_asbl%np_sph_org,                    &
     &                          sph_asbl%org_sph_array)
      allocate(sph_asbl%j_table(sph_asbl%np_sph_org))
!
      end subroutine alloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_spectr_data_4_assemble(sph_asbl)
!
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, sph_asbl%org_sph_array%num_pe
        call dealloc_mode_table_4_assemble(sph_asbl%j_table(ip))
      end do
      deallocate(sph_asbl%j_table)
!
      call dealloc_sph_mesh_array(sph_asbl%org_sph_array)
!
      end subroutine dealloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      end module t_spectr_data_4_assemble
