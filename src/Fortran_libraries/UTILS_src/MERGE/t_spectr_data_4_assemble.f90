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
!!      subroutine dealloc_spectr_data_4_assemble                       &
!!     &         (id_rank, nprocs, sph_asbl)
!!        type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!!@endverbatim
!
      module t_spectr_data_4_assemble
!
      use m_precision
      use t_SPH_mesh_field_data
      use t_phys_data
      use t_field_data_IO
      use t_time_data
      use r_interpolate_marged_sph
      use parallel_assemble_sph
!
      implicit none
!
!
      type spectr_data_4_assemble
        integer :: np_sph_org
        type(sph_mesh_data), allocatable :: org_sph_mesh(:)
        type(phys_data), allocatable ::     org_sph_phys(:)
!
        integer :: np_sph_new
        type(sph_mesh_data), allocatable :: new_sph_mesh(:)
        type(phys_data), allocatable ::     new_sph_phys(:)
!
        type(rj_assemble_tbl), allocatable :: j_table(:,:)
!
        type(field_IO) :: new_fst_IO
        type(time_data) :: fst_time_IO
!
        type(sph_radial_itp_data) :: r_itp
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
      allocate( sph_asbl%org_sph_mesh(sph_asbl%np_sph_org) )
      allocate( sph_asbl%org_sph_phys(sph_asbl%np_sph_org) )
      allocate( sph_asbl%new_sph_mesh(sph_asbl%np_sph_new) )
      allocate( sph_asbl%new_sph_phys(sph_asbl%np_sph_new) )
      allocate(sph_asbl%j_table(sph_asbl%np_sph_org,                    &
     &                          sph_asbl%np_sph_new))
!
      end subroutine alloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_spectr_data_4_assemble                         &
     &         (id_rank, nprocs, sph_asbl)
!
      integer, intent(in) :: id_rank, nprocs
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
      integer(kind = kint) :: ip, jp
!
!
      do jp = 1, sph_asbl%np_sph_new
        if(mod(jp-1,nprocs) .ne. id_rank) cycle
        do ip = 1, sph_asbl%np_sph_org
          call dealloc_mode_table_4_assemble(sph_asbl%j_table(ip,jp))
        end do
      end do
      deallocate(sph_asbl%j_table)
!
      deallocate(sph_asbl%org_sph_mesh, sph_asbl%org_sph_phys)
      deallocate(sph_asbl%new_sph_mesh, sph_asbl%new_sph_phys)
!
      end subroutine dealloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      end module t_spectr_data_4_assemble
