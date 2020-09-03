!>@file   count_nnod_4_asseble_sph.f90
!!@brief  module count_nnod_4_asseble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Count number of nodes for data assemble
!!
!!@verbatim
!!      subroutine s_count_nnod_4_asseble_sph                           &
!!     &         (np_sph_new, new_sph_mesh, new_fst_IO)
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!!        type(field_IO), intent(inout) :: new_fst_IO
!!@endverbatim
!
      module count_nnod_4_asseble_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_SPH_mesh_field_data
      use t_field_data_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_count_nnod_4_asseble_sph                             &
     &         (np_sph_new, new_sph_mesh, new_fst_IO)
!
      use calypso_mpi_int
!
      integer, intent(in) :: np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh
!
      type(field_IO), intent(inout) :: new_fst_IO
!
      integer(kind = kint) :: jp
      integer(kind = kint), allocatable :: nnod_list(:)
!
!
      allocate(nnod_list(nprocs))
      nnod_list(1:nprocs) = 0
!
      call calypso_mpi_allgather_one_int                                &
     &   (new_sph_mesh%sph%sph_rj%nnod_rj, nnod_list)
!
      call alloc_merged_field_stack(np_sph_new, new_fst_IO)
      new_fst_IO%istack_numnod_IO(0) = 0
      do jp = 1, np_sph_new
        new_fst_IO%istack_numnod_IO(jp)                                 &
     &      = new_fst_IO%istack_numnod_IO(jp-1) + nnod_list(jp)
      end do
!
      deallocate(nnod_list)
!
      end subroutine s_count_nnod_4_asseble_sph
!
! ----------------------------------------------------------------------
!
      end module count_nnod_4_asseble_sph
