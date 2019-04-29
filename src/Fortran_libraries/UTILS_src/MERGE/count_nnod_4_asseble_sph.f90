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
      integer(kind = kint), allocatable :: nnod_list_lc(:)
      integer(kind = kint), allocatable :: nnod_list(:)
      integer(kind = kint_gl), allocatable :: istack_nnod_list(:)
      private :: nnod_list_lc, nnod_list, istack_nnod_list
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
      integer, intent(in) :: np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!
      type(field_IO), intent(inout) :: new_fst_IO
!
      integer(kind = kint) :: jp, irank_new
      integer(kind = kint_gl) :: num64
!
!
      allocate(nnod_list_lc(np_sph_new))
      allocate(nnod_list(np_sph_new))
      allocate(istack_nnod_list(0:np_sph_new))
      nnod_list_lc(1:np_sph_new) = 0
      nnod_list(1:np_sph_new) = 0
!
      do jp = 1, np_sph_new
        irank_new = jp - 1
        if(mod(irank_new,nprocs) .ne. my_rank) cycle
        nnod_list_lc(jp) = new_sph_mesh(jp)%sph%sph_rj%nnod_rj
      end do
!
      num64 = int(np_sph_new, KIND(num64))
      call calypso_mpi_allreduce_int                                    &
     &   (nnod_list_lc, nnod_list, num64, MPI_SUM)
!
      istack_nnod_list(0) = 0
      do jp = 1, np_sph_new
        istack_nnod_list(jp) = istack_nnod_list(jp-1) + nnod_list(jp)
      end do
      call alloc_merged_field_stack(np_sph_new, new_fst_IO)
      new_fst_IO%istack_numnod_IO(0:np_sph_new)                         &
     &     = istack_nnod_list(0:np_sph_new)
!
      deallocate(istack_nnod_list)
      deallocate(nnod_list)
      deallocate(nnod_list_lc)
!
      end subroutine s_count_nnod_4_asseble_sph
!
! ----------------------------------------------------------------------
!
      end module count_nnod_4_asseble_sph
