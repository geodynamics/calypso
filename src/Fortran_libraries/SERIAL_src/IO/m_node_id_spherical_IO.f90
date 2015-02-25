!
!      module m_node_id_spherical_IO
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine allocate_nod_id_sph_IO
!!      subroutine allocate_idx_sph_1d1_IO
!!      subroutine allocate_idx_sph_1d2_IO
!!      subroutine allocate_idx_sph_1d3_IO
!!
!!      subroutine deallocate_nod_id_sph_IO
!!      subroutine deallocate_idx_sph_1d1_IO
!!      subroutine deallocate_idx_sph_1d2_IO
!!      subroutine deallocate_idx_sph_1d3_IO
!!
!!      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!
      module m_node_id_spherical_IO
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: sph_rank_IO(3)
!
      integer(kind = kint) :: nidx_gl_sph_IO(1:3)
      integer(kind = kint) :: ltr_gl_IO
!
      integer(kind = kint) :: ndir_sph_IO
      integer(kind = kint) :: nnod_sph_IO
!
      integer(kind = kint_gl), allocatable :: inod_gl_sph_IO(:)
      integer(kind = kint), allocatable :: idx_gl_sph_IO(:,:)
!
      integer(kind = kint) :: nidx_sph_IO(3)
      integer(kind = kint) :: ist_sph_IO(3)
      integer(kind = kint) :: ied_sph_IO(3)
      integer(kind = kint) :: ncomp_itbl_1d_IO(3)
      integer(kind = kint), allocatable :: idx_gl_1_IO(:)
      integer(kind = kint), allocatable :: idx_gl_2_IO(:,:)
      integer(kind = kint), allocatable :: idx_gl_3_IO(:,:)
!
      real(kind = kreal), allocatable :: r_gl_1_IO(:)
!
!
      integer(kind = kint), parameter :: mesh_file_id = 14
!
      character(len=kchara) :: sph_file_head =     "in_sph"
      integer(kind = kint) :: iflag_sph_file_fmt = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_id_sph_IO
!
!
      allocate( inod_gl_sph_IO(nnod_sph_IO) )
      allocate( idx_gl_sph_IO(nnod_sph_IO,ndir_sph_IO) )
!
      inod_gl_sph_IO = 0
      idx_gl_sph_IO =  0
!
      end subroutine allocate_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d1_IO
!
      allocate( idx_gl_1_IO(nidx_sph_IO(1)) )
      allocate( r_gl_1_IO(nidx_sph_IO(1)) )
      idx_gl_1_IO =  0
      r_gl_1_IO = 0.0d0
!
      end subroutine allocate_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d2_IO
!
      allocate( idx_gl_2_IO(nidx_sph_IO(2),ncomp_itbl_1d_IO(2)) )
      idx_gl_2_IO =  0
!
      end subroutine allocate_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d3_IO
!
      allocate( idx_gl_3_IO(nidx_sph_IO(3),ncomp_itbl_1d_IO(3)) )
      idx_gl_3_IO =  0
!
      end subroutine allocate_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_id_sph_IO
!
      deallocate( inod_gl_sph_IO )
      deallocate( idx_gl_sph_IO  )
!
      end subroutine deallocate_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d1_IO
!
      deallocate( idx_gl_1_IO )
      deallocate( r_gl_1_IO )
!
      end subroutine deallocate_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d2_IO
!
      deallocate( idx_gl_2_IO )
!
      end subroutine deallocate_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d3_IO
!
      deallocate( idx_gl_3_IO )
!
      end subroutine deallocate_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
!
      iflag_sph_file_fmt = iflag_fmt
      write(sph_file_head,'(a)') trim(file_head)
!
      end subroutine set_sph_mesh_file_fmt_prefix
!
! -------------------------------------------------------------------
!
      end module m_node_id_spherical_IO
